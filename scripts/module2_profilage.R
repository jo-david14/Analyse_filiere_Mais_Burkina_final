###############################################################################
# MODULE 2 - PROFILAGE DES MENAGES
###############################################################################
# Objectif : classer les menages en 4 groupes par rapport au produit X, puis
# comparer leur profil (socio-demo, pauvrete, faim FIES, diversite HDDS).
#
# Style : simple, debutant, base sur tidyverse (suite logique de import_bases)
# Prerequis : avoir execute import_bases_ehcvm.R (les bases sont en memoire)
###############################################################################

library(haven)
library(tidyverse)
library(labelled)   # pour val_labels() et as_factor()

## =============================================================================
##  PARAMETRE : LE PRODUIT X (a adapter selon le choix du Module 1)
## =============================================================================
# Exemple : le Mil. En production c'est 1 seul code, en conso c'est plusieurs (grain + derives).
X <- "Maïs"
code_culture_X  <- 4                
codes_conso_X   <- c(5, 6, 12, 13)


## =============================================================================
##  ETAPE 1 - DEFINIR LES 4 GROUPES (qui produit X ? qui consomme X ?)
## =============================================================================
cat("\n=== ETAPE 1 : Typologie des menages ===\n")

# --- 1a. Producteurs : menages qui cultivent X (depuis s16c) ---
producteurs <- s16c %>%
  filter(s16cq04 == code_culture_X) %>%     # on garde les lignes du produit X
  distinct(hhid) %>%                         # un menage = une fois
  mutate(producteur = 1)                     # indicatrice 0/1
cat("  - Menages producteurs de", X, ":", nrow(producteurs), "\n")

# --- 1b. Consommateurs : menages qui mangent X (depuis s07b) ---
consommateurs <- s07b %>%
  filter(s07bq01 %in% codes_conso_X, s07bq02 == 1) %>%  # produit X + a consomme (Oui=1)
  distinct(hhid) %>%
  mutate(consommateur = 1)
cat("  - Menages consommateurs de", X, ":", nrow(consommateurs), "\n")

# --- 1c. Tableau de synthese : tous les menages ---
# Base maitresse = ehcvm_welfare_2b (contient deja hhid unique, hhweight, pcexp, etc.)
menages <- ehcvm_welfare_2b %>%
  select(hhid, grappe, menage, hhweight, hhsize, pcexp, zref, milieu, region) %>%
  left_join(producteurs, by = "hhid") %>%
  left_join(consommateurs, by = "hhid") %>%
  replace_na(list(producteur = 0, consommateur = 0)) %>%
  # Creation des 4 groupes
  mutate(groupe = case_when(
    producteur == 1 & consommateur == 1 ~ "1. Producteur-Conso",
    producteur == 1 & consommateur == 0 ~ "2. Producteur seul",
    producteur == 0 & consommateur == 1 ~ "3. Conso seul",
    producteur == 0 & consommateur == 0 ~ "4. Ni prod ni conso"
  ))

# --- Effectifs par groupe (ponderes) ---
cat("\nRepartition des 4 groupes (ponderes) :\n")
repartition <- menages %>%
  group_by(groupe) %>%
  summarise(nb_menages = sum(hhweight),       # nombre national
            pct = 100 * sum(hhweight) / sum(menages$hhweight))
print(repartition)


## =============================================================================
##  ETAPE 2 - PROFIL SOCIO-DEMOGRAPHIQUE DU CHEF DE MENAGE
## =============================================================================
cat("\n=== ETAPE 2 : Profil socio-demographique du chef ===\n")

# hhsize, milieu, pcexp et zref sont DEJA dans le tableau "menages" (via welfare).
# Il ne reste qu'a recuperer le profil du chef de menage (sexe, age, education).

# Le chef de menage = s01q02 == 1. On recupere sexe, age, education.
# Education se trouve dans s02 (s02q29 = niveau d'etudes le plus eleve).
chef <- s01 %>%
  filter(s01q02 == 1) %>%                      # garder le chef uniquement
  select(hhid, pid, sexe_chef = s01q01, age_chef = s01q04a) %>%
  left_join(s02_me %>% select(hhid, pid, educ_chef = s02q29), by = c("hhid", "pid"))

# On rattache au tableau principal
menages <- menages %>%
  left_join(chef %>% select(-pid), by = "hhid")


## =============================================================================
##  ETAPE 3 - SCORE FIES (la faim) + VERIFICATION DU REPONDANT
## =============================================================================
cat("\n=== ETAPE 3 : Score FIES (securite alimentaire) ===\n")

# Les 8 questions FIES : s08aq01 a s08aq08 (1=Oui, 2=Non, 98/99=manquant).
# Etape indispensable : recoder en 0/1 avant de sommer.
fies <- s08a %>%
  # Garder uniquement les 8 questions et recoder Oui=1, Non=0
  mutate(across(s08aq01:s08aq08,
                ~ if_else(.x == 1, 1, 0))) %>%   # Oui -> 1, le reste -> 0
  mutate(score_fies = s08aq01 + s08aq02 + s08aq03 + s08aq04 +
                     s08aq05 + s08aq06 + s08aq07 + s08aq08,
         # Seuils officiels FIES : modere >= 3, severe >= 6
         fies_modere  = if_else(score_fies >= 3, 1, 0),
         fies_severe  = if_else(score_fies >= 6, 1, 0)) %>%
  select(hhid, score_fies, fies_modere, fies_severe)

menages <- menages %>%
  left_join(fies, by = "hhid")

# --- VERIFICATION DU REPONDANT (point de vigilance du sujet) ---
cat("\n  >> Verification : QUI repond au FIES ?\n")
repondant_fies <- s08a %>%
  select(hhid, pid_repondant = s08aq00) %>%
  left_join(s01 %>% select(hhid, pid, s01q02, s01q01),
            by = c("hhid" = "hhid", "pid_repondant" = "pid")) %>%
  mutate(lien_repondant = as_factor(s01q02)) %>%
  count(lien_repondant) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  arrange(desc(n))
print(repondant_fies)
cat("  >> Si 'Chef de menage' domine (>80%), a signaler comme biais possible.\n")


## =============================================================================
##  ETAPE 4 - SCORE HDDS (diversite alimentaire)
## =============================================================================
cat("\n=== ETAPE 4 : Score HDDS (diversite alimentaire) ===\n")
# HDDS = nombre de GROUPES alimentaires consommes (sur 12 groupes FAO).
# Il faut mapper le code produit (s07bq01) vers un groupe FAO.
# Voici une table de passage simplifiee (a completer si besoin) :

passage_fao <- tibble::tribble(
  ~code_produit, ~groupe_fao,
  # Cereales
  1, "Cereales", 2, "Cereales", 3, "Cereales", 4, "Cereales", 6, "Cereales",
  7, "Cereales", 8, "Cereales", 12, "Cereales", 14, "Cereales", 16, "Cereales",
  # Tubercules
  123, "Tubercules", 124, "Tubercules", 126, "Tubercules", 128, "Tubercules",
  # Legumes
  88, "Legumes", 89, "Legumes", 90, "Legumes", 96, "Legumes", 100, "Legumes",
  # Fruits
  71, "Fruits", 72, "Fruits", 73, "Fruits", 76, "Fruits",
  # Viande
  27, "Viande", 29, "Viande", 30, "Viande", 34, "Viande",
  # Oeufs
  60, "Oeufs",
  # Poisson
  40, "Poisson", 44, "Poisson", 51, "Poisson",
  # Legumineuses
  112, "Legumineuses", 113, "Legumineuses", 114, "Legumineuses",
  # Lait
  52, "Lait", 53, "Lait",
  # Huiles
  63, "Huiles", 66, "Huiles", 67, "Huiles",
  # Sucre
  134, "Sucre"
)

# HDDS = nb de groupes distincts consommes par le menage sur 7 jours
hdds <- s07b %>%
  filter(s07bq02 == 1) %>%                      # menages qui ont consomme
  inner_join(passage_fao, by = c("s07bq01" = "code_produit")) %>%
  distinct(hhid, groupe_fao) %>%                # dedoublonner les groupes
  count(hhid, name = "hdds")                    # compte les groupes (0 a 12)

menages <- menages %>%
  left_join(hdds, by = "hhid") %>%
  replace_na(list(hdds = 0))


## =============================================================================
##  ETAPE 5 - STATS COMPARATIVES (le tableau final)
## =============================================================================
cat("\n=== ETAPE 5 : Tableau comparatif des 4 groupes ===\n")

# Pour chaque groupe, on calcule la moyenne ponderee des indicateurs cles.
# On definit une petite fonction "moyenne ponderee" pour la lisibilite.
wmean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)

comparatif <- menages %>%
  mutate(pauvre = if_else(pcexp < zref, 1, 0)) %>%   # 1 si pauvre
  group_by(groupe) %>%
  summarise(
    nb_menages        = sum(hhweight),
    age_chef          = wmean(age_chef, hhweight),
    pct_femme_chef    = 100 * wmean(sexe_chef == 2, hhweight),   # sexe=2 = Femme
    taille_menage     = wmean(hhsize, hhweight),
    pct_urbain        = 100 * wmean(milieu == 1, hhweight),      # milieu=1 = Urbain
    incidence_pauvrete= 100 * wmean(pauvre, hhweight),
    fies_score        = wmean(score_fies, hhweight),
    fies_modere_pct   = 100 * wmean(fies_modere, hhweight),
    fies_severe_pct   = 100 * wmean(fies_severe, hhweight),
    hdds_moyen        = wmean(hdds, hhweight)
  )

# Affichage arrondi
print(comparatif %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

cat("\n=== Module 2 termine. Le tableau 'comparatif' est pret. ===\n")
cat("Il peut etre directement utilise dans le dashboard (onglet 2) ou le rapport.\n")

# Graphique FIES par groupe
graph_fies <- ggplot(comparatif, aes(x = groupe, y = fies_score, fill = groupe)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(fies_score, 2)), vjust = -0.5) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Score FIES moyen par groupe de ménage",
       x = "Groupe", y = "Score FIES moyen (0-8)") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("sorties/graph_m2_fies.png", plot = graph_fies, width = 8, height = 5, dpi = 300)

# Sauvegarde pour utilisation ulterieure (dashboard, package, presentation)
write.csv(comparatif, "sorties/module2_comparatif.csv", row.names = FALSE)
saveRDS(comparatif, "sorties/tab_m2_profil.rds")
saveRDS(menages, "sorties/menages_typologie.RDS")
