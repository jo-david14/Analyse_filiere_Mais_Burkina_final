#=======================================================================
#          MODULE 3 : ANALYSE DE LA PRODUCTION ET DES RENDEMENTS
#=======================================================================
# Module 3 : production et rendements du maïs.
#
#   - La table de conversion "phase 2.xlsx" est REMPLACÉE par la base officielle
#     EHCVM  ehcvm_nsu_bfa2021.dta  (poids calibrés par strate region×milieu).
#   - Le mapping s16cq16b -> uniteID est COMPLET (Yorouba, Tine désormais couverts,
#     alors que l'ancien script les laissait en NA -> 30-40 % de pertes).
#   - La Branche B ("récolte en cours") ne prend pas s16cq16c tel quel :
#     cette variable est incohérente (ratios c/a de 0 à 1249, NA pour les kg).
#     On RECONSTRUIT la quantité en kg depuis s16cq16a × poids NSU.
#
# Carnet de décisions Module 3 (à jour)
# -----------------------------------------------------------
# 1. s16cq11 = "Avez-vous FINI la récolte ?" -> 1=terminée, 2=en cours.
#      q16a/b/c ne sont renseignées QUE pour "en cours" (s16cq11==2).
# 2. Production "récolte terminée" reconstituée depuis S16D :
#      somme(conso + don + vente + stock) convertie en kg via la NSU.
#      s16dq05c (estimation kg de la vente) utilisée prioritairement.
# 3. Conversion des unités locales via la NSU, en joignant sur la strate
#      du ménage (region×milieu reconstruit depuis ehcvm_welfare_2b).
#      produitID (codpr) dépend de l'état : épi = 5, grain = 6.
# 4. Branche B : quantité = s16cq16a × poids_median(codpr, uniteID, strate)/1000.
#      s16cq16c est explicitement ÉCARTÉE (variable de contrôle incohérente).
# 5. Micro-parcelles (< 0.05 ha) exclues.
# 6. Plafond agronomique 5 000 kg/ha + winsorisation p1/p99.

# --- Référentiel de codes produit -------------------------------------------
# S16C et S16D : "4" = Maïs. (Différent de S07B utilisé pour la conso, Module 1.)
codes_mais_s16c <- 4

# Packages nécessaires
library(httr)
library(haven)
library(jsonlite)
library(purrr)
library(dplyr)
library(fixest)
library(sf)
library(geodata)
library(readxl)
library(labelled)

# Pré-requis : préambule.R et module1 déjà sourcés depuis main.R
# (les objets s16a, s16b, s16c, s16d, s00, s01, s02_me, hhweight doivent exister).

# =====================================================================
# 0. PRÉPARATION DE LA TABLE DE CONVERSION NSU
#    Poids en grammes par (codpr, uniteID, tailleID, strate).
#    La strate = region*10 + milieu (13 régions × 2 milieux = 26 strates).
# =====================================================================
nsu_conv <- read_dta("donnee/base_burkina/ehcvm_nsu_bfa2021.dta") %>%
  filter(!is.na(poids_moyen)) %>%
  mutate(strate = region * 10 + milieu)

# Agrégation sur la taille : S16C/S16D ne déclarent pas de tailleID,
# on prend la moyenne des tailles disponibles
nsu_mais <- nsu_conv %>%
  filter(codpr %in% c(5, 6)) %>%                      # 5 = épi, 6 = grain
  group_by(codpr, uniteID, strate) %>%
  summarise(poids_moyen_g = median(poids_moyen, na.rm = TRUE), .groups = "drop") %>%
  mutate(poids_moyen_kg = poids_moyen_g / 1000)

# Filet national : si une strate manque pour un couple (codpr, uniteID),
# on complète par la moyenne nationale (toutes strates confondues).
nsu_mais_national <- nsu_mais %>%
  group_by(codpr, uniteID) %>%
  summarise(poids_moyen_kg_nat = median(poids_moyen_kg, na.rm = TRUE),
            .groups = "drop")

nsu_mais <- nsu_mais %>%
  left_join(nsu_mais_national, by = c("codpr", "uniteID")) %>%
  mutate(poids_moyen_kg = coalesce(poids_moyen_kg, poids_moyen_kg_nat)) %>%
  select(-poids_moyen_kg_nat)

# Correspondance COMPLÈTE code-unité EHCVM (1-7) -> uniteID NSU.
# Cœur de l'amélioration : Yorouba (3) et Tine (4) sont désormais couverts.
#   1=Kg ->100 ; 2=Unité(ne sait pas)->exclu ; 3=Yorouba->149 ; 4=Tine->145 ;
#   5=Sac moyen->138 (50 kg) ; 6=Sac gros->135 (100 kg) ; 7=Autres->recodage.
corresp_unite <- tibble(
  code_unite   = c( 1,    3,    4,    5,    6),
  uniteID_nsu  = c(100,  149,  145,  138,  135)
)

# Recodage des "Autres" (code 7) via le champ texte _autre.
# Mapping établi à partir du dictionnaire NSU (cf. analyse préalable) :
#   boîte/boites/botes/boîte moyen -> 107 ; panier -> 128 ; grand tas -> 143 ;
#   sac petit -> 137 ; caisses -> 114 ; sachet de 25f -> 139 ;
#   tonne/tonnes -> déjà en kg (100) ; reste (grenier, 0, ne sais pas...) -> NA.
recoder_autre <- function(texte) {
  t <- tolower(as.character(texte))
  case_when(
    grepl("bo[îi]te|botes|boites", t)              ~ 107L,
    grepl("panier", t)                             ~ 128L,
    grepl("grand tas", t)                          ~ 143L,
    grepl("sac petit", t)                          ~ 137L,
    grepl("caisse", t)                             ~ 114L,
    grepl("sachet", t)                             ~ 139L,
    grepl("tonne", t)                              ~ 100L,  # déjà en kg
    TRUE                                           ~ NA_integer_
  )
}

# =====================================================================
# 1. SURFACE DE LA PARCELLE (S16A) + PART PLANTÉE EN MAÏS
# =====================================================================
parcelles <- s16a %>%
  transmute(
    hhid, s16aq02, s16aq03,
    surface_parcelle_ha = case_when(
      s16aq09b == 1 ~ s16aq09a,
      s16aq09b == 2 ~ s16aq09a / 10000,
      TRUE          ~ NA_real_)
  ) %>%
  distinct(hhid, s16aq02, s16aq03, .keep_all = TRUE)

surface_mais <- s16c %>%
  filter(s16cq04 %in% codes_mais_s16c) %>%
  left_join(parcelles, by = c("hhid", "s16cq02" = "s16aq02",
                              "s16cq03" = "s16aq03")) %>%
  mutate(
    pct_mais = if_else(s16cq07 == 1 & is.na(s16cq08), 100,
                       as.numeric(s16cq08)),
    surface_mais_ha = surface_parcelle_ha * pct_mais / 100
  ) %>%
  select(hhid, s16cq02, s16cq03, surface_mais_ha,
         s16cq11, s16cq12, s16cq15)

surface_mais_hh <- surface_mais %>%
  group_by(hhid) %>%
  summarise(surface_mais_ha = sum(surface_mais_ha, na.rm = TRUE),
            .groups = "drop")

# =====================================================================
# 1b. STRATE DE CHAQUE MÉNAGE (region×milieu) pour la jointure NSU
#     region/milieu proviennent d'ehcvm_welfare_2b (jointure sur hhid).
# =====================================================================
strate_hh <- ehcvm_welfare_2b %>%
  select(hhid, region, milieu) %>%
  mutate(strate = region * 10 + milieu)

# =====================================================================
# 2. FONCTION DE CONVERSION kg (version NSU, calée sur la strate)
#    L'état du produit (épi vs grain) détermine le codpr (5 vs 6).
# =====================================================================
convertir_kg_nsu <- function(df, var_qte, var_unite, var_etat, var_autre = NULL) {
  tmp <- df %>%
    mutate(
      .qte       = .data[[var_qte]],
      code_unite = .data[[var_unite]],
      etat       = .data[[var_etat]],
      # Recodage des "Autres" (code 7) via le texte, si dispo
      uniteID = case_when(
        code_unite %in% corresp_unite$code_unite ~
          corresp_unite$uniteID_nsu[match(code_unite, corresp_unite$code_unite)],
        code_unite == 7 & !is.null(var_autre)     ~
          recoder_autre(.data[[var_autre]]),
        TRUE ~ NA_real_
      ),
      codpr = case_when(
        etat == 1            ~ 5,   # Épi
        etat %in% c(2, 3, 4) ~ 6,   # Grain / décortiqué / non-décortiqué
        TRUE                 ~ NA_real_)
    ) %>%
    left_join(strate_hh, by = "hhid") %>%
    left_join(nsu_mais, by = c("codpr", "uniteID", "strate"))
  
  # Si l'unité est déjà le kilogramme (uniteID 100, codpr 6), le poids NSU
  # est 1 kg : la quantité déclarée est prise telle quelle.
  tmp$.qte * tmp$poids_moyen_kg
}

# ---------------------------------------------------------------------
# 3. SURCHARGE : wrapper de conversion S16D (passe le bon champ _autre)
# ---------------------------------------------------------------------
convertir_kg_16d <- function(df, var_qte, var_unite, var_etat) {
  var_autre <- paste0(var_unite, "_autre")
  if (!var_autre %in% names(df)) var_autre <- NULL
  convertir_kg_nsu(df, var_qte, var_unite, var_etat, var_autre)
}

# =====================================================================
# 4. BRANCHE A : MÉNAGES "RÉCOLTE TERMINÉE" (s16cq11 == 1, ~91 %)
#    Production = somme des usages déclarés en S16D, convertis en kg.
# =====================================================================
menages_fini <- s16c %>%
  filter(s16cq04 %in% codes_mais_s16c, s16cq11 == 1) %>%
  distinct(hhid)

s16d_mais_fini <- s16d %>%
  filter(s16dq01 %in% codes_mais_s16c) %>%
  semi_join(menages_fini, by = "hhid") %>%
  mutate(
    conso_kg = convertir_kg_16d(., "s16dq02a", "s16dq02b", "s16dq02c"),
    don_kg   = convertir_kg_16d(., "s16dq03a", "s16dq03b", "s16dq03c"),
    vente_kg = coalesce(s16dq05c,
                        convertir_kg_16d(., "s16dq05a", "s16dq05b",
                                         "s16dq05d")),
    stock_kg = convertir_kg_16d(., "s16dq13a", "s16dq13b", "s16dq13c")
  ) %>%
  rowwise() %>%
  mutate(
    n_postes = sum(!is.na(c_across(c(conso_kg, don_kg, vente_kg,
                                     stock_kg)))),
    production_kg = if_else(n_postes > 0,
                            sum(c_across(c(conso_kg, don_kg, vente_kg, stock_kg)),
                                na.rm = TRUE),
                            NA_real_)
  ) %>%
  ungroup()

cat("Couverture de la conversion par poste (récolte terminée) :\n")
summary(s16d_mais_fini[c("conso_kg", "don_kg", "vente_kg", "stock_kg")])

production_fini <- s16d_mais_fini %>%
  left_join(surface_mais_hh, by = "hhid") %>%
  mutate(source_production = "S16D - récolte terminée") %>%
  filter(is.finite(surface_mais_ha), surface_mais_ha > 0,
         is.finite(production_kg),  production_kg > 0)

cat("Ménages 'récolte terminée' exploitables (production reconstituée) :",
    nrow(production_fini), "sur", nrow(menages_fini), "\n")

# =====================================================================
# 5. BRANCHE B : MÉNAGES "RÉCOLTE EN COURS" (s16cq11 == 2)
#    HARMONISATION NSU : on convertit s16cq16a (quantité en unité locale)
#    via la NSU. s16cq16c est ÉCARTÉE (variable de contrôle incohérente).
# =====================================================================
production_encours <- s16c %>%
  filter(s16cq04 %in% codes_mais_s16c,
         s16cq11 == 2, !is.na(s16cq16a)) %>%
  mutate(
    qte_kg_nsu = convertir_kg_nsu(., "s16cq16a", "s16cq16b", "s16cq16d",
                                  "s16cq16b_autre")
  ) %>%
  left_join(surface_mais_hh, by = "hhid") %>%
  mutate(
    production_kg     = qte_kg_nsu,
    source_production = "S16C - récolte en cours (NSU)"
  ) %>%
  filter(is.finite(surface_mais_ha), surface_mais_ha > 0,
         is.finite(production_kg),  production_kg > 0)

cat("Ménages 'récolte en cours' exploitables :", nrow(production_encours),
    "\n")

# =====================================================================
# 6. FUSION + RENDEMENT
# =====================================================================
production_mais <- bind_rows(
  production_fini %>%
    select(hhid, hhweight, surface_mais_ha, production_kg,
           source_production),
  production_encours %>%
    select(hhid, hhweight, surface_mais_ha, production_kg,
           source_production)
) %>%
  mutate(rendement_kg_ha = production_kg / surface_mais_ha)

cat("Total ménages producteurs de maïs exploitables :",
    nrow(production_mais), "\n")
print(table(production_mais$source_production))

# =====================================================================
# 7. FILTRAGE AGRONOMIQUE + WINSORISATION p1/p99
# =====================================================================
production_filtre <- production_mais %>%
  filter(surface_mais_ha >= 0.05,
         rendement_kg_ha <= 5000)

bornes_rendement <- quantile(production_filtre$rendement_kg_ha,
                             probs = c(0.01, 0.99), na.rm = TRUE)

production_mais_analyse <- production_filtre %>%
  filter(between(rendement_kg_ha,
                 bornes_rendement[[1]], bornes_rendement[[2]]))

cat("Observations après filtrage :", nrow(production_mais_analyse),
    "sur", nrow(production_mais), "brutes\n")

# 8. BILAN NATIONAL
bilan_mais <- production_mais_analyse %>%
  summarise(
    observations = n(),
    superficie_nationale_ha = sum(surface_mais_ha * hhweight,
                                  na.rm = TRUE),
    production_nationale_tonnes = sum(production_kg * hhweight,
                                      na.rm = TRUE) / 1000,
    rendement_national_kg_ha =
      sum(production_kg * hhweight, na.rm = TRUE) /
      sum(surface_mais_ha * hhweight, na.rm = TRUE),
    rendement_median_kg_ha = median(rendement_kg_ha, na.rm = TRUE))
print(bilan_mais)

# --- 9. VALIDATION FAOSTAT ------
rendement_faostat_kg_ha <- 1520.8
cat(sprintf("Rendement national estimé : %.0f kg/ha\n",
            bilan_mais$rendement_national_kg_ha))
cat(sprintf("Rendement FAOSTAT (référence) : %.0f kg/ha\n",
            rendement_faostat_kg_ha))
cat(sprintf("Écart : %+.1f %%\n",
            100 * (bilan_mais$rendement_national_kg_ha / rendement_faostat_kg_ha - 1)))

# --- 11. Pertes (analyse séparée, demandée par le sujet p.9) ---
pertes <- s16c %>%
  filter(s16cq04 %in% codes_mais_s16c) %>%
  mutate(perte_pct = coalesce(as.numeric(zap_labels(s16cq15)), 0)) %>%
  summarise(
    n_total = n(),
    n_perte_totale = sum(as.numeric(zap_labels(s16cq11)) == 3, na.rm = TRUE),
    perte_moyenne_pct = mean(perte_pct, na.rm = TRUE))
print(pertes)

# Bilan par source de reconstitution de la production (utilisé par le 1er graphique)
bilan_mais_source <- production_mais_analyse %>%
  group_by(source_production) %>%
  summarise(
    observations = n(),
    superficie_ha = sum(surface_mais_ha * hhweight, na.rm = TRUE),
    production_tonnes = sum(production_kg * hhweight, na.rm = TRUE) / 1000,
    rendement_median_kg_ha = median(rendement_kg_ha, na.rm = TRUE),
    .groups = "drop")
print(bilan_mais_source)

# --- 12. Graphiques ---
ggsave("sorties/sorties_module_3/production_mais.png",
       ggplot(bilan_mais_source,
              aes(reorder(source_production, production_tonnes),
                  production_tonnes)) +
         geom_col(fill = "forestgreen") +
         coord_flip() + theme_minimal() +
         labs(title = "Production nationale estimée de maïs par source",
              x = "Source de la donnée", y = "Production (tonnes)"),
       width = 9, height = 6, dpi = 300)

ggsave("sorties/sorties_module_3/rendements_mais.png",
       ggplot(production_mais_analyse,
              aes(source_production, rendement_kg_ha)) +
         geom_boxplot(fill = "goldenrod", outlier.alpha = 0.25) +
         theme_minimal() +
         labs(title = "Distribution des rendements du maïs, par source de reconstitution",
              x = "Source", y = "Rendement (kg/ha)") +
         theme(axis.text.x = element_text(angle = 20, hjust = 1)),
       width = 9, height = 6, dpi = 300)

# --- 13. Intrants des ménages producteurs de maïs (S16B) ---
# S16B est renseigné au niveau ménage, pas au niveau culture : ces intrants
# décrivent les ménages producteurs de maïs, pas des intrants exclusivement
# dédiés au maïs (limite à mentionner dans le rapport).
menages_mais <- production_mais_analyse %>%
  distinct(hhid, hhweight)

intrants_mais <- s16b %>%
  semi_join(menages_mais, by = "hhid") %>%
  mutate(intrant = as_factor(s16bq01)) %>%
  filter(s16bq02 == 1) %>%
  group_by(intrant) %>%
  summarise(
    menages_ponderes = sum(hhweight, na.rm = TRUE),
    depense_totale_fcfa = sum(s16bq09c * hhweight, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(part_utilisations = 100 * menages_ponderes /
           sum(menages_ponderes)) %>%
  arrange(desc(part_utilisations))
print(intrants_mais)

ggsave("sorties/sorties_module_3/intrants_producteurs_mais.png",
       ggplot(intrants_mais,
              aes(reorder(intrant, part_utilisations), part_utilisations)) +
         geom_col(fill = "steelblue") + coord_flip() + theme_minimal() +
         labs(title = "Utilisation d'intrants chez les producteurs de maïs",
              x = "Intrant", y = "Part des utilisations déclarées (%)"),
       width = 9, height = 6, dpi = 300)

# Modèle : ln(Rendement) = a + b1.Intrants + b2.Semences + b3.Irrigation
#          + b4.Education + effets fixes grappe

menages_mais_reg <- production_mais_analyse %>% distinct(hhid, hhweight)

# Valeur totale des intrants utilisés (S16B)
intrants_valeur_hh <- s16b %>%
  semi_join(menages_mais_reg, by = "hhid") %>%
  filter(s16bq02 == 1) %>%
  group_by(hhid) %>%
  summarise(valeur_intrants_fcfa = sum(s16bq09c, na.rm = TRUE), .groups = "drop")

# Semences améliorées (S16C, s16cq09 : 1=Locales, 2=Améliorées)
semence_hh <- s16c %>%
  filter(s16cq04 %in% codes_mais_s16c) %>%
  semi_join(menages_mais_reg, by = "hhid") %>%
  group_by(hhid) %>%
  summarise(semence_amelioree = as.integer(any(zap_labels(s16cq09) == 2, na.rm = TRUE)),
            .groups = "drop")

# Irrigation (S16A, s16aq17). 4=Pluviale ; 1,2,3,5=Irrigué ; 6=Autre.
# Codes numériques utilisés directement, plus fiable que les labels accentués.
irrigation_hh <- s16a %>%
  semi_join(menages_mais_reg, by = "hhid") %>%
  mutate(irr_parcelle = case_when(
    zap_labels(s16aq17) == 4 ~ 0L,
    zap_labels(s16aq17) %in% c(1, 2, 3, 5) ~ 1L,
    TRUE ~ NA_integer_)) %>%
  group_by(hhid) %>%
  summarise(irrigation = as.integer(any(irr_parcelle == 1, na.rm = TRUE)),
            .groups = "drop")

# Éducation du chef de ménage (S01 + S02). Je filtre sur le code numérique
# (1 = chef) et pas sur le label, car les comparaisons de chaînes accentuées
# échouent silencieusement selon l'encodage (Windows vs UTF-8).
chef_menage <- s01 %>%
  filter(zap_labels(s01q02) == 1) %>%
  distinct(hhid, pid)

education_hh <- chef_menage %>%
  left_join(s02_me %>% select(hhid, pid, s02q03), by = c("hhid", "pid")) %>%
  mutate(
    education_scolarise = case_when(
      zap_labels(s02q03) == 1 ~ 1L,
      zap_labels(s02q03) == 2 ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(hhid, education_scolarise)

cat("Chefs de ménage sans info exploitable sur l'éducation :",
    sum(is.na(education_hh$education_scolarise)), "sur", nrow(education_hh), "\n")

grappe_hh <- s16c %>% distinct(hhid, grappe)

# --- Assemblage de la table de régression ---
data_reg_mais <- production_mais_analyse %>%
  mutate(ln_rendement = log(rendement_kg_ha)) %>%
  left_join(intrants_valeur_hh, by = "hhid") %>%
  left_join(semence_hh,         by = "hhid") %>%
  left_join(irrigation_hh,      by = "hhid") %>%
  left_join(education_hh,       by = "hhid") %>%
  left_join(grappe_hh,          by = "hhid") %>%
  mutate(
    valeur_intrants_fcfa = coalesce(valeur_intrants_fcfa, 0),
    ln_intrants = log1p(valeur_intrants_fcfa),
    semence_amelioree = coalesce(semence_amelioree, 0L),
    irrigation = coalesce(irrigation, 0L)
  ) %>%
  filter(is.finite(ln_rendement), !is.na(education_scolarise))

cat("Observations disponibles pour la régression :", nrow(data_reg_mais),
    "sur", nrow(production_mais_analyse), "ménages producteurs de maïs retenus\n")

# --- Régression OLS, effets fixes de grappe, erreurs robustes pondérées ---
reg_rendement_mais <- feols(
  ln_rendement ~ ln_intrants + semence_amelioree + irrigation + education_scolarise | grappe,
  data    = data_reg_mais,
  weights = ~hhweight,
  cluster = ~grappe
)
summary(reg_rendement_mais)

# Version sans effets fixes de grappe, pour comparaison
reg_rendement_mais_sansFE <- feols(
  ln_rendement ~ ln_intrants + semence_amelioree + irrigation + education_scolarise,
  data = data_reg_mais, weights = ~hhweight
)

etable(reg_rendement_mais, reg_rendement_mais_sansFE,
       headers = c("Avec FE grappe", "Sans FE grappe"))

# --- Carte des rendements par grappe ---
rendement_grappe <- production_mais_analyse %>%
  left_join(grappe_hh, by = "hhid") %>%
  group_by(grappe) %>%
  summarise(
    n_menages = n(),
    rendement_moyen_kg_ha = sum(rendement_kg_ha * hhweight, na.rm = TRUE) /
      sum(hhweight, na.rm = TRUE),
    .groups = "drop"
  )

gps_grappe <- s00 %>%
  filter(!is.na(GPS__Latitude), !is.na(GPS__Longitude)) %>%
  group_by(grappe) %>%
  summarise(lat = mean(GPS__Latitude, na.rm = TRUE),
            lon = mean(GPS__Longitude, na.rm = TRUE),
            .groups = "drop")

rendement_geo <- rendement_grappe %>%
  inner_join(gps_grappe, by = "grappe") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

cat("Grappes cartographiables :", nrow(rendement_geo), "sur", nrow(rendement_grappe), "\n")

bfa_shp <- gadm(country = "BFA", level = 1, path = tempdir()) %>% st_as_sf()

carte_rendement_mais <- ggplot() +
  geom_sf(data = bfa_shp, fill = "grey95", color = "grey60") +
  geom_sf(data = rendement_geo,
          aes(size = n_menages, color = rendement_moyen_kg_ha),
          alpha = 0.75) +
  scale_color_viridis_c(
    name = "Rendement\n(kg/ha)",
    option = "viridis",
    limits = c(0, 2000),
    breaks = seq(500, 2000, 500)
  ) +
  scale_size_continuous(name = "Nb. ménages\nproducteurs", range = c(1, 6)) +
  theme_minimal() +
  labs(title = "Rendement moyen du maïs par grappe — Burkina Faso (2021)",
       subtitle = "Chaque point = une grappe EHCVM ; taille = nombre de ménages producteurs",
       caption = "Source : EHCVM 2021, calculs des auteurs") +
  theme(axis.title = element_blank())

ggsave("sorties/sorties_module_3/carte_rendement_mais.png", carte_rendement_mais, width = 10, height = 8, dpi = 300)

# --- Rendement vs pluviométrie (NASA POWER) ---
get_pluie_nasa_power <- function(lon, lat) {
  url <- "https://power.larc.nasa.gov/api/temporal/daily/point"
  res <- GET(url, query = list(
    parameters = "PRECTOTCORR",
    community  = "AG",
    longitude  = lon,
    latitude   = lat,
    start      = "20210501",
    end        = "20211031",
    format     = "JSON"
  ))
  if (status_code(res) != 200) return(NA_real_)
  dat <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  valeurs <- dat$properties$parameter$PRECTOTCORR
  sum(unlist(valeurs), na.rm = TRUE)
}

points_grappes <- rendement_geo %>%
  st_drop_geometry() %>%
  select(grappe, lon, lat)

# Je mets en cache localement pour ne pas re-taper l'API à chaque run
if (file.exists("pluie_grappe_nasapower.rds")) {
  cat("Chargement de la pluviométrie depuis le fichier local pluie_grappe_nasapower.rds...\n")
  pluie_grappe <- readRDS("pluie_grappe_nasapower.rds")
} else {
  test <- get_pluie_nasa_power(points_grappes$lon[1], points_grappes$lat[1])
  cat("Test premier point :", test, "mm\n")
  
  pluie_grappe <- points_grappes %>%
    mutate(pluie_totale_mm = map2_dbl(lon, lat, function(lo, la) {
      Sys.sleep(0.3)
      get_pluie_nasa_power(lo, la)
    }))
  
  cat("Grappes avec pluviométrie récupérée :",
      sum(!is.na(pluie_grappe$pluie_totale_mm)), "sur", nrow(pluie_grappe), "\n")
  
  saveRDS(pluie_grappe, "pluie_grappe_nasapower.rds")
}

rendement_geo <- rendement_geo %>%
  left_join(pluie_grappe %>% select(grappe, pluie_totale_mm), by = "grappe")

ggplot(rendement_geo %>% st_drop_geometry(),
       aes(pluie_totale_mm, rendement_moyen_kg_ha)) +
  geom_point(aes(size = n_menages), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  theme_minimal() +
  labs(title = "Rendement du maïs vs pluviométrie cumulée (mai-octobre 2021)",
       subtitle = "Chaque point = une grappe ; taille = nombre de ménages producteurs",
       x = "Pluviométrie cumulée (mm)", y = "Rendement moyen (kg/ha)",
       caption = "Source : NASA POWER (power.larc.nasa.gov)")
ggsave("sorties/sorties_module_3/rendement_vs_pluie.png", width = 9, height = 6, dpi = 300)

mod_pluie <- lm(rendement_moyen_kg_ha ~ pluie_totale_mm,
                data = rendement_geo %>% st_drop_geometry(),
                weights = n_menages)
summary(mod_pluie)

# Sauvegarde des objets RDS
saveRDS(bilan_mais, "sorties/tab_m3_rendement.rds")
saveRDS(production_mais_analyse, "sorties/production_mais_analyse.rds")