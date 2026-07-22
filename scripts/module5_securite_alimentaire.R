

library(labelled)
library(broom)
library(ggplot2)
library(dplyr)
library(forcats)

# --- Quantité vendue de maïs, par ménage (S16D, directement en kg) ---------
vente_mais_hh <- s16d %>%
  filter(s16dq01 == codes_mais_s16c, s16dq04 == 1) %>%
  group_by(hhid) %>%
  summarise(
    vendu_kg = sum(s16dq05c, na.rm = TRUE),
    revenu_vente_fcfa = sum(s16dq06, na.rm = TRUE),
    .groups = "drop"
  )

cat("Ménages avec vente de maïs enregistrée :", nrow(vente_mais_hh), "\n")

# --- Table de participation filière, au niveau de TOUS les ménages ---------
# (pas seulement les producteurs : un non-producteur a taux_vente = 0 par
# construction, cohérent avec la logique de la régression)
filiere_mais <- menages %>%
  distinct(hhid) %>%
  left_join(production_mais_analyse %>% distinct(hhid, production_kg), by = "hhid") %>%
  left_join(vente_mais_hh, by = "hhid") %>%
  mutate(
    production_kg = replace_na(production_kg, 0),
    vendu_kg = replace_na(vendu_kg, 0),
    revenu_vente_fcfa = replace_na(revenu_vente_fcfa, 0),
    taux_vente_mais = case_when(
      production_kg > 0                    ~ pmin(vendu_kg / production_kg, 1),
      production_kg == 0 & vendu_kg == 0   ~ 0,             # non-producteur/non-vendeur, cas normal
      production_kg == 0 & vendu_kg > 0    ~ NA_real_,       # incohérence -> exclu, à documenter
      TRUE ~ NA_real_
    ),
    ln_revenu_mais = log1p(revenu_vente_fcfa)
  ) %>%
  select(hhid, production_kg, vendu_kg, taux_vente_mais, revenu_vente_fcfa, ln_revenu_mais)

possession_terre_hh <- s16a %>%
  group_by(hhid) %>%
  summarise(possede_terre = as.integer(any(s16aq10 == 1, na.rm = TRUE)), .groups = "drop")

cat("Ménages avec au moins une parcelle en propriété :", sum(possession_terre_hh$possede_terre), "\n")
cat("Ménages agricoles sans info de propriété :",
    nrow(possession_terre_hh) - sum(!is.na(possession_terre_hh$possede_terre)), "\n")

educ_chef_m5 <- s01 %>%
  filter(s01q02 == 1) %>%
  select(hhid, pid) %>%
  left_join(s02_me %>% select(hhid, pid, s02q03), by = c("hhid", "pid")) %>%
  mutate(educ_chef_scolarise = case_when(
    s02q03 == 1 ~ 1L,   # Oui, a fait/fait des études
    s02q03 == 2 ~ 0L,   # Non, jamais scolarisé
    TRUE ~ NA_integer_
  )) %>%
  select(hhid, educ_chef_scolarise)

cat("Chefs de ménage sans info exploitable sur l'éducation :",
    sum(is.na(educ_chef_m5$educ_chef_scolarise)), "sur", nrow(educ_chef_m5), "\n")

age_chef_m5 <- ehcvm_welfare_2b %>%
  select(hhid, age_chef = hage) %>%
  distinct(hhid, .keep_all = TRUE)

summary(age_chef_m5$age_chef)

controles_m5 <- menages %>%
  select(hhid, grappe, hhweight, hhsize, sexe_chef, milieu, region, pcexp) %>%
  left_join(age_chef_m5, by = "hhid") %>%       # <- age_chef CORRIGÉ (hage)
  left_join(possession_terre_hh, by = "hhid") %>%
  left_join(educ_chef_m5, by = "hhid") %>%
  mutate(
    possede_terre = replace_na(possede_terre, 0L),
    milieu = as_factor(milieu),
    region = as_factor(region),
    ln_pcexp = log(pcexp)
  )

data_reg_m5 <- menages %>%
  select(hhid, grappe, hhweight, groupe, producteur, score_fies,
         fies_modere, fies_severe, hdds) %>%
  left_join(filiere_mais, by = "hhid") %>%
  left_join(controles_m5 %>% select(-hhweight, -grappe), by = "hhid") %>%
  rename(producteur_mais = producteur, groupe_mais = groupe)

cat("Table assemblée :", nrow(data_reg_m5), "lignes\n")

# Échantillon final : on exclut les lignes avec des valeurs manquantes sur
# les variables essentielles à la régression (score_fies, contrôles)
ech_reg_m5 <- data_reg_m5 %>%
  filter(!is.na(score_fies), !is.na(age_chef), !is.na(sexe_chef),
         !is.na(educ_chef_scolarise), !is.na(ln_pcexp))

cat("Échantillon de régression :", nrow(ech_reg_m5), "ménages",
    "(", round(100*nrow(ech_reg_m5)/nrow(data_reg_m5), 1), "% de la table assemblée)\n")

ech_reg_m5 %>%
  summarise(
    n = n(),
    pct_producteurs = 100 * mean(producteur_mais),
    taux_vente_moyen = mean(taux_vente_mais, na.rm = TRUE),
    pct_NA_taux_vente = 100 * mean(is.na(taux_vente_mais)),
    revenu_vente_median = median(revenu_vente_fcfa[producteur_mais == 1]),
    score_fies_moyen = weighted.mean(score_fies, hhweight, na.rm = TRUE),
    hdds_moyen = weighted.mean(hdds, hhweight, na.rm = TRUE)
  ) %>% print()

# Vérifie surtout : parmi les non-producteurs (producteur_mais == 0),
# taux_vente_mais et ln_revenu_mais doivent être à 0 systématiquement
ech_reg_m5 %>% filter(producteur_mais == 0) %>%
  summarise(max_taux_vente = max(taux_vente_mais, na.rm = TRUE),
            max_ln_revenu  = max(ln_revenu_mais, na.rm = TRUE))

reg_m5_fies <- feols(
  score_fies ~ producteur_mais + ln_revenu_mais + taux_vente_mais +
    hhsize + educ_chef_scolarise + possede_terre | milieu + region,
  weights = ~hhweight,
  data = ech_reg_m5 %>% filter(!is.na(taux_vente_mais)),
  vcov = "hetero"
)
summary(reg_m5_fies)

reg_m5_hdds <- feols(
  hdds ~ producteur_mais + ln_revenu_mais + taux_vente_mais +
    hhsize + educ_chef_scolarise + possede_terre | milieu + region,
  weights = ~hhweight,
  data = ech_reg_m5 %>% filter(!is.na(taux_vente_mais)),
  vcov = "hetero"
)

modelsummary::modelsummary(
  list("FIES (0-8, ↑ = pire)" = reg_m5_fies,
       "HDDS (0-12, ↑ = mieux)" = reg_m5_hdds),
  stars = TRUE
)

cat("N régression FIES :", nobs(reg_m5_fies), "\n")
cat("N régression HDDS :", nobs(reg_m5_hdds), "\n")


communaute_m5 <- s03_co %>%
  distinct(grappe, .keep_all = TRUE) %>%
  transmute(
    grappe,
    cooperative_presente = case_when(s03q03 == 1 ~ 1L, s03q03 == 2 ~ 0L, TRUE ~ NA_integer_),
    irrigation_village   = case_when(s03q17 == 1 ~ 1L, s03q17 == 2 ~ 0L, TRUE ~ NA_integer_)
  )

ech_reg_m5_hetero <- ech_reg_m5 %>%
  filter(!is.na(taux_vente_mais)) %>%
  left_join(communaute_m5, by = "grappe")

cat("Couverture coopérative :", sum(!is.na(ech_reg_m5_hetero$cooperative_presente)),
    "/", nrow(ech_reg_m5_hetero), "\n")
cat("Couverture irrigation  :", sum(!is.na(ech_reg_m5_hetero$irrigation_village)),
    "/", nrow(ech_reg_m5_hetero), "\n")

reg_m5_hetero_coop <- feols(
  score_fies ~ producteur_mais + ln_revenu_mais +
    taux_vente_mais * cooperative_presente +
    hhsize + educ_chef_scolarise + possede_terre | milieu + region,
  weights = ~hhweight, data = ech_reg_m5_hetero, vcov = "hetero"
)

reg_m5_hetero_irrig2 <- feols(
  score_fies ~ producteur_mais * irrigation_village + ln_revenu_mais +
    taux_vente_mais + hhsize + educ_chef_scolarise + possede_terre | milieu + region,
  weights = ~hhweight, data = ech_reg_m5_hetero, vcov = "hetero"
)

modelsummary::modelsummary(
  list("Interaction coop." = reg_m5_hetero_coop,
       "Interaction irrig." = reg_m5_hetero_irrig2),
  stars = TRUE
)


# --- Extraction propre des coefficients + IC 95%, sans l'intercept/FE ---
extraire_coefs <- function(modele, nom_modele) {
  broom::tidy(modele, conf.int = TRUE) %>%
    filter(!term %in% c("(Intercept)")) %>%
    mutate(modele = nom_modele)
}

coefs_m5 <- bind_rows(
  extraire_coefs(reg_m5_fies, "FIES (0-8, \u2191 = pire)"),
  extraire_coefs(reg_m5_hdds, "HDDS (0-12, \u2191 = mieux)")
)

# --- Labels lisibles pour les variables (au lieu des noms de code R) ---
labels_var <- c(
  producteur_mais      = "Producteur de maïs",
  ln_revenu_mais        = "ln(Revenu vente maïs)",
  taux_vente_mais       = "Taux de commercialisation",
  hhsize                = "Taille du ménage",
  educ_chef_scolarise   = "Chef scolarisé",
  possede_terre         = "Possède des terres"
)

coefs_m5 <- coefs_m5 %>%
  mutate(
    term_label = recode(term, !!!labels_var),
    # ordre d'affichage : variable de recherche principale en haut
    term_label = factor(term_label, levels = rev(labels_var))
  )

# --- Graphique ---
graphique_coefs_m5 <- ggplot(coefs_m5, aes(x = estimate, y = term_label, color = modele)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.5), size = 0.6) +
  scale_color_manual(values = c("FIES (0-8, \u2191 = pire)" = "#C0392B",
                                "HDDS (0-12, \u2191 = mieux)" = "#2471A3")) +
  labs(
    title = "Déterminants de la sécurité alimentaire — filière maïs",
    subtitle = "Coefficients de régression avec intervalles de confiance à 95 %",
    x = "Coefficient estimé (effet marginal)",
    y = NULL,
    color = "Modèle (variable dépendante)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(graphique_coefs_m5)

ggsave("sorties/Sorties_module_5/coefficients_m5_fies_hdds.png",
       graphique_coefs_m5, width = 9, height = 6, dpi = 300)

cat("Graphique sauvegardé : sorties/Sorties_module_5/coefficients_m5_fies_hdds.png\n")


# --- Extraction, réutilise la même fonction que pour le graphique précédent ---
extraire_coefs <- function(modele, nom_modele) {
  broom::tidy(modele, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(modele = nom_modele)
}

coefs_hetero <- bind_rows(
  extraire_coefs(reg_m5_hetero_coop,  "Interaction coopérative"),
  extraire_coefs(reg_m5_hetero_irrig2, "Interaction irrigation")
)

# --- Labels lisibles, y compris pour les termes d'interaction ---
labels_var_hetero <- c(
  producteur_mais                        = "Producteur de maïs",
  ln_revenu_mais                         = "ln(Revenu vente maïs)",
  taux_vente_mais                        = "Taux de commercialisation",
  cooperative_presente                   = "Coopérative présente (village)",
  irrigation_village                     = "Irrigation pratiquée (village)",
  hhsize                                 = "Taille du ménage",
  educ_chef_scolarise                    = "Chef scolarisé",
  possede_terre                          = "Possède des terres",
  "taux_vente_mais:cooperative_presente" = "Taux vente \u00d7 Coopérative",
  "producteur_mais:irrigation_village"   = "Producteur \u00d7 Irrigation"
)

coefs_hetero <- coefs_hetero %>%
  mutate(
    term_label = recode(term, !!!labels_var_hetero),
    # on distingue visuellement les termes d'interaction du reste
    type_terme = if_else(str_detect(term, ":"), "Terme d'interaction", "Effet principal")
  )

# --- Ordre d'affichage : on garde les interactions en haut de chaque facette ---
coefs_hetero <- coefs_hetero %>%
  group_by(modele) %>%
  mutate(term_label = fct_reorder(term_label, type_terme == "Terme d'interaction")) %>%
  ungroup()

# --- Graphique en deux facettes ---
graphique_hetero_m5 <- ggplot(coefs_hetero,
                              aes(x = estimate, y = term_label, color = type_terme)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.6) +
  scale_color_manual(values = c("Effet principal" = "#5D6D7E",
                                "Terme d'interaction" = "#C0392B")) +
  facet_wrap(~modele, scales = "free_y") +
  labs(
    title = "Hétérogénéité spatiale de l'effet filière sur le score FIES",
    subtitle = "Coefficients avec IC 95 % — variable dépendante : score FIES (0-8, \u2191 = pire)",
    x = "Coefficient estimé", y = NULL, color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

print(graphique_hetero_m5)

ggsave("sorties/Sorties_module_5/coefficients_m5_heterogeneite.png",
       graphique_hetero_m5, width = 11, height = 6, dpi = 300)

cat("Graphique sauvegardé : sorties/Sorties_module_5/coefficients_m5_heterogeneite.png\n")
saveRDS(coefs_m5, "sorties/tab_m5_fies.rds")
