#=======================================================================
#          MODULE 3 : ANALYSE DE LA PRODUCTION ET DES RENDEMENTS
#=======================================================================
#
# Carnet de decisions Module 3 (a documenter dans le rapport)
# -----------------------------------------------------------
# 1. s16cq11 = "Avez-vous FINI la récolte de cette culture ?"
#      -> 1 [Oui]  = récolte TERMINÉE  (91 % des ménages maïs)
#      -> 2 [Non]  = récolte EN COURS  (9 %)
#      Les variables q16a/b/c ne sont renseignées QUE pour "Non".
# 2. s16cq16c = "Estimation Quantité totale UML en kg" -> déjà en kg,
#      PAS un facteur de conversion. Ne pas diviser par part récoltée.
# 3. Production "récolte terminée" reconstituée depuis S16D :
#      somme(conso + don + vente + stock) en kg.
#      s16dq05c (estimation en kg de la vente) utilisée prioritairement.
# 4. Conversion des unités locales (conso/don/stock) via la table de
#      conversion phase 2, matchant (produitID, uniteID), moyennée sur
#      les tailles (S16D ne déclare pas de tailleID).
#      produitID dépend de l'état : épi = 5, grain = 6.
#      Unitions non couvertes (Yorouba=3, Tine=4, Autres=7) -> NA,
#      environ 30-40 % des observations -> sous-estimation à documenter.
# 5. Micro-parcelles (< 0.05 ha) exclues : non représentatives des
#      rendements paysans (jardins de case).
# 6. Plafond agronomique 5 000 kg/ha + winsorisation p1/p99.
# 7. Rendement national estimé ~750 kg/ha vs ~1 700 FAOSTAT (-56 %),
#      écart attendu : récolte partielle (branche "en cours"), unités
#      non converties, et production S16D partielle à l'instant de
#      l'enquête.

# --- Référentiel de codes produit -------------------------------------------
# S16C et S16D partagent le même référentiel : "4" = Maïs.
# Ce référentiel est DIFFERENT de celui de S07B (codes_mais <- c(5,6,12,13)
# utilisé uniquement pour la consommation, Module 1). Ne pas confondre.
codes_mais_s16c <- 4

# Packages nécessaires
library(httr)
library(jsonlite)
library(purrr)
library(dplyr)
library(fixest)
library(sf)
library(geodata)

# Pré-requis : préambule.R et module1 déjà sourcés depuis main.R.

# ---------------------------------------------------------------------
# 1. SURFACE DE LA PARCELLE (S16A) + PART PLANTÉE EN MAÏS
# ---------------------------------------------------------------------
parcelles <- s16a %>%
  transmute(
    hhid, s16aq02, s16aq03,
    surface_parcelle_ha = case_when(
      s16aq09b == 1 ~ s16aq09a,
      s16aq09b == 2 ~ s16aq09a / 10000,
      TRUE          ~ NA_real_)
  ) %>%
  distinct(hhid, s16aq02, s16aq03, .keep_all = TRUE)

# Part de la parcelle consacrée au maïs (S16C q07 = culture pure ; q08 = %)
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

# Surface totale maïs par ménage (S16D = 1 ligne / hhid / culture)
surface_mais_hh <- surface_mais %>%
  group_by(hhid) %>%
  summarise(surface_mais_ha = sum(surface_mais_ha, na.rm = TRUE),
            .groups = "drop")

# ---------------------------------------------------------------------
# 2. TABLE DE CONVERSION EN kg (produitID, uniteID)
#    Moyennée sur les tailles car S16D ne déclare pas la tailleID.
# ---------------------------------------------------------------------
conv_raw <- read_excel("donnee/Table de conversion phase 2.xlsx",
                       sheet = "nationale") %>%
  mutate(poids = as.numeric(gsub(",", ".",
                                 gsub(";", ".",
                                      gsub(" ", "", poids))))) %>%
  filter(!is.na(poids), produitID %in% c(5, 6))

conv_mais <- conv_raw %>%
  group_by(produitID, uniteID) %>%
  summarise(poids_g = mean(poids, na.rm = TRUE), .groups = "drop") %>%
  mutate(poids_kg = poids_g / 1000)

# Correspondance code-unité S16D (1-7) -> uniteID table de conversion.
# Yorouba (3), Tine (4), Autres (7) : absents de la table -> NA.
# Sac moyen (5) -> 138 (Sac 50 kg) ; Sac gros (6) -> 135 (Sac 100 kg).
corresp_unite <- tibble(
  code_unite_16d = c(1, 2, 3, 4, 5, 6, 7),
  uniteID_conv   = c(100, NA, NA, NA, 138, 135, NA)
)

# ---------------------------------------------------------------------
# 3. FONCTION DE CONVERSION kg D'UNE QUANTITÉ S16D
#    L'état du produit (épi vs grain) détermine le produitID.
# ---------------------------------------------------------------------
convertir_kg_16d <- function(df, var_qte, var_unite, var_etat) {
  tmp <- df %>%
    mutate(
      .qte       = .data[[var_qte]],
      code_unite = .data[[var_unite]],
      etat       = .data[[var_etat]],
      produitID  = case_when(
        etat == 1            ~ 5,   # Épi
        etat %in% c(2, 3, 4) ~ 6,   # Grain / décortiqué / non-décortiqué
        TRUE                 ~ NA_real_)
    ) %>%
    left_join(corresp_unite,
              by = c("code_unite" = "code_unite_16d")) %>%
    left_join(conv_mais,
              by = c("produitID", "uniteID_conv" = "uniteID"))
  tmp$.qte * tmp$poids_kg
}

# ---------------------------------------------------------------------
# 4. BRANCHE A : MÉNAGES "RÉCOLTE TERMINÉE" (s16cq11 == 1, ~91 %)
#    Production = somme des usages déclarés en S16D
#    (conso + don + vente + stock), chacun converti en kg.
#    La vente s16dq05c (kg directs) est utilisée prioritairement.
# ---------------------------------------------------------------------
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

# Diagnostic de couverture
cat("Couverture de la conversion par poste (récolte terminée) :\n")
summary(s16d_mais_fini[c("conso_kg", "don_kg", "vente_kg", "stock_kg")])

production_fini <- s16d_mais_fini %>%
  left_join(surface_mais_hh, by = "hhid") %>%
  mutate(source_production = "S16D - récolte terminée") %>%
  filter(is.finite(surface_mais_ha), surface_mais_ha > 0,
         is.finite(production_kg),  production_kg > 0)

cat("Ménages 'récolte terminée' exploitables (production reconstituée) :",
    nrow(production_fini), "sur", nrow(menages_fini), "\n")

# ---------------------------------------------------------------------
# 5. BRANCHE B : MÉNAGES "RÉCOLTE EN COURS" (s16cq11 == 2)
#    s16cq16c = "Estimation Quantité totale UML en kg" -> déjà en kg.
#    ATTENTION : ne pas diviser par la part récoltée.
# ---------------------------------------------------------------------
production_encours <- s16c %>%
  filter(s16cq04 %in% codes_mais_s16c,
         s16cq11 == 2, !is.na(s16cq16c)) %>%
  left_join(surface_mais_hh, by = "hhid") %>%
  mutate(
    production_kg     = s16cq16c,
    source_production = "S16C - récolte en cours"
  ) %>%
  filter(is.finite(surface_mais_ha), surface_mais_ha > 0,
         is.finite(production_kg),  production_kg > 0)

cat("Ménages 'récolte en cours' exploitables :", nrow(production_encours),
    "\n")

# ---------------------------------------------------------------------
# 6. FUSION + RENDEMENT
# ---------------------------------------------------------------------
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

# ---------------------------------------------------------------------
# 7. FILTRAGE AGRONOMIQUE + WINSORISATION p1/p99
#    Le maïs pluvial au Burkina Faso ne dépasse pas ~5 t/ha en
#    pratique paysanne. On applique :
#      a) seuil minimal de surface >= 0.05 ha (micro-parcelles
#         / jardins de case non représentatifs) ;
#      b) plafond agronomique rendement <= 5 000 kg/ha ;
#      c) winsorisation p1/p99 sur le reste.
# ---------------------------------------------------------------------
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

# ---------------------------------------------------------------------
# 8. BILAN NATIONAL
# ---------------------------------------------------------------------
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

# ---------------------------------------------------------------------
# 9. VALIDATION FAOSTAT
#    Le rendement FAOSTAT du maïs au BF est ~1 700 kg/ha
#    (1,7 t/ha campagne 2021). Source : FAOSTAT, Module 1.
# ---------------------------------------------------------------------
rendement_faostat_kg_ha <- 1520.8
cat(sprintf("Rendement national estimé : %.0f kg/ha\n",
            bilan_mais$rendement_national_kg_ha))
cat(sprintf("Rendement FAOSTAT (référence) : %.0f kg/ha\n",
            rendement_faostat_kg_ha))
cat(sprintf("Écart : %+.1f %%\n",
            100 * (bilan_mais$rendement_national_kg_ha / rendement_faostat_kg_ha - 1)))

# ---------------------------------------------------------------------
# 10. BILAN PAR SOURCE (diagnostic de robustesse)
# ---------------------------------------------------------------------
bilan_mais_source <- production_mais_analyse %>%
  group_by(source_production) %>%
  summarise(
    observations = n(),
    superficie_ha = sum(surface_mais_ha * hhweight, na.rm = TRUE),
    production_tonnes = sum(production_kg * hhweight, na.rm = TRUE) / 1000,
    rendement_median_kg_ha = median(rendement_kg_ha, na.rm = TRUE),
    .groups = "drop")
print(bilan_mais_source)

# ---------------------------------------------------------------------
# 11. PERTES (analyse séparée, comme demandé par le sujet p.9)
# ---------------------------------------------------------------------
pertes <- s16c %>%
  filter(s16cq04 %in% codes_mais_s16c) %>%
  mutate(perte_pct = coalesce(as.numeric(s16cq15), 0)) %>%
  summarise(
    n_total = n(),
    n_perte_totale = sum(as.numeric(s16cq11) == 3, na.rm = TRUE),
    perte_moyenne_pct = mean(perte_pct, na.rm = TRUE))
print(pertes)

# ---------------------------------------------------------------------
# 12. GRAPHIQUES
# ---------------------------------------------------------------------
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

# ---------------------------------------------------------------------
# 13. INTRANTS DES MÉNAGES PRODUCTEURS DE MAÏS (S16B)
#      S16B est renseigné au niveau ménage, pas au niveau culture :
#      ces intrants décrivent les ménages producteurs de maïs, pas des
#      intrants exclusivement dédiés au maïs (limite à mentionner).
# ---------------------------------------------------------------------
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

# ln(Rendement_ij) = α + β1 Intrants_ij + β2 Semences_ij + β3 Irrigation_ij
#                     + β4 Education_i + γj (FE grappe) + ε_ij

menages_mais_reg <- production_mais_analyse %>% distinct(hhid, hhweight)

# --- 1. Intrants : valeur totale des intrants utilisés (S16B) --------------
intrants_valeur_hh <- s16b %>%
  semi_join(menages_mais_reg, by = "hhid") %>%
  filter(s16bq02 == 1) %>%
  group_by(hhid) %>%
  summarise(valeur_intrants_fcfa = sum(s16bq09c, na.rm = TRUE), .groups = "drop")

# --- 2. Semences améliorées (S16C, s16cq09 : 1=Locales confirmé, 2=Améliorées confirmé) --
semence_hh <- s16c %>%
  filter(s16cq04 %in% codes_mais_s16c) %>%
  semi_join(menages_mais_reg, by = "hhid") %>%
  group_by(hhid) %>%
  summarise(semence_amelioree = as.integer(any(s16cq09 == 2, na.rm = TRUE)),
            .groups = "drop")

# --- 3. Irrigation (S16A, s16aq17 : source d'eau de la parcelle, confirmé) --
irrigation_hh <- s16a %>%
  semi_join(menages_mais_reg, by = "hhid") %>%
  mutate(source_eau = as_factor(s16aq17),
         irr_parcelle = case_when(
           source_eau == "Pluviale" ~ 0L,
           source_eau %in% c("Irrigation, propre puits", "Irrigation canal",
                             "Irrigation ruisseau", "Marais/\"wetlands\"") ~ 1L,
           TRUE ~ NA_integer_)) %>%
  group_by(hhid) %>%
  summarise(irrigation = as.integer(any(irr_parcelle == 1, na.rm = TRUE)),
            .groups = "drop")

# --- 4. Éducation du chef de ménage (S1 + S02, confirmé) -------------------
chef_menage <- s01 %>%
  filter(as_factor(s01q02) == "Chef de ménage") %>%
  distinct(hhid, pid)

education_hh <- chef_menage %>%
  left_join(s02_me %>% select(hhid, pid, s02q03), by = c("hhid", "pid")) %>%
  mutate(
    education_scolarise = case_when(
      s02q03 == 1 ~ 1L,   # Oui, a fait/fait des études
      s02q03 == 2 ~ 0L,   # Non, jamais scolarisé
      TRUE ~ NA_integer_
    )
  ) %>%
  select(hhid, education_scolarise)

cat("Chefs de ménage sans info exploitable sur l'éducation :",
    sum(is.na(education_hh$education_scolarise)), "sur", nrow(education_hh), "\n")

# --- 5. Grappe (déjà présente nativement dans s16c) -------------------------
grappe_hh <- s16c %>% distinct(hhid, grappe)

#=======================================================================
# ASSEMBLAGE DE LA TABLE DE RÉGRESSION
#=======================================================================
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

#=======================================================================
# RÉGRESSION OLS AVEC EFFETS FIXES DE GRAPPE ET ERREURS ROBUSTES PONDÉRÉES
#=======================================================================
reg_rendement_mais <- feols(
  ln_rendement ~ ln_intrants + semence_amelioree + irrigation + education_scolarise | grappe,
  data    = data_reg_mais,
  weights = ~hhweight,
  cluster = ~grappe
)
summary(reg_rendement_mais)

# Spécification sans effets fixes de grappe, pour comparaison/robustesse
reg_rendement_mais_sansFE <- feols(
  ln_rendement ~ ln_intrants + semence_amelioree + irrigation + education_scolarise,
  data = data_reg_mais, weights = ~hhweight
)

etable(reg_rendement_mais, reg_rendement_mais_sansFE,
       headers = c("Avec FE grappe", "Sans FE grappe"))

# 1. Rendement moyen par grappe
rendement_grappe <- production_mais_analyse %>%
  left_join(grappe_hh, by = "hhid") %>%
  group_by(grappe) %>%
  summarise(
    n_menages = n(),
    rendement_moyen_kg_ha = sum(rendement_kg_ha * hhweight, na.rm = TRUE) /
      sum(hhweight, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Coordonnées GPS moyennes par grappe
gps_grappe <- s00 %>%
  filter(!is.na(GPS__Latitude), !is.na(GPS__Longitude)) %>%
  group_by(grappe) %>%
  summarise(lat = mean(GPS__Latitude, na.rm = TRUE),
            lon = mean(GPS__Longitude, na.rm = TRUE),
            .groups = "drop")

# 3. Fusion
rendement_geo <- rendement_grappe %>%
  inner_join(gps_grappe, by = "grappe") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

cat("Grappes cartographiables :", nrow(rendement_geo), "sur", nrow(rendement_grappe), "\n")

# 4. Fond de carte
bfa_shp <- gadm(country = "BFA", level = 1, path = tempdir()) %>% st_as_sf()

# 5. Carte
carte_rendement_mais <- ggplot() +
  geom_sf(data = bfa_shp, fill = "grey95", color = "grey60") +
  geom_sf(data = rendement_geo,
          aes(size = n_menages, color = rendement_moyen_kg_ha),
          alpha = 0.75) +
  scale_color_viridis_c(name = "Rendement\n(kg/ha)", option = "viridis") +
  scale_size_continuous(name = "Nb. ménages\nproducteurs", range = c(1, 6)) +
  theme_minimal() +
  labs(title = "Rendement moyen du maïs par grappe — Burkina Faso (2021)",
       subtitle = "Chaque point = une grappe EHCVM ; taille = nombre de ménages producteurs",
       caption = "Source : EHCVM 2021, calculs des auteurs") +
  theme(axis.title = element_blank())

ggsave("sorties/sorties_module_3/carte_rendement_mais.png", carte_rendement_mais, width = 10, height = 8, dpi = 300)

#--- Rendement + Pluie ------------------------

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

