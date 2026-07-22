# =============================================================================
#  SCRIPT DE DÉMONSTRATION - UTILISATION DU PACKAGE filiereBFA
# =============================================================================
#  Ce script montre comment utiliser les fonctions du package pour analyser
#  la filière d'un produit (ici, le Maïs) de A à Z.
# =============================================================================

# 1. Charger le package (et les autres outils nécessaires)
library(filiereBFA)
library(tidyverse)
library(haven)
library(readxl)
library(sf)
library(leaflet)

# 2. Définir les paramètres de l'analyse
dossier_donnees <- "donnee/base_burkina"          # Dossier contenant les .dta
fichier_excel_conv <- "donnee/Table de conversion phase 2.xlsx" # Table de conversion S07B

produit_nom <- "Maïs"
codes_prod_mais  <- 4L               # Code maïs dans S16C/S16D (production)
codes_conso_mais <- c(5, 6, 12, 13)  # Codes maïs dans S07B (conso : épi, grain, farine, semoule)


# =============================================================================
# MODULE 1 & 2 : Chargement, Typologie et Profilage
# =============================================================================

cat("\n--- MODULE 1 & 2 : Chargement et Profilage ---\n")

# Charger toutes les données et créer la typologie (producteur/conso)
data_filiere <- load_filiere(
  dossier = dossier_donnees,
  produit = produit_nom,
  codes_prod = codes_prod_mais,
  codes_conso = codes_conso_mais,
  pays = "BFA"
)

# Calculer le score FIES (sécurité alimentaire)
fies_data <- calc_fies(data_filiere$s08, data_filiere$welfare)

# Calculer le score HDDS (diversité alimentaire)
# On a besoin de la table de passage produit -> groupe FAO
passage_fao <- tibble::tribble(
  ~code_produit, ~groupe_fao,
  1, "Cereales", 2, "Cereales", 3, "Cereales", 4, "Cereales",
  5, "Cereales", 6, "Cereales", 7, "Cereales", 8, "Cereales",
  9, "Cereales", 10, "Cereales", 11, "Cereales", 12, "Cereales",
  13, "Cereales", 14, "Cereales", 15, "Cereales", 16, "Cereales",
  17, "Cereales", 18, "Cereales", 19, "Cereales", 20, "Cereales",
  130, "Cereales", 131, "Cereales", 132, "Cereales",
  165, "Cereales", 166, "Cereales", 167, "Cereales",
  168, "Cereales", 169, "Cereales",
  123, "Tubercules", 124, "Tubercules", 125, "Tubercules",
  126, "Tubercules", 127, "Tubercules", 128, "Tubercules",
  129, "Tubercules", 178, "Tubercules",
  88, "Legumes", 89, "Legumes", 90, "Legumes", 91, "Legumes",
  92, "Legumes", 93, "Legumes", 94, "Legumes", 95, "Legumes",
  96, "Legumes", 97, "Legumes", 98, "Legumes", 99, "Legumes",
  100, "Legumes", 101, "Legumes", 102, "Legumes", 103, "Legumes",
  104, "Legumes", 105, "Legumes", 106, "Legumes", 177, "Legumes",
  71, "Fruits", 72, "Fruits", 73, "Fruits", 74, "Fruits",
  75, "Fruits", 76, "Fruits", 77, "Fruits", 78, "Fruits",
  79, "Fruits", 80, "Fruits", 133, "Fruits", 176, "Fruits",
  27, "Viande", 28, "Viande", 29, "Viande", 30, "Viande",
  31, "Viande", 32, "Viande", 33, "Viande", 34, "Viande",
  35, "Viande", 36, "Viande", 37, "Viande", 38, "Viande",
  39, "Viande", 170, "Viande",
  60, "Oeufs", 61, "Oeufs", 62, "Oeufs",
  40, "Poisson", 41, "Poisson", 42, "Poisson", 43, "Poisson",
  44, "Poisson", 45, "Poisson", 46, "Poisson", 47, "Poisson",
  48, "Poisson", 49, "Poisson", 50, "Poisson", 51, "Poisson",
  171, "Poisson", 172, "Poisson", 179, "Poisson",
  110, "Legumineuses", 111, "Legumineuses", 112, "Legumineuses",
  113, "Legumineuses", 114, "Legumineuses", 115, "Legumineuses",
  116, "Legumineuses", 117, "Legumineuses", 118, "Legumineuses",
  119, "Legumineuses", 120, "Legumineuses", 121, "Legumineuses",
  122, "Legumineuses",
  52, "Lait", 53, "Lait", 54, "Lait", 55, "Lait",
  56, "Lait", 173, "Lait",
  63, "Huiles", 64, "Huiles", 65, "Huiles", 66, "Huiles",
  67, "Huiles", 68, "Huiles", 69, "Huiles", 70, "Huiles", 175, "Huiles",
  134, "Sucre", 135, "Sucre", 136, "Sucre", 137, "Sucre"
)

hdds_data <- calc_hdds(data_filiere$s07b, passage_fao)

# Générer le tableau comparatif des 4 groupes
profil <- profil_menage(data_filiere, fies_data, hdds_data)
print(profil)


# =============================================================================
# MODULE 3 : Analyse de la Production et des Rendements
# =============================================================================

cat("\n--- MODULE 3 : Production et Rendements ---\n")

# Charger la table de conversion (utilisée pour la commercialisation, Module 4)
table_conv <- read_excel(fichier_excel_conv, sheet = "nationale") %>%
  mutate(poids = as.numeric(gsub(",", ".", gsub(";", ".", gsub(" ", "", poids))))) %>%
  filter(!is.na(poids)) %>%
  mutate(Key = paste0(produitID, uniteID, tailleID)) # Ajout de la Key (Cours 7)

# Calculer les rendements (la base NSU est déjà chargée par load_filiere()
# dans data_filiere$nsu ; calc_rendement() l'utilise automatiquement)
rendement_data <- calc_rendement(data_filiere, codes_prod_mais)

cat("Rendement moyen national (kg/ha) :", round(weighted.mean(rendement_data$rendement_kg_ha, rendement_data$hhweight, na.rm=TRUE)), "\n")


# =============================================================================
# MODULE 4 : Analyse de la Commercialisation et des Prix
# =============================================================================

cat("\n--- MODULE 4 : Commercialisation et Prix ---\n")

# Calculer la chaîne de prix (producteur, conso, marge)
prix_data <- prix_chaine(data_filiere, codes_prod_mais, codes_conso_mais, table_conv)

# Afficher la marge commerciale par région
print(prix_data$marge_region)


# =============================================================================
# CARTOGRAPHIE (Modules 3 & 4)
# =============================================================================

cat("\n--- CARTOGRAPHIE ---\n")

# 1. Préparer les données du rendement par grappe (village)
rendement_grappe <- rendement_data %>%
  left_join(data_filiere$welfare %>% select(hhid, grappe), by = "hhid") %>%
  filter(!is.na(grappe)) %>%
  group_by(grappe) %>%
  summarise(rendement_moyen = weighted.mean(rendement_kg_ha, hhweight, na.rm = TRUE))

# 2. Générer la carte interactive avec la fonction du package
carte_rendement <- carte_filiere(
  data = data_filiere,
  indicateur = rendement_grappe,
  nom_col = "rendement_moyen",
  titre = "Rendement maïs (kg/ha)"
)

# 3. Sauvegarder la carte
library(htmlwidgets)
dir.create("sorties", showWarnings = FALSE)
saveWidget(carte_rendement, "sorties/carte_rendement_package.html", selfcontained = TRUE)
cat("Carte du rendement sauvegardée dans 'sorties/'.\n")


# =============================================================================
# MODULE 5 : Régression d'impact (Sécurité Alimentaire)
# =============================================================================
# Note : La régression nécessite de construire une base complète. 
# Ici on montre comment appeler la fonction reg_filiere() une fois la base prête.

# data_reg_complete <- data_filiere$typologie %>%
#   left_join(fies_data, by = "hhid") %>%
#   left_join(rendement_data, by = "hhid")
# 
# modele_final <- reg_filiere(
#   data_reg = data_reg_complete,
#   outcome = "score_fies",
#   filiere_vars = c("producteur", "taux_vente_mais"),
#   controls = c("hage", "hhsize", "ln_pcexp"),
#   weights = "hhweight",
#   cluster = "grappe"
# )
# summary(modele_final)

cat("\nDémonstration terminée. Le package filiereBFA fonctionne !\n")
