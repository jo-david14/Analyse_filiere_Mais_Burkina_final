#=======================================================================
#  ANALYSE FILIÈRE MAÏS — BURKINA FASO (EHCVM 2021)
#  Projet ISEP2 : filière et sécurité alimentaire (UEMOA)
#
#  Point d'entrée unique : exécuter ce fichier lance toute la chaîne.
#  Les modules sont dans ./scripts/ et sont sourcés dans l'ordre :
#    préambule -> module1 -> module2 -> module3 -> module4 -> module5
#  Chaque module NE doit PAS re-sourcer les autres (déjà fait ici).
#=======================================================================

# --- Environnement de travail -------------------------------------------
# setwd("C:/Users/ndaoa/Documents/ISEP2/R/Projet/Projet R")
Sys.setlocale("LC_ALL", "French_France.65001")   # UTF-8 pour les chemins accentués

# --- Packages -----------------------------------------------------------
library(tidyverse)
library(haven)
library(ggplot2)
library(stringr)
library(readxl)
library(labelled)
library(fixest)
library(sf)
library(geodata)
library(httr)
library(jsonlite)
library(purrr)
library(gstat)
library(leaflet)
library(terra)
library(htmlwidgets)
library(broom)

# --- Dossiers de sorties ------------------------------------------------
for (d in c("sorties",
            "sorties/Sorties_preambule",
            "sorties/Sorties_module_1",
            "sorties/Sorties_module_2",
            "sorties/sorties_module_3",
            "sorties/Sorties_module_4",
            "sorties/Sorties_module_5",
            "dashboard/data")) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

#=======================================================================
#                            PREAMBULE
#  Import des données EHCVM + table de conversion
#=======================================================================
dossier <- "donnee/base_burkina"

fichiers <- c(
  ponderation       = "ehcvm_ponderations_bfa2021.dta",
  ehcvm_welfare_2b  = "ehcvm_welfare_2b_bfa2021.dta",
  nsu               = "ehcvm_nsu_bfa2021.dta",
  calories          = "calorie_conversion_wa_2021.dta",
  s00    = "s00_me_bfa2021.dta",
  s01    = "s01_me_bfa2021.dta",
  s02_me = "s02_me_bfa2021.dta",
  s07a1  = "s07a_1_me_bfa2021.dta",
  s07a2  = "s07a_2_me_bfa2021.dta",
  s07b   = "s07b_me_bfa2021.dta",
  s08a   = "s08a_me_bfa2021.dta",
  s10a   = "s10a_me_bfa2021.dta",
  s10b   = "s10b_me_bfa2021.dta",
  s16a   = "s16a_me_bfa2021.dta",
  s16b   = "s16b_me_bfa2021.dta",
  s16c   = "s16c_me_bfa2021.dta",
  s16d   = "s16d_me_bfa2021.dta",
  s17    = "s17_me_bfa2021.dta",
  s19    = "s19_me_bfa2021.dta",
  s01_co = "s01_co_bfa2021.dta",
  s02_co = "s02_co_bfa2021.dta",
  s03_co = "s03_co_bfa2021.dta"
)

importer <- function(liste) {
  for (i in names(liste)) {
    assign(i, read_dta(file.path(dossier, liste[[i]])), envir = .GlobalEnv)
  }
}

# Importation des bases dans l'environnement global
importer(fichiers)
Table_de_conversion <- read_excel("donnee/Table de conversion phase 2.xlsx")

#=======================================================================
#  EXÉCUTION DES MODULES (dans l'ordre, une seule fois chacun)
#=======================================================================
source("scripts/préambule.R")
source("scripts/module1_choix_et_justification.R")
source("scripts/module2_profilage.R")
source("scripts/module3_production_et_rendements.R")
source("scripts/module4_commercialisation.R")
source("scripts/module5_securite_alimentaire.R")

#=======================================================================
#  PREPARATION DES DONNEES DU DASHBOARD SHINY
#  (calcule les objets derives manquants + exporte tous les .rds
#   attendus par dashboard/app.R dans dashboard/data/)
#=======================================================================
if (file.exists("dashboard/data_prep/preparer_donnees_dashboard.R")) {
  source("dashboard/data_prep/preparer_donnees_dashboard.R")
} else {
  cat("Note: Le script 'dashboard/data_prep/preparer_donnees_dashboard.R' est manquant, saut de la préparation du dashboard.\n")
}

cat("\n=== CHAÎNE COMPLÈTE TERMINÉE ===\n")
cat("Pour lancer le dashboard : shiny::runApp(\"dashboard\")\n")
