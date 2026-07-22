# filiereBFA <img src="man/figures/logo.png" align="right" height="120" alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Description

`filiereBFA` est un package R développé dans le cadre du projet **ISEP2 (Filière et Sécurité Alimentaire - UEMOA)** pour le **Binôme 3 (Burkina Faso)**.

Il encapsule toute la chaîne analytique (Modules 1 à 5 du sujet) pour analyser la filière d'un produit stratégique (maïs, mil, sorgho...) à partir des données de l'Enquête Harmonisée sur les Conditions de Vie des Ménages (EHCVM 2021/2022).

Le package permet de calculer des rendements agricoles, des prix, des marges commerciales, des indicateurs de sécurité alimentaire (FIES, HDDS), de profiler les ménages et de cartographier les résultats.

## Installation

Pour installer ce package depuis le dossier local :

```r
# Si devtools n'est pas installé
# install.packages("devtools")

# Installation du package local
devtools::install("C:/Users/hadem/Projets_Persos/projet_R/filiereBFA")
```

## Les 8 fonctions principales

| Fonction | Rôle | Module du sujet |
|---|---|---|
| `load_filiere()` | Charge et fusionne les données EHCVM + crée la typologie | Préambule |
| `calc_fies()` | Calcule le score FIES (0-8) et les seuils de faim | Module 2 & 5 |
| `calc_hdds()` | Calcule le score de diversité alimentaire (0-12) | Module 2 & 5 |
| `profil_menage()` | Génère le tableau comparatif des 4 groupes (Prod/Conso) | Module 2 |
| `calc_rendement()` | Calcule le rendement en kg/ha (avec filtres agronomiques) | Module 3 |
| `prix_chaine()` | Calcule les prix producteur/conso et la marge commerciale | Module 4 |
| `carte_filiere()` | Génère une carte interactive (leaflet) d'un indicateur | Module 3 & 4 |
| `reg_filiere()` | Lance la régression d'impact (FIES ~ filière) avec `fixest` | Module 5 |

---

## Exemple d'utilisation (Workflow complet)

Voici comment analyser la filière du **Maïs** au Burkina Faso en quelques lignes de code, du chargement à la régression finale.

```r
library(filiereBFA)
library(tidyverse)

# 1. Paramétrisation du produit (ex: le Maïs)
dossier_donnees <- "Données transversales"
codes_prod_mais  <- 4L               # Code maïs dans S16C (production)
codes_conso_mais <- c(5, 6, 12, 13)  # Codes maïs dans S07B (conso : épi, grain, farine, semoule)

# 2. Chargement des données et création de la typologie
data <- load_filiere(
  dossier = dossier_donnees,
  produit = "Maïs",
  codes_prod = codes_prod_mais,
  codes_conso = codes_conso_mais,
  pays = "BFA"
)

# 3. Calcul des indicateurs transversaux (FIES et HDDS)
fies <- calc_fies(data$s08, data$welfare)

# Pour le HDDS, il faut une table de passage produit -> groupe FAO
# (incluse dans le package ou à fournir)
data(passage_fao, package = "filiereBFA")
hdds <- calc_hdds(data$s07b, passage_fao)

# 4. Profilage des ménages (Module 2)
tableau_profil <- profil_menage(data, fies, hdds)
print(tableau_profil)

# 5. Analyse de la production (Module 3)
table_conv <- readxl::read_excel("Table de conversion phase 2.xlsx", sheet = "nationale")
rendement <- calc_rendement(data, codes_prod_mais, table_conv)

# 6. Analyse de la commercialisation (Module 4)
prix <- prix_chaine(data, codes_prod_mais, codes_conso_mais, table_conv)
print(prix$marge_region)

# 7. Cartographie (Modules 3 & 4)
# Carte des rendements par grappe
rendement_grappe <- rendement %>%
  left_join(data$welfare %>% select(hhid, grappe), by = "hhid") %>%
  group_by(grappe) %>%
  summarise(rendement_moyen = mean(rendement_kg_ha, na.rm = TRUE))

carte <- carte_filiere(data, rendement_grappe, "rendement_moyen", "Rendement (kg/ha)")

# 8. Régression d'impact (Module 5)
# (Nécessite d'avoir assemblé la base de régression complète)
#modele <- reg_filiere(
#  data_reg = base_reg_m5,
#  outcome = "score_fies",
#  filiere_vars = c("producteur_mais", "taux_vente_mais"),
#  controls = c("age_chef", "taille_menage", "ln_pcexp"),
#  weights = "hhweight",
#  cluster = "grappe"
#)
#summary(modele)
```

## Généralisation

Ce package a été conçu pour être **généralisable**. Pour analyser un autre produit (ex: le Mil au Sénégal), il suffit de changer les paramètres au début du script :

```r
# Analyse du Mil au Senegal
data_mil <- load_filiere(
  dossier = "donnees_senegal",
  produit = "Mil",
  codes_prod = 1L,
  codes_conso = c(7, 14, 15),
  pays = "SEN"
)
```
