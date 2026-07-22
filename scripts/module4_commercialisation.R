###############################################################################
# MODULE 4 - COMMERCIALISATION (PARTIE S16D UNIQUEMENT)
###############################################################################
# Objectif : Analyser la commercialisation du produit X cote producteur.
#
# Conformement au sujet (Module 4, partie "Depuis S16D") :
#   - Taux de commercialisation = quantite vendue / quantite totale produite
#   - Prix producteur unitaire  = montant total vente (s16dq06) / quantite vendue
#   - Type d'acheteur (s16dq08) : repartition des canaux
#   - Methode de stockage (s16dq11) + pertes post-recolte (graphique)
#
#  IMPORTANTS POINTS VERIFIES DANS LES DONNEES :
#   - Le code culture est dans s16dq01 (PAS BESOIN de fusionner avec s16c)
#   - s16dq05b contient des codes 1-7 (unites) + codes bizarres (8,10,13,31,43...)
#     qui sont en fait des codes cultures mal enregistres -> A EXCLURE (filtre <=7)
#   - Les unites de production (Yorouba, Tine, Sac) ne sont PAS dans l'Excel
#     de conversion (qui est pour la consommation). On utilise le NSU.
###############################################################################

library(haven)
library(tidyverse)
library(readxl)
library(labelled)
library(ggplot2)
library(sf)
library(gstat)
library(leaflet)
library(terra)
library(htmlwidgets)

## =============================================================================
##  PARAMETRE : LE PRODUIT X
## =============================================================================
X <- "Maïs"
code_culture_X <- 4 


## =============================================================================
##  ETAPE 0 - PREPARATION DES POIDS DE CONVERSION (NSU, par region x milieu)
## =============================================================================
# Les unites de production (Sac, Yorouba, Tine...) ne sont pas dans l'Excel
# de conversion (reserve a la consommation S07B). On utilise le NSU qui donne
# un poids_moyen (en grammes) par produit x unite x strate (region x milieu).
#
# Table de passage : code unite de s16d (1-7) -> uniteID du NSU
passage_unite_nsu <- tibble::tribble(
  ~s16_unite, ~uniteID_nsu, ~nom_unite,
  1, 100,     "Kilogramme",   # Kg -> poids = 1000 g (direct)
  3, 149,     "Yorouba",      # Plat Yoruba
  4, 145,     "Tine",
  5, 136,     "Sac moyen",    # ~ Sac 25 kg
  6, 138,     "Sac gros"      # ~ Sac 50 kg
)

# Poids moyen de chaque unite (NSU) - VERSION SIMPLIFIEE (1 seule cle : unite)
# On prend une moyenne nationale par unite, sans croiser region/milieu/produit,
# car la jointure à 3 cles elimine presque toutes les lignes (cf. diagnostic).
# ATTENTION : poids_sd contient des NA qui contaminent weighted.mean -> on les met a 0.
poids_unites <- nsu %>%   # NB: dans import_bases_ehcvm.R la cle est 'ehcvm_nsu'
  filter(!is.na(poids_moyen), uniteID %in% passage_unite_nsu$uniteID_nsu) %>%
  mutate(uniteID = as.numeric(uniteID),
         poids_sd = if_else(is.na(poids_sd), 0, poids_sd)) %>%   # nettoyer les NA
  group_by(uniteID) %>%
  summarise(poids_grammes = weighted.mean(poids_moyen, poids_sd, na.rm = TRUE))

cat("=== Poids des unites (NSU, moyenne nationale) ===\n")
cat("Nombre d'unites :", nrow(poids_unites), "\n")
print(poids_unites %>% mutate(poids_grammes = round(poids_grammes, 0)))


## =============================================================================
##  ETAPE 1 - SELECTION DU PRODUIT X ET NETTOYAGE DES UNITES
## =============================================================================
# s16dq01 = code culture (1=Mil, 2=Sorgho...). PAS besoin de s16c.
# s16dq05b = unite. On EXCLUT les codes > 7 (donnees corrompues = codes cultures).
base <- s16d %>%
  filter(s16dq01 == code_culture_X) %>%          # produit X uniquement
  filter(s16dq05b <= 7 | is.na(s16dq05b)) %>%   # exclure les codes corrompus (>7)
  mutate(unite = as_factor(s16dq05b))

cat("\nLignes produit", X, "(apres nettoyage) :", nrow(base), "\n")


## =============================================================================
##  ETAPE 2 - CONVERSION DES QUANTITES VENDUES EN KG
## =============================================================================
# Quantite vendue = s16dq05a (valeur) + s16dq05b (unite).
# On convertit en kg en multipliant par le poids du NSU / 1000.

ventes <- base %>%
  filter(s16dq04 == 1) %>%                       # a vendu une partie (Oui=1)
  filter(!is.na(s16dq05a), !is.na(s16dq05b), s16dq05b <= 6) %>%  # unites valides
  mutate(s16dq05b = as.numeric(s16dq05b)) %>%
  left_join(passage_unite_nsu, by = c("s16dq05b" = "s16_unite")) %>%
  left_join(poids_unites, by = c("uniteID_nsu" = "uniteID")) %>%
  # Cas special : Kilogramme (code 1) -> poids fixe 1000 g
  mutate(poids_grammes = if_else(s16dq05b == 1, 1000, poids_grammes)) %>%
  filter(!is.na(poids_grammes), poids_grammes > 0) %>%
  mutate(qte_vendue_kg = s16dq05a * poids_grammes / 1000)   # CONVERSION EN KG

cat("\nVentes converties en kg :", nrow(ventes), "lignes\n")


## =============================================================================
##  ETAPE 3 - PRIX PRODUCTEUR UNITAIRE (FCFA/kg)
## =============================================================================
# FORMULE DU SUJET : Prix producteur = montant total vente / quantite vendue
prix_prod <- ventes %>%
  filter(!is.na(s16dq06), s16dq06 > 0) %>%
  mutate(prix_producteur_kg = s16dq06 / qte_vendue_kg)     # FORMULE DU SUJET

cat("\n=== PRIX PRODUCTEUR UNITAIRE ===\n")
cat("  Prix moyen  :", round(weighted.mean(prix_prod$prix_producteur_kg,
        prix_prod$hhweight, na.rm = TRUE)), "FCFA/kg\n")
cat("  Prix median :", round(median(prix_prod$prix_producteur_kg, na.rm = TRUE)),
    "FCFA/kg\n")


## =============================================================================
##  ETAPE 3bis - TAUX DE COMMERCIALISATION
## =============================================================================
# FORMULE DU SUJET : Taux = quantite vendue / quantite totale produite
# Quantite totale produite = somme des usages de la recolte :
#   autoconsommation (s16dq02a) + don (s16dq03a) + vente (s16dq05a) + stock (s16dq13a)
# Toutes ces quantites sont en unites locales -> conversion en kg (meme methode).

convertir_usage_kg <- function(df, qte_col, unite_col) {
  # Convertit une quantite (en unite locale) en kg (unite seule, moyenne nationale)
  df %>%
    mutate(.row_id = row_number(),
           .u = as.numeric(.data[[unite_col]])) %>%
    left_join(passage_unite_nsu, by = c(".u" = "s16_unite")) %>%
    left_join(poids_unites, by = c("uniteID_nsu" = "uniteID")) %>%
    mutate(poids_grammes = if_else(.u == 1, 1000, poids_grammes)) %>%
    mutate(qte_kg = .data[[qte_col]] * poids_grammes / 1000) %>%
    select(hhid, qte_kg)
}

# Conversion de chaque usage
conso_kg <- convertir_usage_kg(base %>% filter(!is.na(s16dq02a), !is.na(s16dq02b)),
                               "s16dq02a", "s16dq02b") %>% rename(conso_kg = qte_kg)
don_kg   <- convertir_usage_kg(base %>% filter(!is.na(s16dq03a), !is.na(s16dq03b)),
                               "s16dq03a", "s16dq03b") %>% rename(don_kg = qte_kg)
stock_kg <- convertir_usage_kg(base %>% filter(!is.na(s16dq13a), !is.na(s16dq13b)),
                               "s16dq13a", "s16dq13b") %>% rename(stock_kg = qte_kg)
vendu_kg <- ventes %>% group_by(hhid) %>%
  summarise(vendu_kg = sum(qte_vendue_kg, na.rm = TRUE))

# Tableau final par menage
taux_menage <- Reduce(function(x,y) full_join(x,y,by="hhid"),
                      list(conso_kg, don_kg, stock_kg, vendu_kg)) %>%
  mutate(across(ends_with("_kg"), ~replace_na(.x, 0))) %>%
  mutate(
    production_kg = conso_kg + don_kg + vendu_kg + stock_kg,
    taux_commercialisation = if_else(production_kg > 0,
                                     vendu_kg / production_kg, NA_real_)
  )

# Pondération par hhweight
taux_menage <- taux_menage %>%
  left_join(ehcvm_welfare_2b %>% select(hhid, hhweight), by = "hhid")

cat("\n=== TAUX DE COMMERCIALISATION ===\n")
cat("  Taux moyen (national, pondere) :",
    round(100 * weighted.mean(taux_menage$taux_commercialisation,
        taux_menage$hhweight, na.rm = TRUE)), "%\n")
cat("  Taux median :",
    round(100 * median(taux_menage$taux_commercialisation, na.rm = TRUE)), "%\n")


## =============================================================================
##  ETAPE 4 - TYPE D'ACHETEUR (canaux de vente)
## =============================================================================
cat("\n=== CANAUX DE VENTE (s16dq08) ===\n")

canaux <- ventes %>%
  filter(!is.na(s16dq08)) %>%
  mutate(canal = as_factor(s16dq08)) %>%
  group_by(canal) %>%
  summarise(nb_vendeurs = sum(hhweight)) %>%
  mutate(pct = 100 * nb_vendeurs / sum(nb_vendeurs)) %>%
  arrange(desc(pct))
print(canaux %>% mutate(across(where(is.numeric), ~ round(.x, 1))))


## =============================================================================
##  ETAPE 5 - METHODE DE STOCKAGE (s16dq11)
## =============================================================================
cat("\n=== METHODES DE STOCKAGE (s16dq11) ===\n")

stockage <- ventes %>%
  filter(!is.na(s16dq11)) %>%
  mutate(methode = as_factor(s16dq11)) %>%
  group_by(methode) %>%
  summarise(nb = sum(hhweight)) %>%
  mutate(pct = 100 * nb / sum(nb)) %>%
  arrange(desc(pct))
print(stockage %>% mutate(across(where(is.numeric), ~ round(.x, 1))))


## =============================================================================
##  ETAPE 6 - PERTES POST-RECOLTE (graphique par cause)
## =============================================================================
# Les causes de pertes sont dans s16d via s16dq05d (etat) -> non, plutot dans s16c.
# En realite s16d n'a pas de variable directe de "cause de perte".
# Les pertes agricoles (secheresse, insectes...) sont dans s16c (s16cq14 + s16cq15).
# On filtre donc sur s16c pour le produit X et on trace le graphique.

pertes <- s16c %>%
  filter(s16cq04 == code_culture_X) %>%          # produit X
  filter(!is.na(s16cq14)) %>%                     # avec une cause de perte
  mutate(cause = as_factor(s16cq14))

tab_pertes <- pertes %>%
  group_by(cause) %>%
  summarise(nb_cas = sum(hhweight)) %>%
  arrange(desc(nb_cas)) %>%
  mutate(cause = factor(cause, levels = cause))   # figer l'ordre

cat("\n=== PERTES PAR CAUSE (s16cq14) ===\n")
print(tab_pertes %>% mutate(nb_cas = round(nb_cas, 1)))

# Graphique en barres horizontales
ggplot(tab_pertes, aes(x = cause, y = nb_cas)) +
  geom_col(fill = "#C0392B") +
  coord_flip() +
  geom_text(aes(label = round(nb_cas, 0)), hjust = -0.1, size = 3.5) +
  labs(
    title    = "Causes des pertes de récolte du produit X",
    subtitle = "Nombre de cas (pondéré) par cause",
    x = NULL,
    y = "Nombre de cas (pondéré)"
  ) +
  theme_minimal(base_size = 11)

if (!dir.exists("sorties")) dir.create("sorties")
ggsave("sorties/Sorties_module_4/graph_pertes_par_cause.png", width = 8, height = 5, dpi = 120)
cat("\nGraphique sauvegarde : sorties/graph_pertes_par_cause.png\n")

cat("\n=== FIN DU MODULE 4 (PARTIE S16D) ===\n")


###############################################################################
# PARTIE 2 - DEPUIS S07B (menage consommateur)
###############################################################################
# Le sujet (Module 4) demande, depuis S07B :
#   - Prix a la consommation = valeur achat (s07bq08) / quantite achetee (s07bq07)
#   - Part d'autoconsommation dans la consommation totale du produit
#
# Ici les unites sont des unites de CONSOMMATION (Boite, Bassine, Sac 25kg...)
# qui SONT dans l'Excel de conversion. On utilise donc la methode du Cours 7
# (Key = produit + unite + taille) contrairement au S16D qui utilisait le NSU.
#
# Pas d'interpolation ni de cartographie dans cette partie (comme demande).
###############################################################################

## =============================================================================
##  PARAMETRE : CODES CONSO DU PRODUIT X
## =============================================================================
# En conso (S07B), le Maïs = plusieurs codes (grain + derives).
codes_conso_X <- c(5, 6, 12, 13)   # Maïs en épi(5), Maïs en grain(6), Farine de maïs(12), Semoule de maïs(13)


## =============================================================================
##  ETAPE 7 - TABLE DE CONVERSION (methode Cours 7, Excel)
## =============================================================================
# Key = paste0(produitID, uniteID, tailleID) ; poids en grammes.
table_conversion <- Table_de_conversion %>%
  filter(!is.na(poids)) %>%
  mutate(
    poids  = as.numeric(poids),
    Utable = uniteID,
    Ttable = tailleID,
    Key    = paste0(produitID, Utable, Ttable)
  ) %>%
  select(produitID, Key, poids, Utable, Ttable) %>%
  distinct()

cat("\n=== PARTIE 2 : DEPUIS S07B (consommateur) ===\n")
cat("Table de conversion (Cours 7) prete :", nrow(table_conversion), "combinaisons\n")


## =============================================================================
##  ETAPE 8 - PRIX A LA CONSOMMATION (FCFA/kg)
## =============================================================================
# FORMULE DU SUJET : Prix conso = valeur achat (s07bq08) / quantite achetee (s07bq07a)
# Quantite achetee en unite locale -> conversion en kg via la Key Cours 7.

conso_achat <- s07b %>%
  filter(s07bq01 %in% codes_conso_X) %>%          # produit X
  filter(s07bq02 == 1) %>%                         # a consomme (Oui=1)
  filter(!is.na(s07bq07a), s07bq07a > 0, !is.na(s07bq08), s07bq08 > 0) %>%
  # Construction de la Key (avec taille s07bq07c)
  mutate(
    produitID = s07bq01,
    Utable    = s07bq07b,
    Ttable    = if_else(is.na(s07bq07c), 0, s07bq07c),   # taille unique si NA
    Key       = paste0(produitID, Utable, Ttable)
  ) %>%
  left_join(table_conversion %>% select(Key, poids), by = "Key") %>%
  filter(!is.na(poids), poids > 0) %>%
  mutate(qte_achetee_kg = s07bq07a * poids / 1000) %>%      # CONVERSION EN KG
  mutate(prix_conso_kg  = s07bq08 / qte_achetee_kg)         # FORMULE DU SUJET

cat("\n=== PRIX A LA CONSOMMATION ===\n")
cat("  Observations valides :", nrow(conso_achat), "\n")
cat("  Prix conso moyen (pondere) :",
    round(weighted.mean(conso_achat$prix_conso_kg, conso_achat$hhweight, na.rm = TRUE)),
    "FCFA/kg\n")
cat("  Prix conso median :",
    round(median(conso_achat$prix_conso_kg, na.rm = TRUE)), "FCFA/kg\n")


## =============================================================================
##  ETAPE 9 - PART D'AUTOCONSOMMATION (en valeur)
## =============================================================================
# FORMULE DU SUJET : Part d'autoconso = valeur autoconso / valeur conso totale
#
# La base s07b donne :
#   - s07bq03a : quantite consommee (totale, en unite locale)
#   - s07bq04  : quantite autoconsommee (produite par le menage)
#   - s07bq08  : valeur du dernier achat
#
# Pour comparer autoconso et conso totale, on convertit les deux en kg
# (meme unite) puis on calcule le ratio en quantite.

autoconso <- s07b %>%
  filter(s07bq01 %in% codes_conso_X) %>%
  filter(s07bq02 == 1) %>%
  filter(!is.na(s07bq03a), s07bq03a > 0) %>%
  # Key pour la quantite consommee (s07bq03a, unite s07bq03b, taille s07bq03c)
  mutate(
    produitID = s07bq01,
    Utable    = s07bq03b,
    Ttable    = if_else(is.na(s07bq03c), 0, s07bq03c),
    Key       = paste0(produitID, Utable, Ttable)
  ) %>%
  left_join(table_conversion %>% select(Key, poids), by = "Key") %>%
  filter(!is.na(poids), poids > 0) %>%
  mutate(
    qte_conso_kg = s07bq03a * poids / 1000,
    qte_auto_kg  = if_else(!is.na(s07bq04), s07bq04 * poids / 1000, 0)  # meme poids
  )

# Ratio moyen au niveau national (pondere)
autoconso_ratio <- autoconso %>%
  mutate(ratio_autoconso = qte_auto_kg / qte_conso_kg)

cat("\n=== PART D'AUTOCONSOMMATION ===\n")
cat("  Part moyenne d'autoconso (ponderee) :",
    round(100 * weighted.mean(autoconso_ratio$ratio_autoconso,
        autoconso_ratio$hhweight, na.rm = TRUE)), "%\n")
cat("  (interprétation : un ratio de 40% signifie que le menage produit lui-meme")
cat("   40% du produit X qu'il consomme)\n")

cat("\n=== FIN DU MODULE 4 (PARTIE S07B) ===\n")


###############################################################################
# PARTIE 3 - CARTOGRAPHIE : INTERPOLATION SPATIALE DES PRIX
###############################################################################
# Sujet (Module 4) : "Interpolation spatiale des prix producteurs et des prix
# a la consommation par grappe - carte des gradients de prix."
#
# Principe : on calcule un prix MOYEN par GRAPPE (village), on lui rattache
# ses coordonnees GPS, puis on interpole pour produire une carte continue.
#
# Methode : IDW (Inverse Distance Weighting) avec gstat + carte tmap/leaflet.
###############################################################################

## =============================================================================
##  ETAPE 10 - PRIX MOYENS PAR GRAPPE + GPS
## =============================================================================
cat("\n=== PARTIE 3 : CARTOGRAPHIE (interpolation spatiale) ===\n")

# --- 10a. Prix producteur moyen par grappe (depuis etape 3) ---
prix_prod_grappe <- prix_prod %>%
  group_by(grappe) %>%
  summarise(prix_prod = weighted.mean(prix_producteur_kg, hhweight, na.rm = TRUE))

# --- 10b. Prix conso moyen par grappe (depuis etape 8) ---
prix_conso_grappe <- conso_achat %>%
  group_by(grappe) %>%
  summarise(prix_conso = weighted.mean(prix_conso_kg, hhweight, na.rm = TRUE))

# --- 10c. GPS des grappes (596 sur 600) ---
grappe_gps <- s00 %>%
  distinct(grappe, GPS__Latitude, GPS__Longitude) %>%
  filter(!is.na(GPS__Latitude), !is.na(GPS__Longitude))

# --- 10d. Tableau final carto ---
prix_carto <- prix_prod_grappe %>%
  full_join(prix_conso_grappe, by = "grappe") %>%
  inner_join(grappe_gps, by = "grappe") %>%
  filter(!is.na(prix_prod) | !is.na(prix_conso))

cat("  Grappes avec prix + GPS :", nrow(prix_carto), "\n")


## =============================================================================
##  ETAPE 11 - INTERPOLATION IDW (gstat + terra) — VERSION SIMPLE
## =============================================================================
# On interpole le prix producteur sur une grille reguliere (raster) couvrant
# l'etendue des grappes, avec la methode IDW (Inverse Distance Weighting).

# --- 11a. Points observes (en WGS84 lat/lon, comme dans s00) ---
pts_ll <- prix_carto %>%
  filter(!is.na(prix_prod)) %>%
  st_as_sf(coords = c("GPS__Longitude", "GPS__Latitude"), crs = 4326)

# --- 11b. Raster grille (en lat/lon WGS84) couvrant l'etendue des grappes ---
bbox_ll <- st_bbox(pts_ll)
raster_grid <- terra::rast(
  xmin = bbox_ll["xmin"], xmax = bbox_ll["xmax"],
  ymin = bbox_ll["ymin"], ymax = bbox_ll["ymax"],
  resolution = 0.05    # 0.05 degre ~ 5 km (ajuster si trop lent)
)
terra::crs(raster_grid) <- "EPSG:4326"

# Convertir le raster en points pour la prediction gstat
coord_pts <- as.data.frame(terra::crds(raster_grid))   # matrix -> data.frame
grille_pts <- st_as_sf(coord_pts, coords = c("x","y"), crs = 4326)

# --- 11c. Modele IDW + prediction sur la grille ---
modele_idw <- gstat(formula = prix_prod ~ 1,
                    data = pts_ll,
                    set = list(idp = 2))   # ponderation 1/distance²

pred_idw <- predict(modele_idw, grille_pts)
cat("  Interpolation IDW realisee :", nrow(pred_idw), "points predits\n")

# Reconvertir les predictions en raster pour l'affichage
raster_prix <- terra::rast(
  cbind(coord_pts, prix = pred_idw$var1.pred),
  type = "xyz", crs = "EPSG:4326"
)


## =============================================================================
##  ETAPE 12 - CARTES INTERACTIVES (leaflet)
## =============================================================================
pal <- leaflet::colorNumeric(palette = "YlOrRd",
                             domain = na.omit(pred_idw$var1.pred),
                             reverse = TRUE)   # jaune (bas) -> rouge (haut)

# --- Carte 1 : prix producteur observes (points) ---
carte_points <- leaflet::leaflet(pts_ll) %>%
  leaflet::addProviderTiles("CartoDB.Positron") %>%
  leaflet::addCircleMarkers(
    radius = 4,
    color = ~pal(prix_prod),
    stroke = FALSE, fillOpacity = 0.8,
    popup = ~paste("Prix prod :", round(prix_prod), "FCFA/kg")
  ) %>%
  leaflet::addLegend("bottomright", pal = pal,
                     values = ~prix_prod,
                     title = "Prix producteur<br/>(FCFA/kg)")

# --- Carte 2 : interpolation continue (raster) ---
carte_raster <- leaflet::leaflet() %>%
  leaflet::addProviderTiles("CartoDB.Positron") %>%
  leaflet::addRasterImage(raster_prix, colors = pal,
                          opacity = 0.6) %>%
  leaflet::addLegend("bottomright", pal = pal,
                     values = terra::values(raster_prix),
                     title = "Prix producteur<br/>estimé (FCFA/kg)")

# --- Sauvegarde en HTML interactif ---
htmlwidgets::saveWidget(carte_points, "sorties/Sorties_module_4/carte_prix_producteur_points.html",
                        selfcontained = FALSE)
htmlwidgets::saveWidget(carte_raster, "sorties/Sorties_module_4/carte_prix_producteur_interp.html",
                        selfcontained = FALSE)
cat("\nCartes sauvegardees :\n")
cat("  - sorties/Sorties_module_4/carte_prix_producteur_points.html (observe)\n")
cat("  - sorties/Sorties_module_4/carte_prix_producteur_interp.html (interpole)\n")


###############################################################################
# PARTIE 4 - MARGE COMMERCIALE & ZONES PRIORITAIRES
###############################################################################
# Sujet : "Calcul de la marge commerciale approchee = prix marche (QC-S5) -
#          prix producteur (S16D). Identifier les grappes ou la marge est la
#          plus elevee (zones d'intervention prioritaire)."
#
# QC-S5 (prix communautaires par grappe) n'existe pas dans nos donnees.
# De plus, un village qui VEND du mil n'a souvent aucun menage qui en ACHETE
# dans le meme village -> l'inner_join par grappe donnerait 0 resultat.
# Solution robuste : calculer la marge au niveau REGION (13 regions).
###############################################################################

## =============================================================================
##  ETAPE 13 - MARGE COMMERCIALE PAR REGION (robuste)
## =============================================================================
cat("\n=== PARTIE 4 : MARGE COMMERCIALE (niveau region) ===\n")

# On a besoin de la region de chaque menage
geo_menages <- ehcvm_welfare_2b %>% select(hhid, region)

# --- 13a. Prix producteur moyen par region ---
prix_prod_region <- prix_prod %>%
  left_join(geo_menages, by = "hhid") %>%
  group_by(region) %>%
  summarise(prix_prod = weighted.mean(prix_producteur_kg, hhweight, na.rm = TRUE))

# --- 13b. Prix conso moyen par region ---
prix_conso_region <- conso_achat %>%
  left_join(geo_menages, by = "hhid") %>%
  group_by(region) %>%
  summarise(prix_conso = weighted.mean(prix_conso_kg, hhweight, na.rm = TRUE))

# --- 13c. Marge par region ---
marge_region <- prix_prod_region %>%
  full_join(prix_conso_region, by = "region") %>%
  mutate(marge = prix_conso - prix_prod,
         nom_region = as_factor(region)) %>%
  filter(is.finite(marge)) %>%
  arrange(desc(marge))

cat("\nMarge commerciale (Prix conso - Prix prod) par region :\n")
print(marge_region %>% mutate(across(where(is.numeric), ~ round(.x, 0))))

# Graphique : marge par region (barres horizontales)
ggplot(marge_region, aes(x = reorder(as_factor(region), marge), y = marge)) +
  geom_col(fill = "#2E86C1") +
  coord_flip() +
  geom_text(aes(label = round(marge, 0)), hjust = -0.1, size = 3.5) +
  labs(title = "Marge commerciale du Maïs par région",
       subtitle = "Prix consommation - Prix producteur (FCFA/kg)",
       x = "Région", y = "Marge (FCFA/kg)") +
  theme_minimal()
ggsave("sorties/Sorties_module_4/graph_marge_region.png", width = 8, height = 5, dpi = 120)
cat("  Graphique sauvegarde : sorties/Sorties_module_4/graph_marge_region.png\n")


## =============================================================================
##  ETAPE 14 - IDENTIFICATION DES ZONES PRIORITAIRES (top regions)
## =============================================================================
cat("\n=== ZONES PRIORITAIRES (marge elevee) ===\n")

# Zones prioritaires = regions avec la marge la plus elevee (top 3)
zones_prioritaires <- marge_region %>% head(3)
print(zones_prioritaires %>% mutate(across(where(is.numeric), ~ round(.x, 0))))

# Carte des prix producteurs par grappe, colores selon la marge regionale
prix_prod_grappe_geo <- prix_prod %>%
  group_by(grappe) %>%
  summarise(prix_prod = weighted.mean(prix_producteur_kg, hhweight, na.rm = TRUE)) %>%
  left_join(grappe_gps, by = "grappe") %>%
  filter(!is.na(GPS__Latitude), !is.na(GPS__Longitude))

if (nrow(prix_prod_grappe_geo) > 0) {
  pts <- st_as_sf(prix_prod_grappe_geo,
                  coords = c("GPS__Longitude", "GPS__Latitude"), crs = 4326)
  pal_pts <- leaflet::colorNumeric(palette = "YlOrRd",
                                   domain = prix_prod_grappe_geo$prix_prod,
                                   reverse = TRUE)
  carte_marge <- leaflet::leaflet(pts) %>%
    leaflet::addProviderTiles("CartoDB.Positron") %>%
    leaflet::addCircleMarkers(
      radius = 4,
      color = ~pal_pts(prix_prod),
      stroke = FALSE, fillOpacity = 0.8,
      popup = ~paste("Grappe :", grappe, "<br>Prix prod :", round(prix_prod), "FCFA/kg")
    ) %>%
    leaflet::addLegend("bottomright", pal = pal_pts,
                       values = ~prix_prod,
                       title = "Prix producteur<br/>(FCFA/kg)")

  htmlwidgets::saveWidget(carte_marge, "sorties/Sorties_module_4/carte_prix_producteur_zones.html",
                          selfcontained = FALSE)
  cat("  Carte sauvegardee : sorties/Sorties_module_4/carte_prix_producteur_zones.html\n")
}

cat("\n=== FIN DU MODULE 4 ===\n")
