#=======================================================================
#  PREPARATION DES DONNEES POUR LE DASHBOARD SHINY — FILIERE MAIS (BFA)
#=======================================================================
#  A executer en fin de main.R, APRES avoir source les modules 1 a 5
#  (tous les objets requis ci-dessous doivent deja etre en memoire).
#
#  Role de ce script : combler l'ecart entre ce que produisent les
#  modules analytiques et ce qu'attend dashboard/app.R (fonction
#  charger()). Deux types d'objets sont traites :
#    (a) objets deja calcules par un module -> simplement sauvegardes
#        tels quels dans dashboard/data/ ;
#    (b) objets DERIVES manquants (coefs_m3, securite_grappe,
#        prix_carto_dash, menages_dash, gps_grappe avec les bons noms
#        de colonnes) -> calcules ici, avec justification en commentaire.
#=======================================================================

library(dplyr)
library(broom)
library(sf)
library(labelled)

dir.create("dashboard/data", recursive = TRUE, showWarnings = FALSE)

# --- Aide : sauvegarde defensive (ne casse pas la chaine si un objet
#     d'un module non encore termine est absent ; previent au lieu de
#     planter, meme logique que besoin_donnees() dans le dashboard) ---
sauver <- function(objet, nom) {
  chemin <- file.path("dashboard/data", paste0(nom, ".rds"))
  if (is.null(objet)) {
    cat("  [MANQUANT] ", nom, " -- objet NULL, non sauvegarde\n")
    return(invisible(NULL))
  }
  saveRDS(objet, chemin)
  cat("  [OK] ", nom, " -> ", chemin, "\n", sep = "")
}

cat("\n=== PREPARATION DES DONNEES DASHBOARD ===\n")

## ---------------------------------------------------------------------
## ONGLET 1 -- IMPORTANCE STRATEGIQUE
## Tous ces objets existent deja tels quels a la fin de module1. On les
## sauvegarde sans transformation.
## ---------------------------------------------------------------------
cat("\n--- Onglet 1 : Importance strategique ---\n")
sauver(mais_freq_cons,                "mais_freq_cons")
sauver(mais_qte_kg,                   "mais_qte_kg")
sauver(source_mais,                   "source_mais")
sauver(part_sup_mais,                 "part_sup_mais")
sauver(part_mais_vendu,               "part_mais_vendu")
sauver(part_producteurs_mais,         "part_producteurs_mais")
sauver(top10_part_producteurs,        "top10_part_producteurs")
sauver(part_mais_calories_nationales, "part_mais_calories_nationales")
sauver(balance_table,                 "balance_table")

## ---------------------------------------------------------------------
## ONGLET 2 -- PROFIL DES MENAGES
##
##  (a) menages_dash : le "menages" de module2 existe deja, mais le
##      dashboard filtre sur milieu_lbl / region_lbl (labels textuels,
##      pas les codes numeriques haven) et sur un quintile_pcexp qui
##      n'existe pas du tout. On ajoute les 3 colonnes.
##      Le quintile est calcule de facon PONDEREE (poids d'enquete
##      hhweight), pas par ntile() brut, sinon les seuils seraient
##      biaises vers les strates sur-representees dans l'echantillon.
##
##  (b) gps_grappe : le gps_grappe interne a module3 renomme les
##      colonnes en lon/lat (pour la jointure NASA POWER). Mais
##      dashboard::carte_profil() appelle GPS__Longitude / GPS__Latitude
##      (comme s00 brut). On reconstruit donc un objet dedie au
##      dashboard, a partir de s00, avec les noms de colonnes attendus --
##      sans toucher au gps_grappe de module3 (toujours necessaire en
##      l'etat pour la pluviometrie).
## ---------------------------------------------------------------------
cat("\n--- Onglet 2 : Profil des menages ---\n")

# Quantile pondere (fonction generique, poids = hhweight)
wtd_quantile <- function(x, w, probs) {
  ok  <- is.finite(x) & is.finite(w)
  x   <- x[ok]; w <- w[ok]
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cum_w <- cumsum(w) / sum(w)
  approx(cum_w, x, probs, rule = 2, ties = "ordered")$y
}

seuils_pcexp <- wtd_quantile(menages$pcexp, menages$hhweight,
                              probs = seq(0.2, 0.8, 0.2))

menages_dash <- menages %>%
  mutate(
    milieu_lbl     = as_factor(milieu),
    region_lbl     = as_factor(region),
    quintile_pcexp = findInterval(pcexp, seuils_pcexp) + 1L   # 1 (plus pauvre) a 5
  )

sauver(menages_dash, "menages_dash")
sauver(comparatif,   "comparatif")

gps_grappe <- s00 %>%
  distinct(grappe, GPS__Latitude, GPS__Longitude) %>%
  filter(!is.na(GPS__Latitude), !is.na(GPS__Longitude))

sauver(gps_grappe, "gps_grappe")

## ---------------------------------------------------------------------
## ONGLET 3 -- PRODUCTION ET RENDEMENTS
##
##  production_mais, production_mais_analyse, intrants_mais,
##  rendement_geo existent deja (module3).
##
##  coefs_m3 manque : le forest-plot "Determinants du rendement" du
##  dashboard (output$plot_coefs_m3) veut un data.frame avec estimate/
##  conf.low/conf.high/term -- format broom::tidy() de la regression
##  reg_rendement_mais (avec effets fixes de grappe), sans l'intercept.
## ---------------------------------------------------------------------
cat("\n--- Onglet 3 : Production et rendements ---\n")

coefs_m3 <- broom::tidy(reg_rendement_mais, conf.int = TRUE) %>%
  filter(term != "(Intercept)")

sauver(production_mais,         "production_mais")
sauver(production_mais_analyse, "production_mais_analyse")
sauver(intrants_mais,           "intrants_mais")
sauver(sf::st_drop_geometry(rendement_geo), "rendement_geo")
sauver(coefs_m3,                "coefs_m3")

## ---------------------------------------------------------------------
## ONGLET 4 -- CHAINE DES PRIX
##
##  prix_prod, canaux, marge_region existent deja (module4).
##
##  prix_carto_dash manque : le slider "distance max. au marche" du
##  dashboard suppose une colonne distance_marche_proxy_km. Le sujet
##  demande la distance au marche le plus proche (QC-S2), mais QC-S2
##  n'est pas mobilise dans ce projet (limite documentee dans le
##  dashboard lui-meme, cf. helpText onglet 4). A defaut, on calcule un
##  PROXY : distance a la grappe voisine la plus proche disposant elle
##  aussi d'un prix producteur observe (plus cette distance est grande,
##  plus la grappe est isolee du reseau de prix observes). Distances en
##  metres via sf::st_distance (CRS 4326), converties en km.
## ---------------------------------------------------------------------
cat("\n--- Onglet 4 : Chaine des prix ---\n")

pts_prix <- prix_carto %>%
  filter(!is.na(prix_prod), !is.na(GPS__Latitude), !is.na(GPS__Longitude)) %>%
  st_as_sf(coords = c("GPS__Longitude", "GPS__Latitude"), crs = 4326, remove = FALSE)

mat_dist <- st_distance(pts_prix)
diag(mat_dist) <- Inf                      # exclure soi-meme du minimum
distance_min_m <- apply(mat_dist, 1, min)

prix_carto_dash <- pts_prix %>%
  st_drop_geometry() %>%
  mutate(distance_marche_proxy_km = as.numeric(distance_min_m) / 1000)

cat("  Grappes avec distance proxy calculee :", nrow(prix_carto_dash), "\n")

sauver(prix_prod,       "prix_prod")
sauver(canaux,          "canaux")
sauver(marge_region,    "marge_region")
sauver(prix_carto_dash, "prix_carto_dash")

## ---------------------------------------------------------------------
## ONGLET 5 -- SECURITE ALIMENTAIRE
##
##  ech_reg_m5, coefs_m5, coefs_hetero existent deja (module5).
##
##  securite_grappe manque : la carte "securite alimentaire par grappe"
##  du dashboard (output$carte_securite) attend fies_moyen/hdds_moyen
##  par grappe + GPS__Latitude/GPS__Longitude. On agrege le score FIES
##  et le score HDDS du tableau "menages" (module2, deja pondere et
##  disponible pour TOUS les menages, pas seulement les producteurs de
##  mais) au niveau grappe, ponderation hhweight.
## ---------------------------------------------------------------------
cat("\n--- Onglet 5 : Securite alimentaire ---\n")

securite_grappe <- menages %>%
  group_by(grappe) %>%
  summarise(
    fies_moyen = weighted.mean(score_fies, hhweight, na.rm = TRUE),
    hdds_moyen = weighted.mean(hdds,       hhweight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(
    s00 %>%
      distinct(grappe, GPS__Latitude, GPS__Longitude) %>%
      filter(!is.na(GPS__Latitude), !is.na(GPS__Longitude)),
    by = "grappe"
  )

cat("  Grappes cartographiables (securite alimentaire) :",
    nrow(securite_grappe), "\n")

sauver(ech_reg_m5,      "ech_reg_m5")
sauver(coefs_m5,        "coefs_m5")
sauver(coefs_hetero,    "coefs_hetero")
sauver(securite_grappe, "securite_grappe")

cat("\n=== DONNEES DASHBOARD PRETES DANS dashboard/data/ ===\n")
cat("Lancer l'application avec : shiny::runApp(\"dashboard\")\n")
