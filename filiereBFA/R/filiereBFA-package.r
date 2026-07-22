#' filiereBFA : Analyse de filiere et securite alimentaire (EHCVM)
#'
#' Ce package encapsule la chaine analytique du projet filiere EHCVM.
#' Il permet de realiser les Modules 1 a 5 du projet ISEP2 pour n'importe
#' quel produit strategique (mais, mil, sorgho...) et pays UEMOA.
#'
#' @section Fonctions principales :
#'
#' \itemize{
#'   \item \code{\link{load_filiere}} : charge et fusionne les bases EHCVM
#'   \item \code{\link{calc_rendement}} : calcul du rendement (kg/ha)
#'   \item \code{\link{calc_fies}} : construction du score FIES
#'   \item \code{\link{calc_hdds}} : score de diversite alimentaire
#'   \item \code{\link{profil_menage}} : profilage socioeconomique des 4 groupes
#'   \item \code{\link{prix_chaine}} : prix producteur vs consommation + marge
#'   \item \code{\link{carte_filiere}} : carte leaflet d'un indicateur
#'   \item \code{\link{reg_filiere}} : regression de l'impact filiere sur FIES/HDDS
#' }
#'
#' @docType package
#' @name filiereBFA-package
"_PACKAGE"

#' Importation et fusion des bases EHCVM
#'
#' Charge les sections EHCVM (menage + communautaire) pour un pays et un produit
#' donnes. Cree un objet "data.filiere" contenant les bases pretes pour l'analyse.
#'
#' @param dossier Le chemin vers le dossier contenant les fichiers .dta
#' @param produit Le nom du produit strategique (ex: "maïs", "mil")
#' @param codes_prod Vecteur des codes culture dans s16c (ex: 4L pour le mais)
#' @param codes_conso Vecteur des codes produit conso dans s07b (ex: c(5,6,12,13))
#' @param pays Code pays (ex: "BFA" pour le Burkina Faso)
#'
#' @return Un objet de type liste contenant les bases chargees et la typologie des menages.
#' @export
#' @import haven tidyverse labelled
load_filiere <- function(dossier, produit, codes_prod, codes_conso, pays = "BFA") {
  
  cat("Chargement des donnees EHCVM pour :", produit, "(", pays, ")\n")
  
  fichiers <- c(
    ponderation = "ehcvm_ponderations_bfa2021.dta",
    nsu         = "ehcvm_nsu_bfa2021.dta",
    welfare     = "ehcvm_welfare_2b_bfa2021.dta",
    s00    = "s00_me_bfa2021.dta",
    s01    = "s01_me_bfa2021.dta",
    s07b   = "s07b_me_bfa2021.dta",
    s08    = "s08a_me_bfa2021.dta",
    s16a   = "s16a_me_bfa2021.dta",
    s16b   = "s16b_me_bfa2021.dta",
    s16c   = "s16c_me_bfa2021.dta",
    s16d   = "s16d_me_bfa2021.dta",
    s01_co = "s01_co_bfa2021.dta",
    s02_co = "s02_co_bfa2021.dta",
    s03_co = "s03_co_bfa2021.dta"
  )
  
  data <- list()
  for (nom in names(fichiers)) {
    chemin <- file.path(dossier, fichiers[nom])
    if (file.exists(chemin)) {
      data[[nom]] <- read_dta(chemin)
      cat("  -", fichiers[nom], ":", nrow(data[[nom]]), "lignes\n")
    } else {
      warning("Fichier manquant : ", fichiers[nom])
    }
  }
  
  # Construction de la typologie (Module 2)
  data$typologie <- data$welfare %>%
    select(hhid, grappe, menage, region, milieu, hhweight, hhsize,
           hgender, hage, heduc, pcexp, zref) %>%
    left_join(
      data$s16c %>% filter(s16cq04 %in% codes_prod) %>%
        distinct(hhid) %>% mutate(producteur = 1L),
      by = "hhid"
    ) %>%
    left_join(
      data$s07b %>% filter(s07bq01 %in% codes_conso, s07bq02 == 1) %>%
        distinct(hhid) %>% mutate(consommateur = 1L),
      by = "hhid"
    ) %>%
    replace_na(list(producteur = 0L, consommateur = 0L)) %>%
    mutate(groupe = case_when(
      producteur == 1 & consommateur == 1 ~ "1. Producteur-Conso",
      producteur == 1 & consommateur == 0 ~ "2. Producteur seul",
      producteur == 0 & consommateur == 1 ~ "3. Conso seul",
      TRUE ~ "4. Ni prod ni conso"
    ))
  
  cat("Donnees chargees. Typologie creee.\n")
  return(data)
}


#' Score FIES (Securite Alimentaire)
#'
#' Construit le score FIES (0-8) et les seuils d'insecurite moderee/severe
#' a partir des 8 questions dichotomiques de la section 8.
#'
#' @param s08 La base s08a_me (section securite alimentaire)
#' @param poids Base welfare contenant hhweight (optionnel)
#'
#' @return Un data.frame avec hhid, score_fies, fies_moder, fies_sever
#' @export
#' @import haven tidyverse labelled
calc_fies <- function(s08, poids = NULL) {
  
  fies <- s08 %>%
    mutate(across(s08aq01:s08aq08, ~ if_else(.x == 1, 1L, 0L))) %>%
    mutate(
      score_fies = s08aq01 + s08aq02 + s08aq03 + s08aq04 +
        s08aq05 + s08aq06 + s08aq07 + s08aq08,
      fies_moder = as.integer(score_fies >= 3),
      fies_sever = as.integer(score_fies >= 6)
    ) %>%
    select(hhid, score_fies, fies_moder, fies_sever)
  
  if (!is.null(poids)) {
    fies <- fies %>% left_join(poids %>% select(hhid, hhweight), by = "hhid")
  }
  cat("FIES calcule pour", nrow(fies), "menages.\n")
  return(fies)
}


#' Score HDDS (Diversite Alimentaire)
#'
#' Calcule le Household Dietary Diversity Score (nombre de groupes alimentaires
#' FAO consommes sur 7 jours) a partir de la section 7B.
#'
#' @param s07b La base s07b_me (consommation alimentaire)
#' @param passage_fao Table de passage code produit -> groupe FAO (12 groupes)
#'
#' @return Un data.frame avec hhid et hdds (score 0-12)
#' @export
#' @import haven tidyverse labelled
calc_hdds <- function(s07b, passage_fao) {
  
  hdds <- s07b %>%
    filter(s07bq02 == 1) %>%
    inner_join(passage_fao, by = c("s07bq01" = "code_produit")) %>%
    distinct(hhid, groupe_fao) %>%
    count(hhid, name = "hdds")
  
  cat("HDDS calcule pour", nrow(hdds), "menages.\n")
  return(hdds)
}


#' Profil socioeconomique des 4 groupes de menages
#'
#' Genere un tableau comparatif des 4 groupes (Producteur-Conso, Producteur seul,
#' Conso seul, Ni prod ni conso) sur des indicateurs cles (age, sexe, education
#' du chef, pauvrete, FIES, HDDS).
#'
#' @param data L'objet renvoye par load_filiere()
#' @param fies La base FIES calculee par calc_fies()
#' @param hdds La base HDDS calculee par calc_hdds()
#'
#' @return Un data.frame groupant les indicateurs par groupe de menage.
#' @export
#' @import haven tidyverse labelled
profil_menage <- function(data, fies, hdds) {
  
  wmean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
  
  menages <- data$typologie %>%
    left_join(fies %>% select(hhid, score_fies, fies_moder, fies_sever), by = "hhid") %>%
    left_join(hdds, by = "hhid") %>%
    replace_na(list(hdds = 0))
  
  comparatif <- menages %>%
    mutate(
      pauvre = as.integer(pcexp < zref),
      educ_chef_scolarise = as.integer(heduc > 1 & !is.na(heduc))
    ) %>%
    group_by(groupe) %>%
    summarise(
      nb_menages         = sum(hhweight),
      age_chef           = wmean(hage, hhweight),
      pct_femme_chef     = 100 * wmean(hgender == 2, hhweight),
      taille_menage      = wmean(hhsize, hhweight),
      pct_urbain         = 100 * wmean(milieu == 1, hhweight),
      incidence_pauvrete = 100 * wmean(pauvre, hhweight),
      fies_score         = wmean(score_fies, hhweight),
      fies_moder_pct     = 100 * wmean(fies_moder, hhweight),
      fies_sever_pct     = 100 * wmean(fies_sever, hhweight),
      hdds_moyen         = wmean(hdds, hhweight),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  
  cat("Profil calcule pour les 4 groupes.\n")
  return(comparatif)
}


#' Calcul du Rendement (kg/ha)
#'
#' Calcule le rendement (kg/ha) avec conversion des unites locales et winsorisation.
#'
#' @param data L'objet renvoye par load_filiere()
#' @param codes_prod Vecteur des codes culture dans s16c
#' @param table_conv La table de conversion (phase 2) pour les unites locales
#'
#' @return Un data.frame avec hhid, surface_ha, production_kg, rendement_kg_ha, hhweight
#' @export
#' @import haven tidyverse labelled
calc_rendement <- function(data, codes_prod, table_conv) {
  
  # 1. Surface par parcelle
  parcelles <- data$s16a %>%
    transmute(
      hhid, s16aq02, s16aq03,
      surface_ha = case_when(
        s16aq09b == 1 ~ s16aq09a,
        s16aq09b == 2 ~ s16aq09a / 10000,
        TRUE          ~ NA_real_)
    ) %>%
    distinct(hhid, s16aq02, s16aq03, .keep_all = TRUE)
  
  # 2. Surface totale par ménage
  surface_hh <- data$s16c %>%
    filter(s16cq04 %in% codes_prod) %>%
    left_join(parcelles, by = c("hhid", "s16cq02" = "s16aq02",
                                "s16cq03" = "s16aq03")) %>%
    mutate(
      pct = if_else(s16cq07 == 1 & is.na(s16cq08), 100, as.numeric(s16cq08)),
      surface_prod_ha = surface_ha * pct / 100
    ) %>%
    group_by(hhid) %>%
    summarise(surface_ha = sum(surface_prod_ha, na.rm = TRUE), .groups = "drop")
  
  # 3. Production en kg par ménage (estimation directe s16dq05c)
  production_hh <- data$s16d %>%
    filter(s16dq01 %in% codes_prod, !is.na(s16dq05c), s16dq05c > 0) %>%
    group_by(hhid) %>%
    summarise(production_kg = sum(s16dq05c, na.rm = TRUE), .groups = "drop")
  
  # 4. Récupérer le POIDS (hhweight) depuis la table welfare
  poids <- data$welfare %>% select(hhid, hhweight)
  
  # 5. Fusionner Surface + Production + Poids
  rendement <- surface_hh %>%
    inner_join(production_hh, by = "hhid") %>%
    left_join(poids, by = "hhid") %>%  # <-- C'EST ICI QU'ON AJOUTE HHWEIGHT
    mutate(rendement_kg_ha = production_kg / surface_ha) %>%
    filter(is.finite(rendement_kg_ha), rendement_kg_ha > 0, rendement_kg_ha <= 5000)
  
  cat("Rendement calcule pour", nrow(rendement), "menages producteurs.\n")
  return(rendement)
}

#' Chaine de Prix (Commercialisation)
#'
#' Calcule les prix producteur et consommation, ainsi que la marge commerciale.
#'
#' @param data L'objet renvoye par load_filiere()
#' @param codes_prod Vecteur des codes culture dans s16c/s16d
#' @param codes_conso Vecteur des codes produit conso dans s07b
#' @param table_conv_s07 La table de conversion pour s07b (methode Cours 7)
#'
#' @return Une liste contenant prix_prod, prix_conso, et marge_region
#' @export
#' @import haven tidyverse labelled
prix_chaine <- function(data, codes_prod, codes_conso, table_conv_s07) {
  
  wmean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
  
  # Prix producteur (S16D - estimation en kg directe pour robustesse)
  prix_prod <- data$s16d %>%
    filter(s16dq01 %in% codes_prod, !is.na(s16dq05c), s16dq05c > 0,
           !is.na(s16dq06), s16dq06 > 0) %>%
    mutate(prix_producteur_kg = s16dq06 / s16dq05c)
  
  # Prix consommation (S07B - conversion via Key produit-unité-taille)
  prix_conso <- data$s07b %>%
    filter(s07bq01 %in% codes_conso, s07bq02 == 1,
           !is.na(s07bq07a), s07bq07a > 0, !is.na(s07bq08), s07bq08 > 0) %>%
    mutate(
      Key = paste0(s07bq01, s07bq07b, if_else(is.na(s07bq07c), 0, s07bq07c))
    ) %>%
    left_join(table_conv_s07 %>% select(Key, poids), by = "Key") %>%
    filter(!is.na(poids), poids > 0) %>%
    mutate(
      qte_achetee_kg = s07bq07a * poids / 1000,
      prix_conso_kg  = s07bq08 / qte_achetee_kg
    )
  
  # Marge regionale (robuste)
  prix_prod_region <- prix_prod %>%
    left_join(data$typologie %>% select(hhid, region), by = "hhid") %>%
    group_by(region) %>%
    summarise(prix_prod = wmean(prix_producteur_kg, hhweight), .groups = "drop")
  
  prix_conso_region <- prix_conso %>%
    left_join(data$typologie %>% select(hhid, region), by = "hhid") %>%
    group_by(region) %>%
    summarise(prix_conso = wmean(prix_conso_kg, hhweight), .groups = "drop")
  
  marge_region <- prix_prod_region %>%
    full_join(prix_conso_region, by = "region") %>%
    mutate(marge = prix_conso - prix_prod) %>%
    filter(is.finite(marge)) %>%
    arrange(desc(marge))
  
  cat("Prix producteur moyen :", round(wmean(prix_prod$prix_producteur_kg, prix_prod$hhweight)),
      "FCFA/kg | Prix conso moyen :", round(wmean(prix_conso$prix_conso_kg, prix_conso$hhweight)),
      "FCFA/kg\n")
  
  return(list(prix_prod = prix_prod, prix_conso = prix_conso, marge_region = marge_region))
}


#' Carte leaflet d'un indicateur de la filiere
#'
#' Genere une carte interactive (leaflet) d'un indicateur au niveau grappe.
#'
#' @param data L'objet renvoye par load_filiere()
#' @param indicateur Un data.frame avec grappe + une colonne numerique a cartographier
#' @param nom_col Le nom de la colonne numerique a afficher
#' @param titre Le titre de la carte
#'
#' @return Un widget leaflet
#' @export
#' @import sf leaflet htmlwidgets
carte_filiere <- function(data, indicateur, nom_col, titre) {
  
  grappe_gps <- data$s00 %>%
    distinct(grappe, GPS__Latitude, GPS__Longitude) %>%
    filter(!is.na(GPS__Latitude), !is.na(GPS__Longitude))
  
  carte_data <- indicateur %>%
    inner_join(grappe_gps, by = "grappe") %>%
    filter(!is.na(.data[[nom_col]]))
  
  if (nrow(carte_data) == 0) stop("Aucune donnee cartographiable.")
  
  pts_sf <- st_as_sf(carte_data,
                     coords = c("GPS__Longitude", "GPS__Latitude"), crs = 4326)
  
  # On extrait la colonne cible en vecteur simple AVANT d'appeler leaflet,
  # pour eviter le mecanisme de formule (~) de leaflet qui n'est pas un
  # data mask tidyeval (.data y est donc invalide).
  valeurs <- carte_data[[nom_col]]
  
  pal <- leaflet::colorNumeric(palette = "YlOrRd",
                               domain = valeurs,
                               reverse = TRUE)
  
  carte <- leaflet::leaflet(pts_sf) %>%
    leaflet::addProviderTiles("CartoDB.Positron") %>%
    leaflet::addCircleMarkers(
      radius = 5,
      color = pal(valeurs),
      stroke = FALSE, fillOpacity = 0.8,
      popup = paste(titre, ":", round(valeurs))
    ) %>%
    leaflet::addLegend("bottomright", pal = pal,
                       values = valeurs, title = titre)
  
  cat("Carte generee pour", nrow(carte_data), "grappes.\n")
  return(carte)
}


#' Regression d'impact de la filiere sur la securite alimentaire
#'
#' Regresse le score FIES (ou HDDS) sur les variables de participation a la
#' filiere, avec controles et effets fixes de grappe.
#'
#' @param data_reg La base de regression pre-assemblee (merge complet)
#' @param outcome La variable dependante ("score_fies" ou "hdds")
#' @param filiere_vars Vecteur des variables de filiere (ex: producteur_mais)
#' @param controls Vecteur des variables de controle
#' @param weights Variable de pondration (ex: hhweight)
#' @param cluster Variable de cluster (ex: grappe)
#'
#' @return Un objet feols (fixest) avec la regression ponderee et clusterisee.
#' @export
#' @import fixest
reg_filiere <- function(data_reg, outcome = "score_fies", filiere_vars,
                        controls, weights = "hhweight", cluster = "grappe") {
  
  # Construction dynamique de la formule
  rhs <- paste(c(filiere_vars, controls), collapse = " + ")
  formule <- as.formula(paste(outcome, "~", rhs, "|", cluster))
  
  modele <- feols(
    formula = formule,
    data = data_reg,
    weights = as.formula(paste("~", weights)),
    cluster = as.formula(paste("~", cluster))
  )
  
  cat("Regression:", outcome, "sur", length(filiere_vars), "variables filiere et",
      length(controls), "controles.\n")
  
  return(modele)
}