###############################################################################
# DASHBOARD SHINY : FILIERE MAIS, BURKINA FASO (EHCVM 2021)
###############################################################################
# 5 onglets, conformes au tableau 6 du sujet (paragraphe 6.2) :
#   1. Importance strategique   4. Chaine des prix
#   2. Profil des menages       5. Securite alimentaire
#   3. Production et rendements
#
# PREREQUIS : avoir execute dashboard/data_prep/preparer_donnees_dashboard.R
# a la fin de main.R (genere les .rds lus ci-dessous dans dashboard/data/).
#
# Lancement : shiny::runApp("dashboard")
###############################################################################

library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(plotly)

validate <- shiny::validate
need <- shiny::need

## =============================================================================
##  CHARGEMENT DES DONNEES (une seule fois, au demarrage de l'appli)
## =============================================================================
charger <- function(nom) {
  fp <- file.path("data", paste0(nom, ".rds"))
  if (file.exists(fp)) readRDS(fp) else NULL
}

# Onglet 1
mais_freq_cons          <- charger("mais_freq_cons")
mais_qte_kg              <- charger("mais_qte_kg")
source_mais              <- charger("source_mais")
part_sup_mais            <- charger("part_sup_mais")
part_mais_vendu          <- charger("part_mais_vendu")
part_producteurs_mais    <- charger("part_producteurs_mais")
top10_part_producteurs   <- charger("top10_part_producteurs")
part_mais_calories       <- charger("part_mais_calories_nationales")
balance_table            <- charger("balance_table")

# Onglet 2
menages_dash  <- charger("menages_dash")
comparatif    <- charger("comparatif")
gps_grappe    <- charger("gps_grappe")

# Onglet 3
production_mais           <- charger("production_mais")            # avant winsor
production_mais_analyse   <- charger("production_mais_analyse")    # reference M3
intrants_mais              <- charger("intrants_mais")
rendement_geo               <- charger("rendement_geo")
coefs_m3                    <- charger("coefs_m3")

# Onglet 4
prix_prod        <- charger("prix_prod")
canaux            <- charger("canaux")
marge_region      <- charger("marge_region")
prix_carto_dash   <- charger("prix_carto_dash")

# Onglet 5
ech_reg_m5       <- charger("ech_reg_m5")
coefs_m5          <- charger("coefs_m5")
coefs_hetero      <- charger("coefs_hetero")
securite_grappe   <- charger("securite_grappe")

# Petit helper pour signaler proprement une donnee manquante dans un output,
# plutot que de planter l'appli (utile tant que tous les modules ne sont pas
# termines / tous les .rds pas encore regeneres).
besoin_donnees <- function(objet, message_defaut = "Donnee non disponible : relancez preparer_donnees_dashboard.R apres avoir execute ce module.") {
  shiny::validate(shiny::need(!is.null(objet), message_defaut))
}

## =============================================================================
##  PALETTE ET THEME
## =============================================================================
# Palette restreinte, sobre, bleu fonce / gris (pas de bleu vif), un seul
# accent gris ardoise en contrepoint.
pal_accent   <- "#2E4C6D"   # bleu marine fonce (accent principal, mise en avant du mais)
pal_accent2  <- "#64748B"   # gris ardoise (contrepoint)
pal_categ    <- c("#2E4C6D", "#64748B", "#7C93AC", "#425466", "#A3AEBA", "#1F3549")
pal_neutre   <- "#B5BCC6"   # gris clair pour les categories non mises en avant

theme_mais <- bs_theme(
  version = 5,
  bg = "#F5F6F8",
  fg = "#26313D",
  primary = pal_accent,
  secondary = pal_accent2,
  success = "#4C6E63",
  info = "#3E6C8C",
  warning = "#8C7A4E",
  danger = "#8C4A4A",
  base_font = font_google("Public Sans"),
  heading_font = font_google("Libre Franklin"),
  "body-bg" = "#F5F6F8",
  "border-radius" = "0.9rem"
)

css_perso <- "
  * { box-sizing: border-box; }
  html, body { overflow-x: hidden; background-color: #F5F6F8; }

  .app-topbar { padding: 22px 6px 6px 6px; }
  .app-topbar h1 { font-family: 'Libre Franklin', sans-serif; font-size: 1.7rem; font-weight: 700;
                   color: #26313D; margin-bottom: 2px; }
  .app-topbar h1 svg { color: #2E4C6D; margin-right: 10px; }
  .app-subtitle { color: #6B7684; font-size: 0.95rem; margin-bottom: 18px; }

  /* --- Sidebar de navigation (navlistPanel) --- */
  .well {
    background-color: #1F2C3A;
    border: none;
    border-radius: 16px;
    padding: 20px 12px;
    position: sticky;
    top: 18px;
  }
  .well .nav > li > a, .well .nav-link {
    color: #D7DDE4 !important;
    border-radius: 10px;
    padding: 11px 14px;
    margin-bottom: 5px;
    display: flex;
    align-items: center;
    font-size: 0.93rem;
    line-height: 1.3;
    transition: background-color 0.15s ease;
  }
  .well .nav > li > a svg, .well .nav-link svg,
  .well .nav > li > a i, .well .nav-link i { margin-right: 10px; flex-shrink: 0; }
  .well .nav > li > a:hover, .well .nav-link:hover {
    background-color: rgba(255,255,255,0.08); color: #ffffff !important;
  }
  .well .nav > li.active > a, .well .nav-link.active {
    background-color: #3E6C8C !important; color: #ffffff !important; font-weight: 600;
  }

  /* --- Barre de filtres --- */
  .filter-bar {
    background-color: #FFFFFF;
    border: 1px solid #E2E6EB;
    border-radius: 16px;
    padding: 16px 20px 4px 20px;
    margin-bottom: 20px;
  }
  .filter-bar .form-group { margin-bottom: 14px; }

  /* --- Cartes de contenu --- */
  .card {
    border: 1px solid #E4E8EC;
    border-radius: 16px;
    box-shadow: 0 1px 3px rgba(38, 49, 61, 0.05);
    margin-bottom: 22px;
    overflow: hidden;
  }
  .card-header {
    background-color: #FFFFFF;
    border-bottom: 1px solid #EBEEF1;
    font-weight: 600;
    font-size: 1.02rem;
    color: #26313D;
    padding: 14px 20px;
  }
  .card-body { padding: 18px 20px; }

  .value-box { border-radius: 16px; box-shadow: 0 1px 3px rgba(38, 49, 61, 0.05); }
  .value-box .value-box-title { font-weight: 500; opacity: 0.85; }

  h4.section-note { font-weight: 600; color: #26313D; }
  .helper-note { color: #6B7684; font-size: 0.9em; }

  /* --- Composants divers --- */
  .irs-bar, .irs-bar-edge, .irs-single, .irs-from, .irs-to { background: #3E6C8C !important; border-color: #3E6C8C !important; }
  select, .form-control, .btn { border-radius: 8px !important; }
  .dataTables_wrapper { max-width: 100%; overflow-x: auto; }
  table.dataTable { width: 100% !important; }
  table.dataTable thead th { background-color: #EEF1F4 !important; }
  .js-plotly-plot, .plotly, img, svg.main-svg { max-width: 100% !important; }
  .js-plotly-plot .mapboxgl-map { border-radius: 14px; overflow: hidden; }
"

## =============================================================================
##  UI
## =============================================================================
ui <- fluidPage(
  theme = theme_mais,
  tags$head(tags$style(HTML(css_perso))),
  
  div(class = "app-topbar",
      h1(icon("wheat-awn"), "Filiere Mais, Burkina Faso"),
      p(class = "app-subtitle", "Tableau de bord EHCVM 2021")
  ),
  
  navlistPanel(
    id = "nav_principal",
    well = TRUE,
    fluid = TRUE,
    widths = c(2, 10),
    
    ## ---------------------------------------------------------------------
    ## ONGLET 1 : IMPORTANCE STRATEGIQUE
    ## ---------------------------------------------------------------------
    tabPanel(
      tagList(icon("seedling"), "Importance strategique"),
      div(class = "filter-bar",
          fluidRow(
            column(6,
                   sliderInput("top_n", "Nombre de cultures affichees (classement) :",
                               min = 5, max = 15, value = 10, step = 1, width = "100%")
            ),
            column(6,
                   p(class = "helper-note", style = "margin-top: 28px;",
                     "Comparaison avec les autres cultures du pays, calculee depuis S16C (superficie / nombre de producteurs).")
            )
          )
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(title = "Freq. de consommation", value = textOutput("val_freq_conso"),
                  showcase = icon("utensils"), theme = "primary"),
        value_box(title = "Part surface agricole", value = textOutput("val_part_surface"),
                  showcase = icon("tractor"), theme = "secondary"),
        value_box(title = "Part valeur des ventes", value = textOutput("val_part_ventes"),
                  showcase = icon("coins"), theme = "warning"),
        value_box(title = "Part calories nationales", value = textOutput("val_part_calories"),
                  showcase = icon("bowl-food"), theme = "info")
      ),
      card(
        card_header("Classement des cultures par part des menages producteurs"),
        plotlyOutput("plot_top_cultures", height = "450px")
      ),
      card(
        card_header("Sources d'approvisionnement du mais"),
        plotlyOutput("plot_source_mais", height = "340px")
      ),
      card(
        card_header("Balance commerciale (FAOSTAT)"),
        DT::dataTableOutput("table_balance")
      )
    ),
    
    ## ---------------------------------------------------------------------
    ## ONGLET 2 : PROFIL DES MENAGES
    ## ---------------------------------------------------------------------
    tabPanel(
      tagList(icon("people-group"), "Profil des menages"),
      div(class = "filter-bar",
          fluidRow(
            column(4, selectInput("f2_milieu", "Milieu :", choices = c("Tous", "Urbain", "Rural"), selected = "Tous", width = "100%")),
            column(4, uiOutput("f2_region_ui")),
            column(4, sliderInput("f2_quintile", "Quintile de depense par tete :",
                                  min = 1, max = 5, value = c(1, 5), step = 1, width = "100%"))
          )
      ),
      card(
        card_header("Repartition des 4 groupes de menages"),
        plotlyOutput("plot_repartition_groupes", height = "380px")
      ),
      card(
        card_header("Statistiques comparatives par groupe"),
        DT::dataTableOutput("table_comparatif")
      ),
      card(
        card_header("Distribution spatiale (score FIES moyen par grappe)"),
        plotlyOutput("carte_profil", height = "460px")
      )
    ),
    
    ## ---------------------------------------------------------------------
    ## ONGLET 3 : PRODUCTION ET RENDEMENTS
    ## ---------------------------------------------------------------------
    tabPanel(
      tagList(icon("wheat-awn"), "Production et rendements"),
      div(class = "filter-bar",
          fluidRow(
            column(4, selectInput("f3_culture", "Culture :", choices = c("Mais"), selected = "Mais", width = "100%")),
            column(4, sliderInput("f3_winsor", "Seuil de winsorisation (percentile) :",
                                  min = 90, max = 100, value = 99, step = 1, width = "100%")),
            column(4, checkboxInput("f3_pluie", "Superposer la pluviometrie (NASA POWER)", value = FALSE))
          ),
          p(class = "helper-note", "Une seule culture est actuellement traitee par la chaine analytique (voir Module 1-3).")
      ),
      card(
        card_header("Distribution des rendements"),
        plotlyOutput("hist_rendements", height = "340px")
      ),
      card(
        card_header("Carte des rendements par grappe"),
        plotlyOutput("carte_rendements", height = "460px")
      ),
      card(
        card_header("Determinants du rendement (regression OLS, effets fixes grappe)"),
        plotlyOutput("plot_coefs_m3", height = "340px")
      ),
      card(
        card_header("Utilisation d'intrants chez les producteurs"),
        plotlyOutput("plot_intrants", height = "340px")
      )
    ),
    
    ## ---------------------------------------------------------------------
    ## ONGLET 4 : CHAINE DES PRIX
    ## ---------------------------------------------------------------------
    tabPanel(
      tagList(icon("money-bill-trend-up"), "Chaine des prix"),
      div(class = "filter-bar",
          fluidRow(
            column(4, uiOutput("f4_distance_ui")),
            column(4, uiOutput("f4_canal_ui")),
            column(4, selectInput("f4_saison", "Saison :", choices = c("Non disponible"), width = "100%"))
          ),
          p(class = "helper-note",
            "La dimension saisonniere n'est pas disponible : l'EHCVM est une enquete en coupe transversale (un seul passage), sans repetition des prix dans le temps (cf. points de vigilance du Module 4).")
      ),
      card(
        card_header("Carte des prix producteurs par grappe"),
        p(class = "helper-note", style = "padding: 0 20px;",
          em("Le rayon du marqueur code la distance a la grappe voisine la plus proche ayant un prix observe (un proxy, faute de distance QC-S2 integree au Module 4).")),
        plotlyOutput("carte_prix", height = "460px")
      ),
      card(
        card_header("Canaux de vente"),
        plotlyOutput("plot_canaux", height = "340px")
      ),
      card(
        card_header("Marge commerciale approchee par region"),
        DT::dataTableOutput("table_marge")
      )
    ),
    
    ## ---------------------------------------------------------------------
    ## ONGLET 5 : SECURITE ALIMENTAIRE
    ## ---------------------------------------------------------------------
    tabPanel(
      tagList(icon("bowl-rice"), "Securite alimentaire"),
      div(class = "filter-bar",
          fluidRow(
            column(6,
                   radioButtons("f5_outcome", "Indicateur :",
                                choices = c("FIES (0-8, plus haut = pire)" = "FIES",
                                            "HDDS (0-12, plus haut = mieux)" = "HDDS"),
                                selected = "FIES", inline = TRUE)
            ),
            column(6, uiOutput("f5_groupe_ui"))
          )
      ),
      layout_columns(
        col_widths = c(6, 6),
        value_box(title = "Score moyen (groupe selectionne)", value = textOutput("val_score_groupe"),
                  showcase = icon("chart-simple"), theme = "primary"),
        value_box(title = "% insecurite severe (FIES >= 6)", value = textOutput("val_pct_severe"),
                  showcase = icon("triangle-exclamation"), theme = "danger")
      ),
      card(
        card_header("Coefficients du modele principal"),
        plotlyOutput("plot_coefs_m5", height = "340px")
      ),
      card(
        card_header("Carte de la securite alimentaire par grappe"),
        plotlyOutput("carte_securite", height = "460px")
      ),
      card(
        card_header("Heterogeneite (cooperative / irrigation)"),
        plotlyOutput("plot_coefs_hetero", height = "340px")
      )
    )
  )
)

## =============================================================================
##  SERVER
## =============================================================================
server <- function(input, output, session) {
  
  ## --------------------------- ONGLET 1 ---------------------------------
  
  output$val_freq_conso <- renderText({
    besoin_donnees(mais_freq_cons)
    paste0(round(sum(mais_freq_cons$pourcentage, na.rm = TRUE), 1), "% des menages")
  })
  
  output$val_part_surface <- renderText({
    besoin_donnees(part_sup_mais)
    paste0(round(part_sup_mais$poids[1], 1), "% de la superficie agricole")
  })
  
  output$val_part_ventes <- renderText({
    besoin_donnees(part_mais_vendu)
    paste0(round(100 * part_mais_vendu$part_vente[1], 1), "% des ventes agricoles")
  })
  
  output$val_part_calories <- renderText({
    besoin_donnees(part_mais_calories)
    paste0(part_mais_calories$part_cal[1], "% des calories disponibles")
  })
  
  output$plot_top_cultures <- renderPlotly({
    besoin_donnees(top10_part_producteurs)
    df <- top10_part_producteurs %>%
      slice_head(n = input$top_n) %>%
      mutate(surligne = str_detect(as.character(produit), regex("mais|maïs", ignore_case = TRUE)))
    
    p <- ggplot(df, aes(x = reorder(produit, part_menages), y = part_menages, fill = surligne,
                        text = paste0(produit, " : ", part_menages, "%"))) +
      geom_col(width = 0.68) +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = pal_accent, "FALSE" = pal_neutre), guide = "none") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank()) +
      labs(title = paste("Top", input$top_n, "des cultures par part des menages producteurs"),
           x = NULL, y = "% des menages producteurs")
    
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 10, r = 20, t = 40))
  })
  
  output$plot_source_mais <- renderPlotly({
    besoin_donnees(source_mais)
    p <- ggplot(source_mais, aes(x = source, y = part, fill = source,
                                 text = paste0(source, " : ", part, "%"))) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = pal_categ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
      labs(title = "Sources d'approvisionnement du mais", x = NULL, y = "Part (%)") +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })
  
  output$table_balance <- DT::renderDataTable({
    besoin_donnees(balance_table)
    balance_table %>% mutate(across(where(is.numeric), ~ round(.x, 0)))
  }, options = list(dom = "t", paging = FALSE, scrollX = TRUE, autoWidth = TRUE),
  class = "stripe hover compact")
  
  ## --------------------------- ONGLET 2 ---------------------------------
  
  output$f2_region_ui <- renderUI({
    besoin_donnees(menages_dash, "")
    if (is.null(menages_dash)) return(NULL)
    choix <- c("Toutes", sort(unique(as.character(menages_dash$region_lbl))))
    selectInput("f2_region", "Region :", choices = choix, selected = "Toutes", width = "100%")
  })
  
  menages_filtres <- reactive({
    besoin_donnees(menages_dash)
    df <- menages_dash
    if (input$f2_milieu != "Tous") {
      cible <- if (input$f2_milieu == "Urbain") "Urbain" else "Rural"
      df <- df %>% filter(as.character(milieu_lbl) == cible)
    }
    if (!is.null(input$f2_region) && input$f2_region != "Toutes") {
      df <- df %>% filter(as.character(region_lbl) == input$f2_region)
    }
    df %>% filter(quintile_pcexp >= input$f2_quintile[1], quintile_pcexp <= input$f2_quintile[2])
  })
  
  output$plot_repartition_groupes <- renderPlotly({
    df <- menages_filtres()
    validate(need(nrow(df) > 0, "Aucun menage ne correspond a ces filtres."))
    rep <- df %>%
      group_by(groupe) %>%
      summarise(nb = sum(hhweight), .groups = "drop") %>%
      mutate(pct = 100 * nb / sum(nb))
    
    p <- ggplot(rep, aes(x = groupe, y = pct, fill = groupe,
                         text = paste0(groupe, " : ", round(pct, 1), "%"))) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = pal_categ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
      labs(title = "Repartition des 4 groupes (ponderee)", x = NULL, y = "% des menages") +
      theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))
    ggplotly(p, tooltip = "text")
  })
  
  wmean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
  
  output$table_comparatif <- DT::renderDataTable({
    df <- menages_filtres()
    validate(need(nrow(df) > 0, "Aucun menage ne correspond a ces filtres."))
    df %>%
      mutate(pauvre = if_else(pcexp < zref, 1, 0)) %>%
      group_by(groupe) %>%
      summarise(
        nb_menages         = round(sum(hhweight)),
        taille_menage      = round(wmean(hhsize, hhweight), 2),
        incidence_pauvrete = round(100 * wmean(pauvre, hhweight), 1),
        fies_moyen         = round(wmean(score_fies, hhweight), 2),
        hdds_moyen         = round(wmean(hdds, hhweight), 2),
        .groups = "drop"
      )
  }, options = list(dom = "t", paging = FALSE, scrollX = TRUE, autoWidth = TRUE),
  class = "stripe hover compact")
  
  output$carte_profil <- renderPlotly({
    besoin_donnees(gps_grappe)
    df <- menages_filtres()
    validate(need(nrow(df) > 0, "Aucun menage ne correspond a ces filtres."))
    fies_grappe <- df %>%
      group_by(grappe) %>%
      summarise(fies_moyen = wmean(score_fies, hhweight), .groups = "drop") %>%
      inner_join(gps_grappe, by = "grappe") %>%
      mutate(popup_text = paste0("Grappe : ", grappe, "<br>FIES moyen : ", round(fies_moyen, 2)))
    validate(need(nrow(fies_grappe) > 0, "Pas de grappes geolocalisees pour cette selection."))
    
    plot_ly(fies_grappe, lat = ~GPS__Latitude, lon = ~GPS__Longitude,
            type = "scattermapbox", mode = "markers",
            marker = list(size = 10, color = ~fies_moyen, colorscale = "RdYlGn", reversescale = TRUE,
                          opacity = 0.85, colorbar = list(title = "FIES moyen\n(vert = mieux)")),
            text = ~popup_text, hoverinfo = "text") %>%
      layout(mapbox = list(style = "carto-positron",
                           center = list(lat = mean(fies_grappe$GPS__Latitude, na.rm = TRUE),
                                         lon = mean(fies_grappe$GPS__Longitude, na.rm = TRUE)),
                           zoom = 5.5),
             margin = list(l = 0, r = 0, t = 0, b = 0))
  })
  
  ## --------------------------- ONGLET 3 ---------------------------------
  
  rendement_winsorise <- reactive({
    besoin_donnees(production_mais, "production_mais (base avant winsorisation) non disponible.")
    bornes <- quantile(production_mais$rendement_kg_ha,
                       probs = c((100 - input$f3_winsor) / 200, 1 - (100 - input$f3_winsor) / 200),
                       na.rm = TRUE)
    production_mais %>%
      filter(surface_mais_ha >= 0.05, rendement_kg_ha <= 5000) %>%
      filter(between(rendement_kg_ha, bornes[[1]], bornes[[2]]))
  })
  
  output$hist_rendements <- renderPlotly({
    df <- rendement_winsorise()
    p <- ggplot(df, aes(x = rendement_kg_ha)) +
      geom_histogram(bins = 40, fill = pal_accent, color = "white", alpha = 0.92) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank()) +
      labs(title = paste0("Distribution des rendements (winsorisation p", input$f3_winsor, ")"),
           x = "Rendement (kg/ha)", y = "Nombre de menages")
    ggplotly(p)
  })
  
  output$carte_rendements <- renderPlotly({
    besoin_donnees(rendement_geo)
    df <- rendement_geo
    var_couleur <- if (input$f3_pluie && "pluie_totale_mm" %in% names(df)) "pluie_totale_mm" else "rendement_moyen_kg_ha"
    validate(need(var_couleur %in% names(df), "Variable de pluviometrie non disponible (voir Module 3)."))
    titre_legende <- if (var_couleur == "pluie_totale_mm") "Pluie (mm)" else "Rendement (kg/ha)"
    
    if ("pluie_totale_mm" %in% names(df)) {
      df <- df %>% mutate(popup_text = paste0("Grappe : ", grappe,
                                              "<br>Rendement : ", round(rendement_moyen_kg_ha), " kg/ha",
                                              "<br>Pluie : ", round(pluie_totale_mm), " mm"))
    } else {
      df <- df %>% mutate(popup_text = paste0("Grappe : ", grappe,
                                              "<br>Rendement : ", round(rendement_moyen_kg_ha), " kg/ha"))
    }
    
    plot_ly(df, lat = ~lat, lon = ~lon, type = "scattermapbox", mode = "markers",
            marker = list(size = ~pmin(6 + n_menages / 3, 22), color = df[[var_couleur]],
                          colorscale = "Viridis", opacity = 0.85,
                          colorbar = list(title = titre_legende)),
            text = ~popup_text, hoverinfo = "text") %>%
      layout(mapbox = list(style = "carto-positron",
                           center = list(lat = mean(df$lat, na.rm = TRUE), lon = mean(df$lon, na.rm = TRUE)),
                           zoom = 5.5),
             margin = list(l = 0, r = 0, t = 0, b = 0))
  })
  
  output$plot_coefs_m3 <- renderPlotly({
    besoin_donnees(coefs_m3)
    p <- ggplot(coefs_m3, aes(x = estimate, y = reorder(term, estimate))) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = pal_accent2) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Determinants du ln(rendement)", x = "Coefficient estime", y = NULL)
    ggplotly(p)
  })
  
  output$plot_intrants <- renderPlotly({
    besoin_donnees(intrants_mais)
    p <- ggplot(intrants_mais, aes(x = reorder(intrant, part_utilisations), y = part_utilisations,
                                   text = paste0(intrant, " : ", part_utilisations, "%"))) +
      geom_col(fill = pal_accent2, width = 0.65) + coord_flip() + theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank()) +
      labs(title = "Utilisation d'intrants chez les producteurs de mais",
           x = NULL, y = "Part des utilisations declarees (%)")
    ggplotly(p, tooltip = "text")
  })
  
  ## --------------------------- ONGLET 4 ---------------------------------
  
  output$f4_distance_ui <- renderUI({
    besoin_donnees(prix_carto_dash, "")
    if (is.null(prix_carto_dash)) return(NULL)
    max_d <- ceiling(max(prix_carto_dash$distance_marche_proxy_km, na.rm = TRUE))
    sliderInput("f4_distance", "Distance max. au marche le plus proche (proxy, km) :",
                min = 0, max = max_d, value = max_d, width = "100%")
  })
  
  output$f4_canal_ui <- renderUI({
    besoin_donnees(canaux, "")
    if (is.null(canaux)) return(NULL)
    selectInput("f4_canal", "Type d'acheteur :",
                choices = c("Tous", as.character(canaux$canal)), selected = "Tous", width = "100%")
  })
  
  output$carte_prix <- renderPlotly({
    besoin_donnees(prix_carto_dash)
    seuil <- if (is.null(input$f4_distance)) max(prix_carto_dash$distance_marche_proxy_km, na.rm = TRUE) else input$f4_distance
    df <- prix_carto_dash %>%
      filter(distance_marche_proxy_km <= seuil, !is.na(prix_prod)) %>%
      mutate(popup_text = paste0("Grappe : ", grappe, "<br>Prix producteur : ", round(prix_prod), " FCFA/kg",
                                 "<br>Distance proxy : ", round(distance_marche_proxy_km, 1), " km"))
    validate(need(nrow(df) > 0, "Aucune grappe dans ce rayon."))
    
    plot_ly(df, lat = ~GPS__Latitude, lon = ~GPS__Longitude, type = "scattermapbox", mode = "markers",
            marker = list(size = ~pmin(6 + distance_marche_proxy_km / 5, 22), color = ~prix_prod,
                          colorscale = "YlOrRd", opacity = 0.85,
                          colorbar = list(title = "Prix producteur\n(FCFA/kg)")),
            text = ~popup_text, hoverinfo = "text") %>%
      layout(mapbox = list(style = "carto-positron",
                           center = list(lat = mean(df$GPS__Latitude, na.rm = TRUE),
                                         lon = mean(df$GPS__Longitude, na.rm = TRUE)),
                           zoom = 5.5),
             margin = list(l = 0, r = 0, t = 0, b = 0))
  })
  
  output$plot_canaux <- renderPlotly({
    besoin_donnees(canaux)
    df <- canaux
    if (!is.null(input$f4_canal) && input$f4_canal != "Tous") {
      df <- df %>% mutate(surligne = as.character(canal) == input$f4_canal)
    } else {
      df <- df %>% mutate(surligne = FALSE)
    }
    p <- ggplot(df, aes(x = reorder(canal, pct), y = pct, fill = surligne,
                        text = paste0(canal, " : ", pct, "%"))) +
      geom_col(width = 0.65) + coord_flip() +
      scale_fill_manual(values = c("TRUE" = pal_accent, "FALSE" = pal_neutre), guide = "none") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank()) +
      labs(title = "Repartition des canaux de vente", x = NULL, y = "% des vendeurs")
    ggplotly(p, tooltip = "text")
  })
  
  output$table_marge <- DT::renderDataTable({
    besoin_donnees(marge_region)
    marge_region %>% mutate(across(where(is.numeric), ~ round(.x, 1)))
  }, options = list(dom = "t", paging = FALSE, scrollX = TRUE, autoWidth = TRUE),
  class = "stripe hover compact")
  
  ## --------------------------- ONGLET 5 ---------------------------------
  
  output$f5_groupe_ui <- renderUI({
    besoin_donnees(ech_reg_m5, "")
    if (is.null(ech_reg_m5)) return(NULL)
    choix <- c("Tous", sort(unique(as.character(ech_reg_m5$groupe_mais))))
    selectInput("f5_groupe", "Groupe de menage :", choices = choix, selected = "Tous", width = "100%")
  })
  
  m5_filtre <- reactive({
    besoin_donnees(ech_reg_m5)
    df <- ech_reg_m5
    if (!is.null(input$f5_groupe) && input$f5_groupe != "Tous") {
      df <- df %>% filter(as.character(groupe_mais) == input$f5_groupe)
    }
    df
  })
  
  output$val_score_groupe <- renderText({
    df <- m5_filtre()
    validate(need(nrow(df) > 0, "N/A"))
    if (input$f5_outcome == "FIES") {
      paste0(round(weighted.mean(df$score_fies, df$hhweight, na.rm = TRUE), 2), " / 8")
    } else {
      paste0(round(weighted.mean(df$hdds, df$hhweight, na.rm = TRUE), 2), " / 12")
    }
  })
  
  output$val_pct_severe <- renderText({
    df <- m5_filtre()
    validate(need(nrow(df) > 0, "N/A"))
    paste0(round(100 * weighted.mean(df$fies_severe, df$hhweight, na.rm = TRUE), 1), "%")
  })
  
  output$plot_coefs_m5 <- renderPlotly({
    besoin_donnees(coefs_m5)
    df <- coefs_m5 %>% filter(str_detect(modele, input$f5_outcome))
    validate(need(nrow(df) > 0, "Pas de coefficients pour cet indicateur."))
    p <- ggplot(df, aes(x = estimate, y = term_label)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                      color = if (input$f5_outcome == "FIES") pal_accent else pal_accent2) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank()) +
      labs(title = paste("Modele principal :", input$f5_outcome), x = "Coefficient estime", y = NULL)
    ggplotly(p)
  })
  
  output$carte_securite <- renderPlotly({
    besoin_donnees(securite_grappe)
    var_couleur <- if (input$f5_outcome == "FIES") "fies_moyen" else "hdds_moyen"
    inverser <- input$f5_outcome == "FIES"   # FIES : rouge = mauvais ; HDDS : rouge = mauvais aussi (echelle non inversee)
    df <- securite_grappe %>%
      mutate(popup_text = paste0("Grappe : ", grappe, "<br>", input$f5_outcome, " moyen : ",
                                 round(.data[[var_couleur]], 2)))
    
    plot_ly(df, lat = ~GPS__Latitude, lon = ~GPS__Longitude, type = "scattermapbox", mode = "markers",
            marker = list(size = 10, color = df[[var_couleur]], colorscale = "RdYlGn", reversescale = inverser,
                          opacity = 0.85, colorbar = list(title = paste(input$f5_outcome, "moyen"))),
            text = ~popup_text, hoverinfo = "text") %>%
      layout(mapbox = list(style = "carto-positron",
                           center = list(lat = mean(df$GPS__Latitude, na.rm = TRUE),
                                         lon = mean(df$GPS__Longitude, na.rm = TRUE)),
                           zoom = 5.5),
             margin = list(l = 0, r = 0, t = 0, b = 0))
  })
  
  output$plot_coefs_hetero <- renderPlotly({
    besoin_donnees(coefs_hetero)
    p <- ggplot(coefs_hetero, aes(x = estimate, y = term_label, color = type_terme)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
      scale_color_manual(values = c("Effet principal" = "#8D98A5", "Terme d'interaction" = pal_accent)) +
      facet_wrap(~modele, scales = "free_y") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank()) +
      labs(x = "Coefficient estime", y = NULL, color = NULL)
    ggplotly(p)
  })
}

shinyApp(ui, server)