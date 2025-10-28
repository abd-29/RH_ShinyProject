############################################################
################### BACK-END ##############################
#########################################################

# Define server logic 
server <- function(input, output, session) 
  {
  
  
  ############################################################################
  ##                       PARTIE ACCUEIL DU SITE                           ##
  ############################################################################
  # ============== CALCUL INDICATEUR PAGE ACCUEIL ==============
  
    # Nombre de salarie
    output$indicateur_salaries <- renderText({
      n_distinct(data$id_salarie)
    })
    
    # calcul du pourcentage de contrat en cdi
    output$indicateur_cdi <- renderText({
      
      if ("contrat" %in% names(data)) {
        part <- mean(data$contrat == "CDI", na.rm = TRUE) # calcul de la moyenne
        paste0(round(100 * part), " %") # format d'affichage en pourcentage
      } else {
        "Non disponible"
      }
    })
    
    
    # Nombre de contrat
    output$indicateur_contrats <- renderText({
      nrow(data)
    })
    
    output$indicateur_salaire_moyen <- renderText({
      if ("salaire" %in% names(data)) {
        moy <- mean(data$salaire, na.rm = TRUE)
        paste0(round(moy), " €")
      } else {
        "Non disponible"
      }
    })
    
    # salaire moyen et salaire median
    output$label_salaire <- renderText({
      if (isTRUE(input$show_mean)) "Salaire moyen" else "Salaire médian"
    })
    
    # Valeur dynamique
    output$indicateur_salaire <- renderText({
      if (!"salaire" %in% names(data)) return("Non disponible")
      
      val <- if (isTRUE(input$show_mean))
        mean(data$salaire, na.rm = TRUE) else
          median(data$salaire, na.rm = TRUE)
      paste(format(round(val), big.mark = " ", scientific = FALSE), "€")
    })
    
    # ------------------------------------------------------
    # Graphique 1 : histogramme des salaires
    # ------------------------------------------------------
    output$plot_histo_salaire <- renderPlotly({
      req("salaire" %in% names(data))
      
      plot_ly(
        data = data,
        x    = ~salaire,
        type = "histogram",
        nbinsx = 30,
        marker = list(
          color = "blue",        # barres bleu ciel
          line  = list(color = "transparent") # pas de bordure
        )
      ) |>
        layout(
          title = "",
          xaxis = list(title = "Salaire"),
          yaxis = list(title = "Effectif"),
          bargap = 0.05
        )
    })
    
    # ------------------------------------------------------
    # Graphique 2 : barres par type de contrat
    # ------------------------------------------------------
    output$plot_pie_contrat <- renderPlotly({
      req("contrat" %in% names(data))
      
      tab <- data |>
        dplyr::mutate(
          contrat = dplyr::if_else(is.na(contrat) | contrat == "", "Non renseigné", as.character(contrat))
        ) |>
        dplyr::count(contrat, name = "Valeur") |>
        dplyr::arrange(dplyr::desc(Valeur))
      
      plot_ly(
        data   = tab,
        labels = ~contrat,
        values = ~Valeur,
        type   = "pie",
        hole   = 0.6,
        textinfo = "label+percent",
        hovertemplate = "%{label} : %{value}<extra></extra>"
      ) |>
        layout(showlegend = TRUE)
    })
    
    
    # le bouton commencer pointe vers la page "Explorer"
    observeEvent(input$btn_commencer, {
    updateNavbarPage(session, "main_nav", selected = "Explorer")
})
    
    ############################################################################
    ##                       PARTIE EXPORTER DU SITE                          ##
    ############################################################################
    observe({
      if ("salaire" %in% names(data)) {
        mx <- suppressWarnings(max(data$salaire, na.rm = TRUE))
        if (is.finite(mx)) updateSliderInput(session, "filtre_salaire_min", max = ceiling(mx))
      }
      if ("age" %in% names(data)) {
        a <- range(data$age, na.rm = TRUE)
        if (all(is.finite(a))) updateSliderInput(session, "age_range", min = floor(a[1]), max = ceiling(a[2]),
                                                 value = c(floor(a[1]), ceiling(a[2])))
      }
      if ("annee_naissance" %in% names(data)) {
        y <- range(data$annee_naissance, na.rm = TRUE)
        if (all(is.finite(y))) updateSliderInput(session, "annee_range", min = y[1], max = y[2], value = y)
      }
    })
    
  
    
    #Application des filtres 
    donnees_filtrees_all <- reactive({
      df <- data
      
      if ("salaire" %in% names(df)) df <- df[df$salaire >= input$filtre_salaire_min | is.na(df$salaire), , drop = FALSE]
      if (nzchar(trimws(input$filtre_contrat)) && "contrat" %in% names(df)) {
        pat <- trimws(input$filtre_contrat)
        df <- df[!is.na(df$contrat) & grepl(pat, df$contrat, ignore.case = TRUE), , drop = FALSE]
      }
      if ("age" %in% names(df)) df <- df[df$age >= input$age_range[1] & df$age <= input$age_range[2], , drop = FALSE]
      if (input$sit_mat != "Toutes" && "etat_civil" %in% names(df)) {
        df <- df[!is.na(df$etat_civil) & df$etat_civil == input$sit_mat, , drop = FALSE]
      }
      if ("annee_naissance" %in% names(df)) df <- df[df$annee_naissance >= input$annee_range[1] &
                                                       df$annee_naissance <= input$annee_range[2], , drop = FALSE]
      if ("enfants" %in% names(df)) df <- df[df$enfants >= input$nb_enfants_min, , drop = FALSE]
      if (input$sexe != "Tous" && "sexe" %in% names(df)) df <- df[df$sexe == input$sexe, , drop = FALSE]
      if (input$heures != "all") {
        colh <- intersect(c("hebdo_duree","duree_hebdo"), names(df))
        if (length(colh) == 1) df <- df[df[[colh]] == as.numeric(input$heures), , drop = FALSE]
      }
      df
    })
    
    # 3) Aperçu (10 lignes si case cochée)
    output$table_filtre_preview <- renderTable({
      df <- if (isTRUE(input$preview10)) head(donnees_filtrees_all(), 10) else donnees_filtrees_all()
      
      # petite fonction d'affichage
      to_display <- function(x) {
        if (is.numeric(x)) {
          ifelse(is.na(x), "Non renseigné",
                 format(x, big.mark = " ", trim = TRUE, scientific = FALSE))
        } else {
          ifelse(is.na(x) | x == "", "Non renseigné", as.character(x))
        }
      }
      
      # colonnes à nettoyer si elles existent
      cols <- intersect(c("enfants", "etat_civil", "salaire"), names(df))
      for (cl in cols) df[[cl]] <- to_display(df[[cl]])
      
      df
    }, striped = TRUE, bordered = TRUE)
    
    # 4) Téléchargements CSV 
    output$dl_salaries_csv <- downloadHandler(
      filename = function() "RH_Salaries.csv",
      content  = function(file) write.csv2(Salaires, file, row.names = FALSE, fileEncoding = "UTF-8")
    )
    
    output$dl_contrats_csv <- downloadHandler(
      filename = function() "RH_Contrats.csv",
      content  = function(file) write.csv2(Contrats, file, row.names = FALSE, fileEncoding = "UTF-8")
    )
    
    output$dl_filtre_csv <- downloadHandler(
      filename = function() "Donnees_filtrees.csv",
      content  = function(file) write.csv2(donnees_filtrees_all(), file, row.names = FALSE, fileEncoding = "UTF-8")
    )
    
}
