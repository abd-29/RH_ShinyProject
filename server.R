############################################################
################### BACK-END ##############################
###########################################################

# Define server logic 
server <- function(input, output, session) 
  {
  
  
  ############################################################################
  ##                       PARTIE ACCUEIL DU SITE                           ##
  ############################################################################
  # ============== CALCUL INDICATEUR PAGE ACCUEIL ==============
  
    # Nombre de salarie
    output$indicateur_salaries <- renderText({
      n_distinct(salaires$id_salarié)
    })
    
    # calcul du pourcentage de contrat en cdi
    output$indicateur_cdi <- renderText({
      
      if ("Contrat" %in% names(contrats)) {
        part <- mean(contrats$Contrat == "CDI", na.rm = TRUE) # calcul de la moyenne
        paste0(round(100 * part), " %") # calcul du pourcentage et on rajoute le signe de pourcent
      } else {
        "Non disponible"
      }
    })
    
    
    # Nombre de contrat
    output$indicateur_contrats <- renderText({
      nrow(contrats)
    })
    
    output$indicateur_salaire_moyen <- renderText({
      if ("Salaire" %in% names(salaires)) {
        moy <- mean(salaires$Salaire, na.rm = TRUE)
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
      if (!"Salaire" %in% names(salaires)) return("Non disponible")
      val <- if (isTRUE(input$show_mean))
        mean(salaires$Salaire, na.rm = TRUE) else
          median(salaires$Salaire, na.rm = TRUE)
      paste(format(round(val), big.mark = " ", scientific = FALSE), "€")
    })
    
    # ------------------------------------------------------
    # Graphique 1 : histogramme des salaires
    # ------------------------------------------------------
    output$plot_histo_salaire <- renderPlotly({
      req("Salaire" %in% names(salaires))
      
      plot_ly(
        data = salaires,
        x    = ~Salaire,
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
      req("Contrat" %in% names(contrats))
      
      tab <- data.frame(
        Etat = c("CDI", "CDD"),
        Valeur = c(sum(contrats$Contrat == "CDI", na.rm = TRUE),
                   sum(contrats$Contrat == "CDD", na.rm = TRUE))
      )
      
      plot_ly(
        data   = tab,
        labels = ~Etat,
        values = ~Valeur,
        type   = "pie",
        hole   = 0.6,
        marker = list(colors = c("blue", "lightgray")),
        textinfo = "label+percent",   # % affichés sur le graphe
        hovertemplate = "%{label} : %{value}<extra></extra>" # infobulle = uniquement nombre
      ) |>
        layout(showlegend = FALSE)
    })
    
    # le bouton commencer pointe vers la page "Explorer"
    observeEvent(input$btn_commencer, {
    updateNavbarPage(session, "main_nav", selected = "Explorer")
})
    
    ############################################################################
    ##                       PARTIE EXPORTER DU SITE                          ##
    ############################################################################
    observe({
      if ("Salaire" %in% names(salaires)) {
        mx <- suppressWarnings(max(salaires$Salaire, na.rm = TRUE))
        if (is.finite(mx)) updateSliderInput(session, "filtre_salaire_min", max = ceiling(mx))
      }
      if ("Age" %in% names(salaires)) {
        a <- range(salaires$Age, na.rm = TRUE)
        if (all(is.finite(a))) updateSliderInput(session, "age_range", min = floor(a[1]), max = ceiling(a[2]),
                                                 value = c(floor(a[1]), ceiling(a[2])))
      }
      if ("Annee_naissance" %in% names(salaires)) {
        y <- range(salaires$Annee_naissance, na.rm = TRUE)
        if (all(is.finite(y))) updateSliderInput(session, "annee_range", min = y[1], max = y[2], value = y)
      }
    })
    
    # 1) Table jointe pour filtrage (clé supposée: id_salarié)
    donnees_jointes <- reactive({
      if ("id_salarié" %in% names(salaires) && "id_salarié" %in% names(contrats)) {
        dplyr::left_join(salaires, contrats, by = "id_salarié")
      } else {
        # si la clé diffère, on renvoie simplement salaires
        salaires
      }
    })
    
    # 2) Application des filtres (on n'applique que si la colonne existe)
    donnees_filtrees_all <- reactive({
      df <- donnees_jointes()
      
      if ("Salaire" %in% names(df)) df <- df[df$Salaire >= input$filtre_salaire_min | is.na(df$Salaire), , drop = FALSE]
      if (nzchar(input$filtre_contrat) && "Contrat" %in% names(df)) df <- df[df$Contrat == input$filtre_contrat, , drop = FALSE]
      if ("Age" %in% names(df)) df <- df[df$Age >= input$age_range[1] & df$Age <= input$age_range[2], , drop = FALSE]
      if ("Annee_naissance" %in% names(df)) df <- df[df$Annee_naissance >= input$annee_range[1] &
                                                       df$Annee_naissance <= input$annee_range[2], , drop = FALSE]
      if ("Nb_enfants" %in% names(df)) df <- df[df$Nb_enfants >= input$nb_enfants_min, , drop = FALSE]
      if (input$sexe != "Tous" && "Sexe" %in% names(df)) df <- df[df$Sexe == input$sexe, , drop = FALSE]
      if (input$heures != "all") {
        colh <- intersect(c("Heures_hebdo","HeuresHebdo","hebdo_heures"), names(df))
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
      cols <- intersect(c("Enfants", "Etat Civil", "Salaire"), names(df))
      for (cl in cols) df[[cl]] <- to_display(df[[cl]])
      
      df
    }, striped = TRUE, bordered = TRUE)
    
    # 4) Téléchargements CSV (séparateur ;, compatible Excel FR)
    output$dl_salaries_csv <- downloadHandler(
      filename = function() "RH_Salaries.csv",
      content  = function(file) write.csv2(salaires, file, row.names = FALSE, fileEncoding = "UTF-8")
    )
    output$dl_contrats_csv <- downloadHandler(
      filename = function() "RH_Contrats.csv",
      content  = function(file) write.csv2(contrats, file, row.names = FALSE, fileEncoding = "UTF-8")
    )
    output$dl_filtre_csv <- downloadHandler(
      filename = function() "Donnees_filtrees.csv",
      content  = function(file) write.csv2(donnees_filtrees_all(), file, row.names = FALSE, fileEncoding = "UTF-8")
    )
    
}
