############################################################
################### BACK-END ##############################
###########################################################

# Define server logic 
server <- function(input, output, session) 
  {
    # Nombre de salarie
    output$indicateur_salaries <- renderText({
      n_distinct(salaires$id_salarié)
    })
    
    # calcul du pourcentage de contrat en cdi
    output$indicateur_cdi      <- renderText({
      
      if ("Contrat" %in% names(contrats)) {
        part <- mean(contrats$Contrat == "CDI", na.rm = TRUE) # calcul de la moyenne
        paste0(round(100 * part), " %") # calcul du pourcentage et on rajoute le signe de pourcent
      } else {
        "Non disponible"
      }
    })
    
    # calcul du salaire median
    output$indicateur_salaire  <- renderText({
      if ("Salaire" %in% names(salaires)) {
        med <- median(salaires$Salaire, na.rm = TRUE)
        paste0(round(med), " €")
      } else {
        "Non disponible"
      }
    })
    
    # Salaire moyen (nouvel indicateur)
    output$indicateur_salaire_moyen <- renderText({
      if ("Salaire" %in% names(salaires)) {
        moy <- mean(salaires$Salaire, na.rm = TRUE)
        paste(format(round(moy), big.mark = " ", scientific = FALSE), "€")
      } else {
        "Non disponible"
      }
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
          color = "skyblue",        # barres bleu ciel
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
    output$plot_bar_contrat <- renderPlotly({
      req("Contrat" %in% names(contrats))
      
      # table -> data.frame pour plotly
      df_tab <- as.data.frame(sort(table(contrats$Contrat), decreasing = TRUE))
      names(df_tab) <- c("Contrat", "Effectif")
      
      plot_ly(
        data = df_tab,
        x    = ~Contrat,
        y    = ~Effectif,
        type = "bar",
        marker = list(
          color = "skyblue",        # barres bleu ciel
          line  = list(color = "transparent") # pas de bordure
        )
      ) |>
        layout(
          title = "",
          xaxis = list(title = "Type de contrat"),
          yaxis = list(title = "Effectif")
        )
    })
    
    
}
