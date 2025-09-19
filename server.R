############################################################
################### BACK-END ##############################
###########################################################

# Define server logic 
server <- function(input, output, session) 
  {
  
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
    
    
}
