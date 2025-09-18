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
    
    
    
    # ------------------------------------------------------
    # Graphique 1 : histogramme des salaires
    # ------------------------------------------------------
    output$plot_histo_salaire <- renderPlot({
      # Sécurité : vérifie l'existence de la colonne
      req("Salaire" %in% names(salaires))
      
      hist(
        salaires$Salaire,
        breaks = 30,            # nombre de classes
        main   = "Salaires (brut) - histogramme",
        xlab   = "Salaire",
        ylab   = "Effectif",
        col = "skyblue",
        border = "white"
      )
    })
    
    # ------------------------------------------------------
    # Graphique 2 : barres par type de contrat
    # ------------------------------------------------------
    output$plot_bar_contrat <- renderPlot({
      # Sécurité : vérifie l'existence de la colonne
      req("Contrat" %in% names(contrats))
      
      # Tableau de fréquences simple
      tab <- sort(table(contrats$Contrat), decreasing = TRUE)
      
      barplot(
        tab,
        main = "Nombre de contrats par type",
        xlab = "Type de contrat",
        ylab = "Effectif",
        las  = 2,                # étiquettes verticales si besoin
        col = "skyblue",
        border ="white"
      )
    })
    
    
}
