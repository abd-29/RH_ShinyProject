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
    
}
