############################################################
################### BACK-END ##############################
###########################################################

# Define server logic 
server <- function(input, output, session) 
  {
    # Pour l’instant : valeurs vides, affichées avec un tiret
    output$indicateur_salaries <- renderText("—")
    output$indicateur_cdi      <- renderText("—")
    output$indicateur_salaire  <- renderText("—")
    
}
