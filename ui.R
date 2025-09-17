############################################################
################### INTERFACE UTILISATEUR #################
############################################################


ui <- navbarPage(
  "RH Pay Insights",
  
  # la page d'accueil
  tabPanel(
    
    "Accueil",
    
    fluidPage(
      
      # Titre principal
      titlePanel("Bienvenue dans l'Analyse des rémunérations RH"),
      
      # Texte d’introduction
      p("Ce site s'inscrit dans le cadre d'un projet universitaire. 
        Il permet d'explorer les salaires, comparer les groupes et exporter des résultats."),
      
      # Boutons 
      fluidRow(
        column(6, actionButton("btn_commencer", "Commencer")),
        column(6, actionButton("btn_methodo", "Voir la méthodologie"))
      ),
      
      br(), # saut de ligne 
      
      #ctrois indicateurs 
      fluidRow(
        column(
          4,
          wellPanel(
            strong("Nombre de salariés"),
            textOutput("indicateur_salaries")
          )
        ),
        column(
          4,
          wellPanel(
            strong("Part des CDI"),
            textOutput("indicateur_cdi")
          )
        ),
        column(
          4,
          wellPanel(
            strong("Salaire médian"),
            textOutput("indicateur_salaire")
          )
        )
      ),
      
      #br(),
      #helpText("Périmètre : à définir • Données : RH_Contrats.xlsx et RH_Salaries.xlsx • Version 0.1")
    )
  ),
  
  # Creation des autres onglets mais vide pour le moment
  tabPanel("Explorer", fluidPage(h3("À venir"))),
  tabPanel("Comparer", fluidPage(h3("À venir"))),
  tabPanel("Contrats", fluidPage(h3("À venir"))),
  tabPanel("Population", fluidPage(h3("À venir"))),
  tabPanel("Exports", fluidPage(h3("À venir"))),
  tabPanel("À propos", fluidPage(h3("À venir")))
)
