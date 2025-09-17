############################################################
################### FRONT-END ##############################
############################################################

# Page principale (UI)
ui <- navbarPage(
  
  "RH Pay Insights",                                                                           # Titre du site
  
  tabPanel(
    "Accueil",                                                                                 # la page d'accueil
    
    fluidPage(
      
      br(),                                                                                     # saut de ligne 
      h1("Bienvenue — Analyse des rémunérations RH"),                                           # titre du site 
      p("Ce site s'inscrit dans le cadre d'un projet universitaire. 
         Il permet d'explorer les salaires, comparer les groupes et exporter des résultats."),  # un simple paragraphe
      
      # Boutons (pas encore branchés)
      div(
        actionButton("btn_commencer", "Commencer", class = "btn-primary"),                     # bouton commencer
        actionButton("btn_methodo", "Voir la méthodologie", class = "ms-2")                    # bouton Voir la méthodologie
      ),
      
      br(),
      
      # on ménage des espaces à remplir dans le futur
      fluidRow(
        column(4, wellPanel(h4("Effectif total"),   h2("—"))),
        column(4, wellPanel(h4("% CDI"),            h2("—"))),
        column(4, wellPanel(h4("Salaire médian"),   h2("—")))
      ),
      
      br(),
      # tags$small("Périmètre : à définir • Données : RH_Contrats.xlsx — RH_Salaries.xlsx • Version 0.1")  # un simple footer, mais on verra
    )
  )
)
