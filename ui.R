############################################################
################### INTERFACE UTILISATEUR #################
############################################################


ui <- navbarPage(
  "RH Pay Insights",
  
  # la page d'accueil
  tabPanel(
    "Accueil",
    fluidPage(
      titlePanel("Bienvenue — Analyse des rémunérations RH"),
      
      # Intro
      p("Ce site s'inscrit dans le cadre d'un projet universitaire. 
      Il permet d'explorer les salaires, comparer les groupes et exporter des résultats."),
      
      # ======= MISE EN PAGE : GAUCHE (texte + indicateurs) / DROITE (graphiques) =======
      fluidRow(
        # ---- Colonne gauche : texte + 3 indicateurs ----
        column(
          width = 8,
          
          # Boutons (placeholders)
          fluidRow(
            column(6, actionButton("btn_commencer", "Commencer")),
            column(6, actionButton("btn_methodo", "Voir la méthodologie"))
          ),
          
          br(),
          
          # Indicateurs (textOutput déjà définis côté serveur)
          fluidRow(
            column(
              4, wellPanel(
                strong("Nombre de salariés"),
                textOutput("indicateur_salaries")
              )
            ),
            column(
              4, wellPanel(
                strong("Part des CDI"),
                textOutput("indicateur_cdi")
              )
            ),
            column(
              4, wellPanel(
                strong("Salaire médian"),
                textOutput("indicateur_salaire")
              )
            )
          ),
          
          br(),
          helpText("Périmètre : à définir • Données : RH_Contrats.xlsx et RH_Salaries.xlsx • Version 0.1")
        ),
        
        # ---- Colonne droite : deux graphiques superposés ----
        column(
          width = 4,
          
          wellPanel(
            strong("Distribution des salaires"),
            plotOutput("plot_histo_salaire", height = 260)
          ),
          
          wellPanel(
            strong("Répartition des contrats"),
            plotOutput("plot_bar_contrat", height = 260)
          )
        )
      )
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
