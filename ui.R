############################################################
################### INTERFACE UTILISATEUR #################
############################################################


ui <- navbarPage(
  title = tags$span("RH Pay Insights", style = "font-weight:bold; color:black;"),
  id = "main_nav", 
   
  header = tags$style(HTML("
    .navbar.navbar-default {
      background-color: white !important;
    }
    .navbar-default .navbar-nav > li > a {
      color: black !important;
      border-bottom: 3px solid transparent;
    }
    /* Effet hover uniquement sur les onglets non actifs */
    .navbar-default .navbar-nav > li:not(.active) > a:hover {
      color: #007BFF !important;
      border-bottom: 3px solid #007BFF;
    }
    /* Onglet actif : reste bleu souligné même au survol des autres */
    .navbar-default .navbar-nav > .active > a {
      color: #007BFF !important;
      background-color: white !important;
      font-weight: bold;
      border-bottom: 3px solid #007BFF;
    }
  ")),
  
  # la page d'accueil
  tabPanel(
    "Accueil",
    fluidPage(
      titlePanel("Analyse de données des Salariés"),
      
      # ======= MISE EN PAGE : GAUCHE (texte + indicateurs) et à DROITE (graphiques) =======
      fluidRow(
        # ---- Colonne gauche ----
        column(
          width = 6,
          
          # Intro
          p(
            "Ce site s'inscrit dans le cadre d'un projet universitaire. 
            Il permet d'explorer les salaires, comparer les groupes et exporter des résultats.",
            style = "font-size:1.2em;"   # augmente la taille
          ),
          
          br(),
          
          # Boutons 
          fluidRow(
            column(
              12,
              div(
                actionButton(
                  "btn_commencer",
                  "Commencer",
                  class = "btn btn-primary btn-lg"
                ),
                tags$a(
                  href   = "https://github.com/abd-29/RH_ShinyProject", # URL du depot git 
                  target = "_blank",
                  class  = "btn btn-default btn-lg",
                  tags$img(src = "logo_git.png",       # on rajoute un logo de git 
                           style = "height:20px; margin-right:6px; vertical-align:middle;",
                           alt = "GitHub"),
                  span("Voir la méthodologie")
                ),
                style = "display:flex; gap:10px;"
              )
            )
          ),
          
          br(),
          
          # --- Indicateurs ---
          fluidRow(
            # Colonne gauche
            column(
              width = 8,
              style = "padding-right:6px;", 
              fluidRow(
                column(
                  12,
                  wellPanel(
                    style = "padding:12px; margin-bottom:6px; min-height:90px;",
                    div(
                      style = "display:flex; align-items:center; gap:6px;",
                      tags$img(src = "salarie_icone.png",  # ton logo dans www/
                               style = "height:20px;"),
                      "Nombre de salariés"
                    ),
                    div(
                      style="font-size:1.8em; font-weight:bold; margin-left:40px;",
                      textOutput("indicateur_salaries")
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  6,
                  wellPanel(
                    style = "padding:12px; margin-bottom:3px; min-height:90px;", # marge réduite
                    strong("CDI"),
                    div(style="font-size:1.2em;", textOutput("indicateur_cdi"))
                  )
                ),
                column(
                  6,
                  wellPanel(
                    style = "padding:12px; margin-bottom:3px; min-height:90px;", # marge réduite
                    strong("Contrats"),
                    div(style="font-size:1.2em;", textOutput("indicateur_contrats"))
                  )
                )
              )
            ),
            
            # Colonne droite : Salaire médian
            column(
              width = 4,
              style = "padding-left:6px;",  
              wellPanel(
                style = "padding:12px; margin-bottom:6px; min-height:190px;",
                strong("Salaire Médian"),
                div(style="font-size:1.8em; font-weight:bold;", textOutput("indicateur_salaire"))
              )
            )
          )
          ,
          
          br(),
          helpText("Données : RH_Contrats.xlsx et RH_Salaries.xlsx")
        ),
        
        # ---- Colonne droite ----
        column(
          width = 6,
          
          # premier graphique
          fluidRow(
            column(
              12,
              div(
                strong("Distribution des salaires"),
                plotlyOutput("plot_pie_contrat", height = 220)
              )
            )
          ),
          
          # deuxième graphique
          fluidRow(
            column(
              12,
              div(
                strong("Répartition des contrats"),
                plotlyOutput("plot_histo_salaire", height = 300)
              )
            )
          ),
          
          # vide pour le moment
          fluidRow(
            column(
              12
            )
          )
        )
      )
    )
  ),
  
  # Creation des autres onglets mais vide pour le moment
  tabPanel("Explorer", fluidPage(h3("À venir"))),
  tabPanel("Exporter les données", fluidPage(h3("À venir"))),
  tabPanel("À propos", fluidPage(h3("À venir")))
)
