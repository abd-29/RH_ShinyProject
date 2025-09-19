############################################################
################### INTERFACE UTILISATEUR #################
############################################################


ui <- navbarPage(
  title = tags$span("RH Pay Insights", style = "font-weight:bold; color:black;"),  # le titre 
  id = "main_nav", 
    
  # defini le style du menu de navigation
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
      
      # ================ GAUCHE (textes + indicateurs) et à DROITE (graphiques) =======
      fluidRow(
        
        # ============ Colonne gauche =================
        column(
          width = 6,
          
          # phrase d'introduction
          p(
            "Ce site s'inscrit dans le cadre d'un projet universitaire. 
            Il permet d'explorer les salaires, comparer les groupes et exporter des résultats.",
            style = "font-size:1.2em;"   # augmente la taille
          ),
          
          br(), # retour à la ligne
          
          # Boutons 
          fluidRow(
            column(
              12,
                # =============== BOUTON COMMENCER ==========
              div(
                actionButton(
                  "btn_commencer",
                  "Commencer",
                  class = "btn btn-primary btn-lg"
                ),
                
                # le role du bouton
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
          
          # =========== les indicateurs ====================
          fluidRow(
            column(
              width = 8,
              style = "padding-right:6px;", 
              fluidRow(
                # ============== BLOC NOMBRE SALARIE ========================
                column(
                  12,
                  wellPanel(
                    style = "padding:12px; margin-bottom:6px; min-height:90px;",
                    div(
                      style = "display:flex; align-items:center; gap:6px;",
                      tags$img(src = "salarie_icone.png",  # icone
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
                # ============== BLOC INDICATEUR CDI ========================
                column(
                  6,
                  wellPanel(
                    style = "padding:12px; margin-bottom:3px; min-height:90px;", 
                    div(
                      style = "display:flex; align-items:center; gap:6px;",
                      tags$img(src = "cdi_icone.png",  
                               style = "height:20px;"),
                      "CDI"
                    ),
                    div(
                      style="font-size:1.6em; font-weight:bold; margin-left:30px;",
                      textOutput("indicateur_cdi")
                    )
                  )
                ),
                
                # ============== BLOC INDICATEUR CONTRAT ========================
                column(
                  6,
                  wellPanel(
                    style = "padding:12px; margin-bottom:3px; min-height:90px;", 
                    div(
                      style = "display:flex; align-items:center; gap:6px;",
                      tags$img(src = "contrat_icone.png",   # mets une icône dans www/
                               style = "height:20px;"),
                      "Contrats"
                    ),
                    div(
                      style="font-size:1.6em; font-weight:bold; margin-left:30px;",
                      textOutput("indicateur_contrats")
                    )
                  )
                )
              )
            ),
            
            # ============== BLOC SALAIRE MEDIAN ========================
            column(
              width = 4,
              style = "padding-left:6px;",
              wellPanel(
                style = "padding:12px; margin-bottom:6px; min-height:190px;",
                
                # Ligne titre + icône
                div(
                  style = "display:flex; align-items:center; gap:6px;",
                  tags$img(src = "salaire_icone.png",   # ajoute une icône dans www/
                           style = "height:20px;"),
                  textOutput("label_salaire")           # libellé dynamique : médian ou moyen
                ),
                
                # Chiffre en gras et grand
                div(
                  style="font-size:1.4em; font-weight:bold; margin-left:30px; margin-top:10px;",
                  textOutput("indicateur_salaire")
                ),
                
                # Bouton switch, placé plus bas
                div(
                  style="margin-top:20px; display:flex; justify-content:flex-end;",  # espace au-dessus
                  materialSwitch(
                    inputId = "show_mean",
                    value = FALSE,     # FALSE = médian par défaut
                    status = "primary"
                  )
                )
              )
            )
          ),
           
          br(),
          
          # Texte explicatif en dessous des indicateurs
          h4(em("À propos des indicateurs")),
          tags$ul(
            style = "font-style:italic; margin:6px 0 0; padding-left:22px; 
           list-style:disc; list-style-position:outside; line-height:1.35; text-align:justify;",
            tags$li("Nombre de salariés : effectif unique des personnes présentes dans les données."),
            tags$li("CDI : part des contrats de type CDI parmi l'ensemble des contrats."),
            tags$li("Contrats : nombre total de contrats recensés."),
            tags$li("Salaire médian / moyen : montants en euros. Le bouton permet d’alterner l’affichage.")
          ),
          
          br(),
          helpText("Données : RH_Contrats.xlsx et RH_Salaries.xlsx")
        ),
        
        # ============== PARTIE DES GRAPHIQUES ========================
        # =============================================================
        
        column(
          width = 6,
          
          # graphique cercle
          fluidRow(
            column(
              12,
              div(
                strong("Répartition des contrats"),
                plotlyOutput("plot_pie_contrat", height = 220)
              )
            )
          ),
          
          # histogramme pour les salaire
          fluidRow(
            column(
              12,
              div(
                strong("Distribution des salaires"),
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
