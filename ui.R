############################################################
################### INTERFACE UTILISATEUR #################
############################################################


ui <- navbarPage(
  "RH Pay Insights",
  
  # la page d'accueil
  tabPanel(
    "Accueil",
    fluidPage(
      titlePanel("Analyse des rémunérations RH"),
      
      # ======= MISE EN PAGE : GAUCHE (texte + indicateurs) / DROITE (graphiques) =======
      fluidRow(
        # ---- Colonne gauche : texte + 3 indicateurs ----
        column(
          width = 6,
          
          # Intro
          p("Ce site s'inscrit dans le cadre d'un projet universitaire. 
      Il permet d'explorer les salaires, comparer les groupes et exporter des résultats."),
          
          # Boutons (placeholders)
          fluidRow(
            column(
              6,
              actionButton(
                inputId = "btn_commencer",
                label   = "Commencer",
                class   = "btn btn-primary"   # bouton bleu
              )
            ),
            column(
              6,
              actionButton(
                inputId = "btn_methodo",
                label   = "Voir la méthodologie",
                class   = "btn btn-default"   # bouton gris standard
              )
            )
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
          helpText("Données : RH_Contrats.xlsx et RH_Salaries.xlsx")
        ),
        
        # ---- Colonne droite : deux graphiques superposés ----
        column(
          width = 6,
          
          # Ligne 1 : premier graphique
          fluidRow(
            column(
              12,
              wellPanel(
                style = "background-color: white;",   # fond blanc
                strong("Distribution des salaires"),
                plotlyOutput("plot_histo_salaire", height = 200)
              )
            )
          ),
          
          # Ligne 2 : deuxième graphique
          fluidRow(
            column(
              12,
              wellPanel(
                style = "background-color: white;",   # fond blanc
                strong("Répartition des contrats"),
                plotlyOutput("plot_bar_contrat", height = 200)
              )
            )
          ),
          
          # Ligne 3 : vide (place réservée)
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
  tabPanel("Comparer", fluidPage(h3("À venir"))),
  tabPanel("Contrats", fluidPage(h3("À venir"))),
  tabPanel("Population", fluidPage(h3("À venir"))),
  tabPanel("Exports", fluidPage(h3("À venir"))),
  tabPanel("À propos", fluidPage(h3("À venir")))
)
