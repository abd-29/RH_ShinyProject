############################################################
################### INTERFACE UTILISATEUR #################
############################################################


ui <- navbarPage(
  "RH Pay Insights",
  
  # la page d'accueil
  tabPanel(
    "Accueil",
    fluidPage(
      titlePanel("Analyse de données des Salariés"),
      
      # ======= MISE EN PAGE : GAUCHE (texte + indicateurs) / DROITE (graphiques) =======
      fluidRow(
        # ---- Colonne gauche : texte + 3 indicateurs ----
        column(
          width = 6,
          
          # Intro
          p(
            "Ce site s'inscrit dans le cadre d'un projet universitaire. 
            Il permet d'explorer les salaires, comparer les groupes et exporter des résultats.",
            style = "font-size:1.2em;"   # augmente la taille
          ),
          
          br(),
          
          # Boutons (placeholders)
          fluidRow(
            column(
              6,
              actionButton(
                inputId = "btn_commencer",
                label   = "Commencer",
                class   = "btn btn-primary btn-lg"   # bouton bleu
              )
            ),
            column(
              6,
              actionButton(
                inputId = "btn_methodo",
                label   = "Voir la méthodologie",
                class   = "btn btn-default btn-lg"   # bouton gris standard
              )
            )
          ),
          
          br(),
          
          # --- Indicateurs (version agrandie) ---
          fluidRow(
            # Colonne gauche
            column(
              width = 8,
              style = "padding-right:6px;",   # réduit l’espace à droite
              fluidRow(
                column(
                  12,
                  wellPanel(
                    style = "padding:12px; margin-bottom:6px; min-height:90px;",
                    strong("Nombre de salariés"),
                    div(style="font-size:1.2em;", textOutput("indicateur_salaries"))
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
              style = "padding-left:6px;",   # réduit l’espace à gauche
              wellPanel(
                style = "padding:12px; margin-bottom:6px; min-height:190px;",
                strong("Salaire médian"),
                div(style="font-size:1.8em; font-weight:bold;", textOutput("indicateur_salaire"))
              )
            )
          )
          ,
          
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
              div(
                strong("Distribution des salaires"),
                plotlyOutput("plot_pie_contrat", height = 220)
              )
            )
          ),
          
          # Ligne 2 : deuxième graphique
          fluidRow(
            column(
              12,
              div(
                strong("Répartition des contrats"),
                plotlyOutput("plot_histo_salaire", height = 300)
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
  tabPanel("Exports", fluidPage(h3("À venir"))),
  tabPanel("À propos", fluidPage(h3("À venir")))
)
