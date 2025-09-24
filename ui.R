############################################################
################### INTERFACE UTILISATEUR #################
############################################################
fluidPage(
  
  theme = shinytheme("lumen"),
  navbarPage(
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
    tabPanel(
      "Explorer",
      fluidPage(
        titlePanel("Explorer les rémunérations"),
        sidebarLayout(
          sidebarPanel(width = 4,
                       h4("Périmètre & filtres"),
                       helpText("Les salariés sans contrat sont exclus automatiquement."),
                       # critère de comparaison
                       selectInput("exp_critere", "Comparer par :", 
                                   choices = c("Sexe", "Tranche d'âge", "État civil", "Type de contrat"),
                                   selected = "Sexe"
                       ),
                       # métrique à afficher sur le tableau récap
                       radioButtons("exp_metric", "Statistique de synthèse", 
                                    choices = c("Médiane" = "median", "Moyenne" = "mean"),
                                    inline = TRUE, selected = "median"
                       ),
                       # filtres (optionnels, s’appliquent avant comparaison)
                       sliderInput("exp_age_range", "Âge", min = 18, max = 70, value = c(18, 70)),
                       selectInput("exp_sexe", "Sexe", choices = c("Tous", "H", "F"), selected = "Tous"),
                       textInput("exp_contrat", "Type de contrat (ex: CDI, CDD)", value = ""),
                       selectInput("exp_etat", "État civil", choices = c("Tous", "Célibataire", "Marié(e)"), selected = "Tous"),
                       # options d'affichage
                       checkboxInput("exp_show_table", "Afficher le tableau récapitulatif", TRUE),
                       checkboxInput("exp_show_points", "Afficher les points sur le boxplot", FALSE)
          ),
          mainPanel(width = 8,
                    tabsetPanel(id = "exp_tabs",
                                tabPanel("Graphique", 
                                         br(),
                                         plotlyOutput("exp_plot", height = 420),
                                         br(),
                                         wellPanel(
                                           strong("Test statistique : "), textOutput("exp_test_label", inline = TRUE), br(),
                                           htmlOutput("exp_test_resultat")
                                         )
                                ),
                                tabPanel("Tableau",
                                         br(),
                                         tableOutput("exp_table")
                                ),
                                tabPanel("Données (aperçu)",
                                         br(),
                                         tableOutput("exp_preview")
                                )
                    )
          )
        )
      )
    )
    ,
    
    
    #############################################################################
    ##                        PAGE EXPORTER LES DONNEES                        ##
    #############################################################################
    tabPanel(
      "Exporter les données",
      fluidPage(
        titlePanel("Exporter les données"),
        sidebarLayout(
          # ---- GAUCHE : FILTRES ----
          sidebarPanel(width = 4,
                       h4("Filtres"),
                       sliderInput("age_range", "Âge", min = 18, max = 70, value = c(18, 70)),
                       selectInput("sit_mat", "Situation matrimoniale",
                                   choices = c("Toutes", "Célibataire", "Marié(e)"),
                                   selected = "Toutes"),
                       numericInput("nb_enfants_min", "Nombre d'enfants (min)", value = 0, min = 0, step = 1),
                       sliderInput("annee_range", "Année de naissance", min = 1950, max = 2010, value = c(1980, 2000)),
                       radioButtons("sexe", "Sexe", choices = c("Tous","H","F"), selected = "Tous", inline = TRUE),
                       radioButtons("heures", "Heures hebdo", choices = c("Toutes" = "all", "24" = "24", "35" = "35"),
                                    selected = "all", inline = TRUE),
                       textInput("filtre_contrat", "Contrat (ex: CDI)", ""),
                       sliderInput("filtre_salaire_min", "Salaire minimum", min = 0, max = 10000, value = 0),
                       checkboxInput("preview10", "Aperçu sur 10 lignes", TRUE)
          ),
          
          # ---- DROITE : MENU TABLEAU / EXPORTER ----
          mainPanel(width = 8,
                    tabsetPanel(
                      tabPanel("Tableau",
                               h4("Données filtrées"),
                               tableOutput("table_filtre_preview")),
                      tabPanel(
                        "Exporter",
                        h4("Téléchargements"),
                        
                        # --- Fichier Salaires ---
                        wellPanel(
                          strong("RH_Salaries.csv"),
                          p("Table des salariés (une ligne par salarié).",
                            "Colonnes principales : id_salarié, Sexe, Année_naissance, Etat Civil, Enfants, Salaire, Age, Durée hebdo (si présente)."),
                          downloadButton("dl_salaries_csv", "Télécharger RH_Salaries.csv")
                        ),
                        
                        # --- Fichier Contrats ---
                        wellPanel(
                          strong("RH_Contrats.csv"),
                          p("Table des contrats associés aux salariés.",
                            "Colonnes principales : id_salarié, Contrat, autres informations disponibles selon le fichier."),
                          downloadButton("dl_contrats_csv", "Télécharger RH_Contrats.csv")
                        ),
                        
                        # --- Fichier filtré ---
                        wellPanel(
                          strong("Donnees_filtrees.csv"),
                          p("Table résultant de vos choix de filtres (âge, sexe, situation, salaire minimum, etc.).",
                            "Utile pour partager exactement la sélection affichée dans l’onglet « Tableau »."),
                          downloadButton("dl_filtre_csv", "Télécharger Donnees_filtrees.csv")
                        )
                      )
                      
                    )
          )
        )
      )
    )
    ,
    tabPanel("À propos", fluidPage(h3("À venir")))
  )
)