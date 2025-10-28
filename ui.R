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
  # Onglet Explorer
  
  tabPanel(
    "Explorer",
    fluidPage(
      tags$head(tags$style(HTML("
  :root{ --ink:#0b1222; }                 /* encore plus foncé */
  .card h4{ 
    color:var(--ink) !important; 
    font-weight:900; 
    letter-spacing:-.2px; 
    margin:0 0 8px;
  }
"))),
      
      tags$head(tags$style(HTML("
  .page-title-wrap{ margin:0 0 12px; }
  .page-title{ margin:0; color:var(--ink); font-weight:800; }

  .card.kpi{
    text-align:center; display:flex; flex-direction:column;
    align-items:center; justify-content:center;
    min-height:100px; padding:12px 20px;    /* plus compacte */
  }
  .card.kpi h3{
    font-size:20px; font-weight:800; margin:0 0 4px; color:var(--ink);
  }
  .card.kpi .val{
    color:var(--brand-dark); font-weight:900; line-height:1;
    font-size:clamp(40px, 5.5vw, 64px);       /* grand mais pas énorme */
    letter-spacing:-0.5px;
  }
  /* Option : encore moins d'espace entre les cartes
  .card{ margin-bottom:14px; }
  
  */
"))),
      
      
      
      div(class = "page-title-wrap", h2("Explorer", class = "page-title")),
      fluidRow(
        # ------------------ FILTRES (gauche) ------------------
        column(3,
               div(class="card",
                   h4("Filtres"),
                   selectInput("exp_contrat", "Type de contrat", choices = "(Tous)"),
                   selectInput("exp_sexe", "Sexe", choices = c("(Tous)", "H", "F")),
                   selectInput("exp_etat", "État civil", choices = "(Tous)"),
                   sliderInput("exp_age", "Âge", min = 18, max = 70, value = c(18, 70)),
                   actionLink("exp_reset", "Réinitialiser")
               )
        ),
        # ------------------ CONTENU (droite) ------------------
        column(9,
               # KPI total salariés
               div(class="card kpi",
                   h3("Total salariés"),
                   div(class="val", textOutput("exp_n"))
               ),
               # Ligne 1 : Genre / Âges
               fluidRow(
                 column(6,
                        div(class="card",
                            h4("Genre"),
                            plotlyOutput("exp_pie_gender", height = 260)
                        )
                 ),
                 column(6,
                        div(class="card",
                            h4("Répartition des âges"),
                            plotlyOutput("exp_bar_age", height = 260)
                        )
                 )
               ),
               # Ligne 2 : Salaire moyen par ...
               div(class="card",
                   div(style="display:flex;justify-content:space-between;align-items:center;gap:10px",
                       h4("Salaire moyen par"),
                       selectInput("exp_by", NULL,
                                   choices = c("Type de contrat","État civil","Sexe"),
                                   width = "220px")
                   ),
                   plotlyOutput("exp_bar_salary", height = 420)
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
                                 choices = c("Toutes", "Célibataire", "Marié"),
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
  tabPanel(
    "À propos",
    fluidPage(
      tags$head(
        tags$style(HTML("
        .about-wrap { max-width: 980px; margin: 0 auto; padding: 32px 8px 60px; }
        .about-hero { padding: 8px 0 12px; }
        .about-hero h1 { font-size: 2.2rem; font-weight: 800; margin: 0 0 8px; }
        .about-hero p  { color:#4b5563; font-size:1.06rem; margin:0; }

        .card { background:#fff; border-radius:16px; box-shadow:0 6px 18px rgba(2,6,23,.08); padding:20px; margin-top:16px; }
        .card h2 { font-size:1.2rem; font-weight:800; margin:0 0 10px; }
        .muted { color:#6b7280; }

        .list { margin:0; padding-left:18px; line-height:1.6; }
        .pill { display:inline-block; padding:6px 10px; border-radius:999px; background:#eef2ff; color:#1f2937; font-weight:600; font-size:.85rem; }

        .link { color:#0A66C2 !important; font-weight:600; text-decoration:none; }
        .link:hover { text-decoration:underline; }

        .authors { margin-top:28px; }
        .grid { display:grid; grid-template-columns: repeat(auto-fit,minmax(260px,1fr)); gap:16px; }
        .author-card { background:#fff; border-radius:16px; box-shadow:0 6px 18px rgba(2,6,23,.08); padding:18px; display:flex; align-items:center; gap:14px; }
        .avatar { width:64px; height:64px; border-radius:999px; border:2px solid #eef2ff; object-fit:cover; }
        .author-name { font-size:1.05rem; font-weight:700; margin:0; }
        .author-role { font-size:.95rem; color:#6b7280; margin:2px 0 8px; }
        .btn-ln  { display:inline-flex; align-items:center; gap:8px; padding:8px 12px; border-radius:10px;
                   background:#0A66C2; color:#fff !important; text-decoration:none; font-weight:600; }
        .btn-ln:hover { filter:brightness(1.05); box-shadow:0 8px 20px rgba(10,102,194,.25); }
        .btn-ln svg { width:16px; height:16px; }
      "))
      ),
      
      div(class="about-wrap",
          
          # En-tête
          div(class="about-hero",
              h1("À propos"),
          ),
          
          # Ce que propose cette page
          div(class="card",
              h2("Ce que propose cette page"),
              tags$ul(class="list",
                      tags$li(HTML("<strong>Contexte & objectifs :</strong> Data Visualisation sur les données RH_salaries et RH_contrats.")),
                      tags$li(HTML("<strong>Fonctionnalités clés :</strong> indicateurs d’accueil, histogrammes interactifs, filtres avancés et exports CSV.")),
                      tags$li(HTML("<strong>Portée & limites :</strong> données fictives à visée pédagogique ; les résultats ne constituent pas une politique RH.")),
              )
          ),
          
          # Données
          div(class="card",
              h2("Méthodologie & données"),
              tags$p(class="muted", "Traitements réalisés avec R et application sous Shiny. Visualisations interactives via plotly et thème bslib."),
              tags$ul(class="list",
                      tags$li(HTML("<span class='pill'>Sources</span> : fichiers <em>RH_Salaries.xlsx</em> et <em>RH_Contrats.xlsx</em>.")),
                      tags$li(HTML("<span class='pill'>Préparation</span> : renommage des variables, exclusion des salariés sans contrat, contrôles de cohérence.")),
              )
          ),

          
          # Auteurs 
          div(class="authors",
              h2("Auteurs"),
              div(class="grid",
                  
                  # Auteur 1
                  div(class="author-card",
                      tags$img(class="avatar",
                               src="avatar_abdoulaye.png",
                               onerror="this.onerror=null;this.src='https://ui-avatars.com/api/?name=TOURE+Abdoulaye+Wade&background=2563eb&color=fff&size=128';",
                               alt="Avatar TOURE Abdoulaye Wade"),
                      div(
                        tags$p(class="author-name","TOURE Abdoulaye Wade"),
                        tags$p(class="author-role","Étudiant en Master Sciences des données"),
                        tags$a(class="btn-ln",
                               href="https://www.linkedin.com/in/abdoulaye-wade-tour%C3%A9-5b1800229/",  
                               target="_blank",
                               HTML('<svg viewBox="0 0 24 24" fill="currentColor"><path d="M4.98 3.5C4.98 4.88 3.86 6 2.5 6S0 4.88 0 3.5 1.12 1 2.5 1s2.48 1.12 2.48 2.5zM.5 8h4V23h-4V8zm7 0h3.84v2.05h.05c.53-1 1.83-2.05 3.77-2.05 4.03 0 4.77 2.65 4.77 6.09V23h-4v-6.64c0-1.58-.03-3.61-2.2-3.61-2.2 0-2.54 1.72-2.54 3.5V23h-4V8z"/></svg>'),
                               "LinkedIn")
                      )
                  ),
                  
                  # Auteur 2
                  div(class="author-card",
                      tags$img(class="avatar",
                               src="avatar_daouda.png",
                               onerror="this.onerror=null;this.src='https://ui-avatars.com/api/?name=DIOP+Daouda&background=059669&color=fff&size=128';",
                               alt="Avatar DIOP Daouda"),
                      div(
                        tags$p(class="author-name","DIOP Daouda"),
                        tags$p(class="author-role","Master en modélisation statistique"),
                        tags$a(class="btn-ln",
                               href="https://www.linkedin.com/",   
                               target="_blank",
                               HTML('<svg viewBox="0 0 24 24" fill="currentColor"><path d="M4.98 3.5C4.98 4.88 3.86 6 2.5 6S0 4.88 0 3.5 1.12 1 2.5 1s2.48 1.12 2.48 2.5zM.5 8h4V23h-4V8zm7 0h3.84v2.05h.05c.53-1 1.83-2.05 3.77-2.05 4.03 0 4.77 2.65 4.77 6.09V23h-4v-6.64c0-1.58-.03-3.61-2.2-3.61-2.2 0-2.54 1.72-2.54 3.5V23h-4V8z"/></svg>'),
                               "LinkedIn")
                      )
                  )
              )
          )
      )
    )
  )
  
)
