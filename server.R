############################################################
################### BACK-END ##############################
###########################################################

# Define server logic 
server <- function(input, output, session) 
  {
  
  
  ############################################################################
  ##                       PARTIE ACCUEIL DU SITE                           ##
  ############################################################################
  # ============== CALCUL INDICATEUR PAGE ACCUEIL ==============
  
    # Nombre de salarie
    output$indicateur_salaries <- renderText({
      n_distinct(salaires$id_salarié)
    })
    
    # calcul du pourcentage de contrat en cdi
    output$indicateur_cdi <- renderText({
      
      if ("Contrat" %in% names(contrats)) {
        part <- mean(contrats$Contrat == "CDI", na.rm = TRUE) # calcul de la moyenne
        paste0(round(100 * part), " %") # calcul du pourcentage et on rajoute le signe de pourcent
      } else {
        "Non disponible"
      }
    })
    
    
    # Nombre de contrat
    output$indicateur_contrats <- renderText({
      nrow(contrats)
    })
    
    output$indicateur_salaire_moyen <- renderText({
      if ("Salaire" %in% names(salaires)) {
        moy <- mean(salaires$Salaire, na.rm = TRUE)
        paste0(round(moy), " €")
      } else {
        "Non disponible"
      }
    })
    
    # salaire moyen et salaire median
    output$label_salaire <- renderText({
      if (isTRUE(input$show_mean)) "Salaire moyen" else "Salaire médian"
    })
    
    # Valeur dynamique
    output$indicateur_salaire <- renderText({
      if (!"Salaire" %in% names(salaires)) return("Non disponible")
      val <- if (isTRUE(input$show_mean))
        mean(salaires$Salaire, na.rm = TRUE) else
          median(salaires$Salaire, na.rm = TRUE)
      paste(format(round(val), big.mark = " ", scientific = FALSE), "€")
    })
    
    # ------------------------------------------------------
    # Graphique 1 : histogramme des salaires
    # ------------------------------------------------------
    output$plot_histo_salaire <- renderPlotly({
      req("Salaire" %in% names(salaires))
      
      plot_ly(
        data = salaires,
        x    = ~Salaire,
        type = "histogram",
        nbinsx = 30,
        marker = list(
          color = "blue",        # barres bleu ciel
          line  = list(color = "transparent") # pas de bordure
        )
      ) |>
        layout(
          title = "",
          xaxis = list(title = "Salaire"),
          yaxis = list(title = "Effectif"),
          bargap = 0.05
        )
    })
    
    # ------------------------------------------------------
    # Graphique 2 : barres par type de contrat
    # ------------------------------------------------------
    output$plot_pie_contrat <- renderPlotly({
      req("Contrat" %in% names(contrats))
      
      tab <- data.frame(
        Etat = c("CDI", "CDD"),
        Valeur = c(sum(contrats$Contrat == "CDI", na.rm = TRUE),
                   sum(contrats$Contrat == "CDD", na.rm = TRUE))
      )
      
      plot_ly(
        data   = tab,
        labels = ~Etat,
        values = ~Valeur,
        type   = "pie",
        hole   = 0.6,
        marker = list(colors = c("blue", "lightgray")),
        textinfo = "label+percent",   # % affichés sur le graphe
        hovertemplate = "%{label} : %{value}<extra></extra>" # infobulle = uniquement nombre
      ) |>
        layout(showlegend = FALSE)
    })
    
    # le bouton commencer pointe vers la page "Explorer"
    observeEvent(input$btn_commencer, {
    updateNavbarPage(session, "main_nav", selected = "Explorer")
})
    
    ############################################################################
    ##                           ONGLET EXPLORER                              ##
    ############################################################################
    
    # --- Helpers robustes aux accents / espaces ---
    .norm_name <- function(x) {
      x <- iconv(x, to = "ASCII//TRANSLIT")
      x <- gsub("[^A-Za-z0-9]+", "_", x)
      tolower(x)
    }
    get_col <- function(df, candidates) {
      nm  <- names(df); nnm <- .norm_name(nm)
      cand_norm <- .norm_name(candidates)
      idx <- match(cand_norm, nnm)
      if (any(!is.na(idx))) nm[na.omit(idx)[1]] else NA_character_
    }
    
    # --- Base : jointure + nettoyage (exclut sans contrat si dispo) ---
    explore_base <- reactive({
      df <- if ("id_salarié" %in% names(salaires) && "id_salarié" %in% names(contrats)) {
        dplyr::left_join(salaires, contrats, by = "id_salarié")
      } else salaires
      
      col_salaire <- get_col(df, c("Salaire","salaire"))
      col_age     <- get_col(df, c("Age","Âge","age"))
      col_sexe    <- get_col(df, c("Sexe","sexe","Gender"))
      col_contrat <- get_col(df, c("Contrat","Type_contrat","type_contrat"))
      col_etat    <- get_col(df, c("Etat civil","État civil","Etat_civil",
                                   "Situation matrimoniale","Situation_matrimoniale"))
      
      # Exclure sans contrat si la colonne existe
      if (!is.na(col_contrat)) {
        df <- dplyr::filter(df, !is.na(.data[[col_contrat]]) & .data[[col_contrat]] != "")
      }
      
      # Tranches d'âge pré-calculées
      if (!is.na(col_age)) {
        df <- dplyr::mutate(
          df,
          TrancheAge = dplyr::case_when(
            .data[[col_age]] < 30 ~ "< 30",
            .data[[col_age]] >= 30 & .data[[col_age]] <= 45 ~ "30 – 45",
            .data[[col_age]] > 45 ~ "> 45",
            TRUE ~ NA_character_
          )
        )
      }
      
      # Rendre explicites les NA -> "Inconnu"
      if (!is.na(col_sexe))    df[[col_sexe]]    <- forcats::fct_explicit_na(as.character(df[[col_sexe]]),    na_level = "Inconnu")
      if (!is.na(col_contrat)) df[[col_contrat]] <- forcats::fct_explicit_na(as.character(df[[col_contrat]]), na_level = "Inconnu")
      if (!is.na(col_etat))    df[[col_etat]]    <- forcats::fct_explicit_na(as.character(df[[col_etat]]),    na_level = "Inconnu")
      
      # Attacher les noms détectés pour la suite
      attr(df, "col_salaire") <- col_salaire
      attr(df, "col_age")     <- col_age
      attr(df, "col_sexe")    <- col_sexe
      attr(df, "col_contrat") <- col_contrat
      attr(df, "col_etat")    <- col_etat
      df
    })
    
    # --- Listes de filtres dynamiques (Sexe & Etat civil) ---
    observe({
      df <- explore_base()
      col_sexe <- attr(df, "col_sexe")
      ch <- "Tous"
      if (!is.na(col_sexe) && col_sexe %in% names(df)) {
        ch <- c("Tous", sort(unique(as.character(df[[col_sexe]]))))
      }
      updateSelectInput(session, "exp_sexe", choices = ch, selected = "Tous")
    })
    observe({
      df <- explore_base()
      col_etat <- attr(df, "col_etat")
      ch <- "Tous"
      if (!is.na(col_etat) && col_etat %in% names(df)) {
        ch <- c("Tous", sort(unique(as.character(df[[col_etat]]))))
      }
      updateSelectInput(session, "exp_etat", choices = ch, selected = "Tous")
    })
    
    # --- Filtres utilisateur ---
    explore_filtered <- reactive({
      df <- explore_base()
      if (nrow(df) == 0) return(df)
      
      col_age     <- attr(df, "col_age")
      col_sexe    <- attr(df, "col_sexe")
      col_contrat <- attr(df, "col_contrat")
      col_etat    <- attr(df, "col_etat")
      
      if (!is.na(col_age)) {
        df <- dplyr::filter(df,
                            .data[[col_age]] >= input$exp_age_range[1],
                            .data[[col_age]] <= input$exp_age_range[2]
        )
      }
      if (!is.na(col_sexe) && input$exp_sexe != "Tous") {
        df <- dplyr::filter(df, .data[[col_sexe]] == input$exp_sexe)
      }
      if (!is.na(col_contrat) && nzchar(input$exp_contrat)) {
        df <- dplyr::filter(df, .data[[col_contrat]] == input$exp_contrat)
      }
      if (!is.na(col_etat) && input$exp_etat != "Tous") {
        df <- dplyr::filter(df, .data[[col_etat]] == input$exp_etat)
      }
      df
    })
    
    # --- Variable de comparaison (selon l’input) ---
    explore_group_var <- reactive({
      base <- explore_base()
      switch(input$exp_critere,
             "Sexe"            = attr(base, "col_sexe"),
             "Tranche d'âge"   = "TrancheAge",
             "État civil"      = attr(base, "col_etat"),
             "Type de contrat" = attr(base, "col_contrat")
      )
    })
    
    # --- Tableau récapitulatif ---
    explore_summary <- reactive({
      df <- explore_filtered()
      if (nrow(df) == 0) return(df[0, , drop = FALSE])
      
      col_salaire <- attr(df, "col_salaire")
      grp <- explore_group_var()
      if (is.na(grp) || !(grp %in% names(df))) return(df[0, , drop = FALSE])
      
      df |>
        dplyr::filter(!is.na(.data[[grp]]), !is.na(.data[[col_salaire]])) |>
        dplyr::group_by(.data[[grp]]) |>
        dplyr::summarise(
          Effectif        = dplyr::n(),
          Salaire_median  = stats::median(.data[[col_salaire]], na.rm = TRUE),
          Salaire_moyen   = base::mean(.data[[col_salaire]],  na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(Effectif)) |>
        dplyr::rename(Groupe = 1)
    })
    
    # --- Graphique interactif ---
    output$exp_plot <- plotly::renderPlotly({
      df  <- explore_filtered()
      grp <- explore_group_var()
      col_salaire <- attr(df, "col_salaire")
      
      validate(
        need(nrow(df) > 0, "Aucune donnée dans le périmètre choisi."),
        need(!is.na(grp) && grp %in% names(df), "Le critère sélectionné n’existe pas dans les données.")
      )
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[grp]], y = .data[[col_salaire]])) +
        ggplot2::geom_boxplot(outlier.shape = NA) +
        ggplot2::labs(x = grp, y = "Salaire")
      
      if (isTRUE(input$exp_show_points)) {
        p <- p + ggplot2::geom_jitter(width = 0.15, alpha = 0.5)
      }
      plotly::ggplotly(p, tooltip = c("x","y"))
    })
    
    # --- Test statistique (Wilcoxon si 2 groupes, sinon Kruskal) ---
    output$exp_test_label <- renderText({
      df  <- explore_filtered()
      grp <- explore_group_var()
      if (is.na(grp) || !(grp %in% names(df))) return("—")
      k <- df |> dplyr::filter(!is.na(.data[[grp]])) |> dplyr::pull(.data[[grp]]) |> unique() |> length()
      if (k <= 1) "—" else if (k == 2) "Wilcoxon (Mann–Whitney)" else "Kruskal–Wallis"
    })
    
    output$exp_test_resultat <- renderUI({
      df  <- explore_filtered()
      grp <- explore_group_var()
      col_salaire <- attr(df, "col_salaire")
      if (is.na(grp) || !(grp %in% names(df)))
        return(HTML("<em>Données insuffisantes.</em>"))
      
      dft <- df |> dplyr::filter(!is.na(.data[[grp]]), !is.na(.data[[col_salaire]]))
      if (nrow(dft) < 3) return(HTML("<em>Données insuffisantes pour un test.</em>"))
      
      k <- length(unique(dft[[grp]]))
      pval <- NA_real_
      if (k == 2) {
        pval <- suppressWarnings(stats::wilcox.test(dft[[col_salaire]] ~ dft[[grp]])$p.value)
      } else if (k > 2) {
        pval <- suppressWarnings(stats::kruskal.test(dft[[col_salaire]] ~ dft[[grp]])$p.value)
      } else {
        return(HTML("<em>Données insuffisantes pour un test.</em>"))
      }
      signif_txt <- if (!is.na(pval) && pval < 0.05)
        "<strong style='color:#007BFF'>Différence significative (p < 0.05)</strong>"
      else
        "<span style='color:#777'>Différence non significative (p ≥ 0.05)</span>"
      
      HTML(paste0("p-value = <code>", format(pval, digits = 3, scientific = TRUE),
                  "</code><br/>", signif_txt))
    })
    
    # --- Tableaux ---
    output$exp_table <- renderTable({
      tab <- explore_summary()
      if (nrow(tab) == 0) return(NULL)
      if (input$exp_metric == "mean") {
        dplyr::select(tab, Groupe, Effectif, Salaire_moyen, Salaire_median)
      } else {
        dplyr::select(tab, Groupe, Effectif, Salaire_median, Salaire_moyen)
      }
    }, striped = TRUE, bordered = TRUE)
    
    output$exp_preview <- renderTable({
      head(explore_filtered(), 10)
    }, striped = TRUE, bordered = TRUE)
    

    
    
    ############################################################################
    ##                       PARTIE EXPORTER DU SITE                          ##
    ############################################################################
    observe({
      if ("Salaire" %in% names(salaires)) {
        mx <- suppressWarnings(max(salaires$Salaire, na.rm = TRUE))
        if (is.finite(mx)) updateSliderInput(session, "filtre_salaire_min", max = ceiling(mx))
      }
      if ("Age" %in% names(salaires)) {
        a <- range(salaires$Age, na.rm = TRUE)
        if (all(is.finite(a))) updateSliderInput(session, "age_range", min = floor(a[1]), max = ceiling(a[2]),
                                                 value = c(floor(a[1]), ceiling(a[2])))
      }
      if ("Annee_naissance" %in% names(salaires)) {
        y <- range(salaires$Annee_naissance, na.rm = TRUE)
        if (all(is.finite(y))) updateSliderInput(session, "annee_range", min = y[1], max = y[2], value = y)
      }
    })
    
    # 1) Table jointe pour filtrage (clé supposée: id_salarié)
    donnees_jointes <- reactive({
      if ("id_salarié" %in% names(salaires) && "id_salarié" %in% names(contrats)) {
        dplyr::left_join(salaires, contrats, by = "id_salarié")
      } else {
        # si la clé diffère, on renvoie simplement salaires
        salaires
      }
    })
    
    # 2) Application des filtres (on n'applique que si la colonne existe)
    donnees_filtrees_all <- reactive({
      df <- donnees_jointes()
      
      if ("Salaire" %in% names(df)) df <- df[df$Salaire >= input$filtre_salaire_min | is.na(df$Salaire), , drop = FALSE]
      if (nzchar(input$filtre_contrat) && "Contrat" %in% names(df)) df <- df[df$Contrat == input$filtre_contrat, , drop = FALSE]
      if ("Age" %in% names(df)) df <- df[df$Age >= input$age_range[1] & df$Age <= input$age_range[2], , drop = FALSE]
      if ("Annee_naissance" %in% names(df)) df <- df[df$Annee_naissance >= input$annee_range[1] &
                                                       df$Annee_naissance <= input$annee_range[2], , drop = FALSE]
      if ("Nb_enfants" %in% names(df)) df <- df[df$Nb_enfants >= input$nb_enfants_min, , drop = FALSE]
      if (input$sexe != "Tous" && "Sexe" %in% names(df)) df <- df[df$Sexe == input$sexe, , drop = FALSE]
      if (input$heures != "all") {
        colh <- intersect(c("Heures_hebdo","HeuresHebdo","hebdo_heures"), names(df))
        if (length(colh) == 1) df <- df[df[[colh]] == as.numeric(input$heures), , drop = FALSE]
      }
      df
    })
    
    # 3) Aperçu (10 lignes si case cochée)
    output$table_filtre_preview <- renderTable({
      df <- if (isTRUE(input$preview10)) head(donnees_filtrees_all(), 10) else donnees_filtrees_all()
      
      # petite fonction d'affichage
      to_display <- function(x) {
        if (is.numeric(x)) {
          ifelse(is.na(x), "Non renseigné",
                 format(x, big.mark = " ", trim = TRUE, scientific = FALSE))
        } else {
          ifelse(is.na(x) | x == "", "Non renseigné", as.character(x))
        }
      }
      
      # colonnes à nettoyer si elles existent
      cols <- intersect(c("Enfants", "Etat Civil", "Salaire"), names(df))
      for (cl in cols) df[[cl]] <- to_display(df[[cl]])
      
      df
    }, striped = TRUE, bordered = TRUE)
    
    # 4) Téléchargements CSV (séparateur ;, compatible Excel FR)
    output$dl_salaries_csv <- downloadHandler(
      filename = function() "RH_Salaries.csv",
      content  = function(file) write.csv2(salaires, file, row.names = FALSE, fileEncoding = "UTF-8")
    )
    output$dl_contrats_csv <- downloadHandler(
      filename = function() "RH_Contrats.csv",
      content  = function(file) write.csv2(contrats, file, row.names = FALSE, fileEncoding = "UTF-8")
    )
    output$dl_filtre_csv <- downloadHandler(
      filename = function() "Donnees_filtrees.csv",
      content  = function(file) write.csv2(donnees_filtrees_all(), file, row.names = FALSE, fileEncoding = "UTF-8")
    )
    
}
