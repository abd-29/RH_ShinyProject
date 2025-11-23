############################################################
################### BACK-END ##############################
#########################################################

# Helpers 

has_col <- function(df, col) {
  col %in% names(df)
}

# formater des vecteurs à afficher dans des tableaux
vecteur_format <- function(x, na_label = "Non renseigné") {
  if (is.numeric(x)) {
    ifelse(
      is.na(x),
      na_label,
      format(x, big.mark = " ", trim = TRUE, scientific = FALSE)
    )
  } else {
    ifelse(
      is.na(x) | x == "",
      na_label,
      as.character(x)
    )
  }
}

# récupérer une valeur
val_recup <- function(df, col, na_label = "Non disponible") {
  if (!has_col(df, col)) return(na_label)
  x <- df[[col]][1]
  
  if (is.na(x) || (is.character(x) && trimws(x) == "")) {
    return(na_label)
  }
  
  if (is.numeric(x)) {
    return(format(x, big.mark = " ", trim = TRUE, scientific = FALSE))
  }
  
  as.character(x)
}

# La logique server

server <- function(input, output, session) {
  
  ############################################################################
  ##                       PARTIE ACCUEIL DU SITE                           ##
  ############################################################################
  # ============== CALCUL INDICATEUR PAGE ACCUEIL ==============
  
  # Nombre de salarié
  output$indicateur_salaries <- renderText({
    n_distinct(data$id_salarie)
  })
  
  # calcul du pourcentage de contrat en CDI
  output$indicateur_cdi <- renderText({
    part <- mean(data$contrat == "CDI", na.rm = TRUE)
    paste0(round(100 * part), " %")
  })
  
  # Nombre de contrats
  output$indicateur_contrats <- renderText({
    nrow(data)
  })
  
  # Salaire moyen simple 
  output$indicateur_salaire_moyen <- renderText({
    moy <- mean(data$salaire, na.rm = TRUE)
    paste0(round(moy), " €")
  })
  
  # salaire moyen ou médian selon le switch
  output$label_salaire <- renderText({
    if (isTRUE(input$show_mean)) "Salaire moyen" else "Salaire médian"
  })
  
  # Valeur dynamique 
  output$indicateur_salaire <- renderText({
    val <- if (isTRUE(input$show_mean)) {
      mean(data$salaire, na.rm = TRUE)
    } else {
      median(data$salaire, na.rm = TRUE)
    }
    
    paste(
      format(round(val), big.mark = " ", scientific = FALSE),
      "€"
    )
  })
  
  # ------------------------------------------------------
  # histogramme des salaires
  # ------------------------------------------------------
  output$plot_histo_salaire <- renderPlotly({
    req(has_col(data, "salaire"))
    
    plot_ly(
      data = data,
      x    = ~salaire,
      type = "histogram",
      nbinsx = 30,
      marker = list(
        color = "blue",
        line  = list(color = "transparent")
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
  #  barres par type de contrat
  # ------------------------------------------------------
  output$plot_pie_contrat <- renderPlotly({
    req(has_col(data, "contrat"))
    
    tab <- data |>
      dplyr::mutate(
        contrat = dplyr::if_else(is.na(contrat) | contrat == "", "Non renseigné", as.character(contrat))
      ) |>
      dplyr::count(contrat, name = "Valeur") |>
      dplyr::arrange(dplyr::desc(Valeur))
    
    plot_ly(
      data   = tab,
      labels = ~contrat,
      values = ~Valeur,
      type   = "pie",
      hole   = 0.6,
      textinfo = "label+percent",
      hovertemplate = "%{label} : %{value}<extra></extra>"
    ) |>
      layout(showlegend = TRUE)
  })
  
  # le bouton commencer pointe vers la page Explorer
  observeEvent(input$btn_commencer, {
    updateNavbarPage(session, "main_nav", selected = "Explorer")
  })
  
  ############################################################################
  ##                       PARTIE EXPORTER DU SITE                          ##
  ############################################################################
  
  observe({
    if (has_col(data, "salaire")) {
      mx <- suppressWarnings(max(data$salaire, na.rm = TRUE))
      if (is.finite(mx)) {
        updateSliderInput(session, "filtre_salaire_min", max = ceiling(mx))
      }
    }
    
    if (has_col(data, "age")) {
      a <- range(data$age, na.rm = TRUE)
      if (all(is.finite(a))) {
        updateSliderInput(
          session, "age_range",
          min   = floor(a[1]),
          max   = ceiling(a[2]),
          value = c(floor(a[1]), ceiling(a[2]))
        )
      }
    }
    
    if (has_col(data, "annee_naissance")) {
      y <- range(data$annee_naissance, na.rm = TRUE)
      if (all(is.finite(y))) {
        updateSliderInput(
          session, "annee_range",
          min   = y[1],
          max   = y[2],
          value = y
        )
      }
    }
  })
  
  # Application des filtres
  donnees_filtrees <- reactive({
    df <- data
    
    if (has_col(df, "salaire")) {
      df <- df[df$salaire >= input$filtre_salaire_min | is.na(df$salaire), , drop = FALSE]
    }
    
    if (nzchar(trimws(input$filtre_contrat)) && has_col(df, "contrat")) {
      pat <- trimws(input$filtre_contrat)
      df <- df[!is.na(df$contrat) & grepl(pat, df$contrat, ignore.case = TRUE), , drop = FALSE]
    }
    
    if (has_col(df, "age")) {
      df <- df[df$age >= input$age_range[1] & df$age <= input$age_range[2], , drop = FALSE]
    }
    
    if (input$sit_mat != "Toutes" && has_col(df, "etat_civil")) {
      df <- df[!is.na(df$etat_civil) & df$etat_civil == input$sit_mat, , drop = FALSE]
    }
    
    if (has_col(df, "annee_naissance")) {
      df <- df[df$annee_naissance >= input$annee_range[1] &
                 df$annee_naissance <= input$annee_range[2], , drop = FALSE]
    }
    
    if (has_col(df, "enfants")) {
      df <- df[df$enfants >= input$nb_enfants_min, , drop = FALSE]
    }
    
    if (input$sexe != "Tous" && has_col(df, "sexe")) {
      df <- df[df$sexe == input$sexe, , drop = FALSE]
    }
    
    if (input$heures != "all") {
      colh <- intersect(c("hebdo_duree", "duree_hebdo"), names(df))
      if (length(colh) == 1) {
        df <- df[df[[colh]] == as.numeric(input$heures), , drop = FALSE]
      }
    }
    
    df
  })
  
  # Aperçu des 10 lignes si la case est cochée
  output$table_filtre_preview <- renderTable({
    df <- if (isTRUE(input$preview10)) head(donnees_filtrees(), 10) else donnees_filtrees()
    
    cols <- intersect(c("enfants", "etat_civil", "salaire"), names(df))
    for (cl in cols) {
      df[[cl]] <- vecteur_format(df[[cl]], na_label = "Non renseigné")
    }
    
    df
  }, striped = TRUE, bordered = TRUE)
  
  # Téléchargements CSV
  output$dl_salaries_csv <- downloadHandler(
    filename = function() "RH_Salaries.csv",
    content  = function(file) write.csv2(Salaires, file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$dl_contrats_csv <- downloadHandler(
    filename = function() "RH_Contrats.csv",
    content  = function(file) write.csv2(Contrats, file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$dl_filtre_csv <- downloadHandler(
    filename = function() "Donnees_filtrees.csv",
    content  = function(file) write.csv2(donnees_filtrees(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  # =========================== ONGLET EXPLORER ============================
  
  # database
  exp_base <- reactive({
    req(exists("data"))
    df <- data |>
      dplyr::mutate(
        contrat    = ifelse(is.na(contrat) | contrat == "", "Inconnu", as.character(contrat)),
        etat_civil = ifelse(is.na(etat_civil) | etat_civil == "", "Inconnu", as.character(etat_civil)),
        sexe       = ifelse(is.na(sexe) | sexe == "", "Inconnu", as.character(sexe))
      )
    df
  })
  
  # Choix dynamiques
  observe({
    df <- exp_base()
    updateSelectInput(session, "exp_contrat",
                      choices = c("(Tous)", sort(unique(df$contrat))), selected = "(Tous)")
    updateSelectInput(session, "exp_etat",
                      choices = c("(Tous)", sort(unique(df$etat_civil))), selected = "(Tous)")
    if (has_col(df, "age")) {
      rg <- range(df$age, na.rm = TRUE)
      if (all(is.finite(rg))) {
        updateSliderInput(session, "exp_age",
                          min = floor(rg[1]), max = ceiling(rg[2]),
                          value = c(floor(rg[1]), ceiling(rg[2])))
      }
    }
  })
  
  # Réinitialiser
  observeEvent(input$exp_reset, {
    updateSelectInput(session, "exp_contrat", selected = "(Tous)")
    updateSelectInput(session, "exp_sexe",    selected = "(Tous)")
    updateSelectInput(session, "exp_etat",    selected = "(Tous)")
  })
  
  # Données filtrées
  exp_df <- reactive({
    df <- exp_base()
    if (has_col(df, "age")) {
      df <- df |>
        dplyr::filter(age >= input$exp_age[1], age <= input$exp_age[2])
    }
    if (input$exp_contrat != "(Tous)") df <- df |> dplyr::filter(contrat == input$exp_contrat)
    if (input$exp_sexe    != "(Tous)") df <- df |> dplyr::filter(sexe    == input$exp_sexe)
    if (input$exp_etat    != "(Tous)") df <- df |> dplyr::filter(etat_civil == input$exp_etat)
    df
  })
  
  output$exp_n <- renderText({
    format(n_distinct(exp_df()$id_salarie), big.mark = " ", scientific = FALSE)
  })
  
  # Genre
  output$exp_pie_gender <- renderPlotly({
    df <- exp_df()
    validate(need(nrow(df) > 0, "Aucune donnée"))
    tab <- df |>
      dplyr::mutate(genre = dplyr::recode(sexe, H = "Hommes", F = "Femmes", .default = "Inconnu")) |>
      dplyr::count(genre, name = "n") |>
      dplyr::arrange(dplyr::desc(n))
    plot_ly(tab, labels = ~genre, values = ~n, type = "pie", hole = 0.6,
            textinfo = "label+percent", hovertemplate = "%{label} : %{value}<extra></extra>") |>
      layout(showlegend = TRUE)
  })
  
  # répartition par classes d'âge en %
  output$exp_bar_age <- renderPlotly({
    df <- exp_df()
    validate(need(nrow(df) > 0, "Aucune donnée"))
    brk <- c(-Inf, 30, 40, 50, 60, Inf)
    lab <- c("< 30", "30–39", "40–49", "50–59", "60+")
    tab <- df |>
      dplyr::mutate(cl_age = cut(age, breaks = brk, labels = lab, right = FALSE)) |>
      dplyr::count(cl_age, name = "n") |>
      dplyr::mutate(pct = 100 * n / sum(n)) |>
      tidyr::drop_na(cl_age)
    plot_ly(tab, x = ~pct, y = ~cl_age, type = "bar", orientation = "h",
            hovertemplate = "%{y} : %{x:.1f} %<extra></extra>") |>
      layout(xaxis = list(title = "%"), yaxis = list(title = ""))
  })
  
  # Salaire moyen par dimension choisie
  output$exp_bar_salary <- renderPlotly({
    df <- exp_df()
    validate(need(nrow(df) > 0, "Aucune donnée"))
    by <- switch(input$exp_by,
                 "Type de contrat" = "contrat",
                 "État civil"      = "etat_civil",
                 "Sexe"            = "sexe")
    validate(need(by %in% names(df), "Dimension invalide"))
    tab <- df |>
      dplyr::filter(!is.na(salaire)) |>
      dplyr::group_by(.data[[by]]) |>
      dplyr::summarise(Salaire_moyen = mean(salaire, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(Salaire_moyen)) |>
      dplyr::rename(Groupe = 1)
    plot_ly(tab, x = ~Salaire_moyen, y = ~Groupe, type = "bar", orientation = "h",
            hovertemplate = "%{y} : %{x:.0f} €<extra></extra>") |>
      layout(xaxis = list(title = "€"), yaxis = list(title = ""))
  })
  
  # ====================== TESTS STATISTIQUES ======================
  
  exp_test_var <- reactive({
    switch(input$exp_test_by,
           "Sexe"            = "sexe",
           "Type de contrat" = "contrat",
           "État civil"      = "etat_civil",
           "Âge"             = "age",
           NULL)
  })
  
  # Lancer le calcul 
  exp_test_res <- eventReactive(input$exp_run_test, {
    df <- exp_df()
    v  <- exp_test_var()
    validate(need(!is.null(v), "Choisisir un critère."),
             need(nrow(df) > 4, "Pas assez de données"))
    
    # Corrélation entre âge et salaire
    if (v == "age") {
      df2 <- df |>
        dplyr::select(age, salaire) |>
        tidyr::drop_na()
      validate(need(nrow(df2) > 4, "Trop peu d’observations pour corrélation."))
      ct <- suppressWarnings(cor.test(df2$age, df2$salaire, method = "spearman"))
      list(
        kind  = "spearman",
        p     = unname(ct$p.value),
        est   = unname(ct$estimate),
        var   = v,
        table = NULL,
        data  = df2
      )
    } else {
      # Variable catégorielle -> t-test (2 groupes) ou Kruskal
      df2 <- df |>
        dplyr::select(salaire, !!v) |>
        tidyr::drop_na()
      df2[[v]] <- droplevels(as.factor(df2[[v]]))
      k <- nlevels(df2[[v]])
      validate(need(k >= 2, "Un seul groupe présent → test impossible."))
      
      if (k == 2) {
        # Welch t-test
        tmp <- setNames(df2, c("salaire", "groupe"))
        tt  <- try(suppressWarnings(t.test(salaire ~ groupe, data = tmp, var.equal = FALSE)), silent = TRUE)
        validate(need(!inherits(tt, "try-error"), "Test impossible (effectifs/variances)."))
        pval <- unname(tt$p.value)
        
        tab <- tmp |>
          dplyr::group_by(groupe) |>
          dplyr::summarise(
            n      = dplyr::n(),
            mean   = mean(salaire),
            median = median(salaire),
            sd     = sd(salaire),
            .groups = "drop"
          )
        list(kind = "welch", p = pval, var = v, table = tab, data = tmp)
      } else {
        # Kruskal–Wallis
        tmp <- setNames(df2, c("salaire", "groupe"))
        kw  <- try(suppressWarnings(kruskal.test(salaire ~ groupe, data = tmp)), silent = TRUE)
        validate(need(!inherits(kw, "try-error"), "Test impossible (effectifs)."))
        pval <- unname(kw$p.value)
        tab <- tmp |>
          dplyr::group_by(groupe) |>
          dplyr::summarise(
            n      = dplyr::n(),
            mean   = mean(salaire),
            median = median(salaire),
            sd     = sd(salaire),
            .groups = "drop"
          ) |>
          dplyr::arrange(dplyr::desc(median))
        list(kind = "kruskal", p = pval, var = v, table = tab, data = tmp)
      }
    }
  }, ignoreInit = TRUE)
  
  # synthèse
  output$exp_test_summary <- renderText({
    req(exp_test_res())
    r <- exp_test_res()
    alpha <- 0.05
    sig <- if (is.finite(r$p) && r$p < alpha) "différence significative" else "pas de différence significative"
    
    if (r$kind == "spearman") {
      sprintf("Corrélation Spearman âge–salaire : rho = %.2f, p = %.4f → %s (α = 0,05).",
              r$est, r$p, sig)
    } else if (r$kind == "welch") {
      sprintf("Welch t-test (%s) : p = %.4f → %s des salaires entre 2 groupes (α = 0,05).",
              r$var, r$p, sig)
    } else {
      sprintf("Kruskal–Wallis (%s) : p = %.4f → %s des salaires entre groupes (α = 0,05).",
              r$var, r$p, sig)
    }
  })
  
  # Graphique par groupe
  output$exp_test_box <- renderPlotly({
    req(exp_test_res())
    r <- exp_test_res()
    
    if (r$kind == "spearman") {
      plot_ly(r$data, x = ~age, y = ~salaire, type = "scatter", mode = "markers",
              hovertemplate = "Âge %{x}<br>Salaire %{y:.0f} €<extra></extra>") |>
        layout(xaxis = list(title = "Âge"), yaxis = list(title = "Salaire (€)"))
    } else {
      plot_ly(type = "box") |>
        add_trace(data = r$data, x = ~groupe, y = ~salaire, boxpoints = "outliers",
                  hovertemplate = "%{x}<br>Salaire %{y:.0f} €<extra></extra>") |>
        layout(xaxis = list(title = ""), yaxis = list(title = "Salaire (€)"))
    }
  })
  
  output$exp_test_table <- renderTable({
    req(exp_test_res())
    r <- exp_test_res()
    if (!is.null(r$table)) {
      
      d <- r$table
      d$mean   <- round(d$mean, 0)
      d$median <- round(d$median, 0)
      d$sd     <- round(d$sd, 0)
      names(d) <- c("Groupe", "n", "Moyenne €", "Médiane €", "SD €")
      d
    }
  }, striped = TRUE, bordered = TRUE)
  
  
  ####################################################################
  ##                      ONGLET FICHE SALARIÉ                      ##
  ####################################################################
  
  # ID sélectionné
  fiche_id <- reactive({
    txt <- trimws(input$fiche_id_text)
    if (nzchar(txt)) {
      return(txt)
    }
    input$fiche_id_select
  })
  
  # 1 salarié ou NULL si non trouvé
  fiche_data <- reactive({
    req(fiche_id())
    df <- data %>% dplyr::filter(id_salarie == fiche_id())
    if (nrow(df) == 0) return(NULL)
    df[1, , drop = FALSE]
  })
  
  output$fiche_salarie_ui <- renderUI({
    df <- fiche_data()
    
    if (is.null(df)) {
      return(
        div(
          class = "card",
          h4("Aucun salarié trouvé"),
          p("Aucun salarié ne correspond à l'identifiant saisi.")
        )
      )
    }
    
    # Sexe brut & label lisible
    sexe_brut <- if (has_col(df, "sexe")) as.character(df$sexe[1]) else NA
    sexe_pretty <- dplyr::case_when(
      is.na(sexe_brut)        ~ "Non disponible",
      sexe_brut == "H"        ~ "Homme",
      sexe_brut == "F"        ~ "Femme",
      TRUE                    ~ sexe_brut
    )
    
    # Avatar selon sexe
    avatar_file <- if (!is.na(sexe_brut) && sexe_brut == "F") {
      "femme.jpg"
    } else if (!is.na(sexe_brut) && sexe_brut == "H") {
      "homme.jpg"
    } else {
      "avatar_neutre.png"  # optionnel si tu en crées un
    }
    
    # fonction pour fabriquer une carte
    fiche_card <- function(label, value, muted = FALSE) {
      div(
        class = "fiche-card",
        div(class = "fiche-label", label),
        div(
          class = paste("fiche-value", if (muted) "muted"),
          value
        )
      )
    }
    
    # Valeurs formatées pour certaines cartes
    age_val <- val_recup(df, "age")
    if (age_val != "Non disponible") age_val <- paste0(age_val, " ans")
    
    annee_val   <- val_recup(df, "annee_naissance")
    enfants_val <- val_recup(df, "enfants")
    
    duree_val <- val_recup(df, "duree_hebdo")
    if (duree_val != "Non disponible") duree_val <- paste0(duree_val, " h")
    
    salaire_val <- val_recup(df, "salaire")
    if (salaire_val != "Non disponible") salaire_val <- paste0(salaire_val, " €")
    
    tagList(
      
      # En-tête fiche
      div(
        class = "fiche-header",
        div(
          class = "fiche-avatar",
          tags$img(
            src   = avatar_file,
            alt   = "Avatar salarié"
          )
        ),
        div(
          class = "fiche-header-text",
          h3(paste("Salarié", val_recup(df, "id_salarie"))),
          p(
            paste(
              "Contrat :", val_recup(df, "contrat"),
              "| Sexe :", sexe_pretty
            )
          )
        )
      ),
      
      # Grille de cartes
      div(
        class = "fiche-grid",
        fiche_card("Sexe", sexe_pretty, muted = (sexe_pretty == "Non disponible")),
        fiche_card("Âge", age_val, muted = (age_val == "Non disponible")),
        fiche_card("Année de naissance", annee_val, muted = (annee_val == "Non disponible")),
        fiche_card("État civil", val_recup(df, "etat_civil"),
                   muted = (val_recup(df, "etat_civil") == "Non disponible")),
        fiche_card("Nombre d'enfants", enfants_val, muted = (enfants_val == "Non disponible")),
        fiche_card("Type de contrat", val_recup(df, "contrat"),
                   muted = (val_recup(df, "contrat") == "Non disponible")),
        fiche_card("Durée hebdomadaire", duree_val, muted = (duree_val == "Non disponible")),
        fiche_card("Salaire", salaire_val, muted = (salaire_val == "Non disponible"))
      )
    )
  })
  
}
