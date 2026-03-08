# ==============================================================================
# SERVIDOR
# ==============================================================================

server <- function(input, output, session) {
  # Credenciales: DB + invitado hardcoded
  db_check <- check_credentials(db = DB_PATH)
  guest_check <- function(user, password) {
    if (tolower(user) == "guest" && password == "guest") {
      list(result = TRUE, user_info = list(user = "guest", admin = FALSE))
    } else {
      db_check(user, password)
    }
  }
  res_auth <- secure_server(
    check_credentials = guest_check,
    keep_token = TRUE
  )

  is_guest <- reactive({ tolower(res_auth$user) == "guest" })
  lang <- reactiveVal(DEFAULT_LANG)
  t <- function(key) tr(key, lang())

  # Helper: badge coloreado con el color del equipo (via CSS dinámico)
  team_badge <- function(driver, extra_class = "") {
    if (is.na(driver) || driver == "") return(span(class = "driver-badge", "-"))
    driver_class <- paste0("tc-", driver)
    span(class = trimws(paste("driver-badge", driver_class, extra_class)), driver)
  }

  badge_row <- function(drivers) {
    tagList(lapply(drivers, function(d) team_badge(d)))
  }

  # Observers dinámicos para cada idioma configurado
  if (MULTILANG) {
    lang_codes <- names(LANG_CONFIG$languages)
    lapply(lang_codes, function(code) {
      observeEvent(input[[paste0("lang_", code)]], {
        lang(code)
        session$sendCustomMessage("toggle_lang", code)
      })
    })
  }

  # --- CSS DINÁMICO: COLORES DE EQUIPO ---
  output$driver_styles <- renderUI({
    cols <- get_driver_colours(CURRENT_YEAR)
    if (length(cols) == 0) return(NULL)
    css_rules <- paste(sapply(names(cols), function(d) {
      paste0(".tc-", d, " { border-left-color: ", cols[[d]], " !important; }")
    }), collapse = "\n")
    tags$style(HTML(css_rules))
  })

  # --- SIDEBAR ---
  output$sidebar_content <- renderUI({
    gp_actual <- get_current_gp(CURRENT_YEAR)
    gp_banner <- NULL
    if (!is.null(gp_actual)) {
      gp_banner <- div(
        style = "background: linear-gradient(135deg, #2c0a0a, #1e1e1e); border: 1px solid #e10600; border-radius: 6px; padding: 10px; margin-bottom: 12px; text-align: center;",
        p(style = "margin: 0; font-size: 0.8em; color: #aaa;", t("current_gp")),
        p(style = "margin: 4px 0 0 0; font-size: 1.1em; font-weight: bold; color: #fff;", gp_actual)
      )
    }
    tagList(
      h4(icon("user-astronaut"), t("panel_title")),
      h3(textOutput("user_info"), style = "color: #e10600; margin-top:0;"),
      gp_banner,
      hr(),
      helpText(t("data_source"))
    )
  })

  output$user_info <- renderText({
    paste(t("code_label"), res_auth$user)
  })

  # --- TABS PRINCIPALES ---
  output$main_tabs <- renderUI({
    l <- lang()
    tabsetPanel(
      id = "main_tabset",
      tabPanel(
        t("tab_submit"), br(),
        if (is_guest()) {
          div(
            class = "card",
            div(class = "card-header", t("new_porra")),
            div(class = "card-body", style = "padding: 40px; text-align: center; color: #888;",
              icon("eye", "fa-3x"), br(), br(),
              p(style = "font-size: 1.1em;", t("guest_notice"))
            )
          )
        } else {
          div(
            class = "card", div(class = "card-header", t("new_porra")),
            div(
              class = "card-body", style = "padding: 20px;",
              fluidRow(
                column(6, selectInput("gp", t("gp_label"), choices = NULL, width = "100%")),
                column(6, selectInput("sesion", t("session_label"),
                  choices = c("", session_display(l)), width = "100%"))
              ),
              hr(style = "border-color: #444;"), h5(t("top5_label"), style = "color: #e10600;"),
              fluidRow(
                column(2, selectInput("p1", t("pos_1"), choices = NULL)),
                column(2, selectInput("p2", t("pos_2"), choices = NULL)),
                column(2, selectInput("p3", t("pos_3"), choices = NULL)),
                column(3, selectInput("p4", t("pos_4"), choices = NULL)),
                column(3, selectInput("p5", t("pos_5"), choices = NULL))
              ),
              conditionalPanel(
                condition = "input.sesion == 'Carrera'",
                hr(style = "border-color: #444;"), h5(t("extras_label"), style = "color: #e10600;"),
                fluidRow(
                  column(6, selectInput("vr", t("fast_lap"), choices = NULL)),
                  column(6, selectInput("maz", t("mazepin_prize"), choices = NULL))
                )
              ),
              br(), actionButton("submit", t("submit_btn"), class = "btn-primary btn-lg w-100", style = "font-weight:bold;")
            )
          )
        }
      ),
      tabPanel(
        t("tab_standings"), br(),
        fluidRow(
          column(5, div(id = "leaderboard_capture", class = "card",
            div(class = "card-header",
              icon("trophy"), t("world_title"),
              tags$button(onclick = "captureElement('leaderboard_capture', 'clasificacion-f1')",
                class = "screenshot-btn", style = "float:right;",
                icon("camera"), t("screenshot_btn"))
            ),
            div(class = "card-body", style = "padding: 0;", uiOutput("leaderboard_ui"))
          )),
          column(7, div(id = "gp_analysis_capture", class = "card",
            div(class = "card-header",
              icon("chart-bar"), t("analysis_title"),
              tags$button(onclick = "captureElement('gp_analysis_capture', 'analisis-gp-f1')",
                class = "screenshot-btn", style = "float:right;",
                icon("camera"), t("screenshot_btn"))
            ),
            div(
              class = "card-body", style = "padding: 15px;",
              div(class = "capture-hide",
                selectInput("gp_stats", t("select_gp"), choices = NULL, width = "100%"),
                div(
                  style = "text-align:center; margin-bottom:10px;",
                  radioButtons("view_scope", label = NULL,
                    choices = stats::setNames(c("me", "all"), c(t("scope_me"), t("scope_all"))),
                    selected = "me", inline = TRUE
                  )
                ),
                hr(style = "border-color: #444;")
              ),
              uiOutput("gp_breakdown_ui")
            )
          ))
        )
      ),
      tabPanel(
        t("tab_rules"), br(),
        div(
          class = "card", div(class = "card-header", t("scoring_title")),
          div(
            class = "card-body", style = "padding: 20px;",
            p(t("scoring_intro")),
            tags$ul(
              class = "list-group",
              tags$li(class = "list-group-item", style = "background:#222;", strong(t("session_race_label")), t("rule_race")),
              tags$li(class = "list-group-item", style = "background:#222;", strong(t("session_quali_label")), t("rule_quali")),
              tags$li(class = "list-group-item", style = "background:#222;", strong(t("session_sprint_label")), t("rule_sprint")),
              tags$li(class = "list-group-item", style = "background:#222;", strong(t("session_sprint_quali_label")), t("rule_sprint_quali"))
            ),
            br(),
            div(
              style = "text-align: center; margin-top: 15px;",
              a(href = "https://docs.google.com/document/d/1XuNS9K2AooqyzmXaK2OU8NinwDEwGaaYydkBTyCNrm4/edit?usp=sharing",
                target = "_blank", class = "btn btn-primary", icon("book"), t("read_full_rules"))
            )
          )
        )
      ),
      # --- EVOLUCIÓN DEL CAMPEONATO ---
      tabPanel(
        t("tab_evolution"), br(),
        div(
          class = "card",
          div(class = "card-header", icon("chart-line"), t("evolution_title")),
          div(class = "card-body", style = "padding: 15px;",
            plotlyOutput("evolution_plot", height = "500px")
          )
        )
      ),
      # --- PREMIOS DIVERTIDOS ---
      tabPanel(
        t("tab_awards"), br(),
        div(id = "awards_capture", class = "card",
          div(class = "card-header",
            icon("award"), t("awards_title"),
            tags$button(onclick = "captureElement('awards_capture', 'premios-f1')",
              class = "screenshot-btn", style = "float:right;",
              icon("camera"), t("screenshot_btn"))
          ),
          div(class = "card-body", style = "padding: 20px;",
            uiOutput("awards_ui")
          )
        )
      ),
      # --- HEATMAP DE PREDICCIONES ---
      tabPanel(
        t("tab_heatmap"), br(),
        div(
          class = "card",
          div(class = "card-header", icon("fire"), t("heatmap_title")),
          div(class = "card-body", style = "padding: 15px;",
            selectInput("heatmap_session", t("session_label"),
              choices = NULL, width = "40%"),
            plotlyOutput("heatmap_plot", height = "500px")
          )
        )
      ),
      # --- HISTORIAL DE PREDICCIONES ---
      tabPanel(
        t("tab_history"), br(),
        div(id = "history_capture", class = "card",
          div(class = "card-header",
            icon("clipboard-list"), t("history_title"),
            tags$button(onclick = "captureElement('history_capture', 'historial-f1')",
              class = "screenshot-btn", style = "float:right;",
              icon("camera"), t("screenshot_btn"))
          ),
          div(class = "card-body", style = "padding: 15px;",
            div(class = "capture-hide",
              fluidRow(
                column(6, selectInput("history_gp", t("gp_label"), choices = NULL, width = "100%")),
                column(6, selectInput("history_session", t("session_label"),
                  choices = NULL, width = "100%"))
              ),
              hr(style = "border-color: #444;")
            ),
            uiOutput("history_table_ui")
          )
        )
      )
    )
  })

  # --- CARGA INICIAL Y RECARGA AL CAMBIAR IDIOMA ---
  observe({
    req(res_auth$user)
    lang()

    cal <- get_calendar(CURRENT_YEAR)
    if (is.null(cal)) cal <- c()
    updateSelectInput(session, "gp", choices = c("", cal))
    updateSelectInput(session, "gp_stats", choices = c("", cal))

    pil <- get_drivers(CURRENT_YEAR)
    if (is.null(pil) || length(pil) == 0) {
      pil <- c("NOR", "PIA", "ANT", "RUS", "VER", "HAD", "LEC", "HAM",
               "ALB", "SAI", "LAW", "LIN", "ALO", "STR", "OCO", "BEA",
               "BOR", "HUL", "GAS", "COL", "PER", "BOT")
    }
    lapply(c("p1", "p2", "p3", "p4", "p5", "vr", "maz"), function(x) updateSelectInput(session, x, choices = sort(c("", pil))))

    sd <- session_display(lang())
    updateSelectInput(session, "heatmap_session",
      choices = c(stats::setNames("all", t("all_sessions")), sd))

    updateSelectInput(session, "history_gp", choices = c("", cal))
    updateSelectInput(session, "history_session", choices = c("", sd))
  })

  # --- ENVIAR PORRA ---
  get_db <- reactive({
    input$submit
    req(res_auth$user)
    read_sheet(SHEET_ID, col_types = "c")
  })

  observeEvent(input$submit, {
    req(!is_guest())
    req(input$gp, input$sesion, input$p1)
    top5 <- c(input$p1, input$p2, input$p3, input$p4, input$p5)
    if (any(duplicated(top5[top5 != ""]))) {
      showNotification(t("err_duplicate"), type = "error")
      return()
    }

    # Regla de similitud 75%
    db_check <- read_sheet(SHEET_ID, col_types = "c")
    otras <- db_check %>%
      filter(gp == input$gp, sesion == input$sesion, usuario != res_auth$user)
    if (nrow(otras) > 0) {
      es_carrera <- input$sesion == "Carrera"
      max_coincidencias <- if (es_carrera) 5 else 3
      for (k in 1:nrow(otras)) {
        coincidencias <- sum(c(
          top5[1] != "" && !is.na(otras$p1[k]) && top5[1] == otras$p1[k],
          top5[2] != "" && !is.na(otras$p2[k]) && top5[2] == otras$p2[k],
          top5[3] != "" && !is.na(otras$p3[k]) && top5[3] == otras$p3[k],
          top5[4] != "" && !is.na(otras$p4[k]) && top5[4] == otras$p4[k],
          top5[5] != "" && !is.na(otras$p5[k]) && top5[5] == otras$p5[k]
        ))
        if (es_carrera) {
          if (!is.null(input$vr) && input$vr != "" && !is.na(otras$vuelta_rapida[k]) && input$vr == otras$vuelta_rapida[k]) coincidencias <- coincidencias + 1
          if (!is.null(input$maz) && input$maz != "" && !is.na(otras$mazepin[k]) && input$maz == otras$mazepin[k]) coincidencias <- coincidencias + 1
        }
        if (coincidencias > max_coincidencias) {
          showNotification(
            paste0(
              t("err_similarity_prefix"), coincidencias,
              t("err_similarity_mid"), toupper(otras$usuario[k]),
              t("err_similarity_max"), max_coincidencias,
              t("err_similarity_suffix")
            ),
            type = "error", duration = NULL
          )
          return()
        }
      }
    }

    new_row <- data.frame(
      usuario = res_auth$user, gp = input$gp, sesion = input$sesion,
      p1 = input$p1, p2 = input$p2, p3 = input$p3, p4 = input$p4, p5 = input$p5,
      vuelta_rapida = if (input$sesion == "Carrera") input$vr else NA,
      mazepin = if (input$sesion == "Carrera") input$maz else NA,
      timestamp = as.character(Sys.time())
    )
    db <- read_sheet(SHEET_ID, col_types = "c")
    existing <- which(db$usuario == res_auth$user & db$gp == input$gp & db$sesion == input$sesion)
    if (length(existing) > 0) {
      range_write(SHEET_ID, new_row, range = paste0("A", existing[1] + 1), col_names = FALSE)
      showNotification(t("notify_updated"), type = "message")
    } else {
      sheet_append(SHEET_ID, new_row)
      showNotification(t("notify_submitted"), type = "message")
    }
  })

  # --- CÁLCULO DE PUNTOS ---
  scores <- reactive({
    db <- get_db()
    if (nrow(db) == 0) return(NULL)
    unique_events <- unique(db[, c("gp", "sesion")])
    final_data <- list()
    withProgress(message = t("calculating"), {
      for (i in 1:nrow(unique_events)) {
        g <- unique_events$gp[i]
        s <- unique_events$sesion[i]
        incProgress(1 / nrow(unique_events), detail = g)
        real_res <- get_results(g, s, CURRENT_YEAR)
        bets <- db %>% filter(gp == g, sesion == s)
        if (!is.null(real_res)) {
          for (j in 1:nrow(bets)) {
            pts <- calcular_score_reglamento(bets[j, ], real_res)
            pts_df <- as.data.frame(pts)
            names(pts_df) <- paste0("pts_", names(pts_df))
            final_data[[length(final_data) + 1]] <- bind_cols(bets[j, ], pts_df)
          }
        } else {
          bets$pts_total <- 0
          final_data[[length(final_data) + 1]] <- bets
        }
      }
    })
    if (length(final_data) > 0) bind_rows(final_data) else NULL
  })

  # --- LEADERBOARD ---
  ranking_data <- reactive({
    req(scores())
    compute_ranking(scores())
  })

  output$leaderboard_ui <- renderUI({
    ranking <- ranking_data()
    if (is.null(ranking)) return(NULL)
    tags$table(
      class = "table table-striped table-hover", style = "margin-bottom:0;",
      tags$thead(tags$tr(tags$th(t("col_rank")), tags$th(t("col_pilot")), tags$th(t("col_pts")))),
      tags$tbody(lapply(1:nrow(ranking), function(i) {
        row <- ranking[i, ]
        rank_display <- if (row$Rank == 1) icon("medal", class = "rank-1") else if (row$Rank == 2) icon("medal", class = "rank-2") else if (row$Rank == 3) icon("medal", class = "rank-3") else paste0(row$Rank, ".")
        tags$tr(tags$td(rank_display, style = "font-size:1.2em;"), tags$td(strong(toupper(row$usuario))), tags$td(span(class = "points-badge", sprintf("%.0f", row$Total))))
      }))
    )
  })

  # --- DESGLOSE POR GP ---
  translate_session_name <- function(sesion_es) {
    l <- lang()
    map <- c(
      "Carrera" = tr("session_race", l),
      "Clasificación" = tr("session_quali", l),
      "Esprint" = tr("session_sprint", l),
      "Clasificación del esprint" = tr("session_sprint_quali", l)
    )
    if (sesion_es %in% names(map)) map[[sesion_es]] else sesion_es
  }

  output$gp_breakdown_ui <- renderUI({
    req(input$gp_stats, scores(), input$view_scope)
    user_data <- scores() %>% filter(gp == input$gp_stats)
    if (input$view_scope == "me") {
      user_data <- user_data %>% filter(usuario == res_auth$user)
    } else {
      user_data <- user_data %>%
        arrange(match(sesion, SESSION_VALUES), usuario)
    }
    if (nrow(user_data) == 0) {
      return(div(style = "text-align:center; padding: 40px; color: #666;", icon("wind", "fa-3x"), br(), t("no_data")))
    }

    # Subtítulo visible solo en captura (GP + quién)
    scope_label <- if (input$view_scope == "me") toupper(res_auth$user) else t("scope_all")
    capture_subtitle <- div(
      class = "capture-subtitle",
      style = "display:none;",
      span(style = "font-size:1.1em; font-weight:bold; color:#e0e0e0;", input$gp_stats),
      span(style = "color:#666; margin-left:8px; font-size:0.85em;", paste0("(", scope_label, ")"))
    )

    tagList(
      capture_subtitle,
      lapply(1:nrow(user_data), function(i) {
        fila <- user_data[i, ]
        res_real <- get_results(fila$gp, fila$sesion, CURRENT_YEAR)
        user_header <- NULL
        if (input$view_scope == "all") {
          css_class <- if (fila$usuario == res_auth$user) "user-header-block user-header-me" else "user-header-block"
          user_header <- div(class = css_class, icon("user"), strong(toupper(fila$usuario)), style = "font-size: 0.9em; color: #fff;")
        }
        if (!is.null(res_real)) {
          real_top5 <- res_real$posiciones[1:5]
          mis_pilotos <- c(as.character(fila[["p1"]]), as.character(fila[["p2"]]), as.character(fila[["p3"]]), as.character(fila[["p4"]]), as.character(fila[["p5"]]))
          extras_html <- NULL
          if (fila$sesion == "Carrera") {
            extras_html <- div(
              style = "margin-top:10px; font-size: 0.85em;",
              div(class = "row",
                div(class = "col-xs-6", span(class = "driver-badge vr-badge", "VR"), span(style = "color:#aaa", " Real: "), team_badge(res_real$vuelta_rapida %||% "-"), " ", span(style = "color:#aaa", "Apu: "), team_badge(fila$vuelta_rapida %||% "-")),
                div(class = "col-xs-6", span(class = "driver-badge mazepin-badge", "MAZ"), span(style = "color:#aaa", " Real: "), team_badge(res_real$mazepin %||% "-"), " ", span(style = "color:#aaa", "Apu: "), team_badge(fila$mazepin %||% "-"))
              )
            )
          }
          div(
            class = "session-row", user_header,
            div(class = "session-title", icon("flag-checkered"), translate_session_name(fila$sesion),
              span(style = "float:right; font-size:0.8em; color:#888;", paste(fila$pts_total, t("pts_suffix")))),
            div(class = "comparison-box",
              div(class = "side-box", p(class = "vs-text", t("result_label")), div(badge_row(real_top5))),
              div(style = "color:#444;", icon("chevron-right")),
              div(class = "side-box", style = "border: 1px solid #444;", p(class = "vs-text", t("porra_label")), div(badge_row(mis_pilotos)))
            ),
            extras_html
          )
        } else {
          div(class = "session-row", user_header, h5(translate_session_name(fila$sesion), style = "color:#888;"), p(icon("clock"), t("waiting_results")))
        }
      })
    )
  })

  `%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

  # ===========================================================================
  # EVOLUCIÓN DEL CAMPEONATO
  # ===========================================================================
  output$evolution_plot <- renderPlotly({
    req(scores())
    cal <- get_calendar(CURRENT_YEAR)

    sc <- scores() |>
      group_by(usuario, gp) |>
      summarise(pts = sum(pts_total, na.rm = TRUE), .groups = "drop")

    gps_with_data <- unique(sc$gp)
    cal_ordered <- cal[cal %in% gps_with_data]
    if (length(cal_ordered) == 0) return(NULL)

    sc <- sc |>
      mutate(gp = factor(gp, levels = cal_ordered)) |>
      tidyr::complete(usuario, gp, fill = list(pts = 0)) |>
      arrange(gp) |>
      group_by(usuario) |>
      mutate(pts_cum = cumsum(pts)) |>
      ungroup() |>
      mutate(usuario = toupper(usuario))

    p <- ggplot(sc, aes(x = gp, y = pts_cum, color = usuario, group = usuario,
                        text = paste0(usuario, "\n", as.character(gp), ": ", pts_cum, " pts"))) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1e1e1e", color = NA),
        panel.background = element_rect(fill = "#1e1e1e", color = NA),
        panel.grid.major = element_line(color = "#333333"),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#e0e0e0"),
        axis.text = element_text(color = "#aaa"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.text = element_text(color = "#e0e0e0"),
        legend.key = element_rect(fill = "#1e1e1e")
      ) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = NULL, y = t("col_pts"), color = t("col_pilot"))

    ggplotly(p, tooltip = "text") |>
      plotly::layout(
        paper_bgcolor = "#1e1e1e", plot_bgcolor = "#1e1e1e",
        font = list(color = "#e0e0e0"),
        legend = list(bgcolor = "#1e1e1e", font = list(color = "#e0e0e0"))
      )
  })

  # ===========================================================================
  # PREMIOS DIVERTIDOS
  # ===========================================================================
  output$awards_ui <- renderUI({
    req(scores(), ranking_data())
    sc <- scores()
    champ <- ranking_data() |> select(usuario, Rank)

    for (col in c("pts_p1", "pts_p2", "pts_p3", "pts_p4", "pts_p5", "pts_vr", "pts_maz")) {
      if (!col %in% names(sc)) sc[[col]] <- 0
    }
    sc <- sc |> mutate(across(starts_with("pts_"), ~replace_na(., 0)))

    if (nrow(sc) == 0) {
      return(div(style = "text-align:center; padding:40px; color:#666;", t("no_awards_data")))
    }

    # Helper: desempate por posición en el campeonato
    # negative = TRUE para premios "negativos" (el peor clasificado "gana")
    break_tie <- function(df, negative = FALSE) {
      if (nrow(df) <= 1) return(df)
      df <- df |> left_join(champ, by = "usuario")
      df <- if (negative) {
        df |> slice_max(Rank, n = 1, with_ties = FALSE)
      } else {
        df |> slice_min(Rank, n = 1, with_ties = FALSE)
      }
      df |> select(-Rank)
    }

    awards <- list()
    by_gp <- sc |>
      group_by(usuario, gp) |>
      summarise(pts = sum(pts_total), .groups = "drop")

    # 1. Oráculo
    best_gp <- by_gp |> slice_max(pts, n = 1, with_ties = TRUE) |> break_tie()
    awards[[length(awards) + 1]] <- list(
      emoji = "\U0001f52e", name = t("award_oracle"), desc = t("award_oracle_desc"),
      winner = paste0(toupper(best_gp$usuario), " \u2014 ", best_gp$gp, " (", best_gp$pts, " pts)")
    )

    # 2. Francotirador
    sc2 <- sc |> mutate(
      pts_exacto = case_when(
        sesion == "Carrera" ~ 6, sesion == "Clasificación" ~ 3,
        sesion == "Esprint" ~ 3, sesion == "Clasificación del esprint" ~ 1, TRUE ~ 0
      ),
      exact_count = (pts_p1 == pts_exacto & pts_p1 > 0) + (pts_p2 == pts_exacto & pts_p2 > 0) +
                    (pts_p3 == pts_exacto & pts_p3 > 0) + (pts_p4 == pts_exacto & pts_p4 > 0) +
                    (pts_p5 == pts_exacto & pts_p5 > 0)
    )
    best_exact <- sc2 |>
      group_by(usuario) |>
      summarise(total_exact = sum(exact_count), .groups = "drop") |>
      slice_max(total_exact, n = 1, with_ties = TRUE) |> break_tie()
    awards[[length(awards) + 1]] <- list(
      emoji = "\U0001f3af", name = t("award_sniper"), desc = t("award_sniper_desc"),
      winner = paste0(toupper(best_exact$usuario), " (", best_exact$total_exact, ")")
    )

    # 3. Sesión estelar
    best_session <- sc |> slice_max(pts_total, n = 1, with_ties = TRUE) |> break_tie()
    awards[[length(awards) + 1]] <- list(
      emoji = "\u2b50", name = t("award_star_session"), desc = t("award_star_session_desc"),
      winner = paste0(toupper(best_session$usuario), " \u2014 ",
                      translate_session_name(best_session$sesion),
                      " (", best_session$gp, ", ", best_session$pts_total, " pts)")
    )

    # 4. GP para olvidar
    worst_gp <- by_gp |> slice_min(pts, n = 1, with_ties = TRUE) |> break_tie(negative = TRUE)
    awards[[length(awards) + 1]] <- list(
      emoji = "\U0001f480", name = t("award_forgettable"), desc = t("award_forgettable_desc"),
      winner = paste0(toupper(worst_gp$usuario), " \u2014 ", worst_gp$gp, " (", worst_gp$pts, " pts)")
    )

    # 5. Rey Mazepin
    best_maz <- sc |>
      group_by(usuario) |>
      summarise(maz_ok = sum(pts_maz > 0), .groups = "drop") |>
      slice_max(maz_ok, n = 1, with_ties = TRUE) |> break_tie()
    awards[[length(awards) + 1]] <- list(
      emoji = "\U0001f422", name = t("award_mazepin_king"), desc = t("award_mazepin_king_desc"),
      winner = paste0(toupper(best_maz$usuario), " (", best_maz$maz_ok, ")")
    )

    # 6. Piloto del pueblo (no es por usuario — sin desempate)
    db <- get_db()
    long <- db |>
      select(p1, p2, p3, p4, p5) |>
      pivot_longer(everything(), values_to = "driver") |>
      filter(!is.na(driver), driver != "") |>
      count(driver, sort = TRUE)
    if (nrow(long) > 0) {
      top_driver <- long |> slice(1)
      awards[[length(awards) + 1]] <- list(
        emoji = "\U0001f3ce\ufe0f", name = t("award_peoples_driver"), desc = t("award_peoples_driver_desc"),
        winner = paste0(top_driver$driver, " (", top_driver$n, " ", t("award_picks"), ")")
      )
    }

    # 7. Bola de cristal — más veces acertando el P1
    p1_hits <- sc |>
      filter(pts_p1 > 0) |>
      count(usuario) |>
      slice_max(n, n = 1, with_ties = TRUE) |> break_tie()
    if (nrow(p1_hits) > 0) {
      awards[[length(awards) + 1]] <- list(
        emoji = "\U0001f3b1", name = t("award_crystal_ball"), desc = t("award_crystal_ball_desc"),
        winner = paste0(toupper(p1_hits$usuario), " (", p1_hits$n, ")")
      )
    }

    # 8. Bocachancla — más sesiones con 0 puntos
    zeroes <- sc |>
      filter(pts_total == 0) |>
      count(usuario) |>
      slice_max(n, n = 1, with_ties = TRUE) |> break_tie(negative = TRUE)
    if (nrow(zeroes) > 0) {
      awards[[length(awards) + 1]] <- list(
        emoji = "\U0001f92b", name = t("award_bigmouth"), desc = t("award_bigmouth_desc"),
        winner = paste0(toupper(zeroes$usuario), " (", zeroes$n, ")")
      )
    }

    # 9. Mr. Consistente — menor desviación típica de puntos por GP
    if (length(unique(by_gp$gp)) >= 2) {
      consistency <- by_gp |>
        group_by(usuario) |>
        summarise(sd_pts = sd(pts, na.rm = TRUE), .groups = "drop") |>
        slice_min(sd_pts, n = 1, with_ties = TRUE) |> break_tie()
      awards[[length(awards) + 1]] <- list(
        emoji = "\U0001f9d8", name = t("award_consistent"), desc = t("award_consistent_desc"),
        winner = paste0(toupper(consistency$usuario), " (\u03c3 = ", round(consistency$sd_pts, 1), ")")
      )
    }

    # 10. Borreguismo — pareja que más coincide en predicciones (sin desempate)
    if (length(unique(db$usuario)) >= 2) {
      db_preds <- db |>
        select(usuario, gp, sesion, p1, p2, p3, p4, p5)
      usuarios <- sort(unique(db_preds$usuario))
      best_pair <- NULL
      best_coincidences <- -1
      for (a_i in seq_along(usuarios)[-length(usuarios)]) {
        for (b_i in (a_i + 1):length(usuarios)) {
          merged <- inner_join(
            db_preds |> filter(usuario == usuarios[a_i]),
            db_preds |> filter(usuario == usuarios[b_i]),
            by = c("gp", "sesion"), suffix = c("_a", "_b")
          )
          if (nrow(merged) == 0) next
          coincidences <- sum(
            merged$p1_a == merged$p1_b & !is.na(merged$p1_a),
            merged$p2_a == merged$p2_b & !is.na(merged$p2_a),
            merged$p3_a == merged$p3_b & !is.na(merged$p3_a),
            merged$p4_a == merged$p4_b & !is.na(merged$p4_a),
            merged$p5_a == merged$p5_b & !is.na(merged$p5_a)
          )
          if (coincidences > best_coincidences) {
            best_coincidences <- coincidences
            best_pair <- c(usuarios[a_i], usuarios[b_i])
          }
        }
      }
      if (!is.null(best_pair)) {
        awards[[length(awards) + 1]] <- list(
          emoji = "\U0001f411", name = t("award_copycat"), desc = t("award_copycat_desc"),
          winner = paste0(toupper(best_pair[1]), " & ", toupper(best_pair[2]), " (", best_coincidences, ")")
        )
      }
    }

    # 11. Rey del GP — más victorias de GP
    # Victorias de GP: desempatar sesiones por campeonato antes de contar
    gp_winners <- by_gp |>
      group_by(gp) |>
      slice_max(pts, n = 1, with_ties = TRUE) |>
      break_tie() |>
      ungroup() |>
      count(usuario) |>
      slice_max(n, n = 1, with_ties = TRUE) |> break_tie()
    if (nrow(gp_winners) > 0) {
      awards[[length(awards) + 1]] <- list(
        emoji = "\U0001f451", name = t("award_gp_king"), desc = t("award_gp_king_desc"),
        winner = paste0(toupper(gp_winners$usuario), " (", gp_winners$n, " ", t("award_gp_wins"), ")")
      )
    }

    # 12. Racha imparable — mayor racha consecutiva de victorias de sesión
    cal <- get_calendar(CURRENT_YEAR)
    sc_ordered <- sc |>
      mutate(
        gp = factor(gp, levels = cal),
        sesion = factor(sesion, levels = SESSION_VALUES)
      ) |>
      arrange(gp, sesion)
    # Desempatar ganadores de sesión por campeonato
    session_win_list <- sc_ordered |>
      group_by(gp, sesion) |>
      slice_max(pts_total, n = 1, with_ties = TRUE) |>
      break_tie() |>
      ungroup() |>
      arrange(gp, sesion) |>
      pull(usuario)
    all_users <- unique(sc$usuario)
    best_streak_user <- NULL
    best_streak_n <- 0
    for (u in all_users) {
      streak <- 0
      max_streak <- 0
      for (w in session_win_list) {
        if (w == u) { streak <- streak + 1; max_streak <- max(max_streak, streak) }
        else { streak <- 0 }
      }
      if (max_streak > best_streak_n) {
        best_streak_n <- max_streak
        best_streak_user <- u
      }
    }
    if (best_streak_n >= 2) {
      awards[[length(awards) + 1]] <- list(
        emoji = "\U0001f525", name = t("award_hot_streak"), desc = t("award_hot_streak_desc"),
        winner = paste0(toupper(best_streak_user), " (", best_streak_n, " ", t("award_sessions_suffix"), ")")
      )
    }

    cards <- lapply(awards, function(a) {
      div(class = "col-sm-4",
        div(class = "award-card",
          div(class = "award-emoji", a$emoji),
          div(class = "award-name", a$name),
          div(class = "award-desc", a$desc),
          div(class = "award-winner", a$winner)
        )
      )
    })

    div(class = "row", cards)
  })

  # ===========================================================================
  # HEATMAP DE PREDICCIONES
  # ===========================================================================
  output$heatmap_plot <- renderPlotly({
    db <- get_db()
    if (is.null(db) || nrow(db) == 0) return(NULL)

    if (!is.null(input$heatmap_session) && input$heatmap_session != "all") {
      db <- db |> filter(sesion == input$heatmap_session)
    }

    long <- db |>
      select(usuario, p1, p2, p3, p4, p5) |>
      pivot_longer(cols = p1:p5, values_to = "driver") |>
      filter(!is.na(driver), driver != "") |>
      count(usuario, driver) |>
      mutate(usuario = toupper(usuario))

    if (nrow(long) == 0) return(NULL)

    driver_order <- long |>
      group_by(driver) |>
      summarise(total = sum(n), .groups = "drop") |>
      arrange(desc(total)) |>
      pull(driver)

    long <- long |>
      tidyr::complete(usuario, driver, fill = list(n = 0)) |>
      mutate(driver = factor(driver, levels = driver_order))

    p <- ggplot(long, aes(x = driver, y = usuario, fill = n,
                          text = paste0(usuario, " \u2192 ", driver, ": ", n))) +
      geom_tile(color = "#2c2c2c", linewidth = 0.5) +
      scale_fill_gradient(low = "#1a1a2e", high = "#e10600", name = t("award_picks")) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1e1e1e", color = NA),
        panel.background = element_rect(fill = "#1e1e1e", color = NA),
        text = element_text(color = "#e0e0e0"),
        axis.text = element_text(color = "#ccc", face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.text = element_text(color = "#e0e0e0")
      ) +
      labs(x = NULL, y = NULL)

    ggplotly(p, tooltip = "text") |>
      plotly::layout(
        paper_bgcolor = "#1e1e1e", plot_bgcolor = "#1e1e1e",
        font = list(color = "#e0e0e0")
      )
  })

  # ===========================================================================
  # HISTORIAL DE PREDICCIONES
  # ===========================================================================
  output$history_table_ui <- renderUI({
    if (is.null(input$history_gp) || input$history_gp == "" ||
        is.null(input$history_session) || input$history_session == "") {
      return(div(style = "text-align:center; padding: 40px; color: #666;",
        icon("clipboard-list", "fa-3x"), br(), br(), t("no_history_data")))
    }

    db <- get_db()
    data <- db |> filter(gp == input$history_gp, sesion == input$history_session)

    if (nrow(data) == 0) {
      return(div(style = "text-align:center; padding: 40px; color: #666;",
        icon("wind", "fa-3x"), br(), br(), t("no_data")))
    }

    is_race <- input$history_session == "Carrera"

    header_cells <- list(
      tags$th(t("col_pilot"), style = "min-width:80px;"),
      tags$th(t("pos_1")), tags$th(t("pos_2")), tags$th(t("pos_3")),
      tags$th(t("pos_4")), tags$th(t("pos_5"))
    )
    if (is_race) {
      header_cells <- c(header_cells, list(
        tags$th("\U0001f680 VR"), tags$th("\U0001f422 MAZ")
      ))
    }

    rows <- lapply(1:nrow(data), function(i) {
      row <- data[i, ]
      is_me <- row$usuario == res_auth$user
      row_class <- if (is_me) "history-me" else ""

      cells <- list(
        tags$td(strong(toupper(row$usuario)),
          style = if (is_me) "color: #e10600;" else ""),
        tags$td(team_badge(row$p1)),
        tags$td(team_badge(row$p2)),
        tags$td(team_badge(row$p3)),
        tags$td(team_badge(row$p4)),
        tags$td(team_badge(row$p5))
      )
      if (is_race) {
        cells <- c(cells, list(
          tags$td(team_badge(row$vuelta_rapida %||% "-", "vr-badge")),
          tags$td(team_badge(row$mazepin %||% "-", "mazepin-badge"))
        ))
      }

      tags$tr(class = row_class, cells)
    })

    history_subtitle <- div(
      class = "capture-subtitle",
      style = "display:none;",
      span(style = "font-size:1.1em; font-weight:bold; color:#e0e0e0;", input$history_gp),
      span(style = "color:#666; margin-left:8px; font-size:0.85em;",
        paste0("(", translate_session_name(input$history_session), ")"))
    )

    tagList(
      history_subtitle,
      tags$table(
        class = "table table-hover history-table",
        style = "margin-bottom:0; text-align:center;",
        tags$thead(tags$tr(header_cells)),
        tags$tbody(rows)
      )
    )
  })
}
