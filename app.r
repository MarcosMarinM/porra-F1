# ==============================================================================
# PROYECTO: LA CARRERA MÁS SURREALIST 
# VERSIÓN: 2026 
# ==============================================================================

library(shiny)
library(shinymanager)
library(googlesheets4)
library(dplyr)
library(httr2)
library(jsonlite)
library(memoise)
library(cachem)
library(bslib)

# ==============================================================================
# 1. CONFIGURACIÓN
# ==============================================================================

# --- A. CONFIGURACIÓN MEDIANTE VARIABLES DE ENTORNO ---
# Carga las credenciales desde el archivo .Renviron (ignorado en Git)
DB_PATH <- Sys.getenv("F1_DB_PATH")
if (DB_PATH == "") DB_PATH <- "usuarios.sqlite"

SHEET_ID <- Sys.getenv("F1_SHEET_ID")
if (SHEET_ID == "") stop("ERROR: Falta configurar F1_SHEET_ID en el archivo .Renviron")

json_path <- Sys.getenv("F1_JSON_PATH")
if (json_path == "") json_path <- "f1-service-account.json"

# --- B. GOOGLE SHEETS & AUTENTICACIÓN ---
if (file.exists(json_path) && json_path != "") {
  # Si encuentra el archivo, usa al Robot
  gs4_auth(path = json_path)
} else {
  # Si no lo encuentra, intenta abrir ventana de navegador (solo local)
  if (interactive()) options(gargle_oauth_email = TRUE)
}

# --- C. API ---
BASE_URL <- "https://api.openf1.org/v1"
CURRENT_YEAR <- 2026

cache_memoria <- cachem::cache_mem(max_age = 3600)

# ==============================================================================
# 2. MOTOR DE DATOS (API)
# ==============================================================================

fetch_api <- function(endpoint, params = list()) {
  url <- paste0(BASE_URL, endpoint)
  tryCatch(
    {
      req <- request(url) %>%
        req_url_query(!!!params) %>%
        req_timeout(20) %>%
        req_retry(max_tries = 3, backoff = ~2)
      resp <- req %>%
        req_perform() %>%
        resp_body_json(simplifyVector = TRUE)
      if (length(resp) == 0) {
        return(NULL)
      }
      return(as.data.frame(resp))
    },
    error = function(e) {
      return(NULL)
    }
  )
}

# Obtener Pilotos (Con caché y fallback)
get_drivers_raw <- function(year) {
  # 1. Intentar buscar pilotos de cualquier sesión del año actual
  sessions <- fetch_api("/sessions", list(year = year))

  if (is.null(sessions) || nrow(sessions) == 0) {
    return(NULL)
  }

  # Usar una lista de las últimas sesiones para asegurar captar a todos (ej. Tests + Carrera 1)
  recent_sessions <- tail(sessions$session_key, 10)

  all_drivers <- c()
  for (sk in recent_sessions) {
    drivers <- fetch_api("/drivers", list(session_key = sk))
    if (!is.null(drivers) && nrow(drivers) > 0) {
      all_drivers <- c(all_drivers, drivers$name_acronym)
    }
  }

  if (length(all_drivers) == 0) {
    return(NULL)
  }

  # Devolver lista única ordenada
  sort(unique(all_drivers[!is.na(all_drivers)]))
}
get_drivers <- memoise(get_drivers_raw, cache = cache_memoria)

# Obtener Calendario Dinámico
get_calendar_raw <- function(year) {
  meetings <- fetch_api("/meetings", list(year = year))
  if (is.null(meetings) || nrow(meetings) == 0) {
    return(NULL)
  }
  # Devolver lista de nombres de GP ordenados por fecha de inicio
  meetings <- meetings %>% arrange(date_start)
  unique(meetings$meeting_name)
}
get_calendar <- memoise(get_calendar_raw, cache = cache_memoria)

# Obtener Resultados (Robusto)
get_results_raw <- function(gp_name, session_name, year) {
  gp_clean <- trimws(gp_name)
  session_clean <- trimws(session_name)

  meetings <- fetch_api("/meetings", list(year = year, meeting_name = gp_clean))
  if (is.null(meetings)) {
    return(NULL)
  }
  keys <- unique(meetings$meeting_key)

  all_sessions_list <- lapply(keys, function(k) fetch_api("/sessions", list(meeting_key = k)))
  all_sessions <- bind_rows(all_sessions_list)
  if (is.null(all_sessions) || nrow(all_sessions) == 0) {
    return(NULL)
  }

  target_session <- NULL
  if (session_clean == "Carrera") {
    target_session <- all_sessions %>% dplyr::filter(session_name == "Race")
  } else if (session_clean == "Clasificación") {
    target_session <- all_sessions %>% dplyr::filter(session_name == "Qualifying")
  } else if (session_clean == "Esprint") {
    target_session <- all_sessions %>% dplyr::filter(session_name == "Sprint")
  } else if (session_clean == "Clasificación del esprint") target_session <- all_sessions %>% dplyr::filter(session_name %in% c("Sprint Qualifying", "Sprint Shootout"))

  if (is.null(target_session) || nrow(target_session) == 0) {
    return(NULL)
  }

  s_key <- tail(target_session, 1)$session_key
  api_actual_name <- tail(target_session, 1)$session_name

  drivers <- fetch_api("/drivers", list(session_key = s_key))
  if (is.null(drivers)) {
    return(NULL)
  }
  d_map <- drivers %>%
    dplyr::select(driver_number, name_acronym) %>%
    distinct()

  res <- fetch_api("/session_result", list(session_key = s_key))
  if (is.null(res)) {
    return(NULL)
  }

  full_res <- res %>%
    left_join(d_map, by = "driver_number") %>%
    arrange(position)
  top5_real <- full_res$name_acronym

  vr_val <- NA
  maz_val <- NA

  if (api_actual_name == "Race") {
    laps <- fetch_api("/laps", list(session_key = s_key))
    if (!is.null(laps) && nrow(laps) > 0) {
      fastest <- laps %>%
        filter(!is.na(lap_duration)) %>%
        arrange(lap_duration) %>%
        slice(1)
      vr_driver <- d_map %>% filter(driver_number == fastest$driver_number)
      if (nrow(vr_driver) > 0) vr_val <- vr_driver$name_acronym[1]
    }
    rc <- fetch_api("/race_control", list(session_key = s_key, category = "Retirement"))
    if (!is.null(rc) && nrow(rc) > 0) {
      first_out <- rc %>%
        arrange(date) %>%
        slice(1)
      maz_driver <- d_map %>% filter(driver_number == first_out$driver_number)
      if (nrow(maz_driver) > 0) maz_val <- maz_driver$name_acronym[1]
    }
    if (is.na(maz_val)) {
      dnfs <- full_res %>%
        filter(dnf == TRUE) %>%
        arrange(number_of_laps)
      if (nrow(dnfs) > 0) {
        maz_val <- dnfs$name_acronym[1]
      } else {
        last_place <- full_res %>% filter(position == max(position, na.rm = TRUE))
        maz_val <- last_place$name_acronym[1]
      }
    }
  }
  return(list(posiciones = top5_real, vuelta_rapida = vr_val, mazepin = maz_val))
}
get_results <- memoise(get_results_raw, cache = cache_memoria)

# ==============================================================================
# 3. LÓGICA REGLAMENTO
# ==============================================================================

calcular_score_reglamento <- function(porra, resultado) {
  puntos <- list(p1 = 0, p2 = 0, p3 = 0, p4 = 0, p5 = 0, vr = 0, maz = 0, total = 0)
  user_top5 <- c(porra$p1, porra$p2, porra$p3, porra$p4, porra$p5)
  real_top5 <- resultado$posiciones[1:5]
  real_top5_clean <- real_top5[!is.na(real_top5)]

  pts_exacto <- 0
  pts_parcial <- 0
  if (porra$sesion == "Clasificación") {
    pts_exacto <- 3
    pts_parcial <- 1
  } else if (porra$sesion == "Carrera") {
    pts_exacto <- 6
    pts_parcial <- 2
  } else if (porra$sesion == "Clasificación del esprint") {
    pts_exacto <- 1
    pts_parcial <- 0
  } else if (porra$sesion == "Esprint") {
    pts_exacto <- 3
    pts_parcial <- 1
  }

  for (i in 1:5) {
    apuesta <- user_top5[i]
    realidad <- if (i <= length(real_top5)) real_top5[i] else NA
    if (!is.na(apuesta) && apuesta != "") {
      if (!is.na(realidad) && apuesta == realidad) {
        puntos[[paste0("p", i)]] <- pts_exacto
      } else if (apuesta %in% real_top5_clean) puntos[[paste0("p", i)]] <- pts_parcial
    }
  }

  if (porra$sesion == "Carrera") {
    if (!is.na(porra$vuelta_rapida) && !is.na(resultado$vuelta_rapida) && porra$vuelta_rapida == resultado$vuelta_rapida) puntos$vr <- 1
    if (!is.na(porra$mazepin) && !is.na(resultado$mazepin) && porra$mazepin == resultado$mazepin) puntos$maz <- 3
  }
  puntos$total <- sum(unlist(puntos))
  return(puntos)
}

# ==============================================================================
# 4. INTERFAZ (UI)
# ==============================================================================

ui <- fluidPage(
  theme = bs_theme(
    bg = "#121212", fg = "#e0e0e0", primary = "#e10600",
    base_font = font_google("Titillium Web"),
    heading_font = font_google("Titillium Web")
  ),
  tags$head(tags$style(HTML("
    body { background-color: #000000; }
    .card { background-color: #1e1e1e; border: 1px solid #333; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.3); }
    .card-header { background-color: #2c2c2c; border-bottom: 2px solid #e10600; font-weight: bold; text-transform: uppercase; letter-spacing: 1px; color: #fff; }

    /* TABLAS */
    .table { color: #ddd; }
    .table-striped tbody tr:nth-of-type(odd) { background-color: rgba(255,255,255,0.05); }
    .rank-1 { color: #FFD700; } .rank-2 { color: #C0C0C0; } .rank-3 { color: #CD7F32; }
    .points-badge { background-color: #e10600; color: white; padding: 3px 8px; border-radius: 10px; font-weight: bold; font-size: 0.9em; }

    /* BADGES */
    .driver-badge { display: inline-block; padding: 2px 6px; border-radius: 4px; background: #333; border-left: 3px solid #e10600; font-family: monospace; margin-right: 5px; font-weight: bold;}
    .mazepin-badge { border-left: 3px solid #FFD700 !important; color: #FFD700 !important; }
    .vr-badge { border-left: 3px solid #bf00ff !important; color: #bf00ff !important; }

    /* ESTRUCTURA RESULTADOS */
    .session-row { border-bottom: 1px solid #333; padding: 15px 0; }
    .session-title { font-size: 1.1em; color: #e10600; font-weight: bold; margin-bottom: 10px; }
    .comparison-box { display: flex; justify-content: space-between; align-items: center; }
    .side-box { flex: 1; padding: 10px; background: rgba(255,255,255,0.03); border-radius: 5px; margin: 0 5px; }
    .vs-text { color: #555; font-weight: bold; font-size: 0.8em; }

    /* HEADER DE USUARIO (VISUALIZACION GRUPAL) */
    .user-header-block { background-color: #2a2a2a; padding: 5px 10px; border-radius: 4px; display: inline-block; margin-bottom: 10px; border-left: 4px solid #888; }
    .user-header-me { border-left: 4px solid #e10600; background-color: #3a1010; }
  "))),
  div(
    style = "padding: 20px 0; border-bottom: 4px solid #e10600; margin-bottom: 20px;",
    fluidRow(column(12,
      align = "center", img(src = "https://upload.wikimedia.org/wikipedia/commons/3/33/F1.svg", height = "40px"),
      h2("LA CARRERA MÁS SURREALIST", style = "display:inline; margin-left: 15px; font-weight:800; color: #fff;")
    ))
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(
        class = "card", style = "padding: 15px;",
        h4(icon("user-astronaut"), "Panel de control"),
        h3(textOutput("user_info"), style = "color: #e10600; margin-top:0;"),
        hr(),
        helpText("Datos oficiales de OpenF1.")
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "📝 Enviar pronóstico", br(),
          div(
            class = "card", div(class = "card-header", "Nueva porra"),
            div(
              class = "card-body", style = "padding: 20px;",
              fluidRow(
                column(6, selectInput("gp", "Gran Premio", choices = NULL, width = "100%")),
                column(6, selectInput("sesion", "Sesión", choices = c("", "Clasificación del esprint", "Esprint", "Clasificación", "Carrera"), width = "100%"))
              ),
              hr(style = "border-color: #444;"), h5("top-5", style = "color: #e10600;"),
              fluidRow(
                column(2, selectInput("p1", "1º", choices = NULL)), column(2, selectInput("p2", "2º", choices = NULL)),
                column(2, selectInput("p3", "3º", choices = NULL)), column(3, selectInput("p4", "4º", choices = NULL)),
                column(3, selectInput("p5", "5º", choices = NULL))
              ),
              conditionalPanel(
                condition = "input.sesion == 'Carrera'", hr(style = "border-color: #444;"), h5("Extras", style = "color: #e10600;"),
                fluidRow(column(6, selectInput("vr", "🚀 Vuelta Rápida", choices = NULL)), column(6, selectInput("maz", "🐢 Premio Mazepin", choices = NULL)))
              ),
              br(), actionButton("submit", "ENVIAR PORRA", class = "btn-primary btn-lg w-100", style = "font-weight:bold;")
            )
          )
        ),
        tabPanel(
          "🏆 Clasificación", br(),
          fluidRow(
            column(5, div(class = "card", div(class = "card-header", icon("trophy"), " Mundial"), div(class = "card-body", style = "padding: 0;", uiOutput("leaderboard_ui")))),
            column(7, div(
              class = "card",
              div(class = "card-header", icon("chart-bar"), " Análisis GP"),
              div(
                class = "card-body", style = "padding: 15px;",
                selectInput("gp_stats", "Seleccionar Gran Premio:", choices = NULL, width = "100%"),
                div(
                  style = "text-align:center; margin-bottom:10px;",
                  radioButtons("view_scope",
                    label = NULL,
                    choices = c("Solo yo" = "me", "Todos" = "all"),
                    selected = "me", inline = TRUE
                  )
                ),
                hr(style = "border-color: #444;"),
                uiOutput("gp_breakdown_ui")
              )
            ))
          )
        ),
        tabPanel(
          "📜 Reglamento", br(),
          div(
            class = "card", div(class = "card-header", "Puntuación"),
            div(
              class = "card-body", style = "padding: 20px;",
              p("A continuación, se resume la obtención de puntos para cada una de las sesiones del fin de semana. Recuerda que para acertar la 'posición exacta', el piloto de tu porra debe coincidir exactamente con la posición final real. El acierto 'parcial' se da cuando tu piloto puntúa en el top-5, pero en un puesto distinto al que predijiste."),
              tags$ul(
                class = "list-group",
                tags$li(class = "list-group-item", style = "background:#222;", strong("Carrera:"), " 6 puntos por posición exacta | 2 puntos por acierto parcial | 3 puntos extra por acertar al Premio Mazepin | 1 punto extra por acertar la Vuelta Rápida"),
                tags$li(class = "list-group-item", style = "background:#222;", strong("Clasificación:"), " 3 puntos por posición exacta | 1 punto por acierto parcial"),
                tags$li(class = "list-group-item", style = "background:#222;", strong("Esprint:"), " 3 puntos por posición exacta | 1 punto por acierto parcial"),
                tags$li(class = "list-group-item", style = "background:#222;", strong("Clasificación del Esprint:"), " 1 punto por posición exacta (no hay puntos por acierto parcial)")
              ),
              br(),
              div(
                style = "text-align: center; margin-top: 15px;",
                a(href = "https://docs.google.com/document/d/1XuNS9K2AooqyzmXaK2OU8NinwDEwGaaYydkBTyCNrm4/edit?usp=sharing", target = "_blank", class = "btn btn-primary", icon("book"), " Leer el reglamento oficial completo")
              )
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# 5. SERVIDOR
# ==============================================================================

server <- function(input, output, session) {
  # Login usando base de datos SQLite en lugar de dataframe
  res_auth <- secure_server(
    check_credentials = check_credentials(
      db = DB_PATH
    )
  )

  output$user_info <- renderText({
    paste("Código:", res_auth$user)
  })

  # Carga Inicial
  observe({
    req(res_auth$user)
    # Obtener calendario dinámicamente de la API
    cal <- get_calendar(CURRENT_YEAR)
    if (is.null(cal)) cal <- c() # Fallback temporal si la API falla

    updateSelectInput(session, "gp", choices = c("", cal))
    updateSelectInput(session, "gp_stats", choices = c("", cal))

    # Pilotos sí desde API (o fallback)
    pil <- get_drivers(CURRENT_YEAR)
    if (is.null(pil) || length(pil) == 0) {
      # Fallback a la parrilla de 2026
      # NOR, PIA, ANT, RUS, VER, HAD, LEC, HAM, ALB, SAI, LAW, LIN, ALO, STR, OCO, BEA, BOR, HÜL, GAS, COL, PER, BOT
      pil <- c("NOR", "PIA", "ANT", "RUS", "VER", "HAD", "LEC", "HAM", "ALB", "SAI", "LAW", "LIN", "ALO", "STR", "OCO", "BEA", "BOR", "HUL", "GAS", "COL", "PER", "BOT")
    }
    lapply(c("p1", "p2", "p3", "p4", "p5", "vr", "maz"), function(x) updateSelectInput(session, x, choices = sort(c("", pil))))
  })

  get_db <- reactive({
    input$submit
    req(res_auth$user)
    read_sheet(SHEET_ID, col_types = "c")
  })

  observeEvent(input$submit, {
    req(input$gp, input$sesion, input$p1)
    top5 <- c(input$p1, input$p2, input$p3, input$p4, input$p5)
    if (any(duplicated(top5[top5 != ""]))) {
      showNotification("Error: Pilotos duplicados", type = "error")
      return()
    }

    # Regla de similitud 75%: comparar con porras de otros usuarios
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
              "\u26A0\uFE0F PORRA BLOQUEADA: Tu porra tiene ", coincidencias,
              " coincidencias con la de ", toupper(otras$usuario[k]),
              " (máximo permitido: ", max_coincidencias, "). ",
              "El reglamento prohíbe un 75% o más de similitud."
            ),
            type = "error",
            duration = NULL
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
      showNotification("Porra actualizada", type = "message")
    } else {
      sheet_append(SHEET_ID, new_row)
      showNotification("Porra enviada", type = "message")
    }
  })

  scores <- reactive({
    db <- get_db()
    if (nrow(db) == 0) {
      return(NULL)
    }
    unique_events <- unique(db[, c("gp", "sesion")])
    final_data <- list()
    withProgress(message = "Calculando...", {
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

  output$leaderboard_ui <- renderUI({
    req(scores())
    ranking <- scores() %>%
      group_by(usuario) %>%
      summarise(Total = sum(pts_total, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      mutate(Rank = row_number())
    tags$table(
      class = "table table-striped table-hover", style = "margin-bottom:0;",
      tags$thead(tags$tr(tags$th("#"), tags$th("PILOTO"), tags$th("PTS"))),
      tags$tbody(lapply(1:nrow(ranking), function(i) {
        row <- ranking[i, ]
        rank_display <- if (row$Rank == 1) icon("medal", class = "rank-1") else if (row$Rank == 2) icon("medal", class = "rank-2") else if (row$Rank == 3) icon("medal", class = "rank-3") else paste0(row$Rank, ".")
        tags$tr(tags$td(rank_display, style = "font-size:1.2em;"), tags$td(strong(toupper(row$usuario))), tags$td(span(class = "points-badge", sprintf("%.0f", row$Total))))
      }))
    )
  })

  output$gp_breakdown_ui <- renderUI({
    req(input$gp_stats, scores(), input$view_scope)

    # Filtro inicial por GP
    user_data <- scores() %>% filter(gp == input$gp_stats)

    # Filtro por Privacidad
    if (input$view_scope == "me") {
      user_data <- user_data %>% filter(usuario == res_auth$user)
    } else {
      # Si es "Todos", ordenamos: Primero YO, luego por puntos
      user_data <- user_data %>%
        arrange(desc(usuario == res_auth$user), desc(pts_total))
    }

    if (nrow(user_data) == 0) {
      return(div(style = "text-align:center; padding: 40px; color: #666;", icon("wind", "fa-3x"), br(), "Sin datos para este GP."))
    }

    tagList(
      lapply(1:nrow(user_data), function(i) {
        fila <- user_data[i, ]
        res_real <- get_results(fila$gp, fila$sesion, CURRENT_YEAR)

        # CABECERA DE USUARIO (Solo si vemos TODOS)
        user_header <- NULL
        if (input$view_scope == "all") {
          # Estilo especial si soy YO
          css_class <- if (fila$usuario == res_auth$user) "user-header-block user-header-me" else "user-header-block"
          user_header <- div(
            class = css_class,
            icon("user"), strong(toupper(fila$usuario)),
            style = "font-size: 0.9em; color: #fff;"
          )
        }

        if (!is.null(res_real)) {
          real_str <- paste(res_real$posiciones[1:5], collapse = " - ")
          mis_pilotos <- c(as.character(fila[["p1"]]), as.character(fila[["p2"]]), as.character(fila[["p3"]]), as.character(fila[["p4"]]), as.character(fila[["p5"]]))
          porra_str <- paste(mis_pilotos, collapse = " - ")

          extras_html <- NULL
          if (fila$sesion == "Carrera") {
            extras_html <- div(
              style = "margin-top:10px; font-size: 0.85em;",
              div(
                class = "row",
                div(class = "col-xs-6", span(class = "driver-badge vr-badge", "VR"), span(style = "color:#aaa", "Real:"), strong(res_real$vuelta_rapida %||% "-"), " | ", span(style = "color:#aaa", "Apu:"), strong(fila$vuelta_rapida %||% "-")),
                div(class = "col-xs-6", span(class = "driver-badge mazepin-badge", "MAZ"), span(style = "color:#aaa", "Real:"), strong(res_real$mazepin %||% "-"), " | ", span(style = "color:#aaa", "Apu:"), strong(fila$mazepin %||% "-"))
              )
            )
          }

          div(
            class = "session-row",
            user_header, # Insertamos cabecera de nombre aquí
            div(class = "session-title", icon("flag-checkered"), fila$sesion, span(style = "float:right; font-size:0.8em; color:#888;", paste(fila$pts_total, "pts"))),
            div(
              class = "comparison-box",
              div(class = "side-box", p(class = "vs-text", "RESULTADO"), p(style = "font-family:monospace; font-size:1.1em;", real_str)),
              div(style = "color:#444;", icon("chevron-right")),
              div(class = "side-box", style = "border: 1px solid #444;", p(class = "vs-text", "PORRA"), p(style = "font-family:monospace; font-size:1.1em; color: #fff;", porra_str))
            ),
            extras_html
          )
        } else {
          div(class = "session-row", user_header, h5(fila$sesion, style = "color:#888;"), p(icon("clock"), "Esperando resultados oficiales..."))
        }
      })
    )
  })
  `%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x
}

# ==============================================================================
# 6. LANZAR APP
# ==============================================================================

# 1. Definir la UI segura usando el idioma base "es"
ui_secure <- secure_app(ui, language = "es")

# 2. "Parchear" las traducciones con tus textos personalizados
# Esta función sobrescribe los textos por defecto
shinymanager::set_labels(
  language = "es",
  "Please authenticate" = "🏁 IDENTIFÍCATE",
  "Username" = "Código de piloto",
  "Password" = "Contraseña",
  "Login" = "🚦 SALIR A PISTA",
  "Logout" = "Entrar al box",
  "Incorrect user or password" = "❌ Error: credenciales no válidas",
  "User not authorized" = "⛔ Acceso denegado"
)

# 3. Lanzar el servidor
shinyApp(ui_secure, server)
