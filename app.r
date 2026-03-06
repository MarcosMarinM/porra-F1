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
DB_PATH <- Sys.getenv("F1_DB_PATH")
if (DB_PATH == "") DB_PATH <- "usuarios.sqlite"

SHEET_ID <- Sys.getenv("F1_SHEET_ID")
if (SHEET_ID == "") stop("ERROR: Falta configurar F1_SHEET_ID en el archivo .Renviron")

json_path <- Sys.getenv("F1_JSON_PATH")
if (json_path == "") json_path <- "f1-service-account.json"

# --- B. GOOGLE SHEETS & AUTENTICACIÓN ---
if (file.exists(json_path) && json_path != "") {
  gs4_auth(path = json_path)
} else {
  if (interactive()) options(gargle_oauth_email = TRUE)
}

# --- C. API ---
BASE_URL <- "https://api.openf1.org/v1"
CURRENT_YEAR <- 2026

cache_memoria <- cachem::cache_mem(max_age = 3600)

# --- D. TRADUCCIONES (opcional — si no existe translations.json, solo español) ---

# Cadenas por defecto en español (fallback sin fichero)
DEFAULT_STRINGS <- list(
  panel_title = "Panel de control", code_label = "Código:", data_source = "Datos oficiales de OpenF1.",
  tab_submit = "\U0001f4dd Enviar pronóstico", tab_standings = "\U0001f3c6 Clasificación", tab_rules = "\U0001f4dc Reglamento",

  new_porra = "Nueva porra", gp_label = "Gran Premio", session_label = "Sesión",
  session_sprint_quali = "Clasificación del esprint", session_sprint = "Esprint",
  session_quali = "Clasificación", session_race = "Carrera",
  top5_label = "top-5", pos_1 = "1º", pos_2 = "2º", pos_3 = "3º", pos_4 = "4º", pos_5 = "5º",
  extras_label = "Extras", fast_lap = "\U0001f680 Vuelta rápida", mazepin_prize = "\U0001f422 Premio Mazepin",
  submit_btn = "ENVIAR PORRA",
  world_title = "Mundial", analysis_title = "Análisis GP", select_gp = "Seleccionar Gran Premio:",
  scope_me = "Solo yo", scope_all = "Todos",
  col_rank = "#", col_pilot = "PILOTO", col_pts = "PTS",
  no_data = "Sin datos para este GP.", result_label = "RESULTADO", porra_label = "PORRA",
  waiting_results = "Esperando resultados oficiales...", pts_suffix = "pts",
  scoring_title = "Puntuación",
  scoring_intro = "A continuación, se resume la obtención de puntos para cada una de las sesiones del fin de semana. Recuerda que para acertar la posición exacta, el piloto de tu porra debe coincidir exactamente con la posición final real. El semiacierto se da cuando tu piloto puntúa en el top-5, pero en un puesto distinto al que predijiste.",
  rule_race = " 6 puntos por posición exacta | 2 puntos por semiacierto | 3 puntos extra por acertar al Premio Mazepin | 1 punto extra por acertar la vuelta rápida",
  rule_quali = " 3 puntos por posición exacta | 1 punto por semiacierto",
  rule_sprint = " 3 puntos por posición exacta | 1 punto por semiacierto",
  rule_sprint_quali = " 1 punto por posición exacta (no hay puntos por semiacierto)",
  read_full_rules = "Leer el reglamento oficial completo",
  err_duplicate = "Error: Pilotos duplicados",
  err_similarity_prefix = "\u26a0\ufe0f PORRA BLOQUEADA: Tu porra tiene ",
  err_similarity_mid = " coincidencias con la de ",
  err_similarity_max = " (máximo permitido: ",
  err_similarity_suffix = "). El reglamento prohíbe un 75 % o más de similitud.",
  notify_updated = "Porra actualizada", notify_submitted = "Porra enviada",
  calculating = "Calculando...",
  session_race_label = "Carrera:", session_quali_label = "Clasificación:",
  session_sprint_label = "Esprint:", session_sprint_quali_label = "Clasificación del esprint:",
  current_gp = "\U0001f3ce\ufe0f GP actual:"
)

# Cargar traducciones si existe el fichero
TRANSLATIONS <- NULL
LANG_CONFIG <- NULL
MULTILANG <- FALSE

if (file.exists("translations.json")) {
  raw <- jsonlite::fromJSON("translations.json")
  if (!is.null(raw[["_config"]])) {
    LANG_CONFIG <- raw[["_config"]]
    raw[["_config"]] <- NULL
  }
  TRANSLATIONS <- raw
  # Multiidioma solo si hay más de un idioma configurado
  if (!is.null(LANG_CONFIG) && length(LANG_CONFIG$languages) > 1) {
    MULTILANG <- TRUE
  }
}

# Idioma por defecto
DEFAULT_LANG <- if (!is.null(LANG_CONFIG)) LANG_CONFIG[["default"]] else "es"

tr <- function(key, lang = DEFAULT_LANG) {
  if (!is.null(TRANSLATIONS) && !is.null(TRANSLATIONS[[key]])) {
    val <- TRANSLATIONS[[key]][[lang]]
    if (!is.null(val)) return(val)
    # Fallback al idioma por defecto si falta la traducción
    val <- TRANSLATIONS[[key]][[DEFAULT_LANG]]
    if (!is.null(val)) return(val)
  }
  # Último fallback: cadena por defecto en español
  val <- DEFAULT_STRINGS[[key]]
  if (!is.null(val)) return(val)
  paste0("[", key, "]")
}

# Valores internos de sesión (siempre en castellano para Google Sheets)
SESSION_VALUES <- c("Clasificación del esprint", "Esprint", "Clasificación", "Carrera")

session_display <- function(lang) {
  labels <- c(
    tr("session_sprint_quali", lang), tr("session_sprint", lang),
    tr("session_quali", lang), tr("session_race", lang)
  )
  stats::setNames(SESSION_VALUES, labels)
}

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
      if (length(resp) == 0) return(NULL)
      return(as.data.frame(resp))
    },
    error = function(e) return(NULL)
  )
}

get_drivers_raw <- function(year) {
  sessions <- fetch_api("/sessions", list(year = year))
  if (is.null(sessions) || nrow(sessions) == 0) return(NULL)
  recent_sessions <- tail(sessions$session_key, 10)
  all_drivers <- c()
  for (sk in recent_sessions) {
    drivers <- fetch_api("/drivers", list(session_key = sk))
    if (!is.null(drivers) && nrow(drivers) > 0) {
      all_drivers <- c(all_drivers, drivers$name_acronym)
    }
  }
  if (length(all_drivers) == 0) return(NULL)
  sort(unique(all_drivers[!is.na(all_drivers)]))
}
get_drivers <- memoise(get_drivers_raw, cache = cache_memoria)

get_calendar_raw <- function(year) {
  meetings <- fetch_api("/meetings", list(year = year))
  if (is.null(meetings) || nrow(meetings) == 0) return(NULL)
  meetings <- meetings %>% arrange(date_start)
  unique(meetings$meeting_name)
}
get_calendar <- memoise(get_calendar_raw, cache = cache_memoria)

# Detectar GP en curso: última sesión disputada que NO sea Race
get_current_gp_raw <- function(year) {
  sessions <- fetch_api("/sessions", list(year = year))
  if (is.null(sessions) || nrow(sessions) == 0) return(NULL)
  meetings <- fetch_api("/meetings", list(year = year))
  if (is.null(meetings) || nrow(meetings) == 0) return(NULL)

  sessions$date_end_utc <- as.POSIXct(sessions$date_end, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  past <- sessions %>%
    filter(date_end_utc < Sys.time()) %>%
    arrange(desc(date_end_utc))
  if (nrow(past) == 0) return(NULL)

  last <- past[1, ]
  # Si la última sesión disputada es una carrera (Race), el GP ha terminado
  if (last$session_name == "Race") return(NULL)

  # Buscar nombre del meeting
  meeting <- meetings %>% filter(meeting_key == last$meeting_key)
  if (nrow(meeting) == 0) return(NULL)
  meeting$meeting_name[1]
}
get_current_gp <- memoise(get_current_gp_raw, cache = cachem::cache_mem(max_age = 1800))

get_results_raw <- function(gp_name, session_name, year) {
  gp_clean <- trimws(gp_name)
  session_clean <- trimws(session_name)
  meetings <- fetch_api("/meetings", list(year = year, meeting_name = gp_clean))
  if (is.null(meetings)) return(NULL)
  keys <- unique(meetings$meeting_key)
  all_sessions_list <- lapply(keys, function(k) fetch_api("/sessions", list(meeting_key = k)))
  all_sessions <- bind_rows(all_sessions_list)
  if (is.null(all_sessions) || nrow(all_sessions) == 0) return(NULL)

  target_session <- NULL
  if (session_clean == "Carrera") {
    target_session <- all_sessions %>% dplyr::filter(session_name == "Race")
  } else if (session_clean == "Clasificación") {
    target_session <- all_sessions %>% dplyr::filter(session_name == "Qualifying")
  } else if (session_clean == "Esprint") {
    target_session <- all_sessions %>% dplyr::filter(session_name == "Sprint")
  } else if (session_clean == "Clasificación del esprint") {
    target_session <- all_sessions %>% dplyr::filter(session_name %in% c("Sprint Qualifying", "Sprint Shootout"))
  }
  if (is.null(target_session) || nrow(target_session) == 0) return(NULL)

  s_key <- tail(target_session, 1)$session_key
  api_actual_name <- tail(target_session, 1)$session_name
  drivers <- fetch_api("/drivers", list(session_key = s_key))
  if (is.null(drivers)) return(NULL)
  d_map <- drivers %>% dplyr::select(driver_number, name_acronym) %>% distinct()

  res <- fetch_api("/session_result", list(session_key = s_key))
  if (is.null(res)) return(NULL)

  full_res <- res %>% left_join(d_map, by = "driver_number") %>% arrange(position)
  top5_real <- full_res$name_acronym
  vr_val <- NA
  maz_val <- NA

  if (api_actual_name == "Race") {
    laps <- fetch_api("/laps", list(session_key = s_key))
    if (!is.null(laps) && nrow(laps) > 0) {
      fastest <- laps %>% filter(!is.na(lap_duration)) %>% arrange(lap_duration) %>% slice(1)
      vr_driver <- d_map %>% filter(driver_number == fastest$driver_number)
      if (nrow(vr_driver) > 0) vr_val <- vr_driver$name_acronym[1]
    }
    rc <- fetch_api("/race_control", list(session_key = s_key, category = "Retirement"))
    if (!is.null(rc) && nrow(rc) > 0) {
      first_out <- rc %>% arrange(date) %>% slice(1)
      maz_driver <- d_map %>% filter(driver_number == first_out$driver_number)
      if (nrow(maz_driver) > 0) maz_val <- maz_driver$name_acronym[1]
    }
    if (is.na(maz_val)) {
      dnfs <- full_res %>% filter(dnf == TRUE) %>% arrange(number_of_laps)
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
    pts_exacto <- 3; pts_parcial <- 1
  } else if (porra$sesion == "Carrera") {
    pts_exacto <- 6; pts_parcial <- 2
  } else if (porra$sesion == "Clasificación del esprint") {
    pts_exacto <- 1; pts_parcial <- 0
  } else if (porra$sesion == "Esprint") {
    pts_exacto <- 3; pts_parcial <- 1
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

# Generar los botones de idioma dinámicamente desde _config
build_lang_switcher <- function() {
  if (!MULTILANG) return(NULL)
  lang_codes <- names(LANG_CONFIG$languages)
  buttons <- lapply(lang_codes, function(code) {
    info <- LANG_CONFIG$languages[[code]]
    is_default <- (code == DEFAULT_LANG)
    span(
      id = paste0("wrap_", code),
      class = paste("lang-wrapper", if (is_default) "lang-active" else ""),
      actionLink(
        paste0("lang_", code),
        label = tags$img(src = info$flag, alt = info$label)
      )
    )
  })
  div(class = "lang-switcher", buttons)
}

ui <- fluidPage(
  theme = bs_theme(
    bg = "#121212", fg = "#e0e0e0", primary = "#e10600",
    base_font = font_google("Titillium Web"),
    heading_font = font_google("Titillium Web")
  ),
  tags$head(
    tags$style(HTML("
      body { background-color: #000000; }
      .card { background-color: #1e1e1e; border: 1px solid #333; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.3); }
      .card-header { background-color: #2c2c2c; border-bottom: 2px solid #e10600; font-weight: bold; text-transform: uppercase; letter-spacing: 1px; color: #fff; }
      .table { color: #ddd; }
      .table-striped tbody tr:nth-of-type(odd) { background-color: rgba(255,255,255,0.05); }
      .rank-1 { color: #FFD700; } .rank-2 { color: #C0C0C0; } .rank-3 { color: #CD7F32; }
      .points-badge { background-color: #e10600; color: white; padding: 3px 8px; border-radius: 10px; font-weight: bold; font-size: 0.9em; }
      .driver-badge { display: inline-block; padding: 2px 6px; border-radius: 4px; background: #333; border-left: 3px solid #e10600; font-family: monospace; margin-right: 5px; font-weight: bold;}
      .mazepin-badge { border-left: 3px solid #FFD700 !important; color: #FFD700 !important; }
      .vr-badge { border-left: 3px solid #bf00ff !important; color: #bf00ff !important; }
      .session-row { border-bottom: 1px solid #333; padding: 15px 0; }
      .session-title { font-size: 1.1em; color: #e10600; font-weight: bold; margin-bottom: 10px; }
      .comparison-box { display: flex; justify-content: space-between; align-items: center; }
      .side-box { flex: 1; padding: 10px; background: rgba(255,255,255,0.03); border-radius: 5px; margin: 0 5px; }
      .vs-text { color: #555; font-weight: bold; font-size: 0.8em; }
      .user-header-block { background-color: #2a2a2a; padding: 5px 10px; border-radius: 4px; display: inline-block; margin-bottom: 10px; border-left: 4px solid #888; }
      .user-header-me { border-left: 4px solid #e10600; background-color: #3a1010; }

      /* Selector de idioma */
      .lang-switcher { display: inline-block; margin-left: 20px; vertical-align: middle; }
      .lang-wrapper { display: inline-block; margin: 0 4px; padding: 3px; border: 2px solid transparent; border-radius: 4px; opacity: 0.45; transition: all 0.2s; cursor: pointer; vertical-align: middle; }
      .lang-wrapper:hover { opacity: 0.75; }
      .lang-wrapper.lang-active { opacity: 1; border-color: #e10600; }
      .lang-wrapper img { height: 22px; display: block; }
      .lang-wrapper .action-button { border: none; background: none; padding: 0; }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle_lang', function(lang) {
        document.querySelectorAll('.lang-wrapper').forEach(function(el) {
          el.classList.remove('lang-active');
        });
        var active = document.getElementById('wrap_' + lang);
        if (active) active.classList.add('lang-active');
      });
    "))
  ),

  # --- HEADER ---
  div(
    style = "padding: 20px 0; border-bottom: 4px solid #e10600; margin-bottom: 20px;",
    fluidRow(column(12, align = "center",
      img(src = "https://upload.wikimedia.org/wikipedia/commons/3/33/F1.svg", height = "40px"),
      h2("LA CARRERA MÁS SURREALIST", style = "display:inline; margin-left: 15px; font-weight:800; color: #fff;"),
      build_lang_switcher()
    ))
  ),

  # --- LAYOUT ---
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "card", style = "padding: 15px;", uiOutput("sidebar_content"))
    ),
    mainPanel(
      width = 9,
      uiOutput("main_tabs")
    )
  )
)

# ==============================================================================
# 5. SERVIDOR
# ==============================================================================

server <- function(input, output, session) {
  res_auth <- secure_server(
    check_credentials = check_credentials(db = DB_PATH)
  )

  lang <- reactiveVal(DEFAULT_LANG)
  t <- function(key) tr(key, lang())

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
      ),
      tabPanel(
        t("tab_standings"), br(),
        fluidRow(
          column(5, div(class = "card",
            div(class = "card-header", icon("trophy"), t("world_title")),
            div(class = "card-body", style = "padding: 0;", uiOutput("leaderboard_ui"))
          )),
          column(7, div(class = "card",
            div(class = "card-header", icon("chart-bar"), t("analysis_title")),
            div(
              class = "card-body", style = "padding: 15px;",
              selectInput("gp_stats", t("select_gp"), choices = NULL, width = "100%"),
              div(
                style = "text-align:center; margin-bottom:10px;",
                radioButtons("view_scope", label = NULL,
                  choices = stats::setNames(c("me", "all"), c(t("scope_me"), t("scope_all"))),
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
      pil <- c("NOR", "PIA", "ANT", "RUS", "VER", "HAD", "LEC", "HAM", "ALB", "SAI", "LAW", "LIN", "ALO", "STR", "OCO", "BEA", "BOR", "HUL", "GAS", "COL", "PER", "BOT")
    }
    lapply(c("p1", "p2", "p3", "p4", "p5", "vr", "maz"), function(x) updateSelectInput(session, x, choices = sort(c("", pil))))
  })

  # --- ENVIAR PORRA ---
  get_db <- reactive({
    input$submit
    req(res_auth$user)
    read_sheet(SHEET_ID, col_types = "c")
  })

  observeEvent(input$submit, {
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
  output$leaderboard_ui <- renderUI({
    req(scores())
    ranking <- scores() %>%
      group_by(usuario) %>%
      summarise(Total = sum(pts_total, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      mutate(Rank = row_number())
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
      user_data <- user_data %>% arrange(desc(usuario == res_auth$user), desc(pts_total))
    }
    if (nrow(user_data) == 0) {
      return(div(style = "text-align:center; padding: 40px; color: #666;", icon("wind", "fa-3x"), br(), t("no_data")))
    }

    tagList(
      lapply(1:nrow(user_data), function(i) {
        fila <- user_data[i, ]
        res_real <- get_results(fila$gp, fila$sesion, CURRENT_YEAR)
        user_header <- NULL
        if (input$view_scope == "all") {
          css_class <- if (fila$usuario == res_auth$user) "user-header-block user-header-me" else "user-header-block"
          user_header <- div(class = css_class, icon("user"), strong(toupper(fila$usuario)), style = "font-size: 0.9em; color: #fff;")
        }
        if (!is.null(res_real)) {
          real_str <- paste(res_real$posiciones[1:5], collapse = " - ")
          mis_pilotos <- c(as.character(fila[["p1"]]), as.character(fila[["p2"]]), as.character(fila[["p3"]]), as.character(fila[["p4"]]), as.character(fila[["p5"]]))
          porra_str <- paste(mis_pilotos, collapse = " - ")
          extras_html <- NULL
          if (fila$sesion == "Carrera") {
            extras_html <- div(
              style = "margin-top:10px; font-size: 0.85em;",
              div(class = "row",
                div(class = "col-xs-6", span(class = "driver-badge vr-badge", "VR"), span(style = "color:#aaa", "Real:"), strong(res_real$vuelta_rapida %||% "-"), " | ", span(style = "color:#aaa", "Apu:"), strong(fila$vuelta_rapida %||% "-")),
                div(class = "col-xs-6", span(class = "driver-badge mazepin-badge", "MAZ"), span(style = "color:#aaa", "Real:"), strong(res_real$mazepin %||% "-"), " | ", span(style = "color:#aaa", "Apu:"), strong(fila$mazepin %||% "-"))
              )
            )
          }
          div(
            class = "session-row", user_header,
            div(class = "session-title", icon("flag-checkered"), translate_session_name(fila$sesion),
              span(style = "float:right; font-size:0.8em; color:#888;", paste(fila$pts_total, t("pts_suffix")))),
            div(class = "comparison-box",
              div(class = "side-box", p(class = "vs-text", t("result_label")), p(style = "font-family:monospace; font-size:1.1em;", real_str)),
              div(style = "color:#444;", icon("chevron-right")),
              div(class = "side-box", style = "border: 1px solid #444;", p(class = "vs-text", t("porra_label")), p(style = "font-family:monospace; font-size:1.1em; color: #fff;", porra_str))
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
}

# ==============================================================================
# 6. LANZAR APP
# ==============================================================================

ui_secure <- secure_app(ui, language = "es")

shinymanager::set_labels(
  language = "es",
  "Please authenticate" = "\U0001f3c1 IDENTIFÍCATE",
  "Username" = "Código de piloto",
  "Password" = "Contraseña",
  "Login" = "\U0001f6a6 SALIR A PISTA",
  "Logout" = "Entrar al box",
  "Incorrect user or password" = "\u274c Error: credenciales no válidas",
  "User not authorized" = "\u26d4 Acceso denegado"
)

shinyApp(ui_secure, server)
