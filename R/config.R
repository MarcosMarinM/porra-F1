# ==============================================================================
# CONFIGURACIÓN: variables de entorno, constantes, traducciones, colores
# ==============================================================================

# --- A. CONFIGURACIÓN MEDIANTE VARIABLES DE ENTORNO ---
DB_PATH <- Sys.getenv("F1_DB_PATH")
if (DB_PATH == "") DB_PATH <- "usuarios.sqlite"

SHEET_ID <- Sys.getenv("F1_SHEET_ID")
if (SHEET_ID == "") stop("ERROR: Falta configurar F1_SHEET_ID en el archivo .Renviron")

json_path <- Sys.getenv("F1_JSON_PATH")
if (json_path == "") json_path <- "f1-service-account.json"

# Preferir credenciales inyectadas desde entorno (por ejemplo Render env var)
json_env <- Sys.getenv("F1_SERVICE_ACCOUNT_JSON")
if (json_env != "") {
  # Permite enviar JSON plano o base64
  if (!file.exists(json_path)) {
    json_content <- if (grepl("^\\s*\\{", json_env)) {
      json_env
    } else {
      if (!requireNamespace("base64enc", quietly = TRUE)) stop("Package base64enc is required to decode F1_SERVICE_ACCOUNT_JSON")
      rawToChar(base64enc::base64decode(json_env))
    }
    writeLines(json_content, con = json_path)
  }
}

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

# --- D. TRADUCCIONES ---

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
  current_gp = "\U0001f3ce\ufe0f GP actual:",
  tab_evolution = "\U0001f4c8 Evolución",
  tab_awards = "\U0001f3c5 Premios",
  tab_heatmap = "\U0001f525 Heatmap",
  evolution_title = "Evolución del campeonato",
  awards_title = "Premios divertidos",
  heatmap_title = "Mapa de predicciones",
  all_sessions = "Todas las sesiones",
  award_oracle = "Oráculo",
  award_oracle_desc = "Mejor puntuación en un solo GP",
  award_sniper = "Francotirador",
  award_sniper_desc = "Más posiciones exactas acertadas",
  award_star_session = "Sesión estelar",
  award_star_session_desc = "Mejor puntuación en una sola sesión",
  award_forgettable = "GP para olvidar",
  award_forgettable_desc = "Peor puntuación en un GP",
  award_mazepin_king = "Rey Mazepin",
  award_mazepin_king_desc = "Más premios Mazepin acertados",
  award_peoples_driver = "Piloto del pueblo",
  award_peoples_driver_desc = "El piloto más apostado por todos",
  award_picks = "apuestas",
  award_crystal_ball = "Bola de cristal",
  award_crystal_ball_desc = "Más veces acertando el P1",
  award_bigmouth = "Bocachancla",
  award_bigmouth_desc = "Más sesiones con 0 puntos",
  award_consistent = "Mr. Consistente",
  award_consistent_desc = "Menor variación de puntos entre GPs",
  award_copycat = "Borreguismo",
  award_copycat_desc = "La pareja que más coincide en sus predicciones",
  award_gp_king = "Rey del GP",
  award_gp_king_desc = "Más victorias de GP",
  award_gp_wins = "victorias",
  award_hot_streak = "Racha imparable",
  award_hot_streak_desc = "Mayor racha de victorias consecutivas de sesión",
  award_sessions_suffix = "sesiones",
  guest_notice = "Est\u00e1s como invitado. Puedes ver todo, pero no enviar pron\u00f3sticos.",
  award_pending = "Pendiente\u2026",
  award_nobody = "Nadie (a\u00fan)",
  no_awards_data = "No hay datos suficientes para los premios.",
  tab_history = "\U0001f4cb Historial",
  history_title = "Historial de predicciones",
  no_history_data = "Selecciona un GP y una sesión para ver las predicciones.",
  screenshot_btn = "Capturar"
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
  if (!is.null(LANG_CONFIG) && length(LANG_CONFIG$languages) > 1) {
    MULTILANG <- TRUE
  }
}

DEFAULT_LANG <- if (!is.null(LANG_CONFIG)) LANG_CONFIG[["default"]] else "es"

tr <- function(key, lang = DEFAULT_LANG) {
  if (!is.null(TRANSLATIONS) && !is.null(TRANSLATIONS[[key]])) {
    val <- TRANSLATIONS[[key]][[lang]]
    if (!is.null(val)) return(val)
    val <- TRANSLATIONS[[key]][[DEFAULT_LANG]]
    if (!is.null(val)) return(val)
  }
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

# --- E. COLORES POR EQUIPO (parrilla 2026) ---
TEAM_COLOURS_2026 <- list(
  NOR = "#F47600", PIA = "#F47600",
  ANT = "#00D7B6", RUS = "#00D7B6",
  VER = "#4781D7", HAD = "#4781D7",
  LEC = "#ED1131", HAM = "#ED1131",
  ALB = "#1868DB", SAI = "#1868DB",
  LAW = "#6C98FF", LIN = "#6C98FF",
  ALO = "#229971", STR = "#229971",
  OCO = "#9C9FA2", BEA = "#9C9FA2",
  BOR = "#F50537", HUL = "#F50537",
  GAS = "#00A1E8", COL = "#00A1E8",
  PER = "#909090", BOT = "#909090"
)
