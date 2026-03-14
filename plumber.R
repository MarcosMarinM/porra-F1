# plumber API para Porra F1
# - Autenticación básica + token (guest + credenciales SQLite)
# - Usa la misma lógica de puntuación y datos que la app Shiny original

library(plumber)
library(dplyr)
library(googlesheets4)
library(jsonlite)
library(memoise)
library(cachem)
library(uuid)
library(shinymanager)

source("R/config.R")
source("R/api.R")
source("R/scoring.R")

# Asegúrate de que exista el fichero de credenciales
if (!file.exists(DB_PATH) && file.exists("usuarios.template.sqlite")) {
  file.copy("usuarios.template.sqlite", DB_PATH)
}

# Token store en memoria (reinicia al reiniciar el servicio)
auth_tokens <- new.env(parent = emptyenv())
token_ttl <- 60 * 60 * 24 * 7  # 7 días

make_token <- function(user) {
  t <- uuid::UUIDgenerate()
  auth_tokens[[t]] <- list(user = user, created = Sys.time())
  t
}

validate_token <- function(token) {
  if (is.null(token) || token == "") return(NULL)
  info <- auth_tokens[[token]]
  if (is.null(info)) return(NULL)
  if (difftime(Sys.time(), info$created, units = "secs") > token_ttl) {
    rm(list = token, envir = auth_tokens)
    return(NULL)
  }
  info$user
}

get_user_from_request <- function(req) {
  auth <- req$HTTP_AUTHORIZATION
  if (!is.null(auth) && grepl("^Bearer ", auth)) {
    token <- sub("^Bearer ", "", auth)
    return(validate_token(token))
  }
  NULL
}

require_auth <- function(req, res) {
  user <- get_user_from_request(req)
  if (is.null(user)) {
    res$status <- 401
    return(NULL)
  }
  user
}

# Compatibilidad con shinymanager (sqlite)
db_check <- shinymanager::check_credentials(db = DB_PATH)

# --- Helpers --------------------------------------------------------------

# Reproduce la lógica de la app Shiny para calcular puntos por todas las porras
compute_all_scores <- function(db) {
  if (nrow(db) == 0) return(db)
  unique_events <- unique(db[, c("gp", "sesion")])
  final_data <- list()
  for (i in seq_len(nrow(unique_events))) {
    g <- unique_events$gp[i]
    s <- unique_events$sesion[i]
    real_res <- get_results(g, s, CURRENT_YEAR)
    bets <- db %>% filter(gp == g, sesion == s)
    if (!is.null(real_res)) {
      for (j in seq_len(nrow(bets))) {
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
  if (length(final_data) > 0) bind_rows(final_data) else db
}

# Lógica de validación de porra (duplicados y similitud)
validate_prediction <- function(user, input) {
  required <- c("gp", "sesion", "p1")
  for (r in required) {
    if (is.null(input[[r]]) || input[[r]] == "") {
      return(paste0("Falta el campo ", r))
    }
  }

  top5 <- c(input$p1, input$p2, input$p3, input$p4, input$p5)
  if (any(duplicated(top5[top5 != ""]))) {
    return("Error: Pilotos duplicados")
  }

  db <- read_sheet(SHEET_ID, col_types = "c")
  otras <- db %>% filter(gp == input$gp, sesion == input$sesion, usuario != user)
  if (nrow(otras) > 0) {
    es_carrera <- input$sesion == "Carrera"
    max_coincidencias <- if (es_carrera) 5 else 3
    for (k in seq_len(nrow(otras))) {
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
        return(paste0(
          "PORRA BLOQUEADA: Tu porra tiene ", coincidencias,
          " coincidencias con ", toupper(otras$usuario[k]),
          " (máximo permitido: ", max_coincidencias, ")"
        ))
      }
    }
  }
  NULL
}

# --- CORS (para frontend en otro dominio) --------------------------------
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

# --- Fix: quitar Content-Length erróneo para archivos estáticos ------------
# (Permite que el navegador reciba el JS completo aunque el header sea incorrecto)
#* @filter fix_content_length
function(req, res) {
  plumber::forward()
  if (!is.null(res$headers$`Content-Length`)) {
    res$setHeader("Content-Length", NULL)
  }
  res
}

# --- Endpoints -----------------------------------------------------------
#* @get /health
function() {
  list(status = "ok", time = as.character(Sys.time()))
}

#* @post /login
#* @param user
#* @param pwd
function(req, res, user = NULL, pwd = NULL) {
  if (is.null(user) || is.null(pwd)) {
    res$status <- 400
    return(list(success = FALSE, message = "user and pwd required"))
  }

  if (tolower(user) == "guest" && pwd == "guest") {
    token <- make_token("guest")
    return(list(success = TRUE, token = as.character(token), user = "guest", admin = FALSE))
  }

  auth <- db_check(user, pwd)
  if (isTRUE(auth$result)) {
    token <- make_token(auth$user_info$user)
    return(list(
      success = TRUE,
      token = as.character(token),
      user = as.character(auth$user_info$user),
      admin = as.logical(auth$user_info$admin)
    ))
  }

  res$status <- 401
  list(success = FALSE, message = "Invalid credentials")
}

#* @get /calendar
function(req, res) {
  user <- require_auth(req, res); if (is.null(user)) return(list(success = FALSE))
  list(success = TRUE, data = get_calendar(CURRENT_YEAR))
}

#* @get /drivers
function(req, res) {
  user <- require_auth(req, res); if (is.null(user)) return(list(success = FALSE))
  list(success = TRUE, data = get_drivers(CURRENT_YEAR))
}

#* @get /leaderboard
function(req, res) {
  user <- require_auth(req, res); if (is.null(user)) return(list(success = FALSE))
  db <- read_sheet(SHEET_ID, col_types = "c")
  if (nrow(db) == 0) return(list(success = TRUE, data = list()))
  scores <- compute_all_scores(db)
  ranking <- compute_ranking(scores)
  list(success = TRUE, data = ranking)
}

#* @post /prediction
#* @param gp
#* @param sesion
#* @param p1
#* @param p2
#* @param p3
#* @param p4
#* @param p5
#* @param vr
#* @param maz
function(req, res, gp = NULL, sesion = NULL, p1 = NULL, p2 = NULL, p3 = NULL, p4 = NULL, p5 = NULL, vr = NULL, maz = NULL) {
  user <- require_auth(req, res); if (is.null(user)) return(list(success = FALSE))

  input <- list(gp = gp, sesion = sesion, p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5, vr = vr, maz = maz)
  msg <- validate_prediction(user, input)
  if (!is.null(msg)) {
    res$status <- 400
    return(list(success = FALSE, message = msg))
  }

  new_row <- data.frame(
    usuario = user, gp = gp, sesion = sesion,
    p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5,
    vuelta_rapida = if (sesion == "Carrera") vr else NA,
    mazepin = if (sesion == "Carrera") maz else NA,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  db <- read_sheet(SHEET_ID, col_types = "c")
  existing <- which(db$usuario == user & db$gp == gp & db$sesion == sesion)
  if (length(existing) > 0) {
    range_write(SHEET_ID, new_row, range = paste0("A", existing[1] + 1), col_names = FALSE)
    return(list(success = TRUE, message = "updated"))
  }

  sheet_append(SHEET_ID, new_row)
  list(success = TRUE, message = "created")
}

#* @plumber
function(pr) {
  # Evitar que plumber devuelva arrays para valores escalares (por ejemplo token/user)
  # y evitar errores de OpenAPI al construir documentación.
  pr %>%
    pr_set_docs(FALSE) %>%
    pr_set_serializer(serializer_unboxed_json()) %>%
    pr_static("/", "www")
}
