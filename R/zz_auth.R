# ==============================================================================
# AUTENTICACIÓN PERSISTENTE ("Recordarme")
# ==============================================================================
# shinymanager guarda los tokens solo en memoria. Para que el login sobreviva
# a reinicios del proceso guardamos los tokens en SQLite y los registramos en
# shinymanager al arrancar. La cookie se lee en el servidor antes de renderizar
# la UI, de modo que no hace falta redirigir desde el navegador.

AUTH_COOKIE <- "f1porra_token"

auth_con <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = DB_PATH)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

ensure_remember_table <- function() {
  con <- auth_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS remember_tokens (
      token TEXT PRIMARY KEY,
      user TEXT NOT NULL,
      admin INTEGER NOT NULL DEFAULT 0,
      created_at TEXT NOT NULL,
      expires_at TEXT NOT NULL
    )
  ")
  invisible(TRUE)
}

remember_time <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

save_remember_token <- function(token, user, admin = FALSE, days = REMEMBER_DAYS) {
  if (is.null(token) || !nzchar(token) || is.null(user) || !nzchar(user)) return(FALSE)
  tryCatch(
    {
      ensure_remember_table()
      now <- Sys.time()
      con <- auth_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbExecute(
        con,
        "INSERT OR REPLACE INTO remember_tokens (token, user, admin, created_at, expires_at)
         VALUES (?, ?, ?, ?, ?)",
        params = list(
          token, user, as.integer(isTRUE(admin)),
          remember_time(now), remember_time(now + days * 86400)
        )
      )
      TRUE
    },
    error = function(e) {
      warning("No se pudo guardar el token persistente: ", conditionMessage(e), call. = FALSE)
      FALSE
    }
  )
}

lookup_remember_token <- function(token) {
  if (is.null(token) || !nzchar(token)) return(NULL)
  tryCatch(
    {
      con <- auth_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      row <- DBI::dbGetQuery(
        con,
        "SELECT user, admin, expires_at FROM remember_tokens WHERE token = ?",
        params = list(token)
      )
      if (nrow(row) == 0) return(NULL)
      expires <- as.POSIXct(row$expires_at[1], tz = "UTC")
      if (is.na(expires) || expires < Sys.time()) {
        DBI::dbExecute(
          con,
          "DELETE FROM remember_tokens WHERE token = ?",
          params = list(token)
        )
        return(NULL)
      }
      list(user = row$user[1], admin = isTRUE(row$admin[1] == 1))
    },
    error = function(e) {
      warning("No se pudo leer el token persistente: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

delete_remember_token <- function(token) {
  if (is.null(token) || !nzchar(token)) return(invisible(FALSE))
  tryCatch(
    {
      con <- auth_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbExecute(
        con,
        "DELETE FROM remember_tokens WHERE token = ?",
        params = list(token)
      )
    },
    error = function(e) {
      warning("No se pudo borrar el token persistente: ", conditionMessage(e), call. = FALSE)
    }
  )
  invisible(TRUE)
}

purge_expired_tokens <- function() {
  tryCatch(
    {
      con <- auth_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbExecute(
        con,
        "DELETE FROM remember_tokens WHERE expires_at < ?",
        params = list(remember_time(Sys.time()))
      )
    },
    error = function(e) {
      warning("No se pudieron purgar los tokens caducados: ", conditionMessage(e), call. = FALSE)
    }
  )
  invisible(TRUE)
}

load_remember_tokens <- function() {
  if (!file.exists(DB_PATH)) {
    warning(
      "No se encontró la base de datos de usuarios (", DB_PATH, "). ",
      "Créala con shinymanager::create_db().",
      call. = FALSE
    )
    return(invisible(0L))
  }
  tryCatch(
    {
      ensure_remember_table()
      purge_expired_tokens()
      con <- auth_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      rows <- DBI::dbGetQuery(con, "SELECT token, user, admin FROM remember_tokens")
      for (i in seq_len(nrow(rows))) {
        shinymanager:::.tok$add(
          rows$token[i],
          list(user = rows$user[i], admin = isTRUE(rows$admin[i] == 1))
        )
      }
      invisible(nrow(rows))
    },
    error = function(e) {
      warning("No se pudieron cargar los tokens persistentes: ", conditionMessage(e), call. = FALSE)
      invisible(0L)
    }
  )
}

auth_cookie_token <- function(request) {
  cookie <- request$HTTP_COOKIE
  if (is.null(cookie) || length(cookie) == 0 || !nzchar(cookie)) return(NULL)

  pattern <- paste0("(?:^|;\\s*)", AUTH_COOKIE, "=([^;]*)")
  match <- regmatches(cookie, regexpr(pattern, cookie, perl = TRUE))
  if (length(match) == 0) return(NULL)

  value <- sub(paste0("^.*", AUTH_COOKIE, "="), "", match)
  value <- tryCatch(utils::URLdecode(value), error = function(e) value)
  value <- gsub('"', "", value)
  if (!nzchar(value)) NULL else value
}

inject_auth_token <- function(request) {
  qs <- request$QUERY_STRING
  if (is.null(qs)) qs <- ""

  query <- tryCatch(shiny::parseQueryString(qs), error = function(e) list())
  query_token <- query$token
  if (!is.null(query_token)) {
    query_token <- query_token[1]
    query_token <- tryCatch(utils::URLdecode(query_token), error = function(e) query_token)
    query_token <- gsub('"', "", query_token)
  }

  candidates <- c(query_token, auth_cookie_token(request))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]

  token <- NULL
  for (candidate in candidates) {
    if (shinymanager:::.tok$is_valid_server(candidate)) {
      token <- candidate
      break
    }
    info <- lookup_remember_token(candidate)
    if (!is.null(info)) {
      shinymanager:::.tok$add(
        candidate,
        list(user = info$user, admin = info$admin)
      )
      token <- candidate
      break
    }
  }

  if (is.null(token)) return(list(request = request, token = query_token))

  qs <- gsub("(^|&)token=[^&]*", "", qs)
  qs <- gsub("^&|&$", "", qs)
  request$QUERY_STRING <- paste0(
    qs, if (nzchar(qs)) "&" else "",
    "token=", utils::URLencode(token, reserved = TRUE)
  )

  list(request = request, token = token)
}

with_remember_cookie <- function(app_ui) {
  force(app_ui)
  function(request) {
    injected <- inject_auth_token(request)
    if (!is.null(injected$token)) {
      on.exit(
        try(shinymanager:::.tok$reset_count(injected$token), silent = TRUE),
        add = TRUE
      )
    }
    app_ui(injected$request)
  }
}

load_remember_tokens()
