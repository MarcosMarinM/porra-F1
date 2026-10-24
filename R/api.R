# ==============================================================================
# MOTOR DE DATOS (API OpenF1)
# ==============================================================================

SESSION_API_NAMES <- list(
  "Carrera" = "Race",
  "Clasificación" = "Qualifying",
  "Esprint" = "Sprint",
  "Clasificación del esprint" = c("Sprint Qualifying", "Sprint Shootout")
)

openf1_access_token <- function(force = FALSE) {
  if (!force && !is.null(.OPENF1_AUTH$token)) {
    if (is.null(.OPENF1_AUTH$expires) || .OPENF1_AUTH$expires > Sys.time()) {
      return(.OPENF1_AUTH$token)
    }
  }

  static_token <- Sys.getenv("OPENF1_TOKEN")
  if (nzchar(static_token)) {
    .OPENF1_AUTH$token <- static_token
    .OPENF1_AUTH$expires <- NULL
    return(static_token)
  }

  user <- Sys.getenv("OPENF1_USERNAME")
  pass <- Sys.getenv("OPENF1_PASSWORD")
  if (!nzchar(user) || !nzchar(pass)) return(NULL)

  tryCatch(
    {
      resp <- request("https://api.openf1.org/token") |>
        req_method("POST") |>
        req_body_form(username = user, password = pass) |>
        req_timeout(10) |>
        req_perform()
      if (resp_status(resp) >= 400) return(NULL)

      data <- resp_body_json(resp)
      token <- data$access_token
      if (is.null(token) || !nzchar(token)) return(NULL)

      expires <- suppressWarnings(as.numeric(data$expires_in))
      .OPENF1_AUTH$token <- token
      .OPENF1_AUTH$expires <- if (is.na(expires)) {
        Sys.time() + 3000
      } else {
        Sys.time() + max(60, expires - 60)
      }
      token
    },
    error = function(e) {
      warning("OpenF1 auth: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

fetch_api <- function(endpoint, params = list(), timeout = 10) {
  url <- paste0(BASE_URL, endpoint)
  tryCatch(
    {
      req <- request(url) |>
        req_url_query(!!!params) |>
        req_timeout(timeout) |>
        req_retry(
          max_tries = 3,
          max_seconds = 20,
          retry_on_failure = TRUE,
          backoff = function(n) min(2 * n, 5)
        ) |>
        req_user_agent("porra-f1/2026 (+https://github.com/MarcosMarinM/porra-F1)")

      token <- openf1_access_token()
      if (!is.null(token)) req <- req_auth_bearer_token(req, token)

      resp <- req_perform(req)
      if (resp_status(resp) == 401) {
        .OPENF1_AUTH$token <- NULL
        .OPENF1_AUTH$expires <- NULL
        return(NULL)
      }
      if (resp_status(resp) >= 400) return(NULL)
      parsed <- resp_body_json(resp, simplifyVector = TRUE)
      if (length(parsed) == 0) return(NULL)
      as.data.frame(parsed)
    },
    error = function(e) {
      warning("OpenF1 ", endpoint, ": ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

cached_call <- function(cache, key, expr, fail_cache = cache_fail) {
  hit <- cache$get(key, missing = NULL)
  if (!is.null(hit)) return(hit)
  fail_key <- paste0("fail", key)
  if (!is.null(fail_cache$get(fail_key, missing = NULL))) return(NULL)
  value <- force(expr)
  if (is.null(value)) {
    fail_cache$set(fail_key, TRUE)
  } else {
    cache$set(key, value)
  }
  value
}

cache_key <- function(...) {
  key <- tryCatch(rlang::hash(list(...)), error = function(e) NULL)
  if (is.null(key) || !nzchar(key)) {
    key <- gsub("[^a-z0-9]", "", tolower(paste(unlist(list(...)), collapse = "")))
  }
  key
}

get_session_drivers <- function(session_key) {
  cached_call(
    cache_drivers,
    cache_key("drivers", session_key),
    fetch_api("/drivers", list(session_key = session_key)),
    fail_cache = cache_fail
  )
}

get_season_raw <- function(year) {
  meetings <- fetch_api("/meetings", list(year = year))
  sessions <- fetch_api("/sessions", list(year = year))
  if (is.null(meetings) || is.null(sessions) || nrow(sessions) == 0) return(NULL)

  sessions$date_start_utc <- as.POSIXct(
    sessions$date_start, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"
  )
  sessions$date_end_utc <- as.POSIXct(
    sessions$date_end, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"
  )

  idx <- match(sessions$meeting_key, meetings$meeting_key)
  sessions$meeting_name <- meetings$meeting_name[idx]
  sessions$is_cancelled <- if ("is_cancelled" %in% names(meetings)) {
    meetings$is_cancelled[idx]
  } else {
    FALSE
  }

  sessions <- sessions[order(sessions$date_start_utc), , drop = FALSE]
  rownames(sessions) <- NULL
  sessions
}

get_season <- function(year) {
  cached_call(cache_season, cache_key("season", year), get_season_raw(year))
}

get_calendar_raw <- function(year) {
  season <- get_season(year)
  if (is.null(season) || nrow(season) == 0) return(NULL)

  cancelled <- season$is_cancelled
  ok <- !is.na(season$meeting_name) &
    (is.na(cancelled) | !cancelled) &
    !grepl("testing", season$meeting_name, ignore.case = TRUE)

  names <- unique(season$meeting_name[ok])
  if (length(names) == 0) NULL else names
}

get_calendar <- function(year) {
  cached_call(cache_api, cache_key("calendar", year), get_calendar_raw(year))
}

get_current_gp_raw <- function(year) {
  season <- get_season(year)
  if (is.null(season) || nrow(season) == 0) return(NULL)

  cancelled <- season$is_cancelled
  ok <- !is.na(season$date_end_utc) &
    !is.na(season$meeting_name) &
    (is.na(cancelled) | !cancelled) &
    !grepl("testing", season$meeting_name, ignore.case = TRUE)
  past <- season[ok & season$date_end_utc <= Sys.time(), , drop = FALSE]
  if (nrow(past) == 0) return(NULL)

  last <- past[which.max(past$date_end_utc), , drop = FALSE]
  if (identical(as.character(last$session_name), "Race")) return(NULL)
  last$meeting_name
}

get_current_gp <- function(year) {
  cached_call(cache_current, cache_key("current_gp", year), get_current_gp_raw(year))
}

recent_session_keys <- function(year, n = 3) {
  season <- get_season(year)
  if (is.null(season) || nrow(season) == 0) return(NULL)
  keys <- unique(season$session_key)
  if (length(keys) == 0) return(NULL)
  utils::tail(keys, n)
}

get_drivers_raw <- function(year) {
  keys <- recent_session_keys(year, 3)
  if (is.null(keys) || length(keys) == 0) return(NULL)

  acronyms <- unlist(lapply(keys, function(sk) {
    drivers <- get_session_drivers(sk)
    if (is.null(drivers) || !"name_acronym" %in% names(drivers)) return(character(0))
    as.character(drivers$name_acronym)
  }), use.names = FALSE)

  acronyms <- sort(unique(acronyms[!is.na(acronyms) & nzchar(acronyms)]))
  if (length(acronyms) == 0) NULL else acronyms
}

get_drivers <- function(year) {
  cached_call(cache_drivers, cache_key("all_drivers", year), get_drivers_raw(year))
}

get_driver_colours_raw <- function(year) {
  colours <- TEAM_COLOURS_2026
  keys <- recent_session_keys(year, 3)
  if (is.null(keys) || length(keys) == 0) return(colours)

  for (sk in keys) {
    drivers <- get_session_drivers(sk)
    if (is.null(drivers) || !"team_colour" %in% names(drivers)) next
    for (i in seq_len(nrow(drivers))) {
      acr <- drivers$name_acronym[i]
      col <- drivers$team_colour[i]
      if (!is.na(acr) && !is.na(col) && nzchar(col)) {
        colours[[as.character(acr)]] <- paste0("#", col)
      }
    }
  }
  colours
}

get_driver_colours <- function(year) {
  cached_call(cache_drivers, cache_key("all_colours", year), get_driver_colours_raw(year))
}

find_session <- function(gp_name, session_name, year) {
  season <- get_season(year)
  if (is.null(season) || nrow(season) == 0) return(NULL)

  gp_clean <- trimws(gp_name)
  api_names <- SESSION_API_NAMES[[trimws(session_name)]]
  if (is.null(api_names)) return(NULL)

  sel <- season[
    season$meeting_name %in% gp_clean & season$session_name %in% api_names,
    , drop = FALSE
  ]
  if (nrow(sel) == 0) return(NULL)
  if (all(is.na(sel$date_start_utc))) return(NULL)

  sel[which.max(sel$date_start_utc), , drop = FALSE]
}

session_ended <- function(gp_name, session_name, year) {
  season <- get_season(year)
  if (is.null(season)) return(NA)
  target <- find_session(gp_name, session_name, year)
  if (is.null(target)) return(FALSE)
  !is.na(target$date_end_utc) && target$date_end_utc <= Sys.time()
}

get_results_raw <- function(gp_name, session_name, year) {
  target <- find_session(gp_name, session_name, year)
  if (is.null(target)) return(NULL)
  if (is.na(target$date_end_utc) || target$date_end_utc > Sys.time()) return(NULL)

  s_key <- target$session_key
  api_name <- as.character(target$session_name)

  drivers <- get_session_drivers(s_key)
  if (is.null(drivers) || nrow(drivers) == 0) return(NULL)
  if (!all(c("driver_number", "name_acronym") %in% names(drivers))) return(NULL)
  d_map <- unique(drivers[, c("driver_number", "name_acronym"), drop = FALSE])

  res <- fetch_api("/session_result", list(session_key = s_key))
  if (is.null(res) || nrow(res) == 0) return(NULL)
  if (!all(c("driver_number", "position") %in% names(res))) return(NULL)

  full_res <- merge(res, d_map, by = "driver_number", all.x = TRUE, sort = FALSE)
  full_res <- full_res[!is.na(full_res$position), , drop = FALSE]
  if (nrow(full_res) == 0) return(NULL)
  full_res <- full_res[order(full_res$position), , drop = FALSE]

  top5_real <- full_res$name_acronym
  vr_val <- NA
  maz_val <- NA

  if (identical(api_name, "Race")) {
    laps <- fetch_api("/laps", list(session_key = s_key))
    if (!is.null(laps) && nrow(laps) > 0 && "lap_duration" %in% names(laps)) {
      laps <- laps[!is.na(laps$lap_duration), , drop = FALSE]
      if (nrow(laps) > 0) {
        fastest <- laps[which.min(laps$lap_duration), , drop = FALSE]
        vr_driver <- d_map$name_acronym[d_map$driver_number == fastest$driver_number]
        if (length(vr_driver) > 0) vr_val <- vr_driver[1]
      }
    }

    rc <- fetch_api("/race_control", list(session_key = s_key, category = "Retirement"))
    if (!is.null(rc) && nrow(rc) > 0 && "date" %in% names(rc)) {
      rc <- rc[order(rc$date), , drop = FALSE]
      first_out <- rc[1, , drop = FALSE]
      maz_driver <- d_map$name_acronym[d_map$driver_number == first_out$driver_number]
      if (length(maz_driver) > 0) maz_val <- maz_driver[1]
    }

    if (is.na(maz_val)) {
      if ("dnf" %in% names(full_res)) {
        dnfs <- full_res[full_res$dnf %in% TRUE, , drop = FALSE]
        if (nrow(dnfs) > 0 && "number_of_laps" %in% names(dnfs)) {
          dnfs <- dnfs[order(dnfs$number_of_laps), , drop = FALSE]
          maz_val <- dnfs$name_acronym[1]
        }
      }
      if (is.na(maz_val)) {
        maz_val <- full_res$name_acronym[nrow(full_res)]
      }
    }
  }

  list(posiciones = top5_real, vuelta_rapida = vr_val, mazepin = maz_val)
}

get_results <- function(gp_name, session_name, year) {
  key <- cache_key("results", year, trimws(gp_name), trimws(session_name))
  cached_call(cache_results, key, get_results_raw(gp_name, session_name, year))
}
