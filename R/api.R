# ==============================================================================
# MOTOR DE DATOS (API OpenF1)
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

get_driver_colours_raw <- function(year) {
  colours <- TEAM_COLOURS_2026
  sessions <- fetch_api("/sessions", list(year = year))
  if (is.null(sessions) || nrow(sessions) == 0) return(colours)
  recent_sessions <- tail(sessions$session_key, 5)
  for (sk in recent_sessions) {
    drivers <- fetch_api("/drivers", list(session_key = sk))
    if (!is.null(drivers) && nrow(drivers) > 0 && "team_colour" %in% names(drivers)) {
      for (i in 1:nrow(drivers)) {
        acr <- drivers$name_acronym[i]
        col <- drivers$team_colour[i]
        if (!is.na(acr) && !is.na(col) && col != "") {
          colours[[acr]] <- paste0("#", col)
        }
      }
    }
  }
  colours
}
get_driver_colours <- memoise(get_driver_colours_raw, cache = cache_memoria)

get_calendar_raw <- function(year) {
  meetings <- fetch_api("/meetings", list(year = year))
  if (is.null(meetings) || nrow(meetings) == 0) return(NULL)
  meetings <- meetings %>% arrange(date_start)
  unique(meetings$meeting_name)
}
get_calendar <- memoise(get_calendar_raw, cache = cache_memoria)

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
  if (last$session_name == "Race") return(NULL)

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
