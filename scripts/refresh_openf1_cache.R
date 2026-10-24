# Refresca la caché de resultados de OpenF1 para que la app arranque rápido.
# Uso: Rscript scripts/refresh_openf1_cache.R

suppressPackageStartupMessages(source("R/00_packages.R"))
source("R/config.R")
source("R/api.R")

year <- CURRENT_YEAR
db <- read_sheet(SHEET_ID, col_types = "c")
if (is.null(db) || nrow(db) == 0) stop("La hoja no tiene datos")

events <- unique(db[, c("gp", "sesion")])
season <- get_season(year)
if (is.null(season)) stop("OpenF1 no devolvió la temporada")

chunks <- function(x, n) split(x, ceiling(seq_along(x) / n))

targets <- do.call(rbind, lapply(seq_len(nrow(events)), function(i) {
  target <- find_session(events$gp[i], events$sesion[i], year)
  if (is.null(target)) return(NULL)
  if (is.na(target$date_end_utc) || target$date_end_utc > Sys.time()) return(NULL)
  data.frame(
    gp = events$gp[i],
    sesion = events$sesion[i],
    session_key = target$session_key,
    session_name = as.character(target$session_name),
    stringsAsFactors = FALSE
  )
}))

if (is.null(targets) || nrow(targets) == 0) stop("No hay sesiones con resultados")
keys <- unique(targets$session_key)
cat("Eventos:", nrow(targets), " sesiones únicas:", length(keys), "\n")

drivers <- fetch_api("/drivers", list(session_key = keys))
if (is.null(drivers) || nrow(drivers) == 0) stop("Sin drivers")
for (sk in unique(drivers$session_key)) {
  cache_drivers$set(
    cache_key("drivers", sk),
    drivers[drivers$session_key == sk, , drop = FALSE]
  )
}
cat("Drivers:", nrow(drivers), "\n")

results <- fetch_api("/session_result", list(session_key = keys))
if (is.null(results) || nrow(results) == 0) stop("Sin resultados de sesión")
cat("Resultados:", nrow(results), " filas\n")

race_keys <- unique(targets$session_key[tolower(targets$sesion) == "carrera"])
laps <- if (length(race_keys) > 0) {
  dplyr::bind_rows(lapply(chunks(race_keys, 5), function(k) {
    fetch_api("/laps", list(session_key = k))
  }))
} else NULL
cat("Vueltas:", if (is.null(laps)) 0 else nrow(laps), "\n")

for (i in seq_len(nrow(targets))) {
  gp <- targets$gp[i]
  sesion <- targets$sesion[i]
  sk <- targets$session_key[i]

  d_map <- unique(drivers[drivers$session_key == sk, c("driver_number", "name_acronym"), drop = FALSE])
  if (nrow(d_map) == 0) next

  res <- results[results$session_key == sk, , drop = FALSE]
  if (nrow(res) == 0) next

  full <- merge(res, d_map, by = "driver_number", all.x = TRUE, sort = FALSE)
  full <- full[!is.na(full$position), , drop = FALSE]
  if (nrow(full) == 0) next
  full <- full[order(full$position), , drop = FALSE]

  vr_val <- NA
  maz_val <- NA

  if (identical(as.character(targets$session_name[i]), "Race")) {
    if (!is.null(laps)) {
      l <- laps[laps$session_key == sk & !is.na(laps$lap_duration), , drop = FALSE]
      if (nrow(l) > 0) {
        fastest <- l[which.min(l$lap_duration), , drop = FALSE]
        vr <- d_map$name_acronym[d_map$driver_number == fastest$driver_number]
        if (length(vr) > 0) vr_val <- vr[1]
      }
    }
    if (is.na(maz_val) && "dnf" %in% names(full)) {
      dnfs <- full[full$dnf %in% TRUE, , drop = FALSE]
      if (nrow(dnfs) > 0 && "number_of_laps" %in% names(dnfs)) {
        dnfs <- dnfs[order(dnfs$number_of_laps), , drop = FALSE]
        maz_val <- dnfs$name_acronym[1]
      }
    }
    if (is.na(maz_val)) maz_val <- full$name_acronym[nrow(full)]
  }

  cache_results$set(
    cache_key("results", year, trimws(gp), trimws(sesion)),
    list(posiciones = full$name_acronym, vuelta_rapida = vr_val, mazepin = maz_val)
  )
}

cat("Caché actualizada:", nrow(targets), "eventos\n")
