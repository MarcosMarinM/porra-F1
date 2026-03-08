# ==============================================================================
# LÓGICA DE REGLAMENTO (puntuación)
# ==============================================================================

# ---------------------------------------------------------------------------
# Clasificación general con desempates reglamentarios
# ---------------------------------------------------------------------------
# Orden de desempate:
#   1. Más plenos en carrera (5 posiciones exactas)
#   2. Más victorias de sesión en carrera
#   3. Más victorias en clasificación
#   4. Más victorias en esprint
#   5. Más victorias en clasificación del esprint
#   6. Más aciertos Mazepin
#   7. Más aciertos vuelta rápida
#   Si persiste el empate → ex-aequo (mismo rango)
compute_ranking <- function(sc) {
  if (is.null(sc) || nrow(sc) == 0) return(NULL)

  for (col in c("pts_p1", "pts_p2", "pts_p3", "pts_p4", "pts_p5", "pts_vr", "pts_maz")) {
    if (!col %in% names(sc)) sc[[col]] <- 0
  }
  sc <- sc |> mutate(across(starts_with("pts_"), ~replace_na(., 0)))

  totales <- sc |>
    group_by(usuario) |>
    summarise(Total = sum(pts_total, na.rm = TRUE), .groups = "drop")

  # 1. Plenos en carrera
  plenos <- sc |>
    filter(sesion == "Carrera") |>
    mutate(pleno = (pts_p1 == 6) & (pts_p2 == 6) & (pts_p3 == 6) & (pts_p4 == 6) & (pts_p5 == 6)) |>
    group_by(usuario) |>
    summarise(tb_plenos = sum(pleno), .groups = "drop")

  # 2-5. Victorias por tipo de sesión
  session_winners <- sc |>
    group_by(gp, sesion) |>
    filter(pts_total == max(pts_total), pts_total > 0) |>
    ungroup()

  count_wins <- function(ses) {
    session_winners |>
      filter(sesion == ses) |>
      count(usuario, name = "n") |>
      select(usuario, n)
  }
  wins_race   <- count_wins("Carrera") |> rename(tb_wins_race = n)
  wins_quali  <- count_wins("Clasificación") |> rename(tb_wins_quali = n)
  wins_sprint <- count_wins("Esprint") |> rename(tb_wins_sprint = n)
  wins_sq     <- count_wins("Clasificación del esprint") |> rename(tb_wins_sq = n)

  # 6-7. Aciertos VR y MAZ
  bonuses <- sc |>
    group_by(usuario) |>
    summarise(
      tb_maz = sum(pts_maz > 0, na.rm = TRUE),
      tb_vr  = sum(pts_vr > 0, na.rm = TRUE),
      .groups = "drop"
    )

  ranking <- totales |>
    left_join(plenos,      by = "usuario") |>
    left_join(wins_race,   by = "usuario") |>
    left_join(wins_quali,  by = "usuario") |>
    left_join(wins_sprint, by = "usuario") |>
    left_join(wins_sq,     by = "usuario") |>
    left_join(bonuses,     by = "usuario") |>
    mutate(across(starts_with("tb_"), ~replace_na(., 0))) |>
    arrange(
      desc(Total),
      desc(tb_plenos), desc(tb_wins_race), desc(tb_wins_quali),
      desc(tb_wins_sprint), desc(tb_wins_sq),
      desc(tb_maz), desc(tb_vr)
    )

  # Asignar rango: ex-aequo solo si TODOS los criterios coinciden
  tb_cols <- c("Total", "tb_plenos", "tb_wins_race", "tb_wins_quali",
               "tb_wins_sprint", "tb_wins_sq", "tb_maz", "tb_vr")
  ranks <- integer(nrow(ranking))
  ranks[1] <- 1L
  if (nrow(ranking) > 1) {
    for (i in 2:nrow(ranking)) {
      if (all(ranking[i, tb_cols] == ranking[i - 1, tb_cols])) {
        ranks[i] <- ranks[i - 1]
      } else {
        ranks[i] <- i
      }
    }
  }
  ranking$Rank <- ranks
  ranking
}

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
