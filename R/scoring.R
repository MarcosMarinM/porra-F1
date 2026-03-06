# ==============================================================================
# LÓGICA DE REGLAMENTO (puntuación)
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
