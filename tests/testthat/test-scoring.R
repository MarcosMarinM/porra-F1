library(dplyr)
library(tidyr)

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(root, "R", "scoring.R"), local = TRUE)

session_ended <- function(gp, sesion, year) {
  identical(sesion, "Carrera")
}

get_results <- function(gp, sesion, year) {
  if (!identical(sesion, "Carrera")) return(NULL)
  list(
    posiciones = c("VER", "NOR", "LEC", "HAM", "PIA"),
    vuelta_rapida = "VER",
    mazepin = "LEC"
  )
}

test_that("carrera: exactos, semiaciertos y extras", {
  porra <- data.frame(
    sesion = "Carrera",
    p1 = "VER", p2 = "NOR", p3 = "LEC", p4 = "HAM", p5 = "PIA",
    vuelta_rapida = "VER", mazepin = "SAI",
    stringsAsFactors = FALSE
  )
  resultado <- list(
    posiciones = c("VER", "NOR", "PIA", "LEC", "HAM", "RUS"),
    vuelta_rapida = "VER",
    mazepin = "SAI"
  )

  pts <- calcular_score_reglamento(porra, resultado)

  expect_equal(pts$p1, 6)
  expect_equal(pts$p2, 6)
  expect_equal(pts$p3, 2)
  expect_equal(pts$p4, 2)
  expect_equal(pts$p5, 2)
  expect_equal(pts$vr, 1)
  expect_equal(pts$maz, 3)
  expect_equal(pts$total, 22)
})

test_that("clasificación: 3 puntos exacto y 1 semiacierto", {
  porra <- data.frame(
    sesion = "Clasificación",
    p1 = "VER", p2 = "LEC", p3 = "XXX", p4 = "HAM", p5 = "PIA",
    vuelta_rapida = NA, mazepin = NA,
    stringsAsFactors = FALSE
  )
  resultado <- list(
    posiciones = c("VER", "NOR", "PIA", "LEC", "HAM"),
    vuelta_rapida = NA,
    mazepin = NA
  )

  pts <- calcular_score_reglamento(porra, resultado)

  expect_equal(pts$p1, 3)
  expect_equal(pts$p2, 1)
  expect_equal(pts$p3, 0)
  expect_equal(pts$p4, 1)
  expect_equal(pts$p5, 1)
  expect_equal(pts$total, 6)
})

test_that("clasificación del esprint: sin semiaciertos", {
  porra <- data.frame(
    sesion = "Clasificación del esprint",
    p1 = "VER", p2 = "PIA", p3 = "LEC", p4 = "HAM", p5 = "RUS",
    vuelta_rapida = NA, mazepin = NA,
    stringsAsFactors = FALSE
  )
  resultado <- list(
    posiciones = c("VER", "NOR", "PIA", "LEC", "HAM"),
    vuelta_rapida = NA,
    mazepin = NA
  )

  pts <- calcular_score_reglamento(porra, resultado)

  expect_equal(pts$p1, 1)
  expect_equal(pts$p2, 0)
  expect_equal(pts$total, 1)
})

test_that("ranking ordena por puntos y calcula rangos", {
  sc <- data.frame(
    usuario = c("ana", "ana", "luis", "luis"),
    gp = c("GP1", "GP2", "GP1", "GP2"),
    sesion = "Carrera",
    pts_total = c(10, 5, 8, 6),
    pts_p1 = c(6, 0, 6, 0),
    pts_p2 = c(0, 0, 0, 0),
    pts_p3 = c(0, 0, 0, 0),
    pts_p4 = c(0, 0, 0, 0),
    pts_p5 = c(0, 0, 0, 0),
    pts_vr = c(0, 0, 0, 0),
    pts_maz = c(0, 0, 0, 0),
    con_resultado = TRUE,
    stringsAsFactors = FALSE
  )

  ranking <- compute_ranking(sc)

  expect_equal(ranking$usuario, c("ana", "luis"))
  expect_equal(ranking$Total, c(15, 14))
  expect_equal(ranking$Rank, c(1L, 2L))
})

test_that("ranking ignora eventos sin resultado", {
  sc <- data.frame(
    usuario = c("ana", "ana"),
    gp = c("GP1", "GP2"),
    sesion = "Carrera",
    pts_total = c(10, 0),
    pts_p1 = c(6, 0),
    pts_p2 = c(0, 0),
    pts_p3 = c(0, 0),
    pts_p4 = c(0, 0),
    pts_p5 = c(0, 0),
    pts_vr = c(0, 0),
    pts_maz = c(0, 0),
    con_resultado = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  ranking <- compute_ranking(sc)

  expect_equal(ranking$Total, 10)
  expect_equal(nrow(ranking), 1)
})

test_that("compute_scores calcula eventos pasados y deja a cero los futuros", {
  db <- data.frame(
    usuario = c("ana", "ana"),
    gp = c("GP1", "GP1"),
    sesion = c("Carrera", "Clasificación"),
    p1 = c("VER", "VER"),
    p2 = c("NOR", "NOR"),
    p3 = c("LEC", "LEC"),
    p4 = c("HAM", "HAM"),
    p5 = c("PIA", "PIA"),
    vuelta_rapida = c("VER", NA),
    mazepin = c("LEC", NA),
    stringsAsFactors = FALSE
  )

  out <- compute_scores(db, 2020, progress = FALSE)

  expect_equal(nrow(out), 2)
  expect_equal(out$con_resultado, c(TRUE, FALSE))
  expect_equal(out$pts_total, c(34, 0))
})
