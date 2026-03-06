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
library(ggplot2)
library(tidyr)
library(plotly)

# Los archivos en R/ se cargan automáticamente por Shiny:
#   R/config.R  — variables de entorno, constantes, traducciones, colores
#   R/api.R     — funciones de acceso a OpenF1
#   R/scoring.R — lógica de puntuación
#   R/ui.R      — definición de la interfaz (ui)
#   R/server.R  — función server()

# ==============================================================================
# LANZAR APP
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
