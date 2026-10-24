# ==============================================================================
# PROYECTO: LA CARRERA MÁS SURREALIST
# VERSIÓN: 2026
# ==============================================================================

# Los archivos en R/ se cargan automáticamente por Shiny (orden alfabético):
#   R/00_packages.R — library() de todos los paquetes
#   R/api.R         — funciones de acceso a OpenF1
#   R/config.R      — variables de entorno, constantes, traducciones, colores
#   R/scoring.R     — lógica de puntuación
#   R/ui.R          — definición de la interfaz (ui) y JS de "recordarme"
#   R/server.R      — función server()
#   R/zz_auth.R     — tokens persistentes y auto-login por cookie

# ==============================================================================
# LANZAR APP
# ==============================================================================

auth_labels <- list(
  "Please authenticate" = "\U0001f3c1 IDENTIFÍCATE",
  "Username:" = "Código de piloto",
  "Password:" = "Contraseña",
  "Login" = "\U0001f6a6 SALIR A PISTA",
  "Logout" = "Entrar al box",
  "Username or password are incorrect" = "\u274c Error: credenciales no válidas",
  "You are not authorized for this application" = "\u26d4 Acceso denegado"
)

shinymanager::set_labels(language = "es", auth_labels)
shinymanager::set_labels(language = "en", auth_labels)

secure_ui <- secure_app(
  ui,
  language = "es",
  head_auth = remember_me_js()
)
ui_secure <- with_remember_cookie(secure_ui)

shinyApp(ui_secure, server)
