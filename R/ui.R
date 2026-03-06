# ==============================================================================
# INTERFAZ (UI)
# ==============================================================================

build_lang_switcher <- function() {
  if (!MULTILANG) return(NULL)
  lang_codes <- names(LANG_CONFIG$languages)
  buttons <- lapply(lang_codes, function(code) {
    info <- LANG_CONFIG$languages[[code]]
    is_default <- (code == DEFAULT_LANG)
    span(
      id = paste0("wrap_", code),
      class = paste("lang-wrapper", if (is_default) "lang-active" else ""),
      actionLink(
        paste0("lang_", code),
        label = tags$img(src = info$flag, alt = info$label)
      )
    )
  })
  div(class = "lang-switcher", buttons)
}

ui <- fluidPage(
  theme = bs_theme(
    bg = "#121212", fg = "#e0e0e0", primary = "#e10600",
    base_font = font_google("Titillium Web"),
    heading_font = font_google("Titillium Web")
  ),
  tags$head(
    tags$style(HTML("
      body { background-color: #000000; }
      .card { background-color: #1e1e1e; border: 1px solid #333; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.3); }
      .card-header { background-color: #2c2c2c; border-bottom: 2px solid #e10600; font-weight: bold; text-transform: uppercase; letter-spacing: 1px; color: #fff; }
      .table { color: #ddd; }
      .table-striped tbody tr:nth-of-type(odd) { background-color: rgba(255,255,255,0.05); }
      .rank-1 { color: #FFD700; } .rank-2 { color: #C0C0C0; } .rank-3 { color: #CD7F32; }
      .points-badge { background-color: #e10600; color: white; padding: 3px 8px; border-radius: 10px; font-weight: bold; font-size: 0.9em; }
      .driver-badge { display: inline-block; padding: 2px 6px; border-radius: 4px; background: #333; border-left: 3px solid #e10600; font-family: monospace; margin-right: 5px; font-weight: bold;}
      .mazepin-badge { border-left: 3px solid #FFD700 !important; color: #FFD700 !important; }
      .vr-badge { border-left: 3px solid #bf00ff !important; color: #bf00ff !important; }
      .session-row { border-bottom: 1px solid #333; padding: 15px 0; }
      .session-title { font-size: 1.1em; color: #e10600; font-weight: bold; margin-bottom: 10px; }
      .comparison-box { display: flex; justify-content: space-between; align-items: center; }
      .side-box { flex: 1; padding: 10px; background: rgba(255,255,255,0.03); border-radius: 5px; margin: 0 5px; }
      .vs-text { color: #555; font-weight: bold; font-size: 0.8em; }
      .user-header-block { background-color: #2a2a2a; padding: 5px 10px; border-radius: 4px; display: inline-block; margin-bottom: 10px; border-left: 4px solid #888; }
      .user-header-me { border-left: 4px solid #e10600; background-color: #3a1010; }

      /* Selector de idioma */
      .lang-switcher { display: inline-block; margin-left: 20px; vertical-align: middle; }
      .lang-wrapper { display: inline-block; margin: 0 4px; padding: 3px; border: 2px solid transparent; border-radius: 4px; opacity: 0.45; transition: all 0.2s; cursor: pointer; vertical-align: middle; }
      .lang-wrapper:hover { opacity: 0.75; }
      .lang-wrapper.lang-active { opacity: 1; border-color: #e10600; }
      .lang-wrapper img { height: 22px; display: block; }
      .lang-wrapper .action-button { border: none; background: none; padding: 0; }

      /* Award cards */
      .award-card { background: linear-gradient(135deg, #1e1e1e, #2a2a2a); border: 1px solid #333; border-radius: 10px; padding: 20px 15px; text-align: center; margin-bottom: 15px; transition: transform 0.2s, border-color 0.2s; }
      .award-card:hover { transform: translateY(-3px); border-color: #e10600; }
      .award-emoji { font-size: 2.8em; margin-bottom: 8px; }
      .award-name { font-size: 1.1em; font-weight: bold; color: #e10600; text-transform: uppercase; letter-spacing: 1.5px; }
      .award-desc { font-size: 0.8em; color: #777; margin: 6px 0 14px; }
      .award-winner { background: #1a1a1a; padding: 8px 14px; border-radius: 6px; font-weight: bold; color: #fff; border-left: 3px solid #e10600; display: inline-block; font-size: 0.95em; }

      /* Screenshot buttons */
      .screenshot-btn { background: transparent; border: 1px solid #555; color: #888; font-size: 0.75em; padding: 2px 8px; border-radius: 4px; cursor: pointer; transition: all 0.2s; }
      .screenshot-btn:hover { background: #e10600; color: #fff; border-color: #e10600; }

      /* History table */
      .history-table td { vertical-align: middle !important; padding: 8px 6px !important; }
      .history-table .driver-badge { font-size: 0.9em; }
      .history-me { background-color: rgba(225, 6, 0, 0.12) !important; border-left: 3px solid #e10600; }
    ")),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
    tags$script(HTML("
      function captureElement(elementId, filename) {
        var el = document.getElementById(elementId);
        if (!el) return;
        var btns = el.querySelectorAll('.screenshot-btn');
        btns.forEach(function(b) { b.style.display = 'none'; });
        html2canvas(el, { backgroundColor: '#1e1e1e', scale: 2 }).then(function(canvas) {
          btns.forEach(function(b) { b.style.display = ''; });
          var link = document.createElement('a');
          link.download = filename + '.png';
          link.href = canvas.toDataURL();
          link.click();
        }).catch(function() {
          btns.forEach(function(b) { b.style.display = ''; });
        });
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle_lang', function(lang) {
        document.querySelectorAll('.lang-wrapper').forEach(function(el) {
          el.classList.remove('lang-active');
        });
        var active = document.getElementById('wrap_' + lang);
        if (active) active.classList.add('lang-active');
      });
    "))
  ),

  # --- HEADER ---
  div(
    style = "padding: 20px 0; border-bottom: 4px solid #e10600; margin-bottom: 20px;",
    fluidRow(column(12, align = "center",
      img(src = "https://upload.wikimedia.org/wikipedia/commons/3/33/F1.svg", height = "40px"),
      h2("LA CARRERA MÁS SURREALIST", style = "display:inline; margin-left: 15px; font-weight:800; color: #fff;"),
      build_lang_switcher()
    ))
  ),

  # --- COLORES DINÁMICOS ---
  uiOutput("driver_styles"),

  # --- LAYOUT ---
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "card", style = "padding: 15px;", uiOutput("sidebar_content"))
    ),
    mainPanel(
      width = 9,
      uiOutput("main_tabs")
    )
  )
)
