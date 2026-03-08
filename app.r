# ==============================================================================
# PROYECTO: LA CARRERA MÁS SURREALIST
# VERSIÓN: 2026
# ==============================================================================

# Los archivos en R/ se cargan automáticamente por Shiny (orden alfabético):
#   R/00_packages.R — library() de todos los paquetes
#   R/api.R         — funciones de acceso a OpenF1
#   R/config.R      — variables de entorno, constantes, traducciones, colores
#   R/scoring.R     — lógica de puntuación
#   R/ui.R          — definición de la interfaz (ui)
#   R/server.R      — función server()

# ==============================================================================
# LANZAR APP
# ==============================================================================

# --- "Recordarme" via cookie + token ---
remember_me_js <- tags$script(HTML("
  document.addEventListener('DOMContentLoaded', function() {

    /* ---- constantes ---- */
    var COOKIE_NAME = 'f1porra_token';
    var COOKIE_DAYS = 90;
    var LS_FLAG     = 'f1porra_remember';

    /* ---- helpers de cookies ---- */
    function setCookie(name, value, days) {
      var d = new Date();
      d.setTime(d.getTime() + days * 86400000);
      document.cookie = name + '=' + encodeURIComponent(value) +
        ';expires=' + d.toUTCString() + ';path=/;SameSite=Lax';
    }
    function getCookie(name) {
      var m = document.cookie.match(new RegExp('(?:^|;)\\\\s*' + name + '=([^;]*)'));
      return m ? decodeURIComponent(m[1]) : null;
    }
    function deleteCookie(name) {
      document.cookie = name + '=;expires=Thu, 01 Jan 1970 00:00:00 GMT;path=/';
    }

    /* ---- 1. Si hay token guardado en cookie, redirigir ---- */
    var saved = getCookie(COOKIE_NAME);
    if (saved && !window.location.search.match(/token=/)) {
      window.location.search = '?token=' + saved;
      return;
    }

    /* ---- 2. Inyectar checkbox y guardar preferencia al hacer click ---- */
    var waitForForm = setInterval(function() {
      var btn = document.getElementById('auth-go_auth');
      if (!btn) return;
      clearInterval(waitForForm);

      var wrapper = document.createElement('div');
      wrapper.style.cssText = 'margin:10px 0;text-align:left;';
      wrapper.innerHTML =
        '<label style=\"cursor:pointer;font-weight:normal;\">' +
        '<input type=\"checkbox\" id=\"remember_me\" style=\"margin-right:6px;\">Recordarme</label>';
      btn.parentNode.insertBefore(wrapper, btn);

      btn.addEventListener('click', function() {
        var cb = document.getElementById('remember_me');
        if (cb && cb.checked) {
          localStorage.setItem(LS_FLAG, '1');
        } else {
          localStorage.removeItem(LS_FLAG);
        }
      });

      /* botón de invitado */
      var guestBtn = document.createElement('button');
      guestBtn.type = 'button';
      guestBtn.className = 'btn btn-default';
      guestBtn.style.cssText = 'width:100%;margin-top:8px;color:#888;border-color:#555;';
      guestBtn.innerHTML = '\\uD83D\\uDC41 Acceder como invitado';
      guestBtn.addEventListener('click', function() {
        // No guardar sesion de invitado
        var cb = document.getElementById('remember_me');
        if (cb) cb.checked = false;
        localStorage.removeItem(LS_FLAG);
        deleteCookie(COOKIE_NAME);

        var userInput = document.getElementById('auth-user_id');
        var pwdInput  = document.getElementById('auth-user_pwd');
        if (userInput) { userInput.value = 'guest'; userInput.dispatchEvent(new Event('change')); }
        if (pwdInput)  { pwdInput.value = 'guest';  pwdInput.dispatchEvent(new Event('change')); }
        setTimeout(function() { btn.click(); }, 100);
      });
      btn.parentNode.insertBefore(guestBtn, btn.nextSibling);
    }, 200);

    /* ---- 3. Vigilar la URL: cuando aparezca el token, guardar cookie ---- */
    /*    Cubre tanto redirect completo como actualización dinámica de URL   */
    var watchToken = setInterval(function() {
      var m = window.location.href.match(/token=([^&]+)/);
      if (!m) return;
      clearInterval(watchToken);
      if (localStorage.getItem(LS_FLAG) === '1') {
        setCookie(COOKIE_NAME, m[1], COOKIE_DAYS);
        localStorage.removeItem(LS_FLAG);
        console.log('[Recordarme] Token guardado en cookie');
      }
    }, 500);

    /* ---- 4. Limpiar cookie al hacer logout ---- */
    new MutationObserver(function() {
      var logoutBtn = document.getElementById('auth-logout');
      if (!logoutBtn || logoutBtn.dataset.bound) return;
      logoutBtn.dataset.bound = '1';
      logoutBtn.addEventListener('click', function() {
        deleteCookie(COOKIE_NAME);
        localStorage.removeItem(LS_FLAG);
      });
    }).observe(document.body, {childList: true, subtree: true});

  });
"))

ui_secure <- secure_app(ui, language = "es", head_auth = remember_me_js)

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
