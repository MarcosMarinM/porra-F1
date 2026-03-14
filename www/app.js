// Configuración: la app se sirve desde la misma URL que la API.
// Si quieres apuntar a otro host, define window.API_BASE antes de cargar app.js.
// Nota: si abres index.html con file://, window.location.origin es "null".
const API_BASE = window.API_BASE || (window.location.protocol === "file:" ? "http://127.0.0.1:8000" : window.location.origin);
const STORAGE_TOKEN = "porra_f1_token";

function apiFetch(path, options = {}) {
  const token = localStorage.getItem(STORAGE_TOKEN);
  const headers = {
    "Content-Type": "application/json",
    ...(options.headers || {}),
  };
  if (token) headers["Authorization"] = "Bearer " + token;

  return fetch(`${API_BASE}${path}`, {
    credentials: "omit",
    ...options,
    headers,
  }).then(async (res) => {
    const json = await res.json().catch(() => null);
    if (!res.ok) {
      const err = json?.message || res.statusText || "Error";
      throw new Error(err);
    }
    return json;
  });
}

function setToken(token, remember) {
  if (token) {
    if (remember) localStorage.setItem(STORAGE_TOKEN, token);
    else sessionStorage.setItem(STORAGE_TOKEN, token);
  }
}

function clearToken() {
  localStorage.removeItem(STORAGE_TOKEN);
  sessionStorage.removeItem(STORAGE_TOKEN);
}

function getStoredToken() {
  return localStorage.getItem(STORAGE_TOKEN) || sessionStorage.getItem(STORAGE_TOKEN);
}

function showScreen(id) {
  document.querySelectorAll(".screen").forEach((el) => el.classList.add("hidden"));
  document.getElementById(id).classList.remove("hidden");
}

function setTab(name) {
  document.querySelectorAll(".tab").forEach((btn) => btn.classList.toggle("active", btn.dataset.tab === name));
  document.querySelectorAll(".tab-panel").forEach((panel) => panel.classList.toggle("hidden", panel.id !== `tab-${name}`));
}

function showError(el, msg) {
  el.textContent = msg || "";
  el.style.display = msg ? "block" : "none";
}

async function doLogin(user, pwd, remember) {
  const loginError = document.getElementById("login-error");
  showError(loginError, "");
  try {
    const resp = await apiFetch("/login", {
      method: "POST",
      body: JSON.stringify({ user, pwd }),
    });
    setToken(resp.token, remember);
    initApp(resp.user);
  } catch (err) {
    showError(loginError, err.message);
  }
}

async function fetchCalendar() {
  const resp = await apiFetch("/calendar");
  return resp.data || [];
}

async function fetchDrivers() {
  const resp = await apiFetch("/drivers");
  return resp.data || [];
}

async function fetchLeaderboard() {
  const resp = await apiFetch("/leaderboard");
  return resp.data || [];
}

async function submitPrediction(data) {
  const resp = await apiFetch("/prediction", {
    method: "POST",
    body: JSON.stringify(data),
  });
  return resp;
}

function fillSelect(select, values, placeholder) {
  select.innerHTML = "";
  const empty = document.createElement("option");
  empty.value = "";
  empty.textContent = placeholder || "";
  select.appendChild(empty);
  values.forEach((v) => {
    const o = document.createElement("option");
    o.value = v;
    o.textContent = v;
    select.appendChild(o);
  });
}

function buildTop5Inputs(drivers) {
  const container = document.getElementById("top5-row");
  container.innerHTML = "";
  for (let i = 1; i <= 5; i += 1) {
    const col = document.createElement("div");
    col.className = "col";
    const label = document.createElement("label");
    label.textContent = `${i}º`;
    const sel = document.createElement("select");
    sel.id = `p${i}`;
    fillSelect(sel, [""].concat(drivers), "");
    col.appendChild(label);
    col.appendChild(sel);
    container.appendChild(col);
  }
}

async function refreshAppData() {
  const calendar = await fetchCalendar();
  fillSelect(document.getElementById("gp-select"), calendar, "Seleccione GP");

  const drivers = await fetchDrivers();
  buildTop5Inputs(drivers);
  fillSelect(document.getElementById("vr-select"), [""].concat(drivers), "");
  fillSelect(document.getElementById("maz-select"), [""].concat(drivers), "");

  const leaderboard = await fetchLeaderboard();
  const tbody = document.querySelector("#leaderboard-table tbody");
  tbody.innerHTML = "";
  leaderboard.forEach((row, idx) => {
    const tr = document.createElement("tr");
    tr.innerHTML = `<td>${idx + 1}</td><td>${(row.usuario || "").toUpperCase()}</td><td>${row.Total || 0}</td>`;
    tbody.appendChild(tr);
  });
}

function initApp(user) {
  document.getElementById("user-label").textContent = user ? user.toUpperCase() : "";
  showScreen("app-screen");
  setTab("porra");
  refreshAppData().catch((err) => console.error(err));
}

function initLogin() {
  showScreen("login-screen");
  setTab("porra");
}

function attachEvents() {
  document.getElementById("login-btn").addEventListener("click", () => {
    const user = document.getElementById("login-user").value.trim();
    const pwd = document.getElementById("login-pwd").value.trim();
    const remember = document.getElementById("remember-me").checked;
    doLogin(user, pwd, remember);
  });

  document.getElementById("guest-btn").addEventListener("click", () => {
    document.getElementById("login-user").value = "guest";
    document.getElementById("login-pwd").value = "guest";
    document.getElementById("remember-me").checked = false;
    doLogin("guest", "guest", false);
  });

  document.getElementById("logout-btn").addEventListener("click", () => {
    clearToken();
    initLogin();
  });

  document.querySelectorAll(".tab").forEach((btn) => {
    btn.addEventListener("click", () => setTab(btn.dataset.tab));
  });

  document.getElementById("sesion-select").addEventListener("change", (event) => {
    const isRace = event.target.value === "Carrera";
    document.getElementById("extras-race").classList.toggle("hidden", !isRace);
  });

  document.getElementById("submit-btn").addEventListener("click", async () => {
    const msgEl = document.getElementById("submit-message");
    msgEl.textContent = "";

    const payload = {
      gp: document.getElementById("gp-select").value,
      sesion: document.getElementById("sesion-select").value,
      p1: document.getElementById("p1").value,
      p2: document.getElementById("p2").value,
      p3: document.getElementById("p3").value,
      p4: document.getElementById("p4").value,
      p5: document.getElementById("p5").value,
      vr: document.getElementById("vr-select").value,
      maz: document.getElementById("maz-select").value,
    };

    try {
      const res = await submitPrediction(payload);
      msgEl.textContent = res.message || "OK";
      setTimeout(() => (msgEl.textContent = ""), 3000);
      await refreshAppData();
    } catch (err) {
      msgEl.textContent = err.message;
    }
  });
}

window.addEventListener("DOMContentLoaded", () => {
  attachEvents();
  const storedToken = getStoredToken();
  if (storedToken) {
    // Intenta usar el token para cargar datos (si falla, vuelve a login)
    apiFetch("/calendar").then(() => initApp(""))
      .catch(() => initLogin());
  } else {
    initLogin();
  }
});
