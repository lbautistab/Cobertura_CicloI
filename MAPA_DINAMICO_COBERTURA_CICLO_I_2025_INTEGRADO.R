#===============================================================================
#     MAPA DINÁMICO INTEGRADO DE LA COBERTURA DEL CICLO I - 2025
#               MINEDU | MINEDU + Cuna Más (SCD) 
#===============================================================================
# Created by Luis Bautista
# 25/11/26


# Librerías
#-------------------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(readxl)
library(mapsPERU)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(base64enc)

# Parámetros
#-------------------------------------------------------------------------------
ARCHIVO_XLSX <- "Ciclo I-Población coberturada y no cobertura por distritos 2025.xlsx"
LOGO_MINEDU  <- "Minedu.png"
LOGO_CUNAMAS <- "CunaMas.png"
LOGO_DOS     <- "MineduCunaMas.png"
SALIDA_HTML  <- "Mapa_Cobertura_Integrado_CicloI_2025.html"

# Funciones auxiliares
#-------------------------------------------------------------------------------
pad_ubigeo <- function(x) str_pad(as.character(as.integer(x)), 6, pad = "0")
fmt_n      <- function(x) formatC(x, format = "d", big.mark = ",")
fmt_p      <- function(x) formatC(x, format = "f", digits = 1)

# Paleta de colores
color_semaforo <- function(p) case_when(
  p == 0          ~ "#F50505",
  p > 0 & p < 2   ~ "#ED7979",
  p >= 2 & p <= 7 ~ "#FB8C00",
  p > 7           ~ "#43A047",
  TRUE            ~ "#BDBDBD"
)


# Cargando la data
#-------------------------------------------------------------------------------
BD_COBERTURA <- read_excel(ARCHIVO_XLSX, skip = 4, sheet = "BD")

df_distritos <- BD_COBERTURA %>%
  mutate(
    COD_DISTRITO = pad_ubigeo(UBIGEO),
    across(c(CANTID_EBE, CANTID_INICIAL, CANTID_PRONOEI, CANTID_CIAI), ~ replace_na(.x, 0)),
    PCT_M     = round(PORC_COBERTURA * 100, 1),       # % cobertura MINEDU
    PCT_T     = round(POR_COBERTURA_TOTAL * 100, 1),  # % cobertura MINEDU + Cuna Más
    APORTE_PP = round(PCT_T - PCT_M, 1),              # puntos porcentuales que suma Cuna Más
    NOCOB_M   = POBLACION - COBERTURA_MINEDU,
    NOCOB_T   = POBLACION - COBERTURA_TOTAl
  )

# Control: debe coincidir con el Excel (8.7% y 13.6%)
cat("Cobertura MINEDU nacional :", round(sum(df_distritos$COBERTURA_MINEDU) / sum(df_distritos$POBLACION) * 100, 1), "%\n")
cat("Cobertura total nacional  :", round(sum(df_distritos$COBERTURA_TOTAl)  / sum(df_distritos$POBLACION) * 100, 1), "%\n")


# Límites distritales
#-------------------------------------------------------------------------------
peru_distritos <- st_as_sf(map_DIST) %>%
  mutate(COD_DISTRITO = pad_ubigeo(COD_DISTRITO)) %>%
  select(COD_DISTRITO)

if (!is.na(st_crs(peru_distritos))) peru_distritos <- st_transform(peru_distritos, 4326)

# Simplificación de polígonos (HTML más liviano)
peru_distritos <- rmapshaper::ms_simplify(peru_distritos, keep = 0.35, keep_shapes = TRUE)


# Unir la data con los shapes
#-------------------------------------------------------------------------------
BD_MAPA <- peru_distritos %>%
  inner_join(df_distritos, by = "COD_DISTRITO")
BD_MAPA <- BD_MAPA[!st_is_empty(BD_MAPA), ]

sin_shape <- anti_join(df_distritos, st_drop_geometry(peru_distritos), by = "COD_DISTRITO")
if (nrow(sin_shape) > 0) {
  message("Distritos sin polígono en mapsPERU: ", nrow(sin_shape))
  print(select(sin_shape, UBIGEO, DEPARTAMENTO, PROVINCIA, DISTRITO))
}


# Ficha (popup) del distrito
#-------------------------------------------------------------------------------
# Cada bloque lleva la clase 'vx' + las vistas en las que debe aparecer:
#   v-M = MINEDU | v-T = MINEDU + Cuna Más | v-S = Aporte Cuna Más
# El panel oculta automáticamente lo que no corresponde a la vista activa.
esc   <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE)
barra <- function(p, col, v) sprintf("<div class='pp-bar vx %s'><i style='width:%s%%;background:%s'></i></div>",
                                     v, pmin(p, 100), col)

BD_MAPA <- BD_MAPA %>%
  mutate(
    COLOR_INICIAL = color_semaforo(PCT_M),
    POPUP = paste0(
      "<div class='pp'>",
        "<div class='pp-h'>",
          "<div class='pp-dist'>", esc(DISTRITO), "</div>",
          "<div class='pp-sub'>", esc(PROVINCIA), " | ", esc(DEPARTAMENTO), " | Ubigeo: ", COD_DISTRITO, "</div>",
        "</div>",
        "<div class='pp-pop'><span>Población de 0 a 2 años</span><b>", fmt_n(POBLACION), "</b></div>",

        "<div class='pp-row vx v-M'><span><i class='dot' style='background:#1a3f8b'></i>MINEDU</span><b>", fmt_p(PCT_M), "%</b></div>",
        barra(PCT_M, "#1a3f8b", "v-M"),
        "<div class='pp-mini vx v-M'>", fmt_n(COBERTURA_MINEDU), " coberturados | ", fmt_n(NOCOB_M), " sin cobertura</div>",

        "<div class='pp-row vx v-T'><span><i class='dot' style='background:#00897B'></i>MINEDU + Cuna Más</span><b>", fmt_p(PCT_T), "%</b></div>",
        barra(PCT_T, "#00897B", "v-T"),
        "<div class='pp-mini vx v-T'>", fmt_n(COBERTURA_TOTAl), " coberturados | ", fmt_n(NOCOB_T), " sin cobertura</div>",

        "<div class='pp-aporte vx v-T v-S'>",
          ifelse(COBERTURA_CIAI > 0,
                 paste0("Cuna Más suma <b>+", fmt_p(APORTE_PP), " pp</b> (", fmt_n(COBERTURA_CIAI), " niños)"),
                 "Sin atención de Cuna Más (SCD) en el distrito"),
        "</div>",

        "<div class='pp-serv'>",
          "<span class='vx v-M v-T'>N° Inicial <b>",  fmt_n(CANTID_INICIAL), "</b></span>",
          "<span class='vx v-M v-T'>N° PRONOEI <b>",  fmt_n(CANTID_PRONOEI), "</b></span>",
          "<span class='vx v-M v-T'>N° EBE <b>",      fmt_n(CANTID_EBE),     "</b></span>",
          "<span class='vx v-T v-S'>N° CIAI <b>",     fmt_n(CANTID_CIAI),    "</b></span>",
        "</div>",
      "</div>"
    )
  )


# Mapa base (una sola capa de distritos; el color lo gestiona el panel)
#-------------------------------------------------------------------------------
Mimapa <- leaflet(BD_MAPA, options = leafletOptions(zoomControl = FALSE, minZoom = 4)) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gris") %>%
  addProviderTiles(providers$Esri.WorldImagery,    group = "Satélite") %>%
  addPolygons(
    layerId      = ~COD_DISTRITO,
    group        = "distritos",
    fillColor    = ~COLOR_INICIAL,
    fillOpacity  = 0.85,
    color        = "#ffffff",
    weight       = 0.3,
    opacity      = 1,
    smoothFactor = 0.5,
    popup        = ~POPUP,
    popupOptions = popupOptions(maxWidth = 320, minWidth = 260, className = "pp-wrap")
  ) %>%
  addLayersControl(
    baseGroups = c("Gris", "Satélite"),
    position   = "topright",
    options    = layersControlOptions(collapsed = TRUE)
  ) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
  addMiniMap(tiles = providers$Esri.WorldImagery, position = "bottomright",
             width = 130, height = 130, toggleDisplay = TRUE, minimized = FALSE)


# Datos que viajan al navegador (panel, leyenda, buscador, top 5)
#-------------------------------------------------------------------------------
datos_js <- BD_MAPA %>%
  st_drop_geometry() %>%
  transmute(
    id   = COD_DISTRITO,
    dist = DISTRITO,
    prov = PROVINCIA,
    dep  = DEPARTAMENTO,
    reg  = REGION,
    pop  = POBLACION,
    cm   = COBERTURA_MINEDU,
    ciai = COBERTURA_CIAI,
    ct   = COBERTURA_TOTAl,
    pm   = PCT_M,
    pt   = PCT_T,
    pa   = APORTE_PP
  )

logos <- list(
  M = dataURI(file = LOGO_MINEDU,  mime = "image/png"),
  T = dataURI(file = LOGO_DOS, mime = "image/png"),
  S = dataURI(file = LOGO_CUNAMAS, mime = "image/png")
)


# Interfaz (encabezado + panel) y lógica interactiva en JavaScript
#-------------------------------------------------------------------------------
js_interfaz <- r"---(
function(el, x, data) {
  var map = this;
  if (document.getElementById('cm-header')) return;

  /* ---------- Datos ---------- */
  var D = data.d;
  var N = D.id.length;

  var COB = [
    {lab:'0%',           col:'#F50505'},
    {lab:'Menos de 2%',  col:'#ED7979'},
    {lab:'De 2% a 7%',   col:'#FB8C00'},
    {lab:'Mayor a 7%',   col:'#43A047'}
  ];
  var APO = [
    {lab:'Sin aporte (0 pp)', col:'#E4DEEE'},
    {lab:'Hasta 5 pp',        col:'#C5B3E6'},
    {lab:'De 5 a 10 pp',      col:'#9575CD'},
    {lab:'De 10 a 20 pp',     col:'#5E35B1'},
    {lab:'Más de 20 pp',      col:'#311B92'}
  ];
  function catCob(v){ if (v === null || isNaN(v)) return -1; if (v === 0) return 0; if (v < 2) return 1; if (v <= 7) return 2; return 3; }
  function catApo(v){ if (v === null || isNaN(v)) return -1; if (v <= 0) return 0; if (v <= 5) return 1; if (v <= 10) return 2; if (v <= 20) return 3; return 4; }

  var MODES = {
    M: { boton:'MINEDU',
         titulo:'Ciclo I: Población coberturada por los servicios educativos del MINEDU, según distritos, 2025',
         fuente:'PN y SIAGIE 2025', logo:data.logos.M, campo:'pm', cats:COB, catf:catCob,
         leyenda:'% de cobertura', unidad:'%' },

    T: { boton:'MINEDU + Cuna Más',
         titulo:'Ciclo I: Población coberturada por los servicios educativos del MINEDU y Cuna Más (SCD), según distritos, 2025',
         fuente:'PN, SIAGIE y Padrón Cuna Más 2025', logo:data.logos.T, campo:'pt', cats:COB, catf:catCob,
         leyenda:'% de cobertura', unidad:'%' },

    S: { boton:'Aporte Cuna Más',
         titulo:'Aporte del Programa Nacional Cuna Más (SCD) a la cobertura, según distritos, 2025',
         fuente:'Padrón Cuna Más 2025', logo:data.logos.S, campo:'pa', cats:APO, catf:catApo,
         leyenda:'PP añadidos', unidad:' pp' }
  };

  var state = { mode:'M', reg:null, cat:null };

  /* ---------- Utilidades ---------- */
  function esc(s){ return String(s).replace(/[&<>"']/g, function(c){ return {'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]; }); }
  function fN(v){ return Math.round(v).toLocaleString('es-PE'); }
  function fP(v){ return (Math.round(v*10)/10).toFixed(1); }
  function anim(elm, to, dec, suf){
    var from = parseFloat(elm.getAttribute('data-v') || 0), t0 = null;
    elm.setAttribute('data-v', to);
    function step(ts){
      if (!t0) t0 = ts;
      var p = Math.min((ts - t0)/650, 1), e = 1 - Math.pow(1 - p, 3), v = from + (to - from)*e;
      elm.textContent = (dec === 0 ? fN(v) : v.toFixed(dec)) + (suf || '');
      if (p < 1) requestAnimationFrame(step);
    }
    requestAnimationFrame(step);
  }

  /* ---------- Capas ---------- */
  var LY = [], boundsPeru = null;
  for (var i = 0; i < N; i++) {
    var l = map.layerManager.getLayer('shape', D.id[i]);
    LY.push(l || null);
    if (l) { boundsPeru = boundsPeru ? boundsPeru.extend(l.getBounds()) : L.latLngBounds(l.getBounds().getSouthWest(), l.getBounds().getNorthEast()); }
  }

  function tipHtml(i){
    var m = MODES[state.mode], v = D[m.campo][i], c = m.catf(v), col = c >= 0 ? m.cats[c].col : '#999';
    return "<div class='tt-d'>" + esc(D.dist[i]) + "</div>" +
           "<div class='tt-s'>" + esc(D.prov[i]) + ", " + esc(D.dep[i]) + "</div>" +
           "<div class='tt-v'><i style='background:" + col + "'></i>" + (state.mode === 'S' ? '+' : '') + fP(v) + m.unidad +
           "<small>" + m.boton + "</small></div>";
  }

  LY.forEach(function(l, i){
    if (!l) return;
    l.bindTooltip(function(){ return tipHtml(i); }, {sticky:true, direction:'top', offset:[0,-10], className:'cm-tt', opacity:1});
    l.on('mouseover', function(){ this.setStyle({weight:2.2, color:'#111'}); if (!L.Browser.ie && !L.Browser.opera) this.bringToFront(); });
    l.on('mouseout',  function(){ this.setStyle({weight:0.3, color:'#ffffff'}); });
  });

  /* ---------- Interfaz ---------- */
  var regiones = Array.from(new Set(D.reg)).sort(function(a,b){ return a.localeCompare(b, 'es'); });
  var botones = Object.keys(MODES).map(function(k){
    return "<button data-m='" + k + "'" + (k === 'M' ? " class='on'" : "") + ">" + MODES[k].boton + "</button>";
  }).join('');
  var opciones = D.dist.map(function(d, i){ return "<option value='" + esc(d + ' — ' + D.prov[i] + ', ' + D.dep[i]) + "'>"; }).join('');

  var html =
  "<link rel='preconnect' href='https://fonts.googleapis.com'>" +
  "<link href='https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap' rel='stylesheet'>" +
  "<style>" +
  ":root{--azul:#1a3f8b;--azul2:#0f2a63;--txt:#1f2937;--sub:#6b7280;--borde:#e5e7eb}" +
  "body{font-family:Inter,'Segoe UI',Arial,sans-serif}" +
  ".leaflet-top{top:86px!important}" +
  ".leaflet-overlay-pane path.leaflet-interactive{transition:fill .5s ease,fill-opacity .5s ease}" +
  "#cm-header{position:fixed;top:0;left:0;right:0;height:78px;z-index:2000;display:grid;grid-template-columns:auto 1fr auto;align-items:center;gap:18px;padding:0 20px;" +
    "background:linear-gradient(100deg,var(--azul2),var(--azul) 55%,#2554b3);color:#fff;box-shadow:0 4px 18px rgba(0,0,0,.28)}" +
  "#cm-logo{background:#fff;border-radius:10px;padding:4px 10px;height:64px;box-sizing:border-box;display:flex;align-items:center;transition:opacity .3s}" +
  "#cm-logo img{height:56px;display:block}" +
  "#cm-title{font-size:19px;font-weight:700;line-height:1.25;transition:opacity .3s}" +
  "#cm-right{text-align:right;font-size:11px;opacity:.95;display:flex;align-items:center;gap:12px}" +
  "#cm-fs{background:rgba(255,255,255,.14);border:1px solid rgba(255,255,255,.35);color:#fff;border-radius:8px;width:34px;height:34px;cursor:pointer;font-size:16px}" +
  "#cm-fs:hover{background:rgba(255,255,255,.28)}" +
  "#cm-panel{position:fixed;top:92px;left:14px;width:330px;max-height:calc(100vh - 108px);overflow:auto;z-index:1500;background:rgba(255,255,255,.96);" +
    "backdrop-filter:blur(6px);border-radius:14px;box-shadow:0 10px 30px rgba(15,42,99,.22);padding:14px;color:var(--txt);font-size:12.5px;transition:transform .35s ease}" +
  "#cm-panel.closed{transform:translateX(calc(-100% - 20px))}" +
  "#cm-toggle{position:fixed;top:92px;left:14px;z-index:1499;background:var(--azul);color:#fff;border:0;border-radius:10px;width:38px;height:38px;cursor:pointer;font-size:18px;box-shadow:0 4px 12px rgba(0,0,0,.25)}" +
  "#cm-panel:not(.closed)+#cm-toggle{display:none}" +
  "#cm-panel::-webkit-scrollbar{width:6px}#cm-panel::-webkit-scrollbar-thumb{background:#cbd5e1;border-radius:6px}" +
  ".cm-sec{margin-top:14px}.cm-h{font-size:10.5px;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:var(--sub);margin-bottom:7px;display:flex;justify-content:space-between;align-items:center}" +
  ".cm-x{white-space:nowrap;margin-left:8px;cursor:pointer;color:var(--azul);font-size:16px;line-height:1;text-transform:none}" +
  "#cm-seg{display:grid;grid-template-columns:1fr 1.25fr 1.2fr;background:#eef2f9;border-radius:10px;padding:3px;gap:3px}" +
  "#cm-seg button{border:0;background:transparent;border-radius:8px;padding:8px 4px;font:600 11.5px Inter,'Segoe UI',sans-serif;color:#334155;cursor:pointer;transition:all .25s}" +
  "#cm-seg button.on{background:var(--azul);color:#fff;box-shadow:0 3px 10px rgba(26,63,139,.35)}" +
  "#cm-seg button:not(.on):hover{background:#dde5f3}" +
  ".cm-in{width:100%;box-sizing:border-box;border:1px solid var(--borde);border-radius:9px;padding:8px 10px;font:500 12.5px Inter,'Segoe UI',sans-serif;color:var(--txt);background:#fff;outline:none}" +
  ".cm-in:focus{border-color:var(--azul);box-shadow:0 0 0 3px rgba(26,63,139,.15)}" +
  ".cm-row{display:flex;gap:6px;margin-top:6px}.cm-row select{flex:1}" +
  "#cm-reset{border:1px solid var(--borde);background:#fff;border-radius:9px;padding:0 10px;cursor:pointer;font-size:14px;color:var(--azul)}#cm-reset:hover{background:#eef2f9}" +
  "#cm-big{display:flex;align-items:baseline;gap:8px}#cm-bigv{font-size:38px;font-weight:800;color:var(--azul);letter-spacing:-1px}#cm-bigl{font-size:12px;color:var(--sub);font-weight:500}" +
  "#cm-stack{display:flex;height:10px;border-radius:6px;overflow:hidden;background:#e5e7eb;margin:8px 0 4px}#cm-stack i{display:block;height:100%;transition:width .6s ease}" +
  ".cm-stl{display:flex;gap:12px;font-size:11px;color:var(--sub)}.cm-stl i{display:inline-block;width:8px;height:8px;border-radius:2px;margin-right:4px}" +
  ".cm-kpis{display:grid;grid-template-columns:repeat(3,1fr);gap:6px;margin-top:10px}" +
  ".cm-kpi{background:#f6f8fc;border-radius:9px;padding:8px 7px}.cm-kpi b{display:block;font-size:14.5px;font-weight:700;color:var(--txt)}.cm-kpi span{font-size:10.5px;color:var(--sub)}" +
  ".cm-leg{display:grid;grid-template-columns:14px 1fr auto;align-items:center;gap:4px 8px;padding:5px 6px;border-radius:7px;cursor:pointer;transition:background .2s,opacity .2s}" +
  ".cm-leg:hover{background:#f1f5f9}.cm-leg.off{opacity:.35}.cm-leg.sel{background:#eef2f9;box-shadow:inset 0 0 0 1px #c7d2fe}" +
  ".cm-sw{width:14px;height:14px;border-radius:4px}.cm-lb{font-weight:500}.cm-ct{font-weight:700;font-size:11.5px}" +
  ".cm-lbar{grid-column:2/4;height:4px;background:#eef2f7;border-radius:3px;overflow:hidden}.cm-lbar i{display:block;height:100%;transition:width .6s ease}" +
  ".cm-top{display:grid;grid-template-columns:18px 1fr auto;gap:6px;align-items:center;padding:6px;border-radius:7px;cursor:pointer}.cm-top:hover{background:#f1f5f9}" +
  ".cm-top em{font-style:normal;font-weight:700;color:var(--sub)}.cm-top small{display:block;color:var(--sub);font-size:10.5px}.cm-top b{color:#b91c1c}" +
  ".cm-foot{margin-top:12px;font-size:10.5px;color:var(--sub);border-top:1px solid var(--borde);padding-top:8px}" +
  ".cm-tt{background:#fff;border:0;border-radius:10px;box-shadow:0 6px 20px rgba(0,0,0,.18);padding:8px 11px;font-family:Inter,'Segoe UI',sans-serif}" +
  ".cm-tt:before{display:none}.tt-d{font-weight:700;font-size:13px;color:var(--txt)}.tt-s{font-size:11px;color:var(--sub)}" +
  ".tt-v{margin-top:4px;font-weight:800;font-size:16px;color:var(--txt);display:flex;align-items:center;gap:6px}.tt-v i{width:10px;height:10px;border-radius:3px}.tt-v small{font-weight:500;font-size:10.5px;color:var(--sub)}" +
  ".pp-wrap .leaflet-popup-content-wrapper{border-radius:14px;padding:0;overflow:hidden;box-shadow:0 12px 32px rgba(0,0,0,.25)}" +
  ".pp-wrap .leaflet-popup-content{margin:0;width:290px!important}" +
  ".pp{font-family:Inter,'Segoe UI',sans-serif;color:#1f2937;font-size:12px}" +
  ".pp-h{background:linear-gradient(100deg,#0f2a63,#1a3f8b);color:#fff;padding:11px 14px}.pp-dist{font-size:15px;font-weight:700}.pp-sub{font-size:10.5px;opacity:.8;margin-top:2px}" +
  ".pp-pop{display:flex;justify-content:space-between;padding:9px 14px;background:#f6f8fc;border-bottom:1px solid #e5e7eb}.pp-pop b{font-size:14px}" +
  ".pp-row{display:flex;justify-content:space-between;align-items:center;padding:9px 14px 0}.pp-row b{font-size:15px}" +
  ".dot{display:inline-block;width:9px;height:9px;border-radius:3px;margin-right:6px}" +
  ".pp-bar{margin:5px 14px 0;height:7px;background:#e5e7eb;border-radius:5px;overflow:hidden}.pp-bar i{display:block;height:100%;border-radius:5px}" +
  ".pp-mini{padding:3px 14px 0;font-size:10.5px;color:#6b7280}" +
  ".pp-aporte{margin:10px 14px 0;padding:7px 10px;background:#f3effb;border-radius:8px;color:#4527a0;font-size:11.5px}" +
  ".pp-serv{display:flex;gap:4px;padding:10px 14px 12px}" +
  "body.cm-M .vx:not(.v-M),body.cm-T .vx:not(.v-T),body.cm-S .vx:not(.v-S){display:none!important}" +
  ".pp-serv span{flex:1;background:#f6f8fc;border-radius:7px;padding:5px 4px;text-align:center;font-size:10px;color:#6b7280}.pp-serv b{display:block;font-size:13px;color:#1f2937}" +
  "@media(max-width:760px){#cm-header{height:66px;gap:10px;padding:0 10px}#cm-logo{height:46px}#cm-logo img{height:36px}#cm-title{font-size:12.5px}#cm-src{display:none}" +
    ".leaflet-top{top:72px!important}#cm-panel{top:76px;width:calc(100% - 28px);max-height:calc(100vh - 92px)}#cm-toggle{top:76px}}" +
  "</style>" +

  "<div id='cm-header'>" +
    "<div id='cm-logo'><img id='cm-logo-img' src='" + MODES.M.logo + "'></div>" +
    "<div><div id='cm-title'>" + MODES.M.titulo + "</div></div>" +
    "<div id='cm-right'><div id='cm-src'>Fuente:<br><b id='cm-fuente'>" + MODES.M.fuente + "</b></div>" +
    "<button id='cm-fs' title='Pantalla completa'>⛶</button></div>" +
  "</div>" +

  "<div id='cm-panel'>" +
    "<div class='cm-h'>Vista del mapa<span class='cm-x' id='cm-close' title='Ocultar panel'>«</span></div>" +
    "<div id='cm-seg'>" + botones + "</div>" +

    "<div class='cm-sec'><div class='cm-h'>Buscar y filtrar</div>" +
      "<input id='cm-search' class='cm-in' list='cm-dl' placeholder='🔍  Buscar distrito…'><datalist id='cm-dl'>" + opciones + "</datalist>" +
      "<div class='cm-row'><select id='cm-reg' class='cm-in'><option value=''>Todo el Perú</option>" +
        regiones.map(function(r){ return "<option>" + esc(r) + "</option>"; }).join('') +
      "</select><button id='cm-reset' title='Restablecer vista'>↺</button></div>" +
    "</div>" +

    "<div class='cm-sec'><div class='cm-h'><span id='cm-ambito'>Perú</span></div>" +
      "<div id='cm-big'><span id='cm-bigv'>0</span><span id='cm-bigl'></span></div>" +
      "<div id='cm-stack'><i id='cm-s1' style='background:#1a3f8b'></i><i id='cm-s2' style='background:#7E57C2'></i></div>" +
      "<div class='cm-stl'><span><i style='background:#1a3f8b'></i>MINEDU <b id='cm-l1'></b></span><span><i style='background:#7E57C2'></i>Cuna Más <b id='cm-l2'></b></span></div>" +
      "<div class='cm-kpis'>" +
        "<div class='cm-kpi'><b id='cm-k1'>0</b><span>Niños 0–2 años</span></div>" +
        "<div class='cm-kpi'><b id='cm-k2'>0</b><span id='cm-k2l'>Coberturados</span></div>" +
        "<div class='cm-kpi'><b id='cm-k3'>0</b><span id='cm-k3l'>Sin cobertura</span></div>" +
      "</div>" +
    "</div>" +

    "<div class='cm-sec'><div class='cm-h'><span id='cm-legt'></span><span class='cm-x' id='cm-legx' style='display:none;font-size:11px'>quitar filtro ✕</span></div><div id='cm-leg'></div></div>" +
    "<div class='cm-sec' id='cm-topsec'><div class='cm-h'><span id='cm-topt'>Más niños sin cobertura</span></div><div id='cm-top'></div></div>" +
    "<div class='cm-foot'>Nota: Pasa el cursor sobre un distrito para ver su valor y haz clic para abrir su ficha.</div>" +
  "</div>" +
  "<button id='cm-toggle' title='Mostrar panel'>☰</button>";

  document.body.insertAdjacentHTML('afterbegin', html);
  var $ = function(id){ return document.getElementById(id); };
  document.body.classList.add('cm-M');

  /* ---------- Render ---------- */
  function render(){
    var m = MODES[state.mode], n = m.cats.length;
    var cnt = new Array(n).fill(0), pop = 0, cm = 0, ci = 0, ct = 0, nscd = 0, idx = [];

    for (var i = 0; i < N; i++) {
      var l = LY[i]; if (!l) continue;
      var c = m.catf(D[m.campo][i]);
      var enReg = !state.reg || D.reg[i] === state.reg;
      var vis   = enReg && (state.cat === null || state.cat === c);
      l.setStyle({ fillColor: c >= 0 ? m.cats[c].col : '#BDBDBD', fillOpacity: vis ? 0.85 : 0.07 });
      if (enReg) {
        if (c >= 0) cnt[c]++;
        pop += D.pop[i]; cm += D.cm[i]; ci += D.ciai[i]; ct += D.ct[i];
        if (D.ciai[i] > 0) nscd++;
        idx.push(i);
      }
    }

    // Indicadores
    $('cm-ambito').textContent = state.reg ? state.reg : 'Perú';
    var pM = pop ? cm/pop*100 : 0, pC = pop ? ci/pop*100 : 0, pT = pop ? ct/pop*100 : 0;
    if (state.mode === 'M') { anim($('cm-bigv'), pM, 1, '%');  $('cm-bigl').textContent = 'de cobertura: MINEDU'; }
    if (state.mode === 'T') { anim($('cm-bigv'), pT, 1, '%');  $('cm-bigl').textContent = 'de cobertura: MINEDU + Cuna Más'; }
    if (state.mode === 'S') { anim($('cm-bigv'), pC, 1, ' pp'); $('cm-bigl').textContent = 'Aporte de Cuna Más'; }
    $('cm-s1').style.width = pM + '%';
    $('cm-s2').style.width = (state.mode === 'M' ? 0 : pC) + '%';
    $('cm-l1').textContent = fP(pM) + '%';
    $('cm-l2').textContent = '+' + fP(pC) + ' pp';

    anim($('cm-k1'), pop, 0);
    if (state.mode === 'M') { anim($('cm-k2'), cm, 0); $('cm-k2l').textContent = 'Coberturados'; anim($('cm-k3'), pop - cm, 0); $('cm-k3l').textContent = 'Sin cobertura'; }
    if (state.mode === 'T') { anim($('cm-k2'), ct, 0); $('cm-k2l').textContent = 'Coberturados'; anim($('cm-k3'), pop - ct, 0); $('cm-k3l').textContent = 'Sin cobertura'; }
    if (state.mode === 'S') { anim($('cm-k2'), ci, 0); $('cm-k2l').textContent = 'Atendidos Cuna Más'; anim($('cm-k3'), nscd, 0); $('cm-k3l').textContent = 'Distritos con SCD'; }

    // Leyenda
    var tot = cnt.reduce(function(a,b){ return a+b; }, 0) || 1;
    $('cm-legt').textContent = m.leyenda + ' | N° distritos';
    $('cm-legx').style.display = state.cat === null ? 'none' : 'inline';
    $('cm-leg').innerHTML = m.cats.map(function(k, j){
      var cls = state.cat === null ? '' : (state.cat === j ? ' sel' : ' off');
      return "<div class='cm-leg" + cls + "' data-c='" + j + "'><span class='cm-sw' style='background:" + k.col + "'></span>" +
             "<span class='cm-lb'>" + k.lab + "</span><span class='cm-ct'>" + fN(cnt[j]) + "</span>" +
             "<div class='cm-lbar'><i style='width:" + (cnt[j]/tot*100) + "%;background:" + k.col + "'></i></div></div>";
    }).join('');

    // Top 5 (no aplica en la vista Aporte Cuna Más)
    $('cm-topsec').style.display = state.mode === 'S' ? 'none' : '';
    if (state.mode === 'S') return;
    var key = state.mode === 'M' ? 'cm' : 'ct';
    idx.sort(function(a, b){ return (D.pop[b] - D[key][b]) - (D.pop[a] - D[key][a]); });
    $('cm-topt').textContent = 'Más niños sin cobertura: ' + (state.mode === 'M' ? 'MINEDU' : 'MINEDU + Cuna Más');
    $('cm-top').innerHTML = idx.slice(0, 5).map(function(i, r){
      return "<div class='cm-top' data-i='" + i + "'><em>" + (r+1) + "</em><div>" + esc(D.dist[i]) +
             "<small>" + esc(D.prov[i]) + ", " + esc(D.dep[i]) + "</small></div><b>" + fN(D.pop[i] - D[key][i]) + "</b></div>";
    }).join('');
  }

  function setMode(k){
    if (k === state.mode) return;
    var cambiaEscala = (MODES[k].cats !== MODES[state.mode].cats);
    document.body.classList.remove('cm-' + state.mode);
    document.body.classList.add('cm-' + k);
    state.mode = k;
    if (cambiaEscala) state.cat = null;
    Array.prototype.forEach.call(document.querySelectorAll('#cm-seg button'), function(b){ b.classList.toggle('on', b.getAttribute('data-m') === k); });
    var m = MODES[k];
    ['cm-title', 'cm-logo'].forEach(function(id){ $(id).style.opacity = 0; });
    setTimeout(function(){
      $('cm-title').textContent = m.titulo;
      $('cm-fuente').textContent = m.fuente;
      $('cm-logo-img').src = m.logo;
      ['cm-title', 'cm-logo'].forEach(function(id){ $(id).style.opacity = 1; });
    }, 280);
    render();
  }

  function boundsReg(r){
    var b = null;
    for (var i = 0; i < N; i++) if (LY[i] && D.reg[i] === r) b = b ? b.extend(LY[i].getBounds()) : L.latLngBounds(LY[i].getBounds().getSouthWest(), LY[i].getBounds().getNorthEast());
    return b;
  }
  function panelPad(){ return window.innerWidth > 760 && !$('cm-panel').classList.contains('closed') ? [360, 20] : [20, 20]; }
  function fitTo(b, maxZ){ if (b) map.flyToBounds(b, {paddingTopLeft: panelPad(), paddingBottomRight:[20,20], maxZoom: maxZ || 10, duration: 0.9}); }

  function zoomTo(i){
    var l = LY[i]; if (!l) return;
    fitTo(l.getBounds(), 11);
    setTimeout(function(){ l.openPopup(l.getBounds().getCenter()); }, 1000);
  }

  /* ---------- Eventos ---------- */
  $('cm-seg').addEventListener('click', function(e){ var b = e.target.closest('button'); if (b) setMode(b.getAttribute('data-m')); });

  $('cm-leg').addEventListener('click', function(e){
    var it = e.target.closest('.cm-leg'); if (!it) return;
    var c = parseInt(it.getAttribute('data-c'), 10);
    state.cat = (state.cat === c) ? null : c; render();
  });
  $('cm-legx').addEventListener('click', function(){ state.cat = null; render(); });

  $('cm-top').addEventListener('click', function(e){ var it = e.target.closest('.cm-top'); if (it) zoomTo(parseInt(it.getAttribute('data-i'), 10)); });

  $('cm-reg').addEventListener('change', function(){
    state.reg = this.value || null; render();
    fitTo(state.reg ? boundsReg(state.reg) : boundsPeru, state.reg ? 10 : 6);
  });

  var etiquetas = {};
  D.dist.forEach(function(d, i){ etiquetas[d + ' — ' + D.prov[i] + ', ' + D.dep[i]] = i; });
  $('cm-search').addEventListener('change', function(){
    var i = etiquetas[this.value];
    if (i !== undefined) { if (window.innerWidth <= 760) cerrarPanel(true); zoomTo(i); }
  });

  $('cm-reset').addEventListener('click', function(){
    state.reg = null; state.cat = null; $('cm-reg').value = ''; $('cm-search').value = '';
    map.closePopup(); render(); fitTo(boundsPeru, 6);
  });

  function cerrarPanel(c){ $('cm-panel').classList.toggle('closed', c); }
  $('cm-close').addEventListener('click', function(){ cerrarPanel(true); });
  $('cm-toggle').addEventListener('click', function(){ cerrarPanel(false); });
  if (window.innerWidth <= 760) cerrarPanel(true);

  $('cm-fs').addEventListener('click', function(){
    if (!document.fullscreenElement) document.documentElement.requestFullscreen && document.documentElement.requestFullscreen();
    else document.exitFullscreen && document.exitFullscreen();
  });

  L.control.zoom({position:'topright'}).addTo(map);

  render();
  setTimeout(function(){ map.invalidateSize(); fitTo(boundsPeru, 6); }, 150);
}
)---"


# Construir y guardar el mapa
#-------------------------------------------------------------------------------
Mimapa_integrado_C1 <- onRender(
  Mimapa,
  js_interfaz,
  data = list(d = datos_js, logos = logos)
)

saveWidget(
  Mimapa_integrado_C1,
  SALIDA_HTML,
  selfcontained = TRUE,
  title = "Ciclo I: Cobertura MINEDU y Cuna Más, 2025"
)

Mimapa_integrado_C1
