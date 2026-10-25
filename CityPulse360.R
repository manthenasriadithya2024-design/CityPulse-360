library(shiny)
library(plotly)
library(DT)
library(leaflet)
set.seed(360)

locations <- c(
  "Katpadi", "VIT Area", "Gandhi Nagar", "Sathuvachari",
  "Bagayam", "Thorapadi"
  , "Town Centre", "CMC Area"
)
location_coordinates <- data.frame(
  location = locations,
  latitude = c(12.9692, 12.9698, 12.9793, 12.9345,
               12.8882, 12.9005, 12.9165, 12.9239),
  longitude = c(79.1453, 79.1559, 79.1425, 79.1335,
                79.1337, 79.1157, 79.1325, 79.1355),
  stringsAsFactors = FALSE
)

dimensions <- c(
  "Mobility", "Cleanliness", "Water",
  "Environment", "Public Facilities", "Development"
)

indicators <- list(
  Mobility = c("Traffic Volume", "Travel Time", "Congestion"),
  Cleanliness = c("Waste Collected", "Waste Segregation", "Waste Issues"),
  Water = c("Water Supply", "Water Quality", "Leakage Reports"),
  Environment = c("Air Quality Index", "Green Cover", "Noise Level"),
  "Public Facilities" = c("Streetlight Availability", "Healthcare Access",
                          "Service Requests"),
  Development = c("Road Quality", "Construction Activity",
                  "Digital Service Access")
)

dates <- seq.Date(
  as.Date("2025-01-01"),
  as.Date("2026-09-01"),
  by = "month"
)

city_data <- expand.grid(
  dimension = dimensions,
  location = locations,
  date = dates,
  stringsAsFactors = FALSE
)

city_data$indicator <- vapply(
  city_data$dimension,
  function(x) sample(indicators[[x]], 1),
  character(1)
)
make_value <- function(indicator) {
  ranges <- list(
    "Traffic Volume" = c(1500, 9500),
    "Travel Time" = c(8, 45),
    "Congestion" = c(10, 100),
    "Waste Collected" = c(200, 2000),
    "Waste Segregation" = c(10, 95),
    "Waste Issues" = c(0, 40),
    "Water Supply" = c(2, 20),
    "Water Quality" = c(40, 100),
    "Leakage Reports" = c(0, 30),
    "Air Quality Index" = c(20, 200),
    "Green Cover" = c(5, 50),
    "Noise Level" = c(35, 95),
    "Streetlight Availability" = c(30, 100),
    "Healthcare Access" = c(20, 100),
    "Service Requests" = c(0, 60),
    "Road Quality" = c(20, 100),
    "Construction Activity" = c(0, 35),
    "Digital Service Access" = c(20, 100)
  )
  
  limits <- ranges[[indicator]]
  round(runif(1, limits[1], limits[2]), 2)
}

city_data$value <- vapply(
  city_data$indicator,
  make_value,
  numeric(1)
)

city_data$source <- "Synthetic demonstration data"

# 2. DESIGN COLORS

navy <- "#10172A"
blue <- "#4D8DFF"
teal <- "#20D6C7"
purple <- "#9B7BFF"
pink <- "#FF6FAE"
orange <- "#FFB45C"

palette <- c(blue, teal, purple, pink, orange, "#1DA8D5")

# 3. CREATE A REUSABLE KPI CARD

kpi_card <- function(title, value, subtitle, color) {
  tags$div(
    class = "kpi-card",
    style = paste0("border-top:4px solid ", color, ";"),
    tags$div(class = "kpi-title", title),
    tags$div(class = "kpi-value", value),
    tags$div(class = "kpi-subtitle", subtitle)
  )
}

# 4. PLOTLY THEME

chart_theme <- function(p, height = 370) {
  p %>%
    layout(
      height = height,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(
        family = "'DM Sans', sans-serif",
        color = "#94A3B8",
        size = 12
      ),
      margin = list(l = 65, r = 30, t = 25, b = 65),
      xaxis = list(
        gridcolor = "rgba(255, 255, 255, 0.06)",
        zerolinecolor = "rgba(255, 255, 255, 0.12)",
        tickfont = list(color = "#94A3B8"),
        titlefont = list(color = "#CBD5E1")
      ),
      yaxis = list(
        gridcolor = "rgba(255, 255, 255, 0.06)",
        zerolinecolor = "rgba(255, 255, 255, 0.12)",
        tickfont = list(color = "#94A3B8"),
        titlefont = list(color = "#CBD5E1")
      ),
      legend = list(
        orientation = "h",
        x = 0,
        y = -0.25,
        font = list(color = "#CBD5E1")
      )
    ) %>%
    config(
      displaylogo = FALSE,
      responsive = TRUE
    )
}

# 5. CUSTOM CSS (OBSIDIAN CYBER-GLASS EXECUTIVE DARK THEME)

custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=Space+Grotesk:wght@500;600;700&display=swap');

:root {
  --bg-space: #070B14;
  --bg-card: rgba(15, 23, 42, 0.78);
  --bg-card-hover: rgba(22, 33, 60, 0.88);
  --border-glass: rgba(255, 255, 255, 0.09);
  --border-glass-glow: rgba(32, 214, 199, 0.35);
  --cyan-glow: #20D6C7;
  --blue-glow: #315BEA;
  --violet-glow: #8B5CF6;
  --text-main: #F1F5F9;
  --text-muted: #94A3B8;
  --text-dim: #64748B;
}

/* Global Reset & Atmospheric Background */
body {
  background: radial-gradient(circle at 12% 12%, #0F172A 0%, #070B14 55%, #03060C 100%) fixed;
  font-family: 'DM Sans', -apple-system, sans-serif;
  color: var(--text-main);
  min-height: 100vh;
  margin: 0;
  padding: 0 15px;
  overflow-x: hidden;
}

h1, h2, h3, h4, h5, h6 {
  font-family: 'Space Grotesk', sans-serif;
  color: #FFFFFF;
  letter-spacing: -0.5px;
}

/* Custom Sleek Scrollbar */
::-webkit-scrollbar {
  width: 7px;
  height: 7px;
}
::-webkit-scrollbar-track {
  background: #060911;
}
::-webkit-scrollbar-thumb {
  background: rgba(255, 255, 255, 0.15);
  border-radius: 4px;
}
::-webkit-scrollbar-thumb:hover {
  background: var(--cyan-glow);
}

/* Header with Glassmorphism & Cyber Depth */
.top-header {
  padding: 24px 34px;
  margin: -15px -15px 26px -15px;
  border-radius: 0 0 28px 28px;
  color: white;
  background: linear-gradient(135deg, rgba(6, 9, 19, 0.96) 0%, rgba(15, 23, 42, 0.94) 50%, rgba(20, 28, 55, 0.96) 100%);
  backdrop-filter: blur(20px);
  -webkit-backdrop-filter: blur(20px);
  border-bottom: 1px solid var(--border-glass);
  box-shadow: 0 16px 40px rgba(0, 0, 0, 0.55), 0 0 35px rgba(32, 214, 199, 0.08);
}

.brand {
  font-size: 30px;
  font-weight: 700;
  letter-spacing: -1px;
  color: #FFFFFF;
}

.brand span {
  background: linear-gradient(135deg, #20D6C7 0%, #38BDF8 50%, #A78BFA 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  text-shadow: 0 0 25px rgba(32, 214, 199, 0.45);
}

.tagline {
  color: #94A3B8;
  font-size: 13px;
  margin-top: 5px;
  letter-spacing: 0.3px;
}

/* Hero Section */
.hero {
  padding: 42px;
  border-radius: 26px;
  color: white;
  margin-bottom: 26px;
  background:
    radial-gradient(circle at 85% 20%, rgba(32, 214, 199, 0.22), transparent 38%),
    radial-gradient(circle at 15% 90%, rgba(139, 92, 246, 0.22), transparent 42%),
    linear-gradient(135deg, rgba(15, 23, 42, 0.9) 0%, rgba(11, 17, 32, 0.92) 100%);
  backdrop-filter: blur(20px);
  -webkit-backdrop-filter: blur(20px);
  border: 1px solid var(--border-glass);
  box-shadow: 0 20px 50px rgba(0, 0, 0, 0.5), inset 0 1px 0 rgba(255, 255, 255, 0.12);
}

.hero h1 {
  font-size: 46px;
  line-height: 1.14;
  letter-spacing: -1.5px;
  text-shadow: 0 4px 20px rgba(0, 0, 0, 0.5);
}

.hero p {
  max-width: 780px;
  color: #CBD5E1;
  font-size: 15px;
  line-height: 1.8;
}

.hero-label {
  color: var(--cyan-glow);
  letter-spacing: 2.5px;
  font-size: 11px;
  font-weight: 700;
  text-shadow: 0 0 12px rgba(32, 214, 199, 0.6);
}

/* Reusable Glassmorphism KPI Card */
.kpi-card {
  background: var(--bg-card);
  backdrop-filter: blur(16px);
  -webkit-backdrop-filter: blur(16px);
  border-radius: 20px;
  padding: 22px;
  margin-bottom: 20px;
  min-height: 130px;
  box-shadow: 0 12px 35px rgba(0, 0, 0, 0.4), inset 0 1px 0 rgba(255, 255, 255, 0.08);
  border: 1px solid var(--border-glass);
  border-top-width: 4px;
  transition: all 0.28s cubic-bezier(0.4, 0, 0.2, 1);
  cursor: pointer;
}

.kpi-card:hover {
  transform: translateY(-4px);
  box-shadow: 0 18px 45px rgba(0, 0, 0, 0.55), 0 0 25px rgba(32, 214, 199, 0.2);
  border-color: rgba(255, 255, 255, 0.2);
}

.kpi-card:active {
  transform: scale(0.98) translateY(1px);
}

.kpi-title {
  color: var(--text-muted);
  text-transform: uppercase;
  font-size: 11px;
  font-weight: 700;
  letter-spacing: 1.2px;
}

.kpi-value {
  font-family: 'Space Grotesk', sans-serif;
  color: #FFFFFF;
  font-size: 32px;
  font-weight: 700;
  margin: 10px 0 4px;
  text-shadow: 0 0 20px rgba(255, 255, 255, 0.15);
}

.kpi-subtitle {
  color: var(--text-dim);
  font-size: 11px;
}

/* Core Obsidian Glass Panel */
.panel {
  background: var(--bg-card) !important;
  backdrop-filter: blur(18px) !important;
  -webkit-backdrop-filter: blur(18px) !important;
  border: 1px solid var(--border-glass) !important;
  border-radius: 22px !important;
  padding: 24px !important;
  margin-bottom: 22px !important;
  box-shadow: 0 14px 40px rgba(0, 0, 0, 0.42), inset 0 1px 0 rgba(255, 255, 255, 0.07) !important;
  transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1) !important;
}

.panel:hover {
  border-color: rgba(32, 214, 199, 0.24) !important;
  box-shadow: 0 18px 48px rgba(0, 0, 0, 0.52), 0 0 25px rgba(32, 214, 199, 0.08) !important;
}

.panel-title {
  color: #FFFFFF !important;
  font-family: 'Space Grotesk', sans-serif !important;
  font-weight: 700 !important;
  font-size: 17px !important;
  margin-bottom: 4px !important;
  letter-spacing: -0.3px !important;
}

.panel-subtitle {
  color: var(--text-muted) !important;
  font-size: 12px !important;
  margin-bottom: 16px !important;
}

.section-title {
  font-size: 29px !important;
  font-weight: 700 !important;
  letter-spacing: -0.8px !important;
  color: #FFFFFF !important;
}

.section-subtitle {
  color: var(--text-muted) !important;
  margin-bottom: 20px !important;
  font-size: 13px !important;
}

/* Capsule Navigation Pills with Glowing Active Tab */
.nav-pills {
  display: flex !important;
  flex-wrap: wrap !important;
  gap: 8px !important;
  background: rgba(11, 17, 32, 0.7) !important;
  border: 1px solid var(--border-glass) !important;
  border-radius: 18px !important;
  padding: 6px !important;
  backdrop-filter: blur(14px) !important;
}

.nav-pills > li > a {
  border-radius: 12px !important;
  font-weight: 600 !important;
  color: var(--text-muted) !important;
  padding: 10px 16px !important;
  font-size: 13px !important;
  transition: all 0.22s cubic-bezier(0.4, 0, 0.2, 1) !important;
  cursor: pointer !important;
}

.nav-pills > li > a:hover {
  color: #FFFFFF !important;
  background: rgba(255, 255, 255, 0.07) !important;
}

.nav-pills > li > a:active {
  transform: scale(0.96) !important;
}

.nav-pills > li.active > a,
.nav-pills > li.active > a:hover,
.nav-pills > li.active > a:focus {
  color: #FFFFFF !important;
  background: linear-gradient(135deg, #315BEA 0%, #6366F1 50%, #20D6C7 100%) !important;
  box-shadow: 0 6px 20px rgba(49, 91, 234, 0.45), 0 0 15px rgba(32, 214, 199, 0.25) !important;
  font-weight: 700 !important;
}

/* Form Controls & Dropdown Inputs - Dark Cyber Edition */
.form-control,
.selectize-input,
.selectize-control.single .selectize-input {
  background: #0B1120 !important;
  border: 1px solid rgba(255, 255, 255, 0.12) !important;
  border-radius: 12px !important;
  color: #F1F5F9 !important;
  box-shadow: 0 3px 10px rgba(0, 0, 0, 0.25) !important;
  padding: 10px 14px !important;
  font-size: 13px !important;
  font-weight: 500 !important;
  transition: all 0.22s cubic-bezier(0.4, 0, 0.2, 1) !important;
  cursor: pointer !important;
}

.selectize-input.focus,
.form-control:focus {
  border-color: var(--cyan-glow) !important;
  box-shadow: 0 0 0 3px rgba(32, 214, 199, 0.25), 0 0 18px rgba(32, 214, 199, 0.25) !important;
  background: #0F172A !important;
}

.selectize-input > .item,
.selectize-input > input {
  color: #F1F5F9 !important;
  font-family: 'DM Sans', sans-serif !important;
}

.selectize-dropdown {
  background: #0B1120 !important;
  border: 1px solid rgba(255, 255, 255, 0.16) !important;
  border-radius: 14px !important;
  box-shadow: 0 16px 45px rgba(0, 0, 0, 0.7) !important;
  color: #F1F5F9 !important;
  overflow: hidden !important;
  padding: 6px !important;
  z-index: 2000 !important;
}

.selectize-dropdown .option {
  color: #CBD5E1 !important;
  padding: 9px 12px !important;
  border-radius: 8px !important;
  font-size: 13px !important;
  transition: background 0.15s ease, color 0.15s ease !important;
}

.selectize-dropdown .option:hover,
.selectize-dropdown .option.active {
  background: linear-gradient(90deg, rgba(49, 91, 234, 0.4), rgba(32, 214, 199, 0.25)) !important;
  color: var(--cyan-glow) !important;
}

label, .control-label {
  color: #CBD5E1 !important;
  font-size: 12px !important;
  font-weight: 600 !important;
  letter-spacing: 0.3px !important;
  margin-bottom: 6px !important;
}

/* Checkbox Inputs with Neon Accent */
.checkbox label {
  color: #CBD5E1 !important;
  font-weight: 500 !important;
  font-size: 12px !important;
  cursor: pointer !important;
  transition: color 0.2s ease !important;
}

.checkbox label:hover {
  color: var(--cyan-glow) !important;
}

.checkbox input[type='checkbox'] {
  accent-color: var(--cyan-glow) !important;
  cursor: pointer !important;
  width: 15px !important;
  height: 15px !important;
  vertical-align: middle !important;
}

/* Interactive Buttons with Shimmer & Click Depression */
.btn-primary {
  background: linear-gradient(135deg, #315BEA 0%, #6366F1 50%, #20D6C7 100%) !important;
  border: 1px solid rgba(255, 255, 255, 0.18) !important;
  border-radius: 12px !important;
  color: #FFFFFF !important;
  font-weight: 700 !important;
  letter-spacing: 0.4px !important;
  box-shadow: 0 6px 20px rgba(49, 91, 234, 0.38) !important;
  transition: all 0.22s cubic-bezier(0.4, 0, 0.2, 1) !important;
  cursor: pointer !important;
}

.btn-primary:hover {
  transform: translateY(-2px) !important;
  box-shadow: 0 10px 28px rgba(32, 214, 199, 0.45), 0 0 20px rgba(49, 91, 234, 0.4) !important;
  color: #FFFFFF !important;
}

.btn-primary:active {
  transform: scale(0.96) translateY(1px) !important;
  box-shadow: 0 3px 10px rgba(49, 91, 234, 0.3) !important;
}

.btn-default {
  background: rgba(30, 41, 59, 0.7) !important;
  border: 1px solid rgba(255, 255, 255, 0.12) !important;
  border-radius: 12px !important;
  color: #E2E8F0 !important;
  font-weight: 600 !important;
  transition: all 0.22s cubic-bezier(0.4, 0, 0.2, 1) !important;
  cursor: pointer !important;
}

.btn-default:hover {
  background: rgba(51, 65, 85, 0.9) !important;
  color: var(--cyan-glow) !important;
  border-color: rgba(32, 214, 199, 0.35) !important;
  transform: translateY(-2px) !important;
  box-shadow: 0 6px 18px rgba(0, 0, 0, 0.4) !important;
}

.btn-default:active {
  transform: scale(0.96) translateY(1px) !important;
}

/* Map Metrics KPI Cards */
.map-stat-card {
  background: var(--bg-card) !important;
  backdrop-filter: blur(16px) !important;
  -webkit-backdrop-filter: blur(16px) !important;
  border-radius: 20px !important;
  padding: 18px 22px !important;
  margin-bottom: 20px !important;
  border: 1px solid var(--border-glass) !important;
  border-left: 5px solid #315BEA !important;
  box-shadow: 0 10px 32px rgba(0, 0, 0, 0.38), inset 0 1px 0 rgba(255, 255, 255, 0.07) !important;
  transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1) !important;
  cursor: pointer !important;
}

.map-stat-card:hover {
  transform: translateY(-4px) !important;
  box-shadow: 0 16px 42px rgba(0, 0, 0, 0.55), 0 0 25px rgba(49, 91, 234, 0.2) !important;
  border-color: rgba(255, 255, 255, 0.2) !important;
}

.map-stat-card:active {
  transform: scale(0.98) translateY(1px) !important;
}

.map-stat-label {
  font-size: 11px !important;
  font-weight: 700 !important;
  color: var(--text-muted) !important;
  text-transform: uppercase !important;
  letter-spacing: 1.1px !important;
}

.map-stat-val {
  font-family: 'Space Grotesk', sans-serif !important;
  font-size: 26px !important;
  font-weight: 700 !important;
  color: #FFFFFF !important;
  margin: 6px 0 3px !important;
  text-shadow: 0 0 16px rgba(255, 255, 255, 0.15) !important;
}

.map-stat-sub {
  font-size: 11px !important;
  color: var(--text-dim) !important;
}

/* Location Intelligence Dossier Card */
.location-dossier-card {
  background: var(--bg-card) !important;
  backdrop-filter: blur(18px) !important;
  -webkit-backdrop-filter: blur(18px) !important;
  border-radius: 22px !important;
  border: 1px solid var(--border-glass) !important;
  padding: 24px !important;
  box-shadow: 0 16px 45px rgba(0, 0, 0, 0.48), inset 0 1px 0 rgba(255, 255, 255, 0.08) !important;
  position: relative !important;
  min-height: 620px !important;
  display: flex !important;
  flex-direction: column !important;
  justify-content: space-between !important;
}

.dossier-header {
  display: flex !important;
  align-items: center !important;
  justify-content: space-between !important;
  border-bottom: 1px solid rgba(255, 255, 255, 0.08) !important;
  padding-bottom: 16px !important;
  margin-bottom: 16px !important;
}

.dossier-title {
  font-family: 'Space Grotesk', sans-serif !important;
  font-size: 24px !important;
  font-weight: 700 !important;
  color: #FFFFFF !important;
  letter-spacing: -0.5px !important;
  text-shadow: 0 0 18px rgba(255, 255, 255, 0.2) !important;
}

.dossier-badge {
  display: inline-block !important;
  padding: 5px 14px !important;
  border-radius: 20px !important;
  font-size: 11px !important;
  font-weight: 700 !important;
  letter-spacing: 0.6px !important;
  border: 1px solid rgba(255, 255, 255, 0.15) !important;
}

.dossier-meter-wrap {
  margin: 12px 0 6px !important;
}

.dossier-meter-bar {
  height: 8px !important;
  border-radius: 4px !important;
  background: rgba(30, 41, 59, 0.8) !important;
  overflow: hidden !important;
  margin-top: 6px !important;
}

.dossier-meter-fill {
  height: 100% !important;
  border-radius: 4px !important;
  transition: width 0.6s cubic-bezier(0.4, 0, 0.2, 1) !important;
  box-shadow: 0 0 10px rgba(32, 214, 199, 0.5) !important;
}

.dossier-grid {
  display: grid !important;
  grid-template-columns: repeat(3, 1fr) !important;
  gap: 8px !important;
  margin-top: 10px !important;
}

.dossier-cell {
  background: rgba(11, 17, 32, 0.65) !important;
  border-radius: 12px !important;
  padding: 10px !important;
  border: 1px solid rgba(255, 255, 255, 0.06) !important;
  text-align: center !important;
  transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1) !important;
  cursor: pointer !important;
}

.dossier-cell:hover {
  transform: translateY(-2px) !important;
  background: rgba(49, 91, 234, 0.22) !important;
  border-color: rgba(32, 214, 199, 0.35) !important;
  box-shadow: 0 4px 14px rgba(32, 214, 199, 0.2) !important;
}

.dossier-cell:active {
  transform: scale(0.95) !important;
}

.dossier-cell-dim {
  font-size: 10px !important;
  font-weight: 700 !important;
  color: var(--text-muted) !important;
  text-transform: uppercase !important;
  white-space: nowrap !important;
  overflow: hidden !important;
  text-overflow: ellipsis !important;
}

.dossier-cell-val {
  font-family: 'Space Grotesk', sans-serif !important;
  font-size: 16px !important;
  font-weight: 700 !important;
  color: var(--cyan-glow) !important;
  margin-top: 2px !important;
}

/* Map Wrap Container */
#geo-map-wrap {
  position: relative !important;
  width: 100% !important;
  background: #080C14 !important;
  border-radius: 22px !important;
  overflow: hidden !important;
  border: 1px solid rgba(255, 255, 255, 0.12) !important;
  box-shadow: 0 16px 45px rgba(0, 0, 0, 0.55), 0 0 30px rgba(49, 91, 234, 0.15) !important;
}

#geo-map-wrap:fullscreen {
  width: 100vw !important;
  height: 100vh !important;
  border-radius: 0 !important;
  border: none !important;
  background: #03060C !important;
}

#geo-map-wrap:fullscreen #geo_map,
#geo-map-wrap:fullscreen .leaflet,
#geo-map-wrap:fullscreen .leaflet-container {
  width: 100% !important;
  height: 100% !important;
}

#geo_map_fullscreen {
  position: absolute !important;
  z-index: 1200 !important;
  top: 14px !important;
  right: 14px !important;
  border-radius: 12px !important;
  box-shadow: 0 4px 16px rgba(0, 0, 0, 0.4) !important;
  font-weight: 600 !important;
  font-size: 12px !important;
  padding: 8px 14px !important;
  transition: all 0.2s ease !important;
}

#geo_map_fullscreen:hover {
  transform: translateY(-1px) !important;
  box-shadow: 0 6px 20px rgba(32, 214, 199, 0.4) !important;
}

/* Dark DataTables Styling */
.dataTables_wrapper {
  color: #E2E8F0 !important;
  font-family: 'DM Sans', sans-serif !important;
  margin-top: 8px !important;
}

.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter,
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate {
  color: var(--text-muted) !important;
  font-size: 12px !important;
}

.dataTables_wrapper .dataTables_filter input {
  background: #0B1120 !important;
  border: 1px solid rgba(255, 255, 255, 0.12) !important;
  border-radius: 10px !important;
  color: #F1F5F9 !important;
  padding: 5px 10px !important;
  margin-left: 6px !important;
}

.dataTables_wrapper .dataTables_filter input:focus {
  border-color: var(--cyan-glow) !important;
  outline: none !important;
  box-shadow: 0 0 10px rgba(32, 214, 199, 0.3) !important;
}

table.dataTable {
  background: transparent !important;
  border-collapse: separate !important;
  border-spacing: 0 4px !important;
}

table.dataTable thead th {
  background: rgba(17, 24, 39, 0.85) !important;
  color: #38BDF8 !important;
  border: none !important;
  border-bottom: 2px solid rgba(255, 255, 255, 0.12) !important;
  font-family: 'Space Grotesk', sans-serif !important;
  font-weight: 700 !important;
  font-size: 12px !important;
  text-transform: uppercase !important;
  letter-spacing: 0.8px !important;
  padding: 12px 10px !important;
}

table.dataTable tbody tr {
  background: rgba(15, 23, 42, 0.55) !important;
  color: #E2E8F0 !important;
  transition: all 0.18s cubic-bezier(0.4, 0, 0.2, 1) !important;
  cursor: pointer !important;
}

table.dataTable tbody tr td {
  border-top: 1px solid rgba(255, 255, 255, 0.04) !important;
  border-bottom: 1px solid rgba(255, 255, 255, 0.04) !important;
  padding: 10px !important;
}

table.dataTable tbody tr:hover {
  background: rgba(49, 91, 234, 0.22) !important;
  transform: translateY(-1px) !important;
  box-shadow: 0 4px 14px rgba(0, 0, 0, 0.35) !important;
}

table.dataTable tbody tr.selected {
  background: linear-gradient(90deg, rgba(49, 91, 234, 0.35), rgba(32, 214, 199, 0.2)) !important;
}

.dataTables_wrapper .dataTables_paginate .paginate_button {
  background: rgba(30, 41, 59, 0.6) !important;
  color: #CBD5E1 !important;
  border: 1px solid rgba(255, 255, 255, 0.1) !important;
  border-radius: 8px !important;
  padding: 4px 10px !important;
  margin: 0 2px !important;
  font-size: 11px !important;
  transition: all 0.15s ease !important;
}

.dataTables_wrapper .dataTables_paginate .paginate_button:hover,
.dataTables_wrapper .dataTables_paginate .paginate_button.current {
  background: linear-gradient(135deg, #315BEA, #20D6C7) !important;
  color: #FFFFFF !important;
  border-color: transparent !important;
  box-shadow: 0 2px 10px rgba(32, 214, 199, 0.35) !important;
}

/* Modern Dark Acrylic Leaflet Popups */
.leaflet-popup-content-wrapper {
  background: rgba(10, 15, 29, 0.95) !important;
  backdrop-filter: blur(16px) !important;
  -webkit-backdrop-filter: blur(16px) !important;
  color: #F8FAFC !important;
  border-radius: 18px !important;
  box-shadow: 0 24px 50px rgba(0, 0, 0, 0.6), 0 0 0 1px rgba(255, 255, 255, 0.15) !important;
  padding: 2px !important;
}

.leaflet-popup-tip {
  background: rgba(10, 15, 29, 0.95) !important;
}

.leaflet-popup-content {
  margin: 12px 16px !important;
  font-family: 'DM Sans', Arial, sans-serif !important;
  line-height: 1.4 !important;
}

.leaflet-container a.leaflet-popup-close-button {
  color: #94A3B8 !important;
  top: 10px !important;
  right: 12px !important;
  font-size: 18px !important;
}

.leaflet-container a.leaflet-popup-close-button:hover {
  color: var(--cyan-glow) !important;
}

/* Remove Leaflet's default white tooltip wrapper around custom dark label pills. */
.leaflet-tooltip.route-frosted-label,
.leaflet-tooltip.route-distance-label,
.leaflet-tooltip.geo-frosted-label,
.leaflet-tooltip.geo-pill-label {
  background: transparent !important;
  border: none !important;
  box-shadow: none !important;
  padding: 0 !important;
  color: inherit !important;
  border-radius: 0 !important;
}
.leaflet-tooltip.route-frosted-label:before,
.leaflet-tooltip.route-distance-label:before,
.leaflet-tooltip.geo-frosted-label:before,
.leaflet-tooltip.geo-pill-label:before {
  display: none !important;
  border: none !important;
}

/* Frosted Map Labels */
.geo-pill-label {
  background: rgba(8, 12, 22, 0.9) !important;
  backdrop-filter: blur(10px) !important;
  -webkit-backdrop-filter: blur(10px) !important;
  border: 1px solid rgba(255, 255, 255, 0.22) !important;
  color: white !important;
  padding: 4px 10px !important;
  border-radius: 20px !important;
  font-family: 'DM Sans', sans-serif !important;
  font-size: 11px !important;
  font-weight: 600 !important;
  display: inline-flex !important;
  align-items: center !important;
  gap: 6px !important;
  box-shadow: 0 6px 18px rgba(0, 0, 0, 0.45) !important;
  pointer-events: none !important;
  white-space: nowrap !important;
}

/* Animations */
@keyframes pulse-ring {
  0% { transform: scale(0.95); opacity: 0.8; }
  50% { transform: scale(1.35); opacity: 0.2; }
  100% { transform: scale(0.95); opacity: 0.8; }
}

@keyframes pulse-glow {
  0%, 100% { filter: drop-shadow(0 0 6px rgba(32, 214, 199, 0.8)); }
  50% { filter: drop-shadow(0 0 16px rgba(49, 91, 234, 0.9)); }
}

@keyframes beacon-pulse {
  0% { box-shadow: 0 0 0 0 rgba(32, 214, 199, 0.7); }
  70% { box-shadow: 0 0 0 10px rgba(32, 214, 199, 0); }
  100% { box-shadow: 0 0 0 0 rgba(32, 214, 199, 0); }
}

.live-beacon {
  display: inline-block;
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: var(--cyan-glow);
  animation: beacon-pulse 2s infinite cubic-bezier(0.4, 0, 0.6, 1);
  margin-right: 6px;
  vertical-align: middle;
}

.footer {
  color: var(--text-dim) !important;
  text-align: center !important;
  font-size: 11px !important;
  padding: 24px 0 !important;
  letter-spacing: 0.5px !important;
}

@media(max-width: 768px) {
  .hero { padding: 24px; }
  .hero h1 { font-size: 32px; }
  .top-header { padding: 20px; }
}
"

# 6. USER INTERFACE

ui <- fluidPage(
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    tags$style(HTML(custom_css)),
    tags$style(HTML("
            .login-shell {position:fixed;inset:0;z-index:99999;min-height:100vh;display:flex;align-items:center;justify-content:center;padding:24px;overflow:hidden;background:linear-gradient(125deg,#050914,#0b1426,#07151a,#0a1020);background-size:300% 300%;animation:citypulse-bg-shift 18s ease infinite}
      .login-shell:before {content:'';position:absolute;inset:-20%;pointer-events:none;background:radial-gradient(circle at 18% 28%,rgba(49,91,234,.32),transparent 27%),radial-gradient(circle at 78% 68%,rgba(32,214,199,.24),transparent 28%),radial-gradient(circle at 55% 8%,rgba(139,92,246,.16),transparent 25%);filter:blur(12px);animation:citypulse-orbs 16s ease-in-out infinite alternate}
      .login-shell:after {content:'';position:absolute;inset:0;pointer-events:none;opacity:.16;background-image:linear-gradient(rgba(148,163,184,.16) 1px,transparent 1px),linear-gradient(90deg,rgba(148,163,184,.16) 1px,transparent 1px);background-size:52px 52px;mask-image:linear-gradient(to bottom,black,transparent 92%)}
      .login-card {position:relative;z-index:2}
      @keyframes citypulse-bg-shift {0%{background-position:0% 50%}50%{background-position:100% 50%}100%{background-position:0% 50%}}
      @keyframes citypulse-orbs {0%{transform:translate3d(-2%,0,0) scale(1)}100%{transform:translate3d(3%,2%,0) scale(1.08)}}
      @media (prefers-reduced-motion:reduce) {.login-shell,.login-shell:before {animation:none!important}}
      .login-card {width:100%;max-width:460px;padding:38px;border-radius:26px;background:rgba(15,23,42,.97);border:1px solid rgba(255,255,255,.12);box-shadow:0 24px 70px rgba(0,0,0,.65),0 0 35px rgba(32,214,199,.10)}
      .login-brand {font-family:'Space Grotesk',sans-serif;font-size:30px;font-weight:700;color:#fff;margin-bottom:8px}
      .login-brand span {color:#20D6C7}
      .login-kicker {font-size:10px;letter-spacing:2px;color:#20D6C7;font-weight:700;margin:12px 0}
      .login-card h2 {font-size:29px;margin:8px 0;color:#fff}
      .login-description {color:#94A3B8;font-size:13px;line-height:1.7;margin-bottom:24px}
      .login-card .form-control {height:44px}
      .login-card .btn {padding:12px;margin-top:8px}
      .login-hint {font-size:11px;color:#94A3B8;text-align:center;margin-top:12px}
      .login-footer {font-size:10px;color:#64748B;text-align:center;margin-top:25px}
      .login-error {color:#FDA4AF;background:rgba(244,63,94,.10);border:1px solid rgba(244,63,94,.25);padding:10px;border-radius:10px;font-size:12px}
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        function setupMapFullscreen() {
          var wrap = document.getElementById('geo-map-wrap');
          var button = document.getElementById('geo_map_fullscreen');
          if (!wrap || !button || button.dataset.fullscreenBound === 'yes') return;
          button.dataset.fullscreenBound = 'yes';
          button.addEventListener('click', function (event) {
            event.preventDefault();
            if (!document.fullscreenElement) {
              if (wrap.requestFullscreen) wrap.requestFullscreen();
              else if (wrap.webkitRequestFullscreen) wrap.webkitRequestFullscreen();
            } else {
              if (document.exitFullscreen) document.exitFullscreen();
              else if (document.webkitExitFullscreen) document.webkitExitFullscreen();
            }
          });
          function refreshMapSize() {
            var mapEl = document.getElementById('geo_map');
            if (mapEl && window.HTMLWidgets) {
              setTimeout(function () {
                if (window.Shiny) Shiny.setInputValue('geo_map_fullscreen_state', !!document.fullscreenElement, {priority: 'event'});
                window.dispatchEvent(new Event('resize'));
              }, 250);
            }
            button.innerHTML = document.fullscreenElement ? '✕ Exit full screen' : '⛶ Full screen map';
          }
          document.addEventListener('fullscreenchange', refreshMapSize);
          document.addEventListener('webkitfullscreenchange', refreshMapSize);
        }
        setupMapFullscreen();
        document.addEventListener('shiny:connected', setupMapFullscreen);
      });
    "))
  ),
  
  # Browser-native speech recognition and speech synthesis for the voice assistant.
  tags$script(HTML("
    (function() {
      function setStatus(message) {
        if (window.Shiny) Shiny.setInputValue('voice_status_text', message, {priority: 'event'});
      }
      document.addEventListener('click', function(event) {
        var id = event.target && event.target.id;
        if (id === 'voice_start') {
          var SR = window.SpeechRecognition || window.webkitSpeechRecognition;
          if (!SR) {
            setStatus('Voice input is not supported in this browser. Try a current version of Chrome or Edge, or type your question.');
            return;
          }
          try {
            var recognition = new SR();
            recognition.lang = 'en-IN';
            recognition.interimResults = false;
            recognition.maxAlternatives = 1;
            recognition.onstart = function() { setStatus('Listening… speak your question now.'); };
            recognition.onerror = function(e) { setStatus('Microphone error: ' + e.error + '. You can type your question instead.'); };
            recognition.onend = function() { setStatus('Voice input finished. Check the recognized text before submitting.'); };
            recognition.onresult = function(e) {
              var transcript = e.results[0][0].transcript;
              var input = document.getElementById('voice_question');
              if (input) {
                input.value = transcript;
                input.dispatchEvent(new Event('input', {bubbles: true}));
                if (window.Shiny) Shiny.setInputValue('voice_question', transcript, {priority: 'event'});
              }
              setStatus('Recognized: ' + transcript);
            };
            recognition.start();
            window.cityPulseRecognition = recognition;
          } catch(err) {
            setStatus('Could not start microphone: ' + err.message);
          }
        }
        if (id === 'voice_stop' && window.cityPulseRecognition) {
          window.cityPulseRecognition.stop();
          setStatus('Stopped listening.');
        }
        if (id === 'voice_read_answer') {
          var answerNode = document.getElementById('assistant_answer');
          if (!answerNode || !answerNode.innerText.trim()) { setStatus('Ask a question first, then read the answer aloud.'); return; }
          if (!('speechSynthesis' in window)) { setStatus('Speech playback is not supported in this browser.'); return; }
          window.speechSynthesis.cancel();
          var answerUtterance = new SpeechSynthesisUtterance(answerNode.innerText);
          answerUtterance.lang = 'en-IN';
          answerUtterance.rate = 0.95;
          answerUtterance.onstart = function() { setStatus('Reading the CityPulse answer aloud.'); };
          answerUtterance.onend = function() { setStatus('Finished reading the answer.'); };
          window.speechSynthesis.speak(answerUtterance);
        }
        if (id === 'voice_read_summary') {
          var node = document.getElementById('viz_summary_text');
          if (!node) { setStatus('Summarize a visualization technique first.'); return; }
          if (!('speechSynthesis' in window)) { setStatus('Speech playback is not supported in this browser.'); return; }
          window.speechSynthesis.cancel();
          var utterance = new SpeechSynthesisUtterance(node.innerText || node.textContent);
          utterance.lang = 'en-IN';
          utterance.rate = 0.95;
          utterance.onstart = function() { setStatus('Reading the visualization summary aloud.'); };
          utterance.onend = function() { setStatus('Finished reading the summary.'); };
          window.speechSynthesis.speak(utterance);
        }
      });
    })();
  ")),
  # Login overlay hides the dashboard until authentication succeeds.
  conditionalPanel(
    condition = "output.logged_in != 'true'",
    div(
      class = "login-shell",
      div(
        class = "login-card",
        div(class = "login-brand", HTML("CityPulse <span>360°</span>")),
        div(class = "login-kicker", "VELLORE CITY INTELLIGENCE"),
        h2("Welcome back"),
        p(class = "login-description",
          "Sign in to explore city indicators, interactive maps and urban analytics."),
        textInput("login_username", "Username", placeholder = "Enter username"),
        passwordInput("login_password", "Password", placeholder = "Enter password"),
        actionButton("login_btn", "Sign in to dashboard", class = "btn btn-primary btn-block"),
        br(), br(),
        uiOutput("login_message"),
        div(class = "login-hint", "Demo login: adithya  |  Password: citypulse360"),
        div(class = "login-footer", "CityPulse 360 · Urban Analytics Platform")
      )
    )
  ),

  div(
    class = "top-header",
    fluidRow(
      column(
        8,
        div(class = "brand", HTML("CityPulse <span>360°</span>")),
        div(
          class = "tagline",
          "Vellore City Intelligence | Advanced Data Visualization | Manthena Adithya"
        )
      ),
      column(
        4,
        align = "right",
        div(
          style = "color:#20D6C7;font-weight:700;font-size:12px;",
          "● URBAN ANALYTICS"
        ),
        div(
          style = "color:#C2CEE5;font-size:12px;margin-top:8px;",
          textOutput("today", inline = TRUE)
        ),
        br(),
        actionButton("logout_btn", "Log out", class = "btn btn-default",
                     style = "margin-top:10px;font-size:11px;padding:6px 12px;")
      )
    )
  ),
  
  tabsetPanel(
    id = "tabs",
    type = "pills",
    
    # HOME PAGE
    tabPanel(
      "⌂ Home",
      value = "home",
      br(),
      
      div(
        class = "hero",
        div(class = "hero-label", "CITY INTELLIGENCE / VELLORE,TAMIL NADU,INDIA"),
        h1(
          HTML(
            "Understand your city.<br>",
            "<span style='color:#20D6C7'>Visualize what matters.</span>"
          )
        ),
        p(
          "An interactive urban analytics platform for exploring mobility,",
          " cleanliness, water, environment, public facilities and development.",
          " Discover patterns, compare locations and investigate indicators",
          " through advanced data visualization."
        ),
        actionButton(
          "explore",
          "Explore the dashboard →",
          class = "btn btn-primary"
        )
      ),
      
      uiOutput("home_kpis"),
      
      fluidRow(
        column(
          7,
          div(
            class = "panel",
            div(class = "panel-title", "Urban dimension overview"),
            div(
              class = "panel-subtitle",
              "Average values across the six city dimensions"
            ),
            plotlyOutput("home_dimensions", height = "330px")
          )
        ),
        column(
          5,
          div(
            class = "panel",
            div(class = "panel-title", "Indicator composition"),
            div(
              class = "panel-subtitle",
              "Share of total indicator averages"
            ),
            plotlyOutput("home_donut", height = "330px")
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          div(
            class = "panel",
            div(class = "panel-title", "Explore the city"),
            div(
              class = "panel-subtitle",
              "Six analytical dimensions available in this dashboard"
            ),
            fluidRow(
              column(
                4,
                div(
                  style = "background:rgba(49,91,234,0.14);border:1px solid rgba(49,91,234,0.35);padding:18px;border-radius:16px;margin-bottom:14px;transition:all 0.2s ease;",
                  tags$b(style = "color:#60A5FA;font-size:14px;", "01  Mobility"),
                  p("Traffic, congestion and travel time.", style = "font-size:12px;margin-top:8px;color:#CBD5E1;")
                )
              ),
              column(
                4,
                div(
                  style = "background:rgba(32,214,199,0.14);border:1px solid rgba(32,214,199,0.35);padding:18px;border-radius:16px;margin-bottom:14px;transition:all 0.2s ease;",
                  tags$b(style = "color:#20D6C7;font-size:14px;", "02  Cleanliness"),
                  p("Waste collection and segregation.", style = "font-size:12px;margin-top:8px;color:#CBD5E1;")
                )
              ),
              column(
                4,
                div(
                  style = "background:rgba(245,158,11,0.14);border:1px solid rgba(245,158,11,0.35);padding:18px;border-radius:16px;margin-bottom:14px;transition:all 0.2s ease;",
                  tags$b(style = "color:#FBBF24;font-size:14px;", "03  Water"),
                  p("Supply, quality and leakage reports.", style = "font-size:12px;margin-top:8px;color:#CBD5E1;")
                )
              )
            ),
            fluidRow(
              column(
                4,
                div(
                  style = "background:rgba(167,139,250,0.14);border:1px solid rgba(167,139,250,0.35);padding:18px;border-radius:16px;margin-bottom:14px;transition:all 0.2s ease;",
                  tags$b(style = "color:#A78BFA;font-size:14px;", "04  Environment"),
                  p("Air quality, green cover and noise.", style = "font-size:12px;margin-top:8px;color:#CBD5E1;")
                )
              ),
              column(
                4,
                div(
                  style = "background:rgba(244,63,94,0.14);border:1px solid rgba(244,63,94,0.35);padding:18px;border-radius:16px;margin-bottom:14px;transition:all 0.2s ease;",
                  tags$b(style = "color:#FB7185;font-size:14px;", "05  Public facilities"),
                  p("Streetlights, healthcare and services.", style = "font-size:12px;margin-top:8px;color:#CBD5E1;")
                )
              ),
              column(
                4,
                div(
                  style = "background:rgba(56,189,248,0.14);border:1px solid rgba(56,189,248,0.35);padding:18px;border-radius:16px;margin-bottom:14px;transition:all 0.2s ease;",
                  tags$b(style = "color:#38BDF8;font-size:14px;", "06  Development"),
                  p("Road quality and digital access.", style = "font-size:12px;margin-top:8px;color:#CBD5E1;")
                )
              )
            )
          )
        )
      )
    ),
    
    # CITY OVERVIEW
    tabPanel(
      "◈ City Overview",
      value = "overview",
      br(),
      div(class = "section-title", "City overview"),
      div(
        class = "section-subtitle",
        "Compare indicators, examine location differences and discover patterns."
      ),
      
      div(
        class = "panel",
        fluidRow(
          column(
            4,
            selectInput(
              "overview_dimension",
              "Choose dimension",
              choices = c("All", dimensions)
            )
          ),
          column(
            4,
            selectInput(
              "overview_location",
              "Choose location",
              choices = c("All", locations)
            )
          ),
          column(
            4,
            selectInput(
              "overview_indicator",
              "Choose indicator",
              choices = c("All", sort(unique(city_data$indicator)))
            )
          )
        )
      ),
      
      uiOutput("overview_kpis"),
      
      fluidRow(
        column(
          7,
          div(
            class = "panel",
            div(class = "panel-title", "Dimension comparison"),
            plotlyOutput("overview_bar", height = "370px")
          )
        ),
        column(
          5,
          div(
            class = "panel",
            div(class = "panel-title", "Indicator ranking"),
            plotlyOutput("overview_rank", height = "370px")
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          div(
            class = "panel",
            div(class = "panel-title", "Location-indicator heatmap"),
            div(
              class = "panel-subtitle",
              "Each indicator is standardized so that different units can be compared."
            ),
            plotlyOutput("overview_heatmap", height = "440px")
          )
        )
      )
    ),
    
    # GEOSPATIAL INTELLIGENCE
    tabPanel(
      "◎ Geospatial",
      value = "geospatial",
      br(),
      div(
        style = "display:flex;justify-content:space-between;align-items:flex-start;flex-wrap:wrap;gap:10px;margin-bottom:18px;",
        div(
          div(class = "section-title", "Geospatial Intelligence & Spatial Cartography"),
          div(
            class = "section-subtitle",
            style = "margin-bottom:0;",
            "Multi-dimensional GIS analytics across Vellore with 12 advanced spatial visualization methods, real-time marker interactions, and location dossier intelligence."
          )
        ),
        div(
          style = "display:flex;align-items:center;gap:8px;",
          span(style = "background:rgba(32,214,199,0.15);color:#20D6C7;border:1px solid rgba(32,214,199,0.4);padding:5px 14px;border-radius:20px;font-size:12px;font-weight:700;box-shadow:0 0 12px rgba(32,214,199,0.25);", HTML("<span class='live-beacon'></span> Live Geo-Engine Active")),
          span(style = "background:rgba(49,91,234,0.18);color:#60A5FA;border:1px solid rgba(49,91,234,0.4);padding:5px 14px;border-radius:20px;font-size:12px;font-weight:700;", "Dynamic Location Coverage")
        )
      ),

      # Dynamic Top Spatial KPI Cards
      uiOutput("geo_kpis"),

      # Spatial Controls & Configuration Toolbar
      div(
        class = "panel",
        style = "padding:20px 22px;margin-bottom:20px;",
        fluidRow(
          column(
            3,
            selectInput(
              "geo_dimension", "Urban Dimension",
              choices = c("All", dimensions), selected = "Mobility"
            )
          ),
          column(
            3,
            selectInput(
              "geo_indicator", "Indicator Metric",
              choices = sort(unique(city_data$indicator)),
              selected = "Traffic Volume"
            )
          ),
          column(
            3,
            selectInput(
              "geo_method", "Spatial Visualization Method",
              choices = c(
                "01 Point Map (Radar Glow)",
                "02 Proportional Symbols (Dual Halo)",
                "03 Cluster Map (Density Groups)",
                "04 Choropleth (Catchment Voronoi)",
                "05 Cartogram (Graduated Distortion)",
                "06 Hex Binning (Adaptive Mesh)",
                "07 Heat Map (Multi-Tier Density)",
                "08 Topographic (Hypsometric Contours)",
                "09 Flow Map (Curved Bezier Arcs)",
                "10 Spider Map (Radial Starburst)",
                "11 Time-Space (Temporal Trajectory)",
                "12 Data Space (Attribute Proximity)"
              ),
              selected = "07 Heat Map (Multi-Tier Density)"
            )
          ),
          column(
            3,
            selectInput(
              "geo_basemap", "Basemap Cartography",
              choices = c(
                "Esri Clarity Hybrid + Labels" = "hybrid",
                "Street Map (OpenStreetMap)" = "osm",
                "Esri World Imagery (Clarity HD)" = "satellite",
                "Topographic / Relief (OpenTopoMap)" = "terrain"
              ),
              selected = "satellite"
            )
          )
        ),
        div(
          style = "border-top:1px solid rgba(255,255,255,0.08);padding-top:16px;margin-top:6px;display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:12px;",
          div(
            style = "display:flex;align-items:center;gap:18px;flex-wrap:wrap;",
            div(
              style = "min-width:240px;",
              selectInput(
                "geo_jump_location", NULL,
                choices = c("📍 Focus Area / Jump to Location..." = "All", locations),
                selected = "All",
                width = "250px"
              )
            ),
            checkboxInput("geo_labels", "🏷️ Frosted Labels", value = TRUE),
            checkboxInput("geo_pulsing", "✨ Live Radar Pulse", value = TRUE),
            checkboxInput("geo_halos", "🌐 Catchment Halos", value = FALSE)
          ),
          div(
            style = "display:flex;gap:10px;",
            actionButton(
              "geo_reset_view",
              "🔄 Reset Overview",
              class = "btn btn-default",
              style = "border-radius:12px;font-weight:600;font-size:12px;border-color:rgba(255,255,255,0.14);color:#CBD5E1;background:rgba(30,41,59,0.7);"
            ),
            actionButton(
              "geo_map_fullscreen",
              "⛶ Fullscreen Map",
              class = "btn btn-primary",
              style = "font-size:12px;padding:7px 14px;"
            )
          )
        )
      ),

      # Main Map and Location Dossier Split
      fluidRow(
        column(
          8,
          div(
            id = "geo-map-wrap",
            leafletOutput("geo_map", height = "620px")
          )
        ),
        column(
          4,
          uiOutput("geo_location_dossier")
        )
      ),
      br(),

      # Spatial Analytics & Proximity Matrix
      fluidRow(
        column(
          6,
          div(
            class = "panel",
            div(class = "panel-title", "Indicator Ranking by Location"),
            div(class = "panel-subtitle", "Click any bar to zoom and center the map on that sector"),
            plotlyOutput("geo_rank", height = "360px")
          )
        ),
        column(
          6,
          div(
            class = "panel",
            div(class = "panel-title", "Inter-Sector Distance Matrix (km)"),
            div(class = "panel-subtitle", "Geodesic distances between monitored Vellore urban hubs"),
            plotlyOutput("geo_distance", height = "360px")
          )
        )
      ),

      # Mapped Location Data Table
      div(
        class = "panel",
        div(
          style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:12px;",
          div(
            div(class = "panel-title", "Spatial Intelligence Registry"),
            div(class = "panel-subtitle", "Select any row in the table below to fly directly to that location on the map")
          ),
          span(style = "font-size:11px;color:#7A859B;", "Tip: Click a row to focus")
        ),
        DTOutput("geo_table")
      ),
      p(
        "Cartographic note: Choose from four basemaps: OpenStreetMap Streets, Esri Satellite, Esri Hybrid imagery, and OpenTopoMap terrain. Spatial methods feature mathematically modeled Voronoi territorial catchments, pure-R hexagonal tessellation grids, and multi-tier density gradients. Indicator values and coordinates are synthetic demonstration data for Vellore, Tamil Nadu.",
        class = "section-subtitle"
      )
    ),

    # MOBILITY
    tabPanel(
      "⌁ Mobility",
      value = "mobility",
      br(),
      div(class = "section-title", "Mobility intelligence"),
      div(
        class = "section-subtitle",
        "Examine traffic patterns and compare movement indicators across locations."
      ),
      
      fluidRow(
        column(
          7,
          div(
            class = "panel",
            div(class = "panel-title", "Monthly traffic trend"),
            selectInput(
              "mobility_indicator",
              "Indicator",
              choices = indicators$Mobility
            ),
            plotlyOutput("mobility_trend", height = "350px")
          )
        ),
        column(
          5,
          div(
            class = "panel",
            div(class = "panel-title", "Location comparison"),
            plotlyOutput("mobility_locations", height = "390px")
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          div(
            class = "panel",
            div(class = "panel-title", "Mobility distribution"),
            plotlyOutput("mobility_box", height = "390px")
          )
        )
      )
    ),
    
    # ENVIRONMENT AND WATER
    tabPanel(
      "☘ Environment",
      value = "environment",
      br(),
      div(class = "section-title", "Environment and water"),
      div(
        class = "section-subtitle",
        "Explore environmental indicators and water-service performance."
      ),
      
      fluidRow(
        column(
          6,
          div(
            class = "panel",
            div(class = "panel-title", "Environmental comparison"),
            plotlyOutput("environment_bar", height = "370px")
          )
        ),
        column(
          6,
          div(
            class = "panel",
            div(class = "panel-title", "Monthly trend"),
            selectInput(
              "environment_indicator",
              "Indicator",
              choices = indicators$Environment
            ),
            plotlyOutput("environment_trend", height = "370px")
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          div(
            class = "panel",
            div(class = "panel-title", "Environmental distribution"),
            plotlyOutput("environment_box", height = "370px")
          )
        )
      )
    ),
    
    # ADVANCED ANALYTICS
    tabPanel(
      "◎ Visual Lab",
      value = "lab",
      br(),
      div(class = "section-title", "Advanced visual analytics lab"),
      div(
        class = "section-subtitle",
        "Use correlation, distribution and multivariate charts to explore the data."
      ),
      
      div(
        class = "panel",
        fluidRow(
          column(
            6,
            selectInput(
              "lab_dimension",
              "Dimension",
              choices = c("All", dimensions)
            )
          ),
          column(
            6,
            selectInput(
              "lab_chart_indicator",
              "Distribution indicator",
              choices = sort(unique(city_data$indicator))
            )
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          div(
            class = "panel",
            div(class = "panel-title", "Correlation matrix"),
            plotlyOutput("correlation_plot", height = "410px")
          )
        ),
        column(
          6,
          div(
            class = "panel",
            div(class = "panel-title", "Distribution explorer"),
            plotlyOutput("distribution_plot", height = "410px")
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          div(
            class = "panel",
            div(class = "panel-title", "Multivariate scatter plot"),
            div(
              class = "panel-subtitle",
              "Compare the two first available indicators in the selected dimension."
            ),
            plotlyOutput("scatter_plot", height = "420px")
          )
        )
      )
    ),
    
    # DATA EXPLORER
    tabPanel(
      "▤ Data Explorer",
      value = "data",
      br(),
      div(class = "section-title", "Data explorer"),
      div(
        class = "section-subtitle",
        "Upload a compatible CSV, filter records and export the results."
      ),
      
      div(
        class = "panel",
        fluidRow(
          column(
            4,
            fileInput(
              "upload",
              "Upload CSV",
              accept = ".csv"
            )
          ),
          column(
            4,
            selectInput(
              "data_dimension",
              "Dimension",
              choices = c("All", dimensions)
            )
          ),
          column(
            4,
            selectInput(
              "data_location",
              "Location",
              choices = c("All", locations)
            )
          )
        ),
        fluidRow(
          column(
            6,
            dateRangeInput(
              "data_dates",
              "Date range",
              start = min(city_data$date),
              end = max(city_data$date)
            )
          ),
          column(
            6,
            br(),
            downloadButton(
              "download_csv",
              "Download filtered CSV",
              class = "btn-primary"
            )
          )
        )
      ),
      
      uiOutput("data_summary"),
      
      div(
        class = "panel",
        div(class = "panel-title", "Dataset records"),
        DTOutput("data_table")
      ),
      
      div(
        class = "panel",
        div(class = "panel-title", "Data quality report"),
        uiOutput("quality_report")
      ),
      
      div(
        class = "panel",
        div(class = "panel-title", "Expected CSV format"),
        p("Required columns: dimension, location, date, indicator, value, source."),
        p(
          "Use YYYY-MM-DD dates and numeric values. This example uses synthetic",
          " data and does not represent verified city statistics."
        )
      )
    ),

    # AI CITY ASSISTANT
    tabPanel(
      "✦ AI City Assistant",
      value = "assistant",
      br(),
      div(class = "section-title", "CityPulse AI Assistant"),
      div(class = "section-subtitle",
          "Ask questions about the active city dataset. The built-in assistant uses rule-based data queries; connect an LLM separately if you need open-ended generative answers. Demo values may be synthetic."),
      div(class = "panel",
          div(class = "panel-title", "Ask your city"),
          p("Try: Which area has the highest congestion? Compare Katpadi and VIT Area. What should the city prioritize?"),
          textInput("assistant_question", "Your question",
                    placeholder = "Ask about locations, indicators, trends or priorities...",
                    width = "100%"),
          actionButton("assistant_ask", "Ask CityPulse  →", class = "btn btn-primary"),
          br(), br(),
          uiOutput("assistant_answer")
      ),
      div(class = "panel",
          div(class = "panel-title", "Voice Assistant & Visualization Explainer"),
          p("Use your browser microphone to ask a question, or choose a visualization technique to get a plain-language explanation. Press Read aloud to hear the summary."),
          fluidRow(
            column(6,
              selectInput("viz_technique", "Explain a visualization technique",
                choices = c(
                  "Point map" = "Point map",
                  "Proportional symbols" = "Proportional symbols",
                  "Cluster map" = "Cluster map",
                  "Voronoi catchment map" = "Voronoi catchment map",
                  "Cartogram" = "Cartogram",
                  "Hexagonal binning" = "Hexagonal binning",
                  "Heat map" = "Heat map",
                  "Topographic contours" = "Topographic contours",
                  "Flow map" = "Flow map",
                  "Spider map" = "Spider map",
                  "Time-space trajectory" = "Time-space trajectory",
                  "Attribute proximity map" = "Attribute proximity map",
                  "Bar chart" = "Bar chart",
                  "Donut chart" = "Donut chart",
                  "Trend chart" = "Trend chart",
                  "Indicator heatmap" = "Indicator heatmap",
                  "Scatter plot" = "Scatter plot",
                  "Interactive data table" = "Interactive data table"
                )
              )
            ),
            column(6,
              div(style = "padding-top:25px;display:flex;gap:8px;flex-wrap:wrap;",
                actionButton("viz_summarize", "Summarize technique", class = "btn btn-primary"),
                actionButton("voice_read_summary", "🔊 Read aloud", class = "btn btn-default")
              )
            )
          ),
          div(style = "display:flex;gap:8px;flex-wrap:wrap;margin:10px 0;",
            actionButton("voice_start", "🎙 Start voice input", class = "btn btn-default"),
            actionButton("voice_stop", "Stop listening", class = "btn btn-default")
          ),
          textInput("voice_question", "Voice question / typed question",
                    placeholder = "Example: Explain the heat map and what it tells us...",
                    width = "100%"),
          actionButton("voice_ask", "Answer question", class = "btn btn-primary"),
          actionButton("voice_read_answer", "🔊 Read answer aloud", class = "btn btn-default"),
          br(), br(),
          uiOutput("viz_summary"),
          uiOutput("voice_status")
      ),
      div(class = "panel",
          div(class = "panel-title", "Important data note"),
          p("This built-in assistant uses transparent data-query rules, not a connected large language model. It does not invent live city facts. Connect an LLM API and verified data source to enable natural-language reasoning beyond these supported queries.")
      )
    ),

    # SMART ROUTE ANALYSIS
    tabPanel(
      "◈ AI Simulation Lab",
      value = "simulation_lab",
      br(),
      div(class = "section-title", "AI Prediction & What-if Simulation Lab"),
      div(class = "section-subtitle",
          "Compare forecasting approaches and simulate hypothetical city changes. Scenario outputs are estimates, not observations."),
      div(class = "panel",
        fluidRow(
          column(3, selectInput("sim_indicator", "Indicator to simulate",
                               choices = sort(unique(city_data$indicator)), selected = "Traffic Volume")),
          column(3, selectInput("sim_model", "Forecast model",
                               choices = c("Linear trend", "Moving average", "Exponential smoothing"),
                               selected = "Linear trend")),
          column(3, sliderInput("sim_change", "Scenario change (%)", min = -50, max = 100, value = 30, step = 5)),
          column(3, sliderInput("sim_horizon", "Forecast horizon (months)", min = 1, max = 12, value = 3))
        ),
        fluidRow(
          column(4, sliderInput("sim_rainfall", "Rainfall stress (%)", min = 0, max = 200, value = 0, step = 10)),
          column(4, sliderInput("sim_road_closure", "Road capacity removed (%)", min = 0, max = 80, value = 0, step = 5)),
          column(4, sliderInput("sim_resource_cut", "Resource availability change (%)", min = -50, max = 50, value = 0, step = 5))
        ),
        actionButton("run_city_simulation", "Run what-if scenario", class = "btn btn-primary")
      ),
      fluidRow(
        column(7, div(class = "panel",
          div(class = "panel-title", "Baseline vs scenario"),
          plotlyOutput("simulation_compare_plot", height = "360px")
        )),
        column(5, div(class = "panel",
          div(class = "panel-title", "Scenario impact summary"),
          uiOutput("simulation_summary"),
          plotlyOutput("model_comparison_plot", height = "230px")
        ))
      ),
      div(class = "panel",
        div(class = "panel-title", "Forecast quality and caveats"),
        p("The model comparison is evaluated on a simple holdout split when sufficient history exists. With synthetic or sparse observations, metrics are illustrative. For a validated forecast, supply real historical data and use time-ordered backtesting.")
      )
    ),

    tabPanel(
      "▣ 3D Digital Twin",
      value = "digital_twin",
      br(),
      div(class = "section-title", "CityPulse 360° | 3D Digital Twin"),
      div(class = "section-subtitle",
          "Explore location-level indicator intensity in a 3D view. The current building forms are illustrative geometry until real building footprints and heights are loaded."),
      div(class = "panel",
        fluidRow(
          column(4, selectInput("twin_indicator", "Indicator",
                                choices = sort(unique(city_data$indicator)), selected = "Traffic Volume")),
          column(4, selectInput("twin_dimension", "Urban dimension",
                                choices = c("All", dimensions), selected = "All")),
          column(4, checkboxInput("twin_labels", "Show location labels", TRUE))
        )
      ),
      fluidRow(
        column(8, div(class = "panel",
          div(class = "panel-title", "Interactive 3D city view"),
          plotlyOutput("digital_twin_plot", height = "580px")
        )),
        column(4, div(class = "panel",
          div(class = "panel-title", "Digital twin status"),
          uiOutput("digital_twin_summary"),
          p("For a true engineering-grade digital twin, import verified building footprints, heights, terrain and live sensor feeds. This prototype visualizes data values over approximate location coordinates.")
        ))
      )
    ),

    tabPanel(
      "⚙ Resource Optimizer",
      value = "resource_optimizer",
      br(),
      div(class = "section-title", "AI Resource Allocation Optimizer"),
      div(class = "section-subtitle",
          "Allocate limited response or maintenance units to locations with the greatest estimated need."),
      div(class = "panel",
        fluidRow(
          column(4, selectInput("allocation_indicator", "Need indicator",
                                choices = sort(unique(city_data$indicator)), selected = "Traffic Volume")),
          column(4, sliderInput("allocation_units", "Available teams / vehicles", min = 1, max = 20, value = 4)),
          column(4, selectInput("allocation_strategy", "Allocation strategy",
                                choices = c("Highest risk first", "Balanced coverage", "Need proportional"),
                                selected = "Highest risk first"))
        ),
        actionButton("run_allocation", "Optimize allocation", class = "btn btn-primary")
      ),
      fluidRow(
        column(7, div(class = "panel",
          div(class = "panel-title", "Recommended allocation"),
          plotlyOutput("allocation_plot", height = "380px")
        )),
        column(5, div(class = "panel",
          div(class = "panel-title", "Allocation plan"),
          DTOutput("allocation_table"),
          uiOutput("allocation_summary")
        ))
      ),
      p("This is a transparent heuristic allocation prototype, not an operational dispatch system. Validate demand estimates, facility capacity and travel-time constraints before real-world use.")
    ),

    tabPanel(
      "⌘ Urban Knowledge Graph",
      value = "knowledge_graph",
      br(),
      div(class = "section-title", "Urban Knowledge Graph & Relationship Explorer"),
      div(class = "section-subtitle",
          "Explore illustrative relationships between city locations, indicators and urban dimensions."),
      div(class = "panel",
        fluidRow(
          column(4, selectInput("graph_dimension", "Focus dimension",
                                choices = c("All", dimensions), selected = "Mobility")),
          column(4, selectInput("graph_location", "Focus location",
                                choices = c("All", locations), selected = "All")),
          column(4, checkboxInput("graph_show_indicators", "Show indicator nodes", TRUE))
        )
      ),
      div(class = "panel",
        div(class = "panel-title", "Interactive relationship graph"),
        plotlyOutput("knowledge_graph_plot", height = "560px")
      ),
      div(class = "panel",
        div(class = "panel-title", "Interpretation"),
        p("Edges show data-model relationships, not proof of causation. Causal claims require validated evidence, temporal analysis and domain review.")
      )
    ),

    tabPanel(
      "➤ Smart Route Analysis",
      value = "routes",
      br(),
      div(class = "section-title", "Smart Route Analysis"),
      div(class = "section-subtitle",
          "Analyze the default road-network route when the routing service is available. If it is unavailable, the app shows a clearly labelled straight-line estimate."),
      div(class = "panel",
          fluidRow(
            column(3, selectInput("route_start", "Starting location",
                                  choices = locations, selected = locations[1])),
            column(3, selectInput("route_end", "Destination",
                                  choices = locations, selected = locations[2])),
            column(3, selectInput("route_priority", "Optimization priority",
                                  choices = c("Fastest route", "Shortest route", "Road-quality aware"),
                                  selected = "Fastest route")),
            column(3, selectInput("route_basemap", "Basemap cartography",
                                  choices = c(
                                    "Street Map (OpenStreetMap)" = "osm",
                                    "Esri World Imagery (Clarity HD)" = "satellite",
                                    "Esri Clarity Hybrid + Labels" = "hybrid",
                                    "Topographic / Relief (OpenTopoMap)" = "terrain"
                                  ), selected = "satellite"))
          ),
          fluidRow(
            column(4, checkboxInput("route_show_labels", "🏷️ Frosted Labels", TRUE)),
            column(4, checkboxInput("route_pulsing", "✨ Live Radar Pulse", TRUE)),
            column(4, checkboxInput("route_halos", "🌐 Catchment Halos", FALSE))
          ),
          actionButton("route_analyze", "Analyze route  →", class = "btn btn-primary"),
          br(), br(),
          uiOutput("route_summary"),
          div(class = "panel-title", "Available road-route alternatives"),
          DTOutput("route_alternatives_table")
      ),
      div(class = "panel",
          div(class = "panel-title", "Geospatial route visualization"),
          leafletOutput("route_map", height = "680px")
      ),
      div(class = "panel",
          div(class = "panel-title", "How route scoring works"),
          p("When OSRM returns alternatives, Fastest route selects the lowest estimated duration and Shortest route selects the lowest distance. Road-quality-aware mode currently adds endpoint context only; it does not score every road segment. Times are not live traffic, and the built-in city indicators are synthetic demo data. Public routing services have usage limits.")
      )
    )
  ),
  
  div(
    class = "footer",
    "CITYPULSE 360° | CSI3005 Advanced Data Visualization | Manthena Sri Adithya | 24MID0177"
  )
)
# GEOSPATIAL HELPER FUNCTIONS: Pure-R Voronoi, Hexagonal Tessellation, Bezier Curves, and Rich Popups

# 1. Pure R Sutherland-Hodgman Polygon Half-Plane Clipper
clip_polygon_halfplane <- function(poly, p1, p2) {
  n <- nrow(poly)
  if (n < 3) return(poly)
  out <- list()
  dx <- p2[1] - p1[1]
  dy <- p2[2] - p1[2]
  is_inside <- function(pt) {
    (dx * (pt[2] - p1[2]) - dy * (pt[1] - p1[1])) >= -1e-9
  }
  line_intersection <- function(a, b) {
    adx <- b[1] - a[1]
    ady <- b[2] - a[2]
    denom <- dx * ady - dy * adx
    if (abs(denom) < 1e-12) return(a)
    t <- (dx * (a[2] - p1[2]) - dy * (a[1] - p1[1])) / (dy * adx - dx * ady)
    c(a[1] + t * adx, a[2] + t * ady)
  }
  for (i in seq_len(n)) {
    cur <- poly[i, ]
    prev <- if (i == 1) poly[n, ] else poly[i - 1, ]
    cur_in <- is_inside(cur)
    prev_in <- is_inside(prev)
    if (cur_in) {
      if (!prev_in) out[[length(out) + 1]] <- line_intersection(prev, cur)
      out[[length(out) + 1]] <- cur
    } else if (prev_in) {
      out[[length(out) + 1]] <- line_intersection(prev, cur)
    }
  }
  if (length(out) == 0) return(matrix(numeric(0), ncol = 2))
  do.call(rbind, out)
}

# 2. Pure R Voronoi / Territorial Catchment Generator
compute_voronoi_cells <- function(coords, bbox_pad = 0.025) {
  min_lng <- min(coords$longitude) - bbox_pad
  max_lng <- max(coords$longitude) + bbox_pad
  min_lat <- min(coords$latitude) - bbox_pad
  max_lat <- max(coords$latitude) + bbox_pad

  initial_box <- rbind(
    c(min_lng, min_lat),
    c(max_lng, min_lat),
    c(max_lng, max_lat),
    c(min_lng, max_lat)
  )

  cells <- list()
  n <- nrow(coords)
  for (i in seq_len(n)) {
    poly <- initial_box
    pi <- c(coords$longitude[i], coords$latitude[i])
    for (j in seq_len(n)) {
      if (i == j) next
      pj <- c(coords$longitude[j], coords$latitude[j])
      mid <- (pi + pj) / 2
      v <- pj - pi
      p_dir <- mid + c(-v[2], v[1])
      poly <- clip_polygon_halfplane(poly, mid, p_dir)
      if (nrow(poly) < 3) break
    }
    cells[[i]] <- poly
  }
  cells
}

# 3. Pure R Quadratic Bezier Curve Generator for Curved Flow Arcs
make_bezier_arc <- function(p0, p1, n = 28, curvature = 0.18) {
  t <- seq(0, 1, length.out = n)
  mid <- (p0 + p1) / 2
  dx <- p1[1] - p0[1]
  dy <- p1[2] - p0[2]
  perp <- c(-dy, dx)
  ctrl <- mid + curvature * perp
  x <- (1 - t)^2 * p0[1] + 2 * (1 - t) * t * ctrl[1] + t^2 * p1[1]
  y <- (1 - t)^2 * p0[2] + 2 * (1 - t) * t * ctrl[2] + t^2 * p1[2]
  data.frame(lng = x, lat = y)
}

# 4. Pure R Hexagonal Tessellation Grid Generator (Zero External Dependencies)
make_hex_grid_pure <- function(d, radius_meters = 950) {
  d <- d[is.finite(d$longitude) & is.finite(d$latitude) & is.finite(d$scaled), , drop = FALSE]
  if (nrow(d) == 0) return(list())

  mean_lat <- mean(d$latitude)
  m_per_deg_lat <- 111320
  m_per_deg_lng <- 111320 * cos(mean_lat * pi / 180)

  r_lat <- radius_meters / m_per_deg_lat
  r_lng <- radius_meters / m_per_deg_lng

  dx <- sqrt(3) * r_lng
  dy <- 1.5 * r_lat

  min_lng <- min(d$longitude) - 2.5 * r_lng
  max_lng <- max(d$longitude) + 2.5 * r_lng
  min_lat <- min(d$latitude) - 2.5 * r_lat
  max_lat <- max(d$latitude) + 2.5 * r_lat

  x_steps <- seq(min_lng, max_lng, by = dx)
  y_steps <- seq(min_lat, max_lat, by = dy)

  hex_list <- list()
  angles <- seq(0, 2 * pi, length.out = 7)[1:6] + pi / 6

  count <- 0
  for (row_idx in seq_along(y_steps)) {
    y0 <- y_steps[row_idx]
    shift <- if (row_idx %% 2 == 0) dx / 2 else 0
    for (x0 in (x_steps + shift)) {
      dist_sq <- (d$longitude - x0)^2 + (d$latitude - y0)^2
      min_idx <- which.min(dist_sq)
      min_dist_m <- sqrt(dist_sq[min_idx]) * m_per_deg_lat
      # Limit hexagons to within 4.2 km of any sensor point
      if (min_dist_m < 4200) {
        count <- count + 1
        weights <- 1 / pmax(dist_sq, 1e-6)^1.5
        est_val <- sum(d$value * weights) / sum(weights)
        est_scaled <- sum(d$scaled * weights) / sum(weights)

        hex_lngs <- x0 + r_lng * cos(angles)
        hex_lats <- y0 + r_lat * sin(angles)

        hex_list[[count]] <- list(
          id = count,
          center_lng = x0,
          center_lat = y0,
          lngs = c(hex_lngs, hex_lngs[1]),
          lats = c(hex_lats, hex_lats[1]),
          value = est_val,
          scaled = est_scaled,
          nearest_loc = d$location[min_idx]
        )
      }
    }
  }
  hex_list
}

# 5. Rich Glassmorphic Interactive Popup HTML Builder
make_rich_popup <- function(location, indicator, value, scaled, rank, total, lat, lng, dimension) {
  status_badge <- if (scaled >= 0.75) {
    "<span style='background:#FF4D4D28;color:#FF6B6B;border:1px solid #FF4D4D66;padding:3px 9px;border-radius:12px;font-size:10px;font-weight:700;'>● Peak Zone</span>"
  } else if (scaled <= 0.25) {
    "<span style='background:#20D6C728;color:#20D6C7;border:1px solid #20D6C766;padding:3px 9px;border-radius:12px;font-size:10px;font-weight:700;'>● Low Zone</span>"
  } else {
    "<span style='background:#FFB45C28;color:#FFB45C;border:1px solid #FFB45C66;padding:3px 9px;border-radius:12px;font-size:10px;font-weight:700;'>● Moderate</span>"
  }

  pct <- round(scaled * 100)

  paste0(
    "<div style='min-width:240px;padding:4px;font-family:\"DM Sans\", sans-serif;'>",
      "<div style='display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;'>",
        "<span style='font-size:17px;font-weight:700;color:#FFFFFF;font-family:\"Space Grotesk\", sans-serif;letter-spacing:-0.5px;'>", location, "</span>",
        status_badge,
      "</div>",
      "<div style='color:#94A3B8;font-size:11px;font-weight:600;text-transform:uppercase;letter-spacing:0.8px;margin-bottom:6px;'>",
        dimension, " &bull; ", indicator,
      "</div>",
      "<div style='display:flex;align-items:baseline;justify-content:space-between;margin-bottom:10px;'>",
        "<span style='font-size:25px;font-weight:700;color:#20D6C7;font-family:\"Space Grotesk\", sans-serif;'>", format(round(value, 2), big.mark = ","), "</span>",
        "<span style='font-size:12px;color:#CBD5E1;font-weight:600;'>Rank #", rank, " of ", total, "</span>",
      "</div>",
      "<div style='margin-bottom:10px;'>",
        "<div style='display:flex;justify-content:space-between;font-size:10px;color:#94A3B8;margin-bottom:3px;'>",
          "<span>City Relative Level</span><span style='color:#E2E8F0;font-weight:600;'>", pct, "%</span>",
        "</div>",
        "<div style='height:6px;background:#1E293B;border-radius:3px;overflow:hidden;'>",
          "<div style='height:100%;width:", pct, "%;background:linear-gradient(90deg, #315BEA, #20D6C7);border-radius:3px;'></div>",
        "</div>",
      "</div>",
      "<div style='display:flex;justify-content:space-between;align-items:center;padding-top:8px;border-top:1px solid rgba(255,255,255,0.12);font-size:10px;color:#94A3B8;'>",
        "<span>GPS: ", round(lat, 4), "&deg; N, ", round(lng, 4), "&deg; E</span>",
        "<span style='color:#38BDF8;font-weight:600;'>Click to inspect dossier &rarr;</span>",
      "</div>",
    "</div>"
  )
}

# 7. SERVER LOGIC

server <- function(input, output, session) {

  # Session-only demo authentication. Use secure database-backed authentication for deployment.
  authenticated <- reactiveVal(FALSE)
  login_attempt_message <- reactiveVal(NULL)

  output$logged_in <- renderText(if (isTRUE(authenticated())) "true" else "false")
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)

  output$login_message <- renderUI({
    msg <- login_attempt_message()
    if (is.null(msg)) return(NULL)
    div(class = "login-error", msg)
  })

  observeEvent(input$login_btn, {
    username <- trimws(if (is.null(input$login_username)) "" else input$login_username)
    password <- if (is.null(input$login_password)) "" else input$login_password
    if (identical(username, "adithya") && identical(password, "citypulse360")) {
      authenticated(TRUE)
      login_attempt_message(NULL)
      showNotification("Login successful. Welcome to CityPulse 360!", type = "message")
    } else {
      authenticated(FALSE)
      login_attempt_message("Invalid username or password. Please try again.")
    }
  })

  observeEvent(input$logout_btn, {
    authenticated(FALSE)
    updateTextInput(session, "login_username", value = "")
    updateTextInput(session, "login_password", value = "")
    login_attempt_message(NULL)
  })

  
  # Keep uploaded data in reactive state.
  rv <- reactiveValues(data = city_data, coordinates = location_coordinates)
  
  output$today <- renderText(format(Sys.Date(), "%d %B %Y"))
  
  observeEvent(input$explore, {
    updateTabsetPanel(session, "tabs", selected = "overview")
  })
  
  # Data quality results are displayed in the Data Explorer.
  quality_state <- reactiveVal(list(
    title = "Using built-in demonstration data",
    issues = c("The built-in dataset is synthetic demonstration data."),
    checked = TRUE
  ))

  # Validate uploaded CSV files before replacing the active dataset.
  observeEvent(input$upload, {
    req(input$upload$datapath)

    uploaded <- tryCatch(
      read.csv(input$upload$datapath, stringsAsFactors = FALSE,
               check.names = FALSE, na.strings = c("", "NA", "N/A")),
      error = function(e) e
    )

    if (inherits(uploaded, "error")) {
      msg <- paste("Could not read this file as a CSV:", conditionMessage(uploaded))
      quality_state(list(title = "File could not be read", issues = msg, checked = FALSE))
      showNotification(msg, type = "error", duration = 12)
      return()
    }

    required <- c("dimension", "location", "date", "indicator", "value", "source")
    missing_columns <- setdiff(required, names(uploaded))
    if (length(missing_columns) > 0) {
      msg <- paste0(
        "Incompatible CSV format. Missing required column(s): ",
        paste(missing_columns, collapse = ", "),
        ". Required columns are: ", paste(required, collapse = ", "), "."
      )
      quality_state(list(title = "Incompatible file", issues = msg, checked = FALSE))
      showNotification(msg, type = "error", duration = 14)
      return()
    }

    issues <- character(0)
    original_n <- nrow(uploaded)
    uploaded$location <- trimws(as.character(uploaded$location))

    # Missing observations in required fields.
    for (nm in required) {
      blank <- is.na(uploaded[[nm]]) | trimws(as.character(uploaded[[nm]])) == ""
      if (any(blank)) {
        issues <- c(issues, paste(sum(blank), "row(s) have missing", nm, "values."))
      }
    }

    # Parse dates strictly and report invalid date strings instead of silently dropping them.
    raw_dates <- trimws(as.character(uploaded$date))
    parsed_dates <- suppressWarnings(as.Date(raw_dates, format = "%Y-%m-%d"))
    invalid_dates <- is.na(parsed_dates) & !is.na(raw_dates) & raw_dates != ""
    if (any(invalid_dates)) {
      examples <- paste(head(unique(raw_dates[invalid_dates]), 3), collapse = ", ")
      msg <- paste0("Invalid date(s): ", sum(invalid_dates),
                    " row(s) are not valid YYYY-MM-DD dates. Examples: ", examples, ".")
      quality_state(list(title = "Date validation failed", issues = c(issues, msg), checked = FALSE))
      showNotification(msg, type = "error", duration = 14)
      return()
    }
    uploaded$date <- parsed_dates

    # Numeric values must be present and finite.
    raw_values <- uploaded$value
    numeric_values <- suppressWarnings(as.numeric(as.character(raw_values)))
    invalid_values <- is.na(numeric_values) | !is.finite(numeric_values)
    if (any(invalid_values)) {
      msg <- paste0("Invalid value column: ", sum(invalid_values),
                    " row(s) have blank, non-numeric, or infinite values. Correct these before uploading.")
      quality_state(list(title = "Value validation failed", issues = c(issues, msg), checked = FALSE))
      showNotification(msg, type = "error", duration = 14)
      return()
    }
    uploaded$value <- numeric_values

    # Unexpected categories: dimensions and indicators should match the dashboard catalogue.
    known_dimensions <- names(indicators)
    unexpected_dimensions <- setdiff(unique(uploaded$dimension[!is.na(uploaded$dimension)]), known_dimensions)
    if (length(unexpected_dimensions)) {
      issues <- c(issues, paste0("Unexpected dimension(s): ", paste(unexpected_dimensions, collapse = ", "), "."))
    }
    known_indicators <- unlist(indicators, use.names = FALSE)
    unexpected_indicators <- setdiff(unique(uploaded$indicator[!is.na(uploaded$indicator)]), known_indicators)
    if (length(unexpected_indicators)) {
      issues <- c(issues, paste0("Unexpected indicator(s): ", paste(unexpected_indicators, collapse = ", "), "."))
    }

    # Detect duplicate observations using the natural observation key.
    key_cols <- c("dimension", "location", "date", "indicator")
    duplicate_rows <- duplicated(uploaded[, key_cols, drop = FALSE])
    duplicate_count <- sum(duplicate_rows)
    if (duplicate_count > 0) {
      issues <- c(issues, paste(duplicate_count, "duplicate observation row(s) found for dimension/location/date/indicator."))
      uploaded <- uploaded[!duplicate_rows, , drop = FALSE]
    }

    # Unit consistency: if a unit column is provided, each indicator must use one consistent unit.
    expected_units <- c(
      "Traffic Volume" = "vehicles/day", "Travel Time" = "minutes",
      "Congestion" = "%", "Waste Collected" = "tonnes/day",
      "Waste Segregation" = "%", "Waste Issues" = "reports",
      "Water Supply" = "ML/day", "Water Quality" = "score",
      "Leakage Reports" = "reports", "Air Quality Index" = "AQI",
      "Green Cover" = "%", "Noise Level" = "dB",
      "Streetlight Availability" = "%", "Healthcare Access" = "score",
      "Service Requests" = "requests", "Road Quality" = "score",
      "Construction Activity" = "sites", "Digital Service Access" = "%"
    )
    if ("unit" %in% names(uploaded)) {
      for (ind in unique(uploaded$indicator)) {
        units_here <- unique(trimws(as.character(uploaded$unit[uploaded$indicator == ind])))
        units_here <- units_here[!is.na(units_here) & units_here != ""]
        if (length(units_here) > 1) {
          issues <- c(issues, paste0("Inconsistent units for '", ind, "': ", paste(units_here, collapse = ", "), "."))
        }
        if (ind %in% names(expected_units) && length(units_here) == 1 && units_here != expected_units[[ind]]) {
          issues <- c(issues, paste0("Unexpected unit for '", ind, "': expected '", expected_units[[ind]], "' but found '", units_here, "'."))
        }
      }
    } else {
      issues <- c(issues, "No 'unit' column supplied; unit consistency could not be checked.")
    }

    # Plausibility checks against the demo indicator ranges; these are warnings, not universal scientific limits.
    value_ranges <- list(
      "Traffic Volume" = c(0, 100000), "Travel Time" = c(0, 300),
      "Congestion" = c(0, 100), "Waste Collected" = c(0, 100000),
      "Waste Segregation" = c(0, 100), "Waste Issues" = c(0, 10000),
      "Water Supply" = c(0, 10000), "Water Quality" = c(0, 100),
      "Leakage Reports" = c(0, 10000), "Air Quality Index" = c(0, 1000),
      "Green Cover" = c(0, 100), "Noise Level" = c(0, 200),
      "Streetlight Availability" = c(0, 100), "Healthcare Access" = c(0, 100),
      "Service Requests" = c(0, 100000), "Road Quality" = c(0, 100),
      "Construction Activity" = c(0, 10000), "Digital Service Access" = c(0, 100)
    )
    unusual <- logical(nrow(uploaded))
    for (i in seq_len(nrow(uploaded))) {
      ind <- as.character(uploaded$indicator[i])
      if (ind %in% names(value_ranges)) {
        r <- value_ranges[[ind]]
        unusual[i] <- uploaded$value[i] < r[1] || uploaded$value[i] > r[2]
      } else {
        unusual[i] <- uploaded$value[i] < 0
      }
    }
    if (any(unusual)) {
      issues <- c(issues, paste(sum(unusual), "unexpected or implausible indicator value(s) found; verify the values and units."))
    }

    # Missing observations: check gaps within each observed dimension/location/indicator series.
    series_key <- interaction(uploaded$dimension, uploaded$location, uploaded$indicator, drop = TRUE)
    series_gaps <- 0L
    for (key in levels(series_key)) {
      dates_for_key <- uploaded$date[series_key == key]
      if (length(dates_for_key) > 1) {
        ordered_dates <- sort(unique(dates_for_key))
        if (length(ordered_dates) > 1) {
          series_gaps <- series_gaps + max(0L, length(seq(min(ordered_dates), max(ordered_dates), by = "day")) - length(ordered_dates))
        }
      }
    }
    if (series_gaps > 0) {
      issues <- c(issues, paste(series_gaps, "calendar day(s) without observations across the observed series; review whether daily coverage is expected."))
    }

    # Missing fields are not silently hidden. Reject if required values are missing.
    required_missing <- any(vapply(required, function(nm) {
      z <- uploaded[[nm]]
      any(is.na(z) | trimws(as.character(z)) == "")
    }, logical(1)))
    if (required_missing) {
      msg <- "Upload stopped: fill missing required fields (dimension, location, date, indicator, value, source) and upload again."
      quality_state(list(title = "Missing required observations", issues = c(issues, msg), checked = FALSE))
      showNotification(msg, type = "error", duration = 14)
      return()
    }

    if (nrow(uploaded) == 0) {
      msg <- "The file contains no usable observations after validation."
      quality_state(list(title = "No usable observations", issues = c(issues, msg), checked = FALSE))
      showNotification(msg, type = "error", duration = 12)
      return()
    }

    # Build coordinates for every uploaded location. CSV latitude/longitude columns
    # are preferred; existing demo coordinates are retained; new locations are geocoded
    # with OpenStreetMap Nominatim when coordinates are not supplied.
    uploaded_locations <- sort(unique(trimws(as.character(uploaded$location))))
    coord_parts <- list()

    # Preserve built-in coordinates only for matching Vellore demo-area names.
    # All other uploaded locations are resolved from supplied coordinates or geocoding.
    existing_coords <- location_coordinates[
      location_coordinates$location %in% uploaded_locations, , drop = FALSE
    ]
    coord_parts[[length(coord_parts) + 1]] <- existing_coords

    if (all(c("latitude", "longitude") %in% names(uploaded))) {
      uploaded$latitude <- suppressWarnings(as.numeric(as.character(uploaded$latitude)))
      uploaded$longitude <- suppressWarnings(as.numeric(as.character(uploaded$longitude)))
      supplied_coords <- uploaded[
        is.finite(uploaded$latitude) & is.finite(uploaded$longitude) &
          uploaded$latitude >= -90 & uploaded$latitude <= 90 &
          uploaded$longitude >= -180 & uploaded$longitude <= 180,
        c("location", "latitude", "longitude"), drop = FALSE
      ]
      if (nrow(supplied_coords)) {
        coord_parts[[length(coord_parts) + 1]] <- aggregate(
          cbind(latitude, longitude) ~ location, data = supplied_coords, FUN = median
        )
      }
    }

    known_coords <- if (length(coord_parts)) {
      do.call(rbind, coord_parts)
    } else {
      data.frame(location = character(), latitude = numeric(), longitude = numeric())
    }
    known_coords <- known_coords[!duplicated(known_coords$location, fromLast = TRUE), , drop = FALSE]
    missing_locations <- setdiff(uploaded_locations, known_coords$location)

    # Geocode new location names automatically when the computer has internet access.
    # For best accuracy, include latitude and longitude columns in the CSV.
    if (length(missing_locations) > 0) {
      for (place in missing_locations) {
        # If the uploaded CSV contains city/state/country columns, use them to
        # disambiguate neighbourhood names. Otherwise geocode the location in India.
        context_parts <- character(0)
        row_for_place <- which(trimws(as.character(uploaded$location)) == place)[1]
        for (context_col in c("city", "state", "country")) {
          if (context_col %in% names(uploaded)) {
            context_value <- trimws(as.character(uploaded[[context_col]][row_for_place]))
            if (length(context_value) && !is.na(context_value) && nzchar(context_value)) {
              context_parts <- c(context_parts, context_value)
            }
          }
        }
        if (!length(context_parts)) context_parts <- "India"
        geocode_query <- paste(c(place, context_parts), collapse = ", ")
        query <- utils::URLencode(geocode_query, reserved = TRUE)
        endpoint <- paste0(
          "https://nominatim.openstreetmap.org/search?format=jsonv2&limit=1&q=",
          query
        )
        result <- tryCatch({
          if (requireNamespace("curl", quietly = TRUE)) {
            response <- curl::curl_fetch_memory(
              endpoint,
              handle = curl::new_handle(
                useragent = "CityPulse360/1.0 (geospatial CSV upload)"
              )
            )
            if (response$status_code >= 400) stop("Geocoding service unavailable")
            json_text <- rawToChar(response$content)
          } else {
            json_text <- paste(readLines(endpoint, warn = FALSE, encoding = "UTF-8"), collapse = "")
          }
          parsed <- jsonlite::fromJSON(json_text)
          if (NROW(parsed) < 1 || !all(c("lat", "lon") %in% names(parsed))) {
            NULL
          } else {
            data.frame(
              location = place,
              latitude = as.numeric(parsed$lat[1]),
              longitude = as.numeric(parsed$lon[1]),
              stringsAsFactors = FALSE
            )
          }
        }, error = function(e) NULL)

        if (!is.null(result) && nrow(result) > 0 &&
            is.finite(result$latitude[1]) && is.finite(result$longitude[1])) {
          known_coords <- rbind(known_coords, result)
          Sys.sleep(1)
        }
      }
    }

    rv$coordinates <- known_coords[!duplicated(known_coords$location, fromLast = TRUE), , drop = FALSE]
    rv$coordinates$latitude <- suppressWarnings(as.numeric(rv$coordinates$latitude))
    rv$coordinates$longitude <- suppressWarnings(as.numeric(rv$coordinates$longitude))
    rv$coordinates <- rv$coordinates[
      is.finite(rv$coordinates$latitude) & is.finite(rv$coordinates$longitude) &
        rv$coordinates$latitude >= -90 & rv$coordinates$latitude <= 90 &
        rv$coordinates$longitude >= -180 & rv$coordinates$longitude <= 180, , drop = FALSE
    ]
    # Refresh route dropdowns to match coordinates available after CSV upload.
    # The old fixed Vellore choices became invalid when uploaded city coordinates replaced them.
    mapped_route_locations <- unique(as.character(rv$coordinates$location))
    mapped_route_locations <- mapped_route_locations[!is.na(mapped_route_locations) & nzchar(mapped_route_locations)]
    if (length(mapped_route_locations) >= 2) {
      current_start <- isolate(input$route_start)
      current_end <- isolate(input$route_end)
      new_start <- if (!is.null(current_start) && current_start %in% mapped_route_locations) current_start else mapped_route_locations[1]
      end_candidates <- setdiff(mapped_route_locations, new_start)
      new_end <- if (!is.null(current_end) && current_end %in% end_candidates) current_end else end_candidates[1]
      updateSelectInput(session, "route_start", choices = mapped_route_locations, selected = new_start)
      updateSelectInput(session, "route_end", choices = mapped_route_locations, selected = new_end)
      showNotification(paste0("Smart Route Analysis updated with ", length(mapped_route_locations), " mapped locations."), type = "message", duration = 5)
    } else if (length(mapped_route_locations) == 1) {
      updateSelectInput(session, "route_start", choices = mapped_route_locations, selected = mapped_route_locations[1])
      updateSelectInput(session, "route_end", choices = mapped_route_locations, selected = mapped_route_locations[1])
      showNotification("Only one mapped location is available. Add coordinates for at least two locations to draw a route.", type = "warning", duration = 8)
    } else {
      showNotification("No valid coordinates were found. Include latitude and longitude columns in the CSV to enable routing.", type = "warning", duration = 10)
    }

    # Coordinates are maintained separately so map joins always use one latitude/longitude pair.
    uploaded$latitude <- NULL
    uploaded$longitude <- NULL
    unmapped_locations <- setdiff(uploaded_locations, rv$coordinates$location)
    if (length(unmapped_locations)) {
      issues <- c(issues, paste0(
        "Could not map these location(s): ", paste(unmapped_locations, collapse = ", "),
        ". For reliable mapping, add latitude and longitude columns to the CSV. You may also add optional city, state, and country columns to improve automatic geocoding."
      ))
    }

    # Keep the validated dataset. Warnings are reported visibly and in a notification.
    rv$data <- uploaded
    quality_state(list(
      title = if (length(issues)) "Loaded with data-quality warnings" else "All checks passed",
      issues = if (length(issues)) issues else c("No duplicate keys, invalid dates, unexpected categories, missing required values, or unit conflicts were detected."),
      checked = TRUE
    ))

    updateSelectInput(session, "overview_dimension", choices = c("All", sort(unique(uploaded$dimension))))
    updateSelectInput(session, "overview_location", choices = c("All", sort(unique(uploaded$location))))
    updateSelectInput(session, "overview_indicator", choices = c("All", sort(unique(uploaded$indicator))))
    updateSelectInput(session, "data_dimension", choices = c("All", sort(unique(uploaded$dimension))))
    updateSelectInput(session, "data_location", choices = c("All", sort(unique(uploaded$location))))
    updateSelectInput(session, "geo_jump_location",
                      choices = c("📍 Focus Area / Jump to Location..." = "All",
                                  sort(unique(uploaded$location))),
                      selected = "All")
    updateSelectInput(session, "geo_dimension",
                      choices = c("All", sort(unique(uploaded$dimension))),
                      selected = if ("Mobility" %in% uploaded$dimension) "Mobility" else "All")
    updateSelectInput(session, "geo_indicator",
                      choices = sort(unique(uploaded$indicator)),
                      selected = if ("Traffic Volume" %in% uploaded$indicator) "Traffic Volume" else sort(unique(uploaded$indicator))[1])
    updateDateRangeInput(session, "data_dates", start = min(uploaded$date), end = max(uploaded$date))

    if (length(issues)) {
      showNotification(paste("Loaded", nrow(uploaded), "records with warnings. Review the Data quality report."), type = "warning", duration = 12)
    } else {
      showNotification(paste(nrow(uploaded), "records loaded. All data-quality checks passed."), type = "message", duration = 8)
    }
  })

  output$quality_report <- renderUI({
    q <- quality_state()
    color <- if (!isTRUE(q$checked)) "#FEE2E2" else if (grepl("warning", q$title, ignore.case = TRUE)) "#FFF7ED" else "#ECFDF5"
    border <- if (!isTRUE(q$checked)) "#DC2626" else if (grepl("warning", q$title, ignore.case = TRUE)) "#F97316" else "#10B981"
    tags$div(
      style = paste0("background:", color, ";border-left:4px solid ", border, ";padding:12px 16px;border-radius:8px;"),
      tags$b(q$title),
      tags$ul(lapply(q$issues, tags$li))
    )
  })
  
  # Global filtered data for the data explorer.
  data_filtered <- reactive({
    d <- rv$data
    
    if (!is.null(input$data_dimension) &&
        input$data_dimension != "All") {
      d <- d[d$dimension == input$data_dimension, , drop = FALSE]
    }
    
    if (!is.null(input$data_location) &&
        input$data_location != "All") {
      d <- d[d$location == input$data_location, , drop = FALSE]
    }
    
    if (!is.null(input$data_dates) &&
        length(input$data_dates) == 2) {
      d <- d[
        d$date >= input$data_dates[1] &
          d$date <= input$data_dates[2],
        ,
        drop = FALSE
      ]
    }
    
    d
  })
  
  # Home KPI cards.
  output$home_kpis <- renderUI({
    d <- rv$data
    
    fluidRow(
      column(
        3,
        kpi_card(
          "Data records",
          format(nrow(d), big.mark = ","),
          "Available observations",
          blue
        )
      ),
      column(
        3,
        kpi_card(
          "Dimensions",
          length(unique(d$dimension)),
          "Urban themes",
          teal
        )
      ),
      column(
        3,
        kpi_card(
          "Locations",
          length(unique(d$location)),
          "Areas represented",
          purple
        )
      ),
      column(
        3,
        kpi_card(
          "Indicators",
          length(unique(d$indicator)),
          "Metrics tracked",
          pink
        )
      )
    )
  })
  
  # Home chart: average values by dimension.
  output$home_dimensions <- renderPlotly({
    d <- rv$data
    agg <- aggregate(value ~ dimension, d, mean, na.rm = TRUE)
    agg <- agg[order(agg$value), ]
    
    p <- plot_ly(
      agg,
      x = ~value,
      y = ~reorder(dimension, value),
      type = "bar",
      orientation = "h",
      marker = list(color = palette),
      text = ~round(value, 1),
      textposition = "outside",
      hovertemplate = "%{y}<br>Mean: %{x:.2f}<extra></extra>"
    )
    
    chart_theme(p, 330) %>%
      layout(
        xaxis = list(title = "Average value"),
        yaxis = list(title = "")
      )
  })
  
  # Home donut chart.
  output$home_donut <- renderPlotly({
    d <- rv$data
    agg <- aggregate(value ~ indicator, d, mean, na.rm = TRUE)
    agg$value <- pmax(agg$value, 0.001)
    agg <- agg[order(agg$value, decreasing = TRUE), ]
    agg <- head(agg, 8)
    
    p <- plot_ly(
      agg,
      labels = ~indicator,
      values = ~value,
      type = "pie",
      hole = 0.60,
      sort = FALSE,
      marker = list(colors = palette),
      textinfo = "percent",
      hovertemplate = "%{label}<br>Mean: %{value:.2f}<extra></extra>"
    )
    
    chart_theme(p, 330) %>%
      layout(showlegend = TRUE)
  })
  
  # Filtered overview data.
  overview_data <- reactive({
    d <- rv$data
    
    if (!is.null(input$overview_dimension) &&
        input$overview_dimension != "All") {
      d <- d[d$dimension == input$overview_dimension, , drop = FALSE]
    }
    
    if (!is.null(input$overview_location) &&
        input$overview_location != "All") {
      d <- d[d$location == input$overview_location, , drop = FALSE]
    }
    
    if (!is.null(input$overview_indicator) &&
        input$overview_indicator != "All") {
      d <- d[d$indicator == input$overview_indicator, , drop = FALSE]
    }
    
    d
  })
  
  output$overview_kpis <- renderUI({
    d <- overview_data()
    
    mean_value <- if (nrow(d)) mean(d$value, na.rm = TRUE) else NA
    median_value <- if (nrow(d)) median(d$value, na.rm = TRUE) else NA
    spread <- if (nrow(d) > 1) sd(d$value, na.rm = TRUE) else NA
    
    fluidRow(
      column(
        3,
        kpi_card(
          "Filtered records",
          format(nrow(d), big.mark = ","),
          "Matching selected filters",
          blue
        )
      ),
      column(
        3,
        kpi_card(
          "Mean",
          if (is.finite(mean_value)) round(mean_value, 2) else "—",
          "Average value",
          teal
        )
      ),
      column(
        3,
        kpi_card(
          "Median",
          if (is.finite(median_value)) round(median_value, 2) else "—",
          "Middle observation",
          purple
        )
      ),
      column(
        3,
        kpi_card(
          "Standard deviation",
          if (is.finite(spread)) round(spread, 2) else "—",
          "Measure of variability",
          pink
        )
      )
    )
  })
  
  output$overview_bar <- renderPlotly({
    d <- overview_data()
    validate(need(nrow(d) > 0, "No records match these filters."))
    
    agg <- aggregate(value ~ dimension, d, mean, na.rm = TRUE)
    
    p <- plot_ly(
      agg,
      x = ~reorder(dimension, value),
      y = ~value,
      type = "bar",
      marker = list(color = palette),
      hovertemplate = "%{x}<br>Mean: %{y:.2f}<extra></extra>"
    )
    
    chart_theme(p, 370) %>%
      layout(
        xaxis = list(title = "", tickangle = -25),
        yaxis = list(title = "Mean value")
      )
  })
  
  output$overview_rank <- renderPlotly({
    d <- overview_data()
    validate(need(nrow(d) > 0, "No records match these filters."))
    
    agg <- aggregate(value ~ indicator, d, mean, na.rm = TRUE)
    agg <- agg[order(agg$value, decreasing = TRUE), ]
    agg <- head(agg, 12)
    
    p <- plot_ly(
      agg,
      x = ~value,
      y = ~reorder(indicator, value),
      type = "bar",
      orientation = "h",
      marker = list(color = purple),
      hovertemplate = "%{y}<br>Mean: %{x:.2f}<extra></extra>"
    )
    
    chart_theme(p, 370) %>%
      layout(
        xaxis = list(title = "Mean value"),
        yaxis = list(title = "")
      )
  })
  
  # Standardized heatmap values allow cross-indicator comparison.
  output$overview_heatmap <- renderPlotly({
    d <- overview_data()
    validate(need(nrow(d) > 0, "No records match these filters."))
    
    agg <- aggregate(
      value ~ location + indicator,
      d,
      mean,
      na.rm = TRUE
    )
    
    agg$zscore <- ave(
      agg$value,
      agg$indicator,
      FUN = function(x) {
        if (length(x) < 2 || is.na(sd(x)) || sd(x) == 0) {
          rep(0, length(x))
        } else {
          as.numeric(scale(x))
        }
      }
    )
    
    z <- xtabs(zscore ~ indicator + location, data = agg)
    
    p <- plot_ly(
      x = colnames(z),
      y = rownames(z),
      z = unclass(z),
      type = "heatmap",
      colorscale = list(
        c(0, "#315BEA"),
        c(0.5, "#F4F7FC"),
        c(1, "#20BFAF")
      ),
      hovertemplate =
        "Location: %{x}<br>Indicator: %{y}<br>Z-score: %{z:.2f}<extra></extra>"
    )
    
    chart_theme(p, 440) %>%
      layout(
        xaxis = list(title = "Location", tickangle = -35),
        yaxis = list(title = "Indicator")
      )
  })
  
  # Mobility trend.
  mobility_data <- reactive({
    d <- rv$data
    d[d$dimension == "Mobility", , drop = FALSE]
  })
  
  # =========================================================================
  # GEOSPATIAL INTELLIGENCE & CARTOGRAPHY SERVER ENGINE
  # =========================================================================

  # Reactive state for the actively selected / focused location
  selected_location <- reactiveVal(NULL)

  # Attach known coordinates and safely filter geospatial data
  geo_data <- reactive({
    d <- rv$data
    req(input$geo_indicator)

    if (!is.null(input$geo_dimension) && input$geo_dimension != "All") {
      d <- d[d$dimension == input$geo_dimension, , drop = FALSE]
    }
    d <- d[d$indicator == input$geo_indicator, , drop = FALSE]

    d <- merge(d, rv$coordinates, by = "location", all.x = TRUE)
    d <- d[is.finite(d$latitude) & is.finite(d$longitude) & is.finite(d$value), , drop = FALSE]
    validate(need(nrow(d) > 0, "No records match this dimension and indicator. Choose a matching dimension or indicator."))

    aggregate(value ~ location + latitude + longitude, data = d, FUN = mean)
  })

  # Dynamic Spatial KPI Ribbon Output
  output$geo_kpis <- renderUI({
    d <- geo_data()
    validate(need(nrow(d) > 0, "No records match this dimension and indicator."))

    mean_val <- mean(d$value, na.rm = TRUE)
    sorted_d <- d[order(d$value, decreasing = TRUE), , drop = FALSE]
    peak_loc <- sorted_d$location[1]
    peak_val <- sorted_d$value[1]
    low_loc <- sorted_d$location[nrow(sorted_d)]
    low_val <- sorted_d$value[nrow(sorted_d)]
    val_sd <- sd(d$value, na.rm = TRUE)

    fluidRow(
      column(
        3,
        tags$div(
          class = "map-stat-card",
          style = "border-left-color:#315BEA;",
          tags$div(class = "map-stat-label", "Citywide Average"),
          tags$div(class = "map-stat-val", format(round(mean_val, 2), big.mark = ",")),
          tags$div(class = "map-stat-sub", "Benchmark across uploaded locations")
        )
      ),
      column(
        3,
        tags$div(
          class = "map-stat-card",
          style = "border-left-color:#FF6FAE;",
          tags$div(class = "map-stat-label", "Peak Hotspot"),
          tags$div(class = "map-stat-val", style = "color:#FF6FAE;", format(round(peak_val, 2), big.mark = ",")),
          tags$div(class = "map-stat-sub", paste0("Highest in ", peak_loc))
        )
      ),
      column(
        3,
        tags$div(
          class = "map-stat-card",
          style = "border-left-color:#20D6C7;",
          tags$div(class = "map-stat-label", "Minimum Zone"),
          tags$div(class = "map-stat-val", style = "color:#078B7F;", format(round(low_val, 2), big.mark = ",")),
          tags$div(class = "map-stat-sub", paste0("Lowest in ", low_loc))
        )
      ),
      column(
        3,
        tags$div(
          class = "map-stat-card",
          style = "border-left-color:#9B7BFF;",
          tags$div(class = "map-stat-label", "Spatial Variance"),
          tags$div(class = "map-stat-val", paste0("±", format(round(if (is.na(val_sd)) 0 else val_sd, 1), big.mark = ","))),
          tags$div(class = "map-stat-sub", "Standard deviation across city")
        )
      )
    )
  })

  # Dynamic Location Intelligence Dossier Panel
  output$geo_location_dossier <- renderUI({
    loc <- selected_location()
    d <- geo_data()
    req(nrow(d) > 0)

    d_sorted <- d[order(d$value, decreasing = TRUE), , drop = FALSE]

    # When no specific location is selected, show the Citywide Overview Dossier
    if (is.null(loc) || loc == "" || loc == "All" || !(loc %in% d$location)) {
      top_loc <- d_sorted$location[1]
      top_val <- round(d_sorted$value[1], 1)

      tags$div(
        class = "location-dossier-card",
        tags$div(
          class = "dossier-header",
          tags$div(
            tags$div(style = "color:#94A3B8;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;", "Location Spotlight"),
            tags$div(class = "dossier-title", "Citywide Overview")
          ),
          tags$span(class = "dossier-badge", style = "background:rgba(99,102,241,0.2);color:#818CF8;border:1px solid rgba(99,102,241,0.35);", paste(nrow(unique(d[, c("location"), drop = FALSE])), "Active Locations"))
        ),
        tags$p(
          style = "font-size:13px;color:#94A3B8;line-height:1.6;",
          "Click any marker, polygon or flow arc on the map — or select a location from the dropdown — to inspect its real-time urban intelligence dossier."
        ),
        tags$div(
          style = "margin:14px 0;padding:12px 14px;background:rgba(15,23,42,0.65);border-radius:12px;border:1px solid rgba(255,255,255,0.08);backdrop-filter:blur(8px);",
          tags$div(style = "font-size:11px;color:#94A3B8;font-weight:700;text-transform:uppercase;", "Current Peak Hotspot"),
          tags$div(
            style = "display:flex;justify-content:space-between;align-items:baseline;margin-top:4px;",
            tags$span(style = "font-family:'Space Grotesk',sans-serif;font-size:18px;font-weight:700;color:#F8FAFC;", top_loc),
            tags$span(style = "font-family:'Space Grotesk',sans-serif;font-size:16px;font-weight:700;color:#F43F5E;text-shadow:0 0 10px rgba(244,63,94,0.4);", format(top_val, big.mark = ","))
          )
        ),
        tags$div(style = "font-size:11px;color:#94A3B8;margin-bottom:8px;font-weight:700;text-transform:uppercase;", "Top Monitored Locations"),
        lapply(seq_len(min(4, nrow(d_sorted))), function(k) {
          loc_k <- d_sorted$location[k]
          val_k <- d_sorted$value[k]
          val_rng <- range(d$value, na.rm = TRUE)
          pct_k <- if (diff(val_rng) == 0) 50 else round(100 * (val_k - val_rng[1]) / diff(val_rng))
          tags$div(
            style = "margin-bottom:8px;",
            tags$div(
              style = "display:flex;justify-content:space-between;font-size:12px;margin-bottom:2px;",
              tags$span(style = "font-weight:600;color:#E2E8F0;", paste0("#", k, " ", loc_k)),
              tags$span(style = "font-family:'Space Grotesk',sans-serif;font-weight:700;color:#38BDF8;", format(round(val_k, 1), big.mark = ","))
            ),
            tags$div(
              class = "dossier-meter-bar",
              tags$div(class = "dossier-meter-fill", style = paste0("width:", pct_k, "%;background:linear-gradient(90deg, #6366F1, #20D6C7);box-shadow:0 0 10px rgba(32,214,199,0.35);"))
            )
          )
        }),
        tags$div(
          style = "margin-top:auto;padding-top:12px;border-top:1px solid rgba(255,255,255,0.08);font-size:11px;color:#64748B;text-align:center;",
          "Interactive GIS Dossier • Vellore 360°"
        )
      )
    } else {
      # Specific Location Dossier
      loc_row <- d[d$location == loc, ]
      req(nrow(loc_row) > 0)

      val <- loc_row$value[1]
      lat <- loc_row$latitude[1]
      lng <- loc_row$longitude[1]

      loc_rank <- which(d_sorted$location == loc)[1]
      city_avg <- mean(d$value, na.rm = TRUE)
      diff_pct <- if (city_avg == 0) 0 else round(100 * (val - city_avg) / city_avg, 1)
      diff_sign <- if (diff_pct >= 0) paste0("+", diff_pct, "%") else paste0(diff_pct, "%")
      diff_color <- if (diff_pct >= 0) "#FB7185" else "#34D399"

      val_rng <- range(d$value, na.rm = TRUE)
      pct <- if (diff(val_rng) == 0) 50 else round(100 * (val - val_rng[1]) / diff(val_rng))

      all_loc_data <- rv$data[rv$data$location == loc, , drop = FALSE]
      dim_means <- if (nrow(all_loc_data) > 0) {
        aggregate(value ~ dimension, data = all_loc_data, FUN = mean, na.rm = TRUE)
      } else {
        data.frame(dimension = character(0), value = numeric(0))
      }

      tags$div(
        class = "location-dossier-card",
        tags$div(
          class = "dossier-header",
          tags$div(
            tags$div(style = "color:#94A3B8;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;", "Sector Intelligence"),
            tags$div(class = "dossier-title", loc)
          ),
          tags$span(
            class = "dossier-badge",
            style = if (pct >= 75) "background:rgba(244,63,94,0.2);color:#FB7185;border:1px solid rgba(244,63,94,0.4);" else if (pct <= 25) "background:rgba(32,214,199,0.2);color:#20D6C7;border:1px solid rgba(32,214,199,0.4);" else "background:rgba(245,158,11,0.2);color:#FBBF24;border:1px solid rgba(245,158,11,0.4);",
            if (pct >= 75) "● High Activity" else if (pct <= 25) "● Low Activity" else "● Moderate"
          )
        ),

        # Primary Metric Card
        tags$div(
          style = "background:rgba(15,23,42,0.65);border:1px solid rgba(255,255,255,0.08);border-radius:14px;padding:14px;margin-bottom:12px;backdrop-filter:blur(8px);",
          tags$div(style = "font-size:11px;color:#94A3B8;font-weight:700;text-transform:uppercase;", input$geo_indicator),
          tags$div(
            style = "display:flex;align-items:baseline;justify-content:space-between;margin-top:4px;",
            tags$span(
              style = "font-family:'Space Grotesk',sans-serif;font-size:26px;font-weight:700;color:#F8FAFC;",
              format(round(val, 2), big.mark = ",")
            ),
            tags$span(
              style = paste0("font-size:12px;font-weight:700;color:", diff_color, ";text-shadow:0 0 8px ", diff_color, "40;"),
              paste0(diff_sign, " vs Avg")
            )
          ),
          tags$div(
            class = "dossier-meter-wrap",
            tags$div(
              style = "display:flex;justify-content:space-between;font-size:10px;color:#94A3B8;font-weight:600;",
              tags$span(paste0("City Rank: #", loc_rank, " of ", nrow(d))),
              tags$span(paste0(pct, "% Intensity"))
            ),
            tags$div(
              class = "dossier-meter-bar",
              tags$div(class = "dossier-meter-fill", style = paste0("width:", pct, "%;background:linear-gradient(90deg, #6366F1, #20D6C7);box-shadow:0 0 10px rgba(32,214,199,0.35);"))
            )
          )
        ),

        # 360° Profile Scorecard
        tags$div(style = "font-size:11px;color:#94A3B8;font-weight:700;text-transform:uppercase;letter-spacing:0.8px;margin-bottom:4px;", "360° Sector Profile"),
        tags$div(
          class = "dossier-grid",
          lapply(seq_len(nrow(dim_means)), function(j) {
            dim_name <- dim_means$dimension[j]
            dim_val <- round(dim_means$value[j], 1)
            tags$div(
              class = "dossier-cell",
              tags$div(class = "dossier-cell-dim", dim_name),
              tags$div(class = "dossier-cell-val", format(dim_val, big.mark = ","))
            )
          })
        ),

        # GPS Coordinates
        tags$div(
          style = "margin-top:12px;padding-top:10px;border-top:1px solid rgba(255,255,255,0.08);display:flex;justify-content:space-between;align-items:center;font-size:11px;color:#64748B;",
          tags$span(paste0("GPS: ", round(lat, 4), "° N, ", round(lng, 4), "° E")),
          tags$span(style = "color:#20D6C7;font-weight:600;text-shadow:0 0 6px rgba(32,214,199,0.4);", "● Active Node")
        ),

        # Actions
        tags$div(
          style = "margin-top:12px;display:flex;gap:8px;",
          actionButton(
            "dossier_filter_dashboard",
            label = "Filter Dashboard ➔",
            class = "btn btn-primary btn-sm",
            style = "flex:1;padding:8px 12px;font-size:12px;"
          ),
          actionButton(
            "dossier_clear",
            label = "✕ Clear Focus",
            class = "btn btn-default btn-sm",
            style = "border-radius:10px;padding:8px 12px;font-size:12px;color:#94A3B8;background:rgba(255,255,255,0.05);border:1px solid rgba(255,255,255,0.12);"
          )
        )
      )
    }
  })

  # Leaflet Marker Click Observer: updates dossier and synchronizes focus
  observeEvent(input$geo_map_marker_click, {
    click <- input$geo_map_marker_click
    req(click$id)
    if (click$id %in% unique(rv$data$location)) {
      selected_location(click$id)
      updateSelectInput(session, "geo_jump_location", selected = click$id)
      leaflet::leafletProxy("geo_map") %>%
        leaflet::flyTo(lng = click$lng, lat = click$lat, zoom = 14)
    }
  })

  # Leaflet Shape Click Observer: polygon / buffer click
  observeEvent(input$geo_map_shape_click, {
    click <- input$geo_map_shape_click
    req(click$id)
    if (click$id %in% unique(rv$data$location)) {
      selected_location(click$id)
      updateSelectInput(session, "geo_jump_location", selected = click$id)
      leaflet::leafletProxy("geo_map") %>%
        leaflet::flyTo(lng = click$lng, lat = click$lat, zoom = 14)
    }
  })

  # Quick Location Jump Dropdown Observer
  observeEvent(input$geo_jump_location, {
    loc <- input$geo_jump_location
    if (is.null(loc) || loc == "" || loc == "All") {
      selected_location(NULL)
    } else if (loc %in% unique(rv$data$location)) {
      selected_location(loc)
      coords <- rv$coordinates[rv$coordinates$location == loc, ]
      if (nrow(coords) > 0) {
        leaflet::leafletProxy("geo_map") %>%
          leaflet::flyTo(lng = coords$longitude[1], lat = coords$latitude[1], zoom = 14)
      }
    }
  })

  # Reset View Button Observer
  observeEvent(input$geo_reset_view, {
    updateSelectInput(session, "geo_jump_location", selected = "All")
    selected_location(NULL)
    coords <- rv$coordinates
    if (nrow(coords) > 0) {
      lon_range <- range(coords$longitude, na.rm = TRUE)
      lat_range <- range(coords$latitude, na.rm = TRUE)
      lon_pad <- max(diff(lon_range) * 0.12, 0.01)
      lat_pad <- max(diff(lat_range) * 0.12, 0.01)
      leaflet::leafletProxy("geo_map") %>%
        leaflet::fitBounds(
          lng1 = lon_range[1] - lon_pad, lat1 = lat_range[1] - lat_pad,
          lng2 = lon_range[2] + lon_pad, lat2 = lat_range[2] + lat_pad
        )
    }
  })

  # Table Row Selection Observer
  observeEvent(input$geo_table_rows_selected, {
    d <- geo_data()
    req(nrow(d) > 0)
    idx <- input$geo_table_rows_selected
    if (length(idx) > 0 && idx <= nrow(d)) {
      d_sorted <- d[order(d$value, decreasing = TRUE), , drop = FALSE]
      loc <- d_sorted$location[idx]
      selected_location(loc)
      updateSelectInput(session, "geo_jump_location", selected = loc)
      leaflet::leafletProxy("geo_map") %>%
        leaflet::flyTo(lng = d_sorted$longitude[idx], lat = d_sorted$latitude[idx], zoom = 14)
    }
  })

  # Plotly Ranking Chart Click Observer
  observeEvent(event_data("plotly_click", source = "geo_rank"), {
    clk <- event_data("plotly_click", source = "geo_rank")
    if (!is.null(clk$y)) {
      loc <- as.character(clk$y)
      if (loc %in% unique(rv$data$location)) {
        selected_location(loc)
        updateSelectInput(session, "geo_jump_location", selected = loc)
        coords <- rv$coordinates[rv$coordinates$location == loc, ]
        if (nrow(coords) > 0) {
          leaflet::leafletProxy("geo_map") %>%
            leaflet::flyTo(lng = coords$longitude[1], lat = coords$latitude[1], zoom = 14)
        }
      }
    }
  })

  # Dossier Action: Filter entire dashboard to selected location
  observeEvent(input$dossier_filter_dashboard, {
    loc <- selected_location()
    req(loc)
    updateSelectInput(session, "overview_location", selected = loc)
    updateTabsetPanel(session, "tabs", selected = "overview")
  })

  # Dossier Action: Clear focus
  observeEvent(input$dossier_clear, {
    updateSelectInput(session, "geo_jump_location", selected = "All")
    selected_location(NULL)
    coords <- rv$coordinates
    if (nrow(coords) > 0) {
      lon_range <- range(coords$longitude, na.rm = TRUE)
      lat_range <- range(coords$latitude, na.rm = TRUE)
      lon_pad <- max(diff(lon_range) * 0.12, 0.01)
      lat_pad <- max(diff(lat_range) * 0.12, 0.01)
      leaflet::leafletProxy("geo_map") %>%
        leaflet::fitBounds(
          lng1 = lon_range[1] - lon_pad, lat1 = lat_range[1] - lat_pad,
          lng2 = lon_range[2] + lon_pad, lat2 = lat_range[2] + lat_pad
        )
    }
  })

  # REAL OPENSTREETMAP BASEMAP WITH 12 ADVANCED GEOSPATIAL METHODS
  output$geo_map <- renderLeaflet({
    d <- geo_data()
    req(nrow(d) > 0)

    method <- input$geo_method
    value_range <- range(d$value, na.rm = TRUE)
    if (!all(is.finite(value_range))) value_range <- c(0, 1)
    if (diff(value_range) == 0) {
      d$scaled <- rep(0.5, nrow(d))
    } else {
      d$scaled <- (d$value - value_range[1]) / diff(value_range)
    }

    # Continuous high-vibrancy color palette
    pal <- leaflet::colorNumeric(
      palette = c("#315BEA", "#20BFAF", "#FFB45C", "#D7191C"),
      domain = d$value
    )
    d$map_color <- pal(d$value)
    # Area-proportional radius scaling (square root)
    d$radius <- if (diff(value_range) == 0) rep(10, nrow(d)) else 7 + 20 * sqrt(d$scaled)

    # Sort for ranking
    d_sorted <- d[order(d$value, decreasing = TRUE), , drop = FALSE]
    d$rank <- vapply(d$location, function(loc) which(d_sorted$location == loc)[1], integer(1))

    # High-detail basemaps using explicit tile URLs (avoids provider-registry alias errors); Esri Clarity imagery is preferred for clearer archived imagery.
    # Carto tiles use {r} retina variants where supported; source imagery resolution is provider-dependent.
    basemap <- if (is.null(input$geo_basemap)) "satellite" else input$geo_basemap
    map <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl=TRUE, preferCanvas=TRUE)) %>%
      leaflet::addTiles(
        urlTemplate="https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution="&copy; OpenStreetMap contributors", group="OpenStreetMap Streets",
        options=leaflet::tileOptions(maxZoom=19, tileSize=256, detectRetina=TRUE)) %>%
      leaflet::addTiles(
        urlTemplate="https://clarity.maptiles.arcgis.com/arcgis/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        attribution="Imagery &copy; Esri, Vantor, Earthstar Geographics and GIS User Community", group="Esri Satellite",
        options=leaflet::tileOptions(maxZoom=20, maxNativeZoom=19, tileSize=256, detectRetina=TRUE)) %>%
      leaflet::addTiles(
        urlTemplate="https://clarity.maptiles.arcgis.com/arcgis/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        attribution="Imagery &copy; Esri, Vantor, Earthstar Geographics and GIS User Community", group="Esri Hybrid imagery",
        options=leaflet::tileOptions(maxZoom=20, maxNativeZoom=19, tileSize=256, detectRetina=TRUE)) %>%
      leaflet::addTiles(
        urlTemplate="https://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}",
        attribution="Labels &copy; Esri", group="Esri Hybrid road labels",
        options=leaflet::tileOptions(opacity=0.95, maxZoom=19, tileSize=256)) %>%
      leaflet::addTiles(
        urlTemplate="https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
        attribution="Map data &copy; OpenStreetMap contributors, SRTM | Map style &copy; OpenTopoMap",
        group="Topographic Relief", options=leaflet::tileOptions(maxZoom=17, tileSize=256)) %>%
      leaflet::addScaleBar(position="bottomleft")

    base_groups <- c("OpenStreetMap Streets", "Esri Satellite",
                     "Esri Hybrid imagery", "Topographic Relief")
    selected_group <- switch(basemap, osm="OpenStreetMap Streets", satellite="Esri Satellite",
      hybrid="Esri Hybrid imagery", terrain="Topographic Relief", "Esri Satellite")
    # Map layer control intentionally hidden; basemap is selected through the section controls.
    for (g in setdiff(base_groups, selected_group)) map <- map %>% leaflet::hideGroup(g)
    if (basemap == "hybrid") map <- map %>% leaflet::showGroup("Esri Hybrid road labels") else map <- map %>% leaflet::hideGroup("Esri Hybrid road labels")

    # Dynamically frame the uploaded city's mapped coordinates instead of Vellore.
    # A small padding is added so a single point or tightly clustered city still fits.
    map <- map %>% leaflet::addScaleBar(position = "bottomleft")
    lon_range <- range(d$longitude, na.rm = TRUE)
    lat_range <- range(d$latitude, na.rm = TRUE)
    lon_span <- diff(lon_range)
    lat_span <- diff(lat_range)
    lon_pad <- max(lon_span * 0.12, 0.01)
    lat_pad <- max(lat_span * 0.12, 0.01)
    map <- map %>% leaflet::fitBounds(
      lng1 = lon_range[1] - lon_pad,
      lat1 = lat_range[1] - lat_pad,
      lng2 = lon_range[2] + lon_pad,
      lat2 = lat_range[2] + lat_pad
    )

    active_dim <- if (is.null(input$geo_dimension) || input$geo_dimension == "All") "Urban Analytics" else input$geo_dimension

    # Construct rich interactive HTML popups
    popup_text <- vapply(seq_len(nrow(d)), function(i) {
      make_rich_popup(
        location = d$location[i],
        indicator = input$geo_indicator,
        value = d$value[i],
        scaled = d$scaled[i],
        rank = d$rank[i],
        total = nrow(d),
        lat = d$latitude[i],
        lng = d$longitude[i],
        dimension = active_dim
      )
    }, character(1))

    # -----------------------------------------------------------------------
    # 12 ADVANCED GEOSPATIAL VISUALIZATION METHODS
    # -----------------------------------------------------------------------

    # 01 Point Map: High-contrast radar nodes with glowing inner cores
    if (grepl("01 Point", method)) {
      if (isTRUE(input$geo_pulsing)) {
        map <- map %>% addCircles(
          data = d, lng = ~longitude, lat = ~latitude, radius = 550,
          color = ~map_color, weight = 1.5, fillColor = ~map_color,
          fillOpacity = 0.22, dashArray = "4, 4"
        )
      }
      map <- map %>% addCircleMarkers(
        data = d, lng = ~longitude, lat = ~latitude, radius = 9,
        color = "#FFFFFF", weight = 3, fillColor = ~map_color,
        fillOpacity = 0.95, popup = popup_text, layerId = ~location
      )

    # 02 Proportional Symbols: Mathematically scaled dual-ring halos
    } else if (grepl("02 Proportional", method)) {
      map <- map %>% addCircles(
        data = d, lng = ~longitude, lat = ~latitude,
        radius = ~ (400 + 1400 * sqrt(scaled)),
        color = ~map_color, weight = 1.2, fill = TRUE,
        fillColor = ~map_color, fillOpacity = 0.16,
        dashArray = "4, 4"
      )
      map <- map %>% addCircleMarkers(
        data = d, lng = ~longitude, lat = ~latitude, radius = ~radius,
        color = "#FFFFFF", weight = 2.5, fillColor = ~map_color,
        fillOpacity = 0.82, popup = popup_text, layerId = ~location
      )

    # 03 Cluster Map: Dynamic density grouping
    } else if (grepl("03 Cluster", method)) {
      map <- map %>% addCircleMarkers(
        data = d, lng = ~longitude, lat = ~latitude, radius = 10,
        color = "#FFFFFF", weight = 2.5, fillColor = ~map_color,
        fillOpacity = 0.95, popup = popup_text, layerId = ~location,
        clusterOptions = markerClusterOptions()
      )

    # 04 Choropleth: Pure-R Voronoi Territorial Catchments
    } else if (grepl("04 Choropleth", method)) {
      cells <- compute_voronoi_cells(d)
      for (i in seq_along(cells)) {
        poly <- cells[[i]]
        if (nrow(poly) >= 3) {
          map <- map %>% addPolygons(
            lng = c(poly[, 1], poly[1, 1]),
            lat = c(poly[, 2], poly[1, 2]),
            fillColor = d$map_color[i],
            fillOpacity = 0.62,
            color = "#FFFFFF",
            weight = 2,
            popup = popup_text[i],
            layerId = d$location[i],
            highlightOptions = highlightOptions(
              weight = 4,
              color = "#20D6C7",
              fillOpacity = 0.85,
              bringToFront = TRUE
            )
          )
        }
      }
      map <- map %>% addCircleMarkers(
        data = d, lng = ~longitude, lat = ~latitude, radius = 6,
        color = "#FFFFFF", weight = 2, fillColor = "#10172A",
        fillOpacity = 1, popup = popup_text, layerId = ~location
      )

    # 05 Cartogram: Value-weighted graduated distortion rings
    } else if (grepl("05 Cartogram", method)) {
      for (i in seq_len(nrow(d))) {
        radii <- c(350, 750, 1300) * (0.6 + 1.2 * d$scaled[i])
        for (r in radii) {
          map <- map %>% addCircles(
            lng = d$longitude[i], lat = d$latitude[i], radius = r,
            color = d$map_color[i], weight = 1.2, fill = TRUE,
            fillColor = d$map_color[i], fillOpacity = 0.12,
            dashArray = "3, 5"
          )
        }
      }
      map <- map %>% addCircleMarkers(
        data = d, lng = ~longitude, lat = ~latitude,
        radius = ~ (8 + 16 * scaled),
        color = "#FFFFFF", weight = 2.5, fillColor = ~map_color,
        fillOpacity = 0.95, popup = popup_text, layerId = ~location
      )

    # 06 Hex Binning: Pure-R Regular Hexagonal Tessellation Mesh
    } else if (grepl("06 Hex Binning", method)) {
      hex_list <- make_hex_grid_pure(d, radius_meters = 850)
      for (h in hex_list) {
        h_color <- pal(pmax(value_range[1], pmin(value_range[2], h$value)))
        h_popup <- paste0(
          "<div style='font-family:\"DM Sans\",sans-serif;padding:3px;'>",
            "<b style='font-size:14px;color:#FFFFFF;'>Hexagonal Grid Cell #", h$id, "</b><br>",
            "<span style='color:#94A3B8;font-size:11px;'>Interpolated Metric:</span> ",
            "<b style='color:#20D6C7;'>", format(round(h$value, 2), big.mark = ","), "</b><br>",
            "<span style='color:#94A3B8;font-size:11px;'>Nearest Node:</span> ", h$nearest_loc, "<br>",
            "<span style='color:#94A3B8;font-size:11px;'>Relative Intensity:</span> ", round(h$scaled * 100), "%",
          "</div>"
        )
        map <- map %>% addPolygons(
          lng = h$lngs, lat = h$lats,
          fillColor = h_color, fillOpacity = 0.70,
          color = "rgba(255,255,255,0.7)", weight = 1,
          popup = h_popup,
          group = "Hexagonal Binning",
          highlightOptions = highlightOptions(
            weight = 3, color = "#FFFFFF", fillOpacity = 0.9, bringToFront = TRUE
          )
        )
      }
      map <- map %>% addCircleMarkers(
        data = d, lng = ~longitude, lat = ~latitude, radius = 5,
        color = "#FFFFFF", weight = 2, fillColor = "#10172A",
        fillOpacity = 1, popup = popup_text, layerId = ~location
      )

    # 07 Heat Map: Multi-Tier Gaussian Density Surface
    } else if (grepl("07 Heat Map", method)) {
      for (i in seq_len(nrow(d))) {
        val_s <- d$scaled[i]
        # Ambient spread (1600m)
        map <- map %>% addCircles(
          lng = d$longitude[i], lat = d$latitude[i],
          radius = 1600 * (0.6 + 0.6 * val_s),
          color = "#315BEA", weight = 0, fillColor = "#315BEA", fillOpacity = 0.08
        )
        # Mid warm layer (1000m)
        map <- map %>% addCircles(
          lng = d$longitude[i], lat = d$latitude[i],
          radius = 1000 * (0.6 + 0.6 * val_s),
          color = "#20D6C7", weight = 0, fillColor = "#20D6C7", fillOpacity = 0.16
        )
        # Near active layer (550m)
        map <- map %>% addCircles(
          lng = d$longitude[i], lat = d$latitude[i],
          radius = 550 * (0.6 + 0.6 * val_s),
          color = "#FFB45C", weight = 0, fillColor = "#FFB45C", fillOpacity = 0.28
        )
        # Epicenter core (220m)
        map <- map %>% addCircles(
          lng = d$longitude[i], lat = d$latitude[i],
          radius = 220 * (0.6 + 0.6 * val_s),
          color = "#D7191C", weight = 0, fillColor = "#D7191C", fillOpacity = 0.65
        )
      }
      map <- map %>% addCircleMarkers(
        data = d, lng = ~longitude, lat = ~latitude, radius = 6,
        color = "#FFFFFF", weight = 2, fillColor = ~map_color,
        fillOpacity = 1, popup = popup_text, layerId = ~location
      )

    # 08 Topographic: 5-Tier Hypsometric Contour Bands
    } else if (grepl("08 Topographic", method)) {
      topo_palette <- c("#2E7D32", "#81C784", "#FDD835", "#FB8C00", "#E53935")
      for (i in seq_len(nrow(d))) {
        sf_val <- 0.6 + 0.8 * d$scaled[i]
        radii <- c(1400, 1000, 650, 380, 160) * sf_val
        for (r_idx in seq_along(radii)) {
          map <- map %>% addCircles(
            lng = d$longitude[i], lat = d$latitude[i],
            radius = radii[r_idx],
            color = topo_palette[r_idx], weight = 1.5,
            fillColor = topo_palette[r_idx],
            fillOpacity = 0.12 + 0.08 * (6 - r_idx),
            popup = paste0("<b>Topographic Contour Band ", 6 - r_idx, "</b><br>", d$location[i], " (Elevation Proxy: ", round(radii[r_idx]), "m)")
          )
        }
      }
      map <- map %>% addCircleMarkers(
        data = d, lng = ~longitude, lat = ~latitude, radius = 5,
        color = "#FFFFFF", weight = 2, fillColor = "#1B5E20",
        fillOpacity = 1, popup = popup_text, layerId = ~location
      )

    # 09 Flow Map: Directional Curved Bezier Arcs
    } else if (grepl("09 Flow Map", method)) {
      ordered <- d[order(d$value, decreasing = TRUE), , drop = FALSE]
      if (nrow(ordered) > 1) {
        for (i in seq_len(nrow(ordered) - 1)) {
          p0 <- c(ordered$longitude[i], ordered$latitude[i])
          p1 <- c(ordered$longitude[i + 1], ordered$latitude[i + 1])
          dist_km <- round(6371 * acos(pmin(1, pmax(-1,
            sin(p0[2]*pi/180)*sin(p1[2]*pi/180) +
            cos(p0[2]*pi/180)*cos(p1[2]*pi/180)*cos((p1[1]-p0[1])*pi/180)
          ))), 2)

          arc <- make_bezier_arc(p0, p1, n = 30, curvature = if (i %% 2 == 1) 0.16 else -0.16)
          map <- map %>% addPolylines(
            lng = arc$lng, lat = arc$lat,
            color = ordered$map_color[i],
            weight = pmax(2.5, 2 + 5 * ordered$scaled[i]),
            opacity = 0.85,
            dashArray = if (i %% 2 == 0) "8, 6" else NULL,
            popup = paste0(
              "<b>Urban Movement Corridor</b><br>",
              "<b>Origin:</b> ", ordered$location[i], " (", round(ordered$value[i], 1), ")<br>",
              "<b>Destination:</b> ", ordered$location[i + 1], " (", round(ordered$value[i + 1], 1), ")<br>",
              "<b>Transit Distance:</b> ", dist_km, " km"
            )
          )
          mid_idx <- round(nrow(arc) / 2)
          map <- map %>% addCircleMarkers(
            lng = arc$lng[mid_idx], lat = arc$lat[mid_idx],
            radius = 4, color = "#FFFFFF", weight = 1.5,
            fillColor = ordered$map_color[i], fillOpacity = 1,
            popup = paste0("Flow Vector: ", ordered$location[i], " &rarr; ", ordered$location[i + 1], " (", dist_km, " km)")
          )
        }
      }
      map <- map %>% addCircleMarkers(
        data = d, lng = ~longitude, lat = ~latitude, radius = ~radius,
        color = "#FFFFFF", weight = 2.5, fillColor = ~map_color,
        fillOpacity = 0.92, popup = popup_text, layerId = ~location
      )

    # 10 Spider Map: Radial Starburst Network from Central Hub
    } else if (grepl("10 Spider Map", method)) {
      hub <- which.max(d$value)
      hub_loc <- d$location[hub]
      p_hub <- c(d$longitude[hub], d$latitude[hub])

      for (hr in c(400, 900, 1500)) {
        map <- map %>% addCircles(
          lng = p_hub[1], lat = p_hub[2], radius = hr,
          color = "#FFB45C", weight = 1.5,
          opacity = 0.5 - hr / 3500, fill = FALSE, dashArray = "4, 6"
        )
      }

      if (nrow(d) > 1) {
        for (i in seq_len(nrow(d))) {
          if (i != hub) {
            p_spoke <- c(d$longitude[i], d$latitude[i])
            dist_km <- round(6371 * acos(pmin(1, pmax(-1,
              sin(p_hub[2]*pi/180)*sin(p_spoke[2]*pi/180) +
              cos(p_hub[2]*pi/180)*cos(p_spoke[2]*pi/180)*cos((p_spoke[1]-p_hub[1])*pi/180)
            ))), 2)

            map <- map %>% addPolylines(
              lng = c(p_hub[1], p_spoke[1]),
              lat = c(p_hub[2], p_spoke[2]),
              color = "#7C3AED", weight = 2.5, opacity = 0.75,
              dashArray = "6, 6",
              popup = paste0(
                "<b>Hub-to-Spoke Ray Vector</b><br>",
                "<b>Central Hub:</b> ", hub_loc, "<br>",
                "<b>Peripheral Node:</b> ", d$location[i], "<br>",
                "<b>Distance:</b> ", dist_km, " km"
              )
            )
          }
        }
      }
      map <- map %>% addCircleMarkers(
        data = d, lng = ~longitude, lat = ~latitude, radius = ~radius,
        color = "#FFFFFF", weight = 2.5, fillColor = ~map_color,
        fillOpacity = 0.92, popup = popup_text, layerId = ~location
      )

    # 11 Time-Space: Chronological Trajectory Trail
    } else if (grepl("11 Time-Space", method)) {
      all_dates <- rv$data[rv$data$indicator == input$geo_indicator, , drop = FALSE]
      if (nrow(all_dates) > 0) {
        date_summary <- aggregate(value ~ date + location, data = all_dates, FUN = mean)
        date_summary <- merge(date_summary, rv$coordinates, by = "location", all.x = TRUE)
        date_summary <- date_summary[order(date_summary$date), , drop = FALSE]

        if (nrow(date_summary) > 1) {
          for (ti in seq_len(min(15, nrow(date_summary) - 1))) {
            map <- map %>% addPolylines(
              lng = c(date_summary$longitude[ti], date_summary$longitude[ti + 1]),
              lat = c(date_summary$latitude[ti], date_summary$latitude[ti + 1]),
              color = "#2563EB", weight = 2.5, opacity = 0.65,
              dashArray = "4, 6"
            )
          }
        }

        date_by_loc <- aggregate(date ~ location, data = all_dates, FUN = max)
        d <- merge(d, date_by_loc, by = "location", all.x = TRUE, sort = FALSE)
        date_pal <- leaflet::colorNumeric(c("#2563EB", "#14B8A6", "#F97316"), domain = as.numeric(d$date))
        d$time_color <- date_pal(as.numeric(d$date))

        map <- map %>% addCircleMarkers(
          data = d, lng = ~longitude, lat = ~latitude, radius = ~radius,
          color = "#FFFFFF", weight = 2.5, fillColor = ~time_color,
          fillOpacity = 0.92, popup = popup_text, layerId = ~location
        )
      }

    # 12 Data Space: Multi-Attribute Similarity Network Graph
    } else if (grepl("12 Data Space", method)) {
      wide_data <- aggregate(value ~ location + indicator, data = rv$data, FUN = mean)
      wide_df <- reshape(wide_data, idvar = "location", timevar = "indicator", direction = "wide")
      num_cols <- names(wide_df)[vapply(wide_df, is.numeric, logical(1))]

      if (length(num_cols) >= 2) {
        mat <- as.matrix(wide_df[, num_cols])
        mat_scaled <- scale(mat)
        mat_scaled[!is.finite(mat_scaled)] <- 0
        dist_matrix <- as.matrix(dist(mat_scaled))
        rownames(dist_matrix) <- wide_df$location
        colnames(dist_matrix) <- wide_df$location

        max_d <- max(dist_matrix)
        if (max_d == 0) max_d <- 1

        for (loc_name in wide_df$location) {
          if (loc_name %in% d$location) {
            dists <- dist_matrix[loc_name, ]
            dists <- dists[names(dists) != loc_name & names(dists) %in% d$location]
            nearest_peers <- head(names(sort(dists)), 2)
            loc1 <- d[d$location == loc_name, ]

            for (peer in nearest_peers) {
              loc2 <- d[d$location == peer, ]
              sim_score <- round(pmax(0, 100 * (1 - dists[peer] / max_d)), 1)
              map <- map %>% addPolylines(
                lng = c(loc1$longitude, loc2$longitude),
                lat = c(loc1$latitude, loc2$latitude),
                color = "#0F766E", weight = 2.5, opacity = 0.72,
                dashArray = "5, 5",
                popup = paste0(
                  "<b>Data-Space Attribute Similarity</b><br>",
                  "<b>Node A:</b> ", loc_name, "<br>",
                  "<b>Node B:</b> ", peer, "<br>",
                  "<b>Multi-Indicator Profile Similarity:</b> ", sim_score, "%"
                )
              )
            }
          }
        }
      }
      map <- map %>% addCircleMarkers(
        data = d, lng = ~longitude, lat = ~latitude, radius = ~radius,
        color = "#FFFFFF", weight = 2.5, fillColor = ~map_color,
        fillOpacity = 0.95, popup = popup_text, layerId = ~location
      )
    }

    # Optional Layer: Territorial Halos
    if (isTRUE(input$geo_halos)) {
      for (i in seq_len(nrow(d))) {
        map <- map %>% addCircles(
          lng = d$longitude[i], lat = d$latitude[i],
          radius = 800 + 1200 * d$scaled[i],
          color = d$map_color[i], weight = 1.2, fill = TRUE,
          fillColor = d$map_color[i], fillOpacity = 0.08,
          dashArray = "4, 4"
        )
      }
    }

    # Optional Layer: Frosted Glass Location Value Labels
    if (isTRUE(input$geo_labels) && !grepl("03 Cluster", method)) {
      for (i in seq_len(nrow(d))) {
        label_html <- paste0(
          "<div class='geo-pill-label'>",
            "<span style='width:6px;height:6px;border-radius:50%;background:", d$map_color[i], ";box-shadow:0 0 6px ", d$map_color[i], ";'></span>",
            "<span>", d$location[i], "</span>",
            "<span style='color:#20D6C7;font-family:\"Space Grotesk\",sans-serif;font-weight:700;'>", format(round(d$value[i], 1), big.mark = ","), "</span>",
          "</div>"
        )
        map <- map %>% addLabelOnlyMarkers(
          lng = d$longitude[i], lat = d$latitude[i],
          label = htmltools::HTML(label_html),
          labelOptions = labelOptions(
            noHide = TRUE, direction = "top", offset = c(0, -14), textOnly = TRUE
          )
        )
      }
    }

    # Color Scale Legend
    map %>% addLegend(
      position = "bottomright", pal = pal, values = d$value,
      title = input$geo_indicator, opacity = 1
    )
  })

  # Dynamic Hexagonal Mesh Update on Zoom
  observeEvent(input$geo_map_zoom, {
    req(grepl("06 Hex Binning", input$geo_method))
    d <- geo_data()
    req(nrow(d) > 0)

    value_range <- range(d$value, na.rm = TRUE)
    if (!all(is.finite(value_range))) value_range <- c(0, 1)
    if (diff(value_range) == 0) {
      d$scaled <- rep(0.5, nrow(d))
    } else {
      d$scaled <- (d$value - value_range[1]) / diff(value_range)
    }

    zoom_level <- input$geo_map_zoom
    radius_meters <- if (zoom_level <= 11) 1800 else if (zoom_level == 12) 1000 else if (zoom_level == 13) 600 else if (zoom_level == 14) 350 else 180

    pal <- leaflet::colorNumeric(
      palette = c("#315BEA", "#20BFAF", "#FFB45C", "#D7191C"),
      domain = d$value
    )

    hex_list <- make_hex_grid_pure(d, radius_meters = radius_meters)
    proxy <- leaflet::leafletProxy("geo_map") %>%
      leaflet::clearGroup("Hexagonal Binning")

    for (h in hex_list) {
      h_color <- pal(pmax(value_range[1], pmin(value_range[2], h$value)))
      h_popup <- paste0(
        "<div style='font-family:\"DM Sans\",sans-serif;padding:3px;'>",
          "<b style='font-size:14px;color:#FFFFFF;'>Hexagonal Grid Cell #", h$id, "</b><br>",
          "<span style='color:#94A3B8;font-size:11px;'>Interpolated Metric:</span> ",
          "<b style='color:#20D6C7;'>", format(round(h$value, 2), big.mark = ","), "</b><br>",
          "<span style='color:#94A3B8;font-size:11px;'>Nearest Node:</span> ", h$nearest_loc, "<br>",
          "<span style='color:#94A3B8;font-size:11px;'>Relative Intensity:</span> ", round(h$scaled * 100), "%",
        "</div>"
      )
      proxy %>% leaflet::addPolygons(
        lng = h$lngs, lat = h$lats,
        fillColor = h_color, fillOpacity = 0.70,
        color = "rgba(255,255,255,0.7)", weight = 1,
        popup = h_popup,
        group = "Hexagonal Binning",
        highlightOptions = highlightOptions(
          weight = 3, color = "#FFFFFF", fillOpacity = 0.9, bringToFront = TRUE
        )
      )
    }
  }, ignoreInit = TRUE)

  # Interactive Ranking Bar Chart
  output$geo_rank <- renderPlotly({
    d <- geo_data()
    d <- d[order(d$value), , drop = FALSE]

    p <- plot_ly(
      d,
      x = ~value,
      y = ~reorder(location, value),
      type = "bar",
      orientation = "h",
      source = "geo_rank",
      marker = list(
        color = ~value,
        colorscale = list(c(0, "#315BEA"), c(0.5, "#20BFAF"), c(1, "#FF6FAE")),
        line = list(color = "#FFFFFF", width = 1)
      ),
      hovertemplate = "<b>%{y}</b><br>Value: %{x:,.2f}<br>Click to focus on map<extra></extra>"
    ) %>%
      event_register("plotly_click")
    chart_theme(p, 360) %>%
      layout(
        xaxis = list(title = input$geo_indicator),
        yaxis = list(title = ""),
        margin = list(l = 80, r = 20, t = 20, b = 50)
      )
  })

  # Inter-Sector Distance Heatmap Matrix
  output$geo_distance <- renderPlotly({
    coords <- rv$coordinates
    n <- nrow(coords)
    dist_mat <- matrix(0, n, n, dimnames = list(coords$location, coords$location))
    for (i in seq_len(n)) for (j in seq_len(n)) {
      lat1 <- coords$latitude[i] * pi / 180
      lat2 <- coords$latitude[j] * pi / 180
      dlat <- lat2 - lat1
      dlon <- (coords$longitude[j] - coords$longitude[i]) * pi / 180
      a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
      dist_mat[i, j] <- 6371 * 2 * atan2(sqrt(a), sqrt(max(0, 1 - a)))
    }
    p <- plot_ly(
      x = coords$location, y = coords$location, z = dist_mat,
      type = "heatmap",
      colorscale = list(c(0, "#10172A"), c(0.35, "#315BEA"), c(0.7, "#20D6C7"), c(1, "#FFB45C")),
      source = "geo_distance",
      hovertemplate = "<b>%{y} &rarr; %{x}</b><br>Distance: %{z:.2f} km<extra></extra>"
    )
    chart_theme(p, 360) %>%
      layout(
        xaxis = list(title = "", tickangle = -35),
        yaxis = list(title = ""),
        margin = list(l = 80, r = 20, t = 20, b = 65)
      )
  })

  # Enhanced Spatial Registry DataTable with Interactive Row Focus
  output$geo_table <- renderDT({
    d <- geo_data()
    validate(need(nrow(d) > 0, "No geospatial records available."))

    d_sorted <- d[order(d$value, decreasing = TRUE), , drop = FALSE]
    d_sorted$rank <- seq_len(nrow(d_sorted))
    val_rng <- range(d_sorted$value, na.rm = TRUE)
    val_span <- diff(val_rng)
    d_sorted$pct <- if (val_span == 0) 50 else round(100 * (d_sorted$value - val_rng[1]) / val_span)
    d_sorted$status <- ifelse(d_sorted$pct >= 75, "High Tier", ifelse(d_sorted$pct <= 25, "Low Tier", "Moderate"))

    display_df <- data.frame(
      "Rank" = paste0("#", d_sorted$rank),
      "Location" = d_sorted$location,
      "Latitude" = round(d_sorted$latitude, 4),
      "Longitude" = round(d_sorted$longitude, 4),
      "Metric Value" = format(round(d_sorted$value, 2), big.mark = ","),
      "City Relative Level" = paste0(d_sorted$pct, "%"),
      "Activity Status" = d_sorted$status,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    datatable(
      display_df,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 8,
        dom = "tp",
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      class = "stripe hover compact"
    )
  })


  # MOBILITY TREND
  output$mobility_trend <- renderPlotly({
    d <- mobility_data()
    validate(need(nrow(d) > 0, "No mobility data found."))
    
    req(input$mobility_indicator)
    
    d <- d[d$indicator == input$mobility_indicator, , drop = FALSE]
    validate(need(nrow(d) > 0, "The selected indicator is unavailable."))
    
    agg <- aggregate(value ~ date + location, d, mean, na.rm = TRUE)
    
    p <- plot_ly(
      agg,
      x = ~date,
      y = ~value,
      color = ~location,
      type = "scatter",
      mode = "lines",
      line = list(width = 2),
      hovertemplate =
        "%{x|%b %Y}<br>%{fullData.name}: %{y:.2f}<extra></extra>"
    )
    
    chart_theme(p, 350) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = input$mobility_indicator)
      )
  })
  
  output$mobility_locations <- renderPlotly({
    d <- mobility_data()
    validate(need(nrow(d) > 0, "No mobility data found."))
    
    agg <- aggregate(value ~ location, d, mean, na.rm = TRUE)
    
    p <- plot_ly(
      agg,
      x = ~value,
      y = ~reorder(location, value),
      type = "bar",
      orientation = "h",
      marker = list(color = blue),
      hovertemplate = "%{y}<br>Mean: %{x:.2f}<extra></extra>"
    )
    
    chart_theme(p, 390) %>%
      layout(
        xaxis = list(title = "Mean indicator value"),
        yaxis = list(title = "")
      )
  })
  
  output$mobility_box <- renderPlotly({
    d <- mobility_data()
    validate(need(nrow(d) > 0, "No mobility data found."))
    
    p <- plot_ly(
      d,
      x = ~location,
      y = ~value,
      color = ~indicator,
      type = "box",
      boxpoints = "outliers"
    )
    
    chart_theme(p, 390) %>%
      layout(
        xaxis = list(title = "", tickangle = -30),
        yaxis = list(title = "Value")
      )
  })
  
  # Environment trend.
  environment_data <- reactive({
    d <- rv$data
    d[d$dimension %in% c("Environment", "Water"), , drop = FALSE]
  })
  
  output$environment_bar <- renderPlotly({
    d <- environment_data()
    validate(need(nrow(d) > 0, "No environment or water data found."))
    
    agg <- aggregate(value ~ indicator, d, mean, na.rm = TRUE)
    agg <- agg[order(agg$value), ]
    
    p <- plot_ly(
      agg,
      x = ~value,
      y = ~reorder(indicator, value),
      type = "bar",
      orientation = "h",
      marker = list(color = teal),
      hovertemplate = "%{y}<br>Mean: %{x:.2f}<extra></extra>"
    )
    
    chart_theme(p, 370) %>%
      layout(
        xaxis = list(title = "Mean indicator value"),
        yaxis = list(title = "")
      )
  })
  
  output$environment_trend <- renderPlotly({
    d <- environment_data()
    validate(need(nrow(d) > 0, "No environment or water data found."))
    
    req(input$environment_indicator)
    
    d <- d[d$indicator == input$environment_indicator, , drop = FALSE]
    validate(need(nrow(d) > 0, "Selected indicator is unavailable."))
    
    agg <- aggregate(value ~ date + location, d, mean, na.rm = TRUE)
    
    p <- plot_ly(
      agg,
      x = ~date,
      y = ~value,
      color = ~location,
      type = "scatter",
      mode = "lines",
      hovertemplate =
        "%{x|%b %Y}<br>%{fullData.name}: %{y:.2f}<extra></extra>"
    )
    
    chart_theme(p, 370) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = input$environment_indicator)
      )
  })
  
  output$environment_box <- renderPlotly({
    d <- environment_data()
    validate(need(nrow(d) > 0, "No environment or water data found."))
    
    p <- plot_ly(
      d,
      x = ~dimension,
      y = ~value,
      color = ~indicator,
      type = "box",
      boxpoints = "outliers"
    )
    
    chart_theme(p, 370) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Value")
      )
  })
  
  # Advanced lab dataset.
  lab_data <- reactive({
    d <- rv$data
    
    if (!is.null(input$lab_dimension) &&
        input$lab_dimension != "All") {
      d <- d[d$dimension == input$lab_dimension, , drop = FALSE]
    }
    
    d
  })
  
  output$correlation_plot <- renderPlotly({
    d <- lab_data()
    validate(need(nrow(d) > 0, "No records available."))
    
    wide <- aggregate(value ~ location + indicator, d, mean, na.rm = TRUE)
    wide <- reshape(
      wide,
      idvar = "location",
      timevar = "indicator",
      direction = "wide"
    )
    
    numeric_cols <- names(wide)[vapply(wide, is.numeric, logical(1))]
    
    validate(
      need(length(numeric_cols) >= 2,
           "Select a dimension with at least two indicators.")
    )
    
    mat <- cor(
      wide[, numeric_cols, drop = FALSE],
      use = "pairwise.complete.obs"
    )
    
    mat[!is.finite(mat)] <- 0
    labs <- sub("^value\\.", "", numeric_cols)
    
    p <- plot_ly(
      x = labs,
      y = labs,
      z = mat,
      type = "heatmap",
      zmin = -1,
      zmax = 1,
      colorscale = list(
        c(0, "#315BEA"),
        c(0.5, "#F4F7FC"),
        c(1, "#20BFAF")
      ),
      text = round(mat, 2),
      hovertemplate =
        "%{y} × %{x}<br>Correlation: %{z:.2f}<extra></extra>"
    )
    
    chart_theme(p, 410) %>%
      layout(
        xaxis = list(title = "", tickangle = -35),
        yaxis = list(title = "")
      )
  })
  
  output$distribution_plot <- renderPlotly({
    d <- rv$data
    req(input$lab_chart_indicator)
    
    d <- d[d$indicator == input$lab_chart_indicator, , drop = FALSE]
    validate(need(nrow(d) > 0, "No records for this indicator."))
    
    p <- plot_ly(
      d,
      x = ~value,
      color = ~location,
      type = "histogram",
      opacity = 0.55,
      nbinsx = 22,
      hovertemplate = "Value: %{x}<br>Count: %{y}<extra></extra>"
    )
    
    chart_theme(p, 410) %>%
      layout(
        barmode = "overlay",
        xaxis = list(title = input$lab_chart_indicator),
        yaxis = list(title = "Frequency")
      )
  })
  
  output$scatter_plot <- renderPlotly({
    d <- lab_data()
    validate(need(nrow(d) > 0, "No records available."))
    
    agg <- aggregate(value ~ location + indicator, d, mean, na.rm = TRUE)
    inds <- sort(unique(agg$indicator))
    
    validate(
      need(length(inds) >= 2,
           "Select a dimension containing at least two indicators.")
    )
    
    xdat <- agg[agg$indicator == inds[1], c("location", "value")]
    ydat <- agg[agg$indicator == inds[2], c("location", "value")]
    
    names(xdat)[2] <- "xvalue"
    names(ydat)[2] <- "yvalue"
    
    both <- merge(xdat, ydat, by = "location")
    
    p <- plot_ly(
      both,
      x = ~xvalue,
      y = ~yvalue,
      type = "scatter",
      mode = "markers+text",
      text = ~location,
      textposition = "top center",
      marker = list(
        size = 13,
        color = purple,
        line = list(color = "white", width = 1)
      ),
      hovertemplate =
        "%{text}<br>X: %{x:.2f}<br>Y: %{y:.2f}<extra></extra>"
    )
    
    chart_theme(p, 420) %>%
      layout(
        xaxis = list(title = inds[1]),
        yaxis = list(title = inds[2])
      )
  })
  
  # Data explorer.
  output$data_summary <- renderUI({
    d <- data_filtered()
    
    tags$div(
      style = "margin:0 0 12px;color:#68758D;font-size:12px;",
      tags$b(paste(format(nrow(d), big.mark = ","), "records shown"))
    )
  })
  
  output$data_table <- renderDT({
    datatable(
      data_filtered(),
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50),
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      class = "stripe hover compact"
    ) %>%
      formatRound("value", digits = 2)
  }, server = TRUE)
  
  # Dashboard-aware assistant. Uses the active dataset and explicitly avoids claiming
  # that synthetic demonstration data is verified or live.
  active_assistant_question <- reactiveVal("")
  observeEvent(input$assistant_ask, {
    active_assistant_question(if (is.null(input$assistant_question)) "" else input$assistant_question)
  }, ignoreInit = TRUE)
  observeEvent(input$voice_ask, {
    active_assistant_question(if (is.null(input$voice_question)) "" else input$voice_question)
  }, ignoreInit = TRUE)

  assistant_response <- eventReactive(active_assistant_question(), {
    q <- tolower(trimws(active_assistant_question()))
    d <- rv$data
    if (!nzchar(q)) return("Please enter a question about a city location or indicator.")
    if (grepl("compare|versus| vs |difference", q)) {
      matched_locations <- locations[vapply(locations, function(x) grepl(tolower(x), q, fixed = TRUE), logical(1))]
      if (length(matched_locations) >= 2) {
        dd <- d[d$location %in% matched_locations[1:2], , drop = FALSE]
        agg <- aggregate(value ~ location + indicator, dd, mean, na.rm = TRUE)
        common <- intersect(agg$indicator[agg$location == matched_locations[1]],
                            agg$indicator[agg$location == matched_locations[2]])
        if (length(common)) {
          lines <- vapply(common, function(ind) {
            a <- agg$value[agg$location == matched_locations[1] & agg$indicator == ind][1]
            b <- agg$value[agg$location == matched_locations[2] & agg$indicator == ind][1]
            paste0(ind, ": ", matched_locations[1], " = ", round(a, 2),
                   "; ", matched_locations[2], " = ", round(b, 2),
                   " (difference ", round(a - b, 2), ").")
          }, character(1))
          return(paste(c("Location comparison from the active dataset:", lines,
                         "Note: these values may be synthetic demonstration data."), collapse = "\\n"))
        }
      }
    }
    indicator_match <- unique(d$indicator[vapply(d$indicator, function(x) grepl(tolower(x), q, fixed = TRUE), logical(1))])
    if (length(indicator_match)) {
      ind <- indicator_match[1]
      agg <- aggregate(value ~ location, d[d$indicator == ind, , drop = FALSE], mean, na.rm = TRUE)
      if (grepl("highest|most|maximum|worst|largest|top", q)) {
        row <- agg[which.max(agg$value), , drop = FALSE]
        return(paste0("Highest average ", ind, " in the active dataset: ", row$location,
                      " (", round(row$value, 2), "). This is a data ranking, not a live city measurement."))
      }
      if (grepl("lowest|least|minimum|best|smallest", q)) {
        row <- agg[which.min(agg$value), , drop = FALSE]
        return(paste0("Lowest average ", ind, " in the active dataset: ", row$location,
                      " (", round(row$value, 2), "). For indicators where lower is not better, this does not necessarily mean the area is healthier."))
      }
      return(paste0("Average ", ind, " by location:\\n",
                    paste(paste0(agg$location, ": ", round(agg$value, 2)), collapse = "\\n"),
                    "\\nThese are averages from the active dataset; check the source column before using them for decisions."))
    }
    if (grepl("priorit|recommend|attention|problem|urgent|risk", q)) {
      risk_indicators <- c("Congestion", "Waste Issues", "Leakage Reports",
                           "Air Quality Index", "Noise Level", "Service Requests")
      dd <- d[d$indicator %in% risk_indicators, , drop = FALSE]
      if (nrow(dd)) {
        agg <- aggregate(value ~ location + indicator, dd, mean, na.rm = TRUE)
        agg <- agg[order(agg$value, decreasing = TRUE), , drop = FALSE]
        top <- head(agg, 5)
        return(paste0("Potential review queue based on high values in selected indicators:\\n",
                      paste(paste0(top$location, " — ", top$indicator, ": ", round(top$value, 2)), collapse = "\\n"),
                      "\\nThis is a simple screening heuristic, not a validated risk model. Verify against real operational data."))
      }
    }
    if (grepl("trend|increase|decrease|over time|forecast", q)) {
      return("The dataset contains dated observations. Open City Overview or Data Explorer and filter by location and indicator to inspect historical patterns. Forecasting is not enabled yet, and the current demonstration data should not be interpreted as a real trend.")
    }
    if (grepl("help|what can|example", q)) {
      return("I can rank an indicator (for example, 'Which area has the highest congestion?'), compare two named locations, list average values for an indicator, or suggest a preliminary review queue. Try using the exact indicator names shown in the dashboard.")
    }
    paste0("I couldn't map that question to a supported data query. Try asking for the highest/lowest value of an indicator, comparing two locations, or identifying areas for review. Current data source: ",
           paste(unique(d$source), collapse = ", "), ".")
  })

  output$assistant_answer <- renderUI({
    answer <- assistant_response()
    div(style = "white-space:pre-line;background:rgba(32,214,199,0.07);border:1px solid rgba(32,214,199,0.25);border-radius:14px;padding:18px;color:#E2E8F0;line-height:1.7;",
        tags$b("CityPulse response"),
        p(answer))
  })

  # Plain-language summaries of visualization techniques used by CityPulse 360.
  viz_summaries <- list(
    "Point map" = "A point map places a marker at each mapped location. In CityPulse 360, marker color and the popup communicate the selected indicator and its relative level. Use it to find where observations are located, not to infer traffic conditions between locations.",
    "Proportional symbols" = "A proportional-symbol map varies symbol size to represent the magnitude of a value. Larger symbols indicate larger mapped values. Compare sizes carefully and check the legend or popup for exact values.",
    "Cluster map" = "A cluster map groups nearby markers when zoomed out. It reduces visual overlap and helps reveal areas with many mapped locations. Zoom in to inspect individual sites; a cluster is not automatically a measured hotspot.",
    "Voronoi catchment map" = "A Voronoi catchment map partitions the map into cells around known points. Each cell represents the area closer to its site than to other sites. These are geometric catchments, not official administrative boundaries.",
    "Cartogram" = "A cartogram changes the visual size or shape of areas to emphasize a data value. It can make differences more visible, but distorted geometry should not be read as true geographic size or distance.",
    "Hexagonal binning" = "Hexagonal binning groups spatial observations into hexagon cells and summarizes values by cell. It helps show broad spatial patterns and reduces the clutter caused by many individual markers. Interpolated or summarized cells are estimates, not direct measurements everywhere inside a cell.",
    "Heat map" = "A heat map uses color intensity to represent the magnitude or concentration of a variable. Stronger colors draw attention to relatively high values. Always inspect the selected indicator and scale; color intensity does not by itself explain why a pattern occurs.",
    "Topographic contours" = "Contour-style mapping draws lines or bands to communicate changes in a value across space. In this dashboard, such surfaces are visual approximations derived from sparse mapped locations, not a surveyed terrain model.",
    "Flow map" = "A flow map draws links between places to emphasize movement or relationships. Direction and line styling can show a connection, but a line should only be interpreted as actual traffic flow when it is backed by flow data.",
    "Spider map" = "A spider map connects a central or selected location to other locations using radial links. It helps inspect spatial relationships, but straight connecting lines are not necessarily roads or travel routes.",
    "Time-space trajectory" = "A time-space trajectory shows how a location or measurement changes across time and space. It is useful for spotting movement or temporal patterns when observations have reliable timestamps. Synthetic observations should not be treated as real historical events.",
    "Attribute proximity map" = "An attribute-proximity view positions records according to similarity in their attributes rather than only geographic distance. Nearby points may have similar indicator profiles, but their positions in attribute space are not real-world coordinates.",
    "Bar chart" = "A bar chart compares values across categories such as locations or urban dimensions. Bar length makes ranking and differences easy to see. Check the axis scale, units, and aggregation method before comparing bars.",
    "Donut chart" = "A donut chart shows how categories contribute to a whole. It works best for a small number of non-negative parts that share the same total. Similar-sized slices are difficult to compare precisely, so use labels or a bar chart when exact ranking matters.",
    "Trend chart" = "A trend chart places time on the horizontal axis and a measurement on the vertical axis. It helps reveal increases, decreases, seasonality, or unusual changes. A trend is only meaningful when dates and source data are reliable.",
    "Indicator heatmap" = "An indicator heatmap uses colored cells to compare many locations and indicators at once. Standardization can make indicators with different units more comparable, but the color scale and direction of good versus bad values must be checked.",
    "Scatter plot" = "A scatter plot places one variable on the horizontal axis and another on the vertical axis. Each point represents a record or location. It can reveal association, clusters, or outliers, but correlation alone does not prove causation.",
    "Interactive data table" = "An interactive data table presents the underlying records in rows and columns. Sorting, filtering, and searching help find specific values and verify what a chart summarizes. Always check missing values, units, dates, and data source."
  )

  output$viz_summary <- renderUI({
    req(input$viz_summarize)
    req(input$viz_technique)
    summary_text <- viz_summaries[[input$viz_technique]]
    if (is.null(summary_text)) summary_text <- "No summary is available for this technique yet."
    div(id = "viz_summary_text",
        style = "white-space:pre-line;background:rgba(32,214,199,0.07);border:1px solid rgba(32,214,199,0.25);border-radius:14px;padding:18px;color:#E2E8F0;line-height:1.75;",
        tags$b(paste(input$viz_technique, "— what it shows")),
        p(summary_text))
  })

  output$voice_status <- renderUI({
    msg <- input$voice_status_text
    if (is.null(msg) || !nzchar(msg)) msg <- "Voice input and spoken summaries depend on browser support and microphone permission."
    div(style = "color:#94A3B8;font-size:12px;margin-top:10px;", msg)
  })

  observeEvent(input$voice_ask, {
    question <- trimws(if (is.null(input$voice_question)) "" else input$voice_question)
    if (!nzchar(question)) {
      showNotification("Speak or type a question first.", type = "warning")
      return()
    }
    updateTextInput(session, "assistant_question", value = question)
    updateTabsetPanel(session, "tabs", selected = "assistant")
  })

  # Smart route: try OSRM's public routing endpoint; fall back to a straight-line
  # estimate if the endpoint or network is unavailable. No live-traffic claim is made.
  route_result <- eventReactive(input$route_analyze, {
    req(input$route_start, input$route_end)
    validate(need(input$route_start != input$route_end, "Choose two different locations."))
    coords <- rv$coordinates
    if (is.null(coords) || !nrow(coords)) {
      validate(need(FALSE, "No mapped locations are available. Upload a CSV with latitude and longitude columns, or allow geocoding to resolve the locations."))
    }
    coords$location <- trimws(as.character(coords$location))
    coords$latitude <- suppressWarnings(as.numeric(coords$latitude))
    coords$longitude <- suppressWarnings(as.numeric(coords$longitude))
    coords <- coords[!is.na(coords$location) & nzchar(coords$location) &
      is.finite(coords$latitude) & is.finite(coords$longitude) &
      coords$latitude >= -90 & coords$latitude <= 90 &
      coords$longitude >= -180 & coords$longitude <= 180, , drop = FALSE]
    a <- coords[coords$location == input$route_start, , drop = FALSE]
    b <- coords[coords$location == input$route_end, , drop = FALSE]
    validate(need(nrow(a) >= 1 && nrow(b) >= 1,
      "The selected locations do not have coordinates. Choose locations from the updated location list, or add valid latitude/longitude columns to the CSV."))
    a <- a[1, , drop = FALSE]
    b <- b[1, , drop = FALSE]

    straight_km <- function(lat1, lon1, lat2, lon2) {
      rad <- pi / 180
      dlat <- (lat2 - lat1) * rad
      dlon <- (lon2 - lon1) * rad
      h <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
      6371 * 2 * atan2(sqrt(h), sqrt(max(0, 1 - h)))
    }
    straight <- straight_km(a$latitude, a$longitude, b$latitude, b$longitude)
    route_coords <- rbind(c(a$longitude, a$latitude), c(b$longitude, b$latitude))
    road_km <- NA_real_
    duration_min <- NA_real_
    source_label <- "Straight-line fallback estimate (not a road route)"
    route_note <- "Routing service unavailable; displayed line is only a geographic approximation."
    alternatives <- data.frame(
      Route = "Straight-line fallback", Distance_km = round(straight, 2),
      Estimated_minutes = NA_real_, Selected = TRUE
    )

    if (requireNamespace("jsonlite", quietly = TRUE)) {
      url <- paste0("https://router.project-osrm.org/route/v1/driving/",
                    a$longitude, ",", a$latitude, ";",
                    b$longitude, ",", b$latitude,
                    "?overview=full&geometries=geojson&steps=false&alternatives=true")
      response <- tryCatch({
        con <- base::url(url, open = "rb", timeout = 5)
        on.exit(close(con), add = TRUE)
        jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = ""))
      }, error = function(e) NULL)
      if (!is.null(response) && identical(response$code, "Ok") &&
          length(response$routes) > 0) {
        route_table <- response$routes
        route_count <- nrow(route_table)
        route_index <- 1L
        if (identical(input$route_priority, "Shortest route")) {
          route_index <- which.min(route_table$distance)
        } else if (identical(input$route_priority, "Fastest route")) {
          route_index <- which.min(route_table$duration)
        }
        route_index <- as.integer(route_index[1])
        road_km <- route_table$distance[route_index] / 1000
        duration_min <- route_table$duration[route_index] / 60
        route_coords <- do.call(rbind, lapply(route_table$geometry$coordinates[[route_index]], function(z) c(z[1], z[2])))
        source_label <- paste0("OSRM road-network estimate (route ", route_index, " of ", route_count, ")")
        route_note <- "Alternative road routes were requested from OSRM. Distance/time estimates are not live traffic and availability depends on the routing service."
        alternatives <- data.frame(
          Route = paste("Alternative", seq_len(route_count)),
          Distance_km = round(route_table$distance / 1000, 2),
          Estimated_minutes = round(route_table$duration / 60, 1),
          Selected = seq_len(route_count) == route_index
        )
      } else {
        alternatives <- data.frame(
          Route = "Straight-line fallback", Distance_km = round(straight, 2),
          Estimated_minutes = NA_real_, Selected = TRUE
        )
      }
    }

    priority <- input$route_priority
    road_quality_note <- ""
    if (identical(priority, "Road-quality aware")) {
      rd <- rv$data[rv$data$indicator == "Road Quality" &
                      rv$data$location %in% c(input$route_start, input$route_end), , drop = FALSE]
      if (nrow(rd)) {
        rq <- aggregate(value ~ location, rd, mean, na.rm = TRUE)
        road_quality_note <- paste0(" Endpoint road-quality averages: ",
                                    paste(paste0(rq$location, " ", round(rq$value, 1), "/100"), collapse = "; "),
                                    ". These are synthetic if the active source says so.")
      } else {
        road_quality_note <- " No Road Quality observations were found for the selected endpoints."
      }
    }
    list(start = input$route_start, end = input$route_end,
         straight_km = straight, road_km = road_km, duration_min = duration_min,
         coords = route_coords, source = source_label, note = paste0(route_note, road_quality_note),
         alternatives = alternatives, priority = priority)
  }, ignoreInit = TRUE)

  output$route_summary <- renderUI({
    r <- route_result()
    if (is.null(r)) {
      return(div(style = "color:#94A3B8;padding:8px;",
                 "Choose an origin and destination, then select Analyze route."))
    }
    dist_text <- if (is.finite(r$road_km)) paste0(round(r$road_km, 2), " km road route")
                 else paste0(round(r$straight_km, 2), " km straight-line distance")
    time_text <- if (is.finite(r$duration_min)) paste0(round(r$duration_min), " min routing estimate")
                 else "Travel time unavailable"
    div(
      h4(paste(r$start, "→", r$end), style = "color:#FFFFFF;"),
      fluidRow(
        column(4, kpi_card("DISTANCE", dist_text, r$priority, teal)),
        column(4, kpi_card("DURATION", time_text, "Not live traffic", blue)),
        column(4, kpi_card("ROUTE SOURCE", r$source, "Verify before travel", purple))
      ),
      p(r$note, style = "color:#CBD5E1;line-height:1.6;")
    )
  })

  output$route_alternatives_table <- DT::renderDT({
    r <- route_result()
    if (is.null(r)) return(DT::datatable(data.frame(Message = "Run route analysis to retrieve available alternatives."),
                                          options = list(dom = "t"), rownames = FALSE))
    DT::datatable(r$alternatives, rownames = FALSE,
      options = list(pageLength = 5, searching = FALSE, dom = "tip")) %>%
      DT::formatRound(c("Distance_km", "Estimated_minutes"), digits = 1)
  })

  output$route_map <- renderLeaflet({
    r <- route_result()
    basemap <- if (is.null(input$route_basemap)) "satellite" else input$route_basemap
    coords <- rv$coordinates
    if (is.null(coords) || !nrow(coords)) {
      coords <- data.frame(location=character(), latitude=numeric(), longitude=numeric())
    }
    coords$latitude <- suppressWarnings(as.numeric(coords$latitude))
    coords$longitude <- suppressWarnings(as.numeric(coords$longitude))
    coords <- coords[is.finite(coords$latitude) & is.finite(coords$longitude) &
                       coords$latitude >= -90 & coords$latitude <= 90 &
                       coords$longitude >= -180 & coords$longitude <= 180, , drop=FALSE]

    # Use explicit tile URL templates rather than provider-name aliases. This avoids
    # "Unknown tile provider" errors on older/different leaflet provider registries.
    # Each layer is added as a real tile layer and selected via the layer control.
    map <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl=TRUE, preferCanvas=TRUE)) %>%
      leaflet::addTiles(
        urlTemplate="https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution="&copy; OpenStreetMap contributors", group="OpenStreetMap Streets",
        options=leaflet::tileOptions(maxZoom=19)) %>%
      leaflet::addTiles(
        urlTemplate="https://clarity.maptiles.arcgis.com/arcgis/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        attribution="Imagery &copy; Esri, Vantor, Earthstar Geographics and GIS User Community", group="Esri Satellite",
        options=leaflet::tileOptions(maxZoom=20, maxNativeZoom=19, detectRetina=TRUE)) %>%
      leaflet::addTiles(
        urlTemplate="https://clarity.maptiles.arcgis.com/arcgis/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        attribution="Imagery &copy; Esri, Vantor, Earthstar Geographics and GIS User Community", group="Esri Hybrid imagery",
        options=leaflet::tileOptions(maxZoom=20, maxNativeZoom=19, detectRetina=TRUE)) %>%
      leaflet::addTiles(
        urlTemplate="https://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}",
        attribution="Labels &copy; Esri", group="Esri Hybrid road labels",
        options=leaflet::tileOptions(opacity=0.95, maxZoom=19)) %>%
      leaflet::addTiles(
        urlTemplate="https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
        attribution="Map data &copy; OpenStreetMap contributors, SRTM | Map style &copy; OpenTopoMap",
        group="Topographic Relief", options=leaflet::tileOptions(maxZoom=17)) %>%
      leaflet::addScaleBar(position="bottomleft")

    # Use the same uploaded/current coordinates and data as the main geospatial map.
    # This means uploaded CSV locations appear here as soon as coordinates are supplied or geocoded.
    if (nrow(coords) > 0) {
      latest_data <- rv$data
      if (nrow(latest_data)) {
        latest_data$date <- suppressWarnings(as.Date(latest_data$date))
        latest_data <- latest_data[order(latest_data$date), , drop=FALSE]
      }
      popup_for <- vapply(coords$location, function(loc) {
        rows <- latest_data[latest_data$location == loc, , drop=FALSE]
        if (nrow(rows)) {
          rows <- rows[order(rows$date), , drop=FALSE]
          lastrow <- rows[nrow(rows), , drop=FALSE]
          paste0("<b>", htmltools::htmlEscape(loc), "</b><br>",
                 "Indicator: ", htmltools::htmlEscape(as.character(lastrow$indicator[1])), "<br>",
                 "Value: ", htmltools::htmlEscape(as.character(round(as.numeric(lastrow$value[1]), 2))),
                 if ("unit" %in% names(lastrow) && !is.na(lastrow$unit[1])) paste0(" ", htmltools::htmlEscape(as.character(lastrow$unit[1]))) else "",
                 "<br>Date: ", htmltools::htmlEscape(as.character(lastrow$date[1])))
        } else paste0("<b>", htmltools::htmlEscape(loc), "</b><br>No uploaded indicator row found.")
      }, character(1))

      if (isTRUE(input$route_halos)) {
        map <- map %>% leaflet::addCircles(data=coords, lng=~longitude, lat=~latitude,
          radius=700, color="#20D6C7", weight=1.2, fill=TRUE, fillColor="#20D6C7",
          fillOpacity=0.07, dashArray="4, 4", group="Catchment halos",
          label=~paste(location, "catchment area"))
      }
      if (isTRUE(input$route_pulsing)) {
        # Radar-style concentric circles; visual overlay is refreshed whenever the map re-renders.
        map <- map %>% leaflet::addCircles(data=coords, lng=~longitude, lat=~latitude,
          radius=420, color="#20D6C7", weight=1.5, fill=TRUE, fillColor="#20D6C7",
          fillOpacity=0.12, dashArray="5, 5", group="Live radar pulse") %>%
          leaflet::addCircles(data=coords, lng=~longitude, lat=~latitude,
          radius=220, color="#62F5E7", weight=1, fill=TRUE, fillColor="#62F5E7",
          fillOpacity=0.10, group="Live radar pulse")
      }
      map <- map %>% leaflet::addCircleMarkers(data=coords, lng=~longitude, lat=~latitude,
        radius=5, color="#FFFFFF", weight=1.5, fillColor="#20D6C7", fillOpacity=0.95,
        label=~location, popup=popup_for, group="City locations")
      if (isTRUE(input$route_show_labels)) {
        for (i in seq_len(nrow(coords))) {
          label_html <- paste0("<div class='geo-pill-label' style='white-space:nowrap;display:flex;gap:6px;align-items:center;padding:5px 9px;border:1px solid rgba(255,255,255,.25);border-radius:14px;background:rgba(15,23,42,.82);color:#F8FAFC;box-shadow:0 3px 14px rgba(0,0,0,.25);backdrop-filter:blur(8px);'>",
            "<span style='width:6px;height:6px;border-radius:50%;background:#20D6C7;box-shadow:0 0 6px #20D6C7;'></span><span>",
            htmltools::htmlEscape(coords$location[i]), "</span></div>")
          map <- map %>% leaflet::addLabelOnlyMarkers(lng=coords$longitude[i], lat=coords$latitude[i],
            label=htmltools::HTML(label_html), labelOptions=leaflet::labelOptions(
              noHide=TRUE, direction="top", offset=c(0,-10), textOnly=FALSE, className="route-frosted-label"),
            group="Frosted labels")
        }
      }
    }

    if (!is.null(r)) {
      route_lng <- suppressWarnings(as.numeric(r$coords[,1]))
      route_lat <- suppressWarnings(as.numeric(r$coords[,2]))
      valid_route <- is.finite(route_lng) & is.finite(route_lat)
      route_lng <- route_lng[valid_route]; route_lat <- route_lat[valid_route]
      if (length(route_lng) >= 2) {
        map <- map %>% leaflet::addPolylines(lng=route_lng, lat=route_lat,
          color="#20D6C7", weight=6, opacity=0.98,
          label=paste(r$start, "to", r$end), group="Selected route")
        mid_i <- max(1L, min(length(route_lng), ceiling(length(route_lng) / 2)))
        line_distance <- if (is.finite(r$road_km)) paste0(format(round(r$road_km, 2), nsmall=2), " km road route") else paste0(format(round(r$straight_km, 2), nsmall=2), " km straight-line")
        map <- map %>% leaflet::addLabelOnlyMarkers(
          lng=route_lng[mid_i], lat=route_lat[mid_i],
          label=htmltools::HTML(paste0("<div style='background:rgba(10,20,35,.94);color:#FFFFFF;border:1px solid #20D6C7;border-radius:8px;padding:5px 9px;font-weight:700;white-space:nowrap;box-shadow:0 0 12px rgba(32,214,199,.45);'>", line_distance, "</div>")),
          labelOptions=leaflet::labelOptions(noHide=TRUE, direction="top", textOnly=FALSE, className="route-distance-label"),
          group="Route distance label")
      }
      start_idx <- match(r$start, coords$location); end_idx <- match(r$end, coords$location)
      valid_idx <- c(start_idx, end_idx); valid_idx <- valid_idx[!is.na(valid_idx)]
      if (length(valid_idx)) {
        map <- map %>% leaflet::addCircleMarkers(
          lng=coords$longitude[valid_idx], lat=coords$latitude[valid_idx],
          color=c("#20D6C7", "#FFB45C")[seq_along(valid_idx)],
          fillColor=c("#20D6C7", "#FFB45C")[seq_along(valid_idx)],
          fillOpacity=1, radius=8, weight=2,
          label=paste(c("Start", "Destination")[seq_along(valid_idx)], coords$location[valid_idx]),
          group="Route endpoints")
      }
    }

    base_groups <- c("OpenStreetMap Streets", "Esri Satellite",
                     "Esri Hybrid imagery", "Topographic Relief")
    overlay_groups <- c("Selected route", "Route distance label", "Route endpoints", "City locations", "Frosted labels",
                        "Live radar pulse", "Catchment halos", "Esri Hybrid road labels")
    # On-map layer selector removed to keep the map canvas clean. Route overlays remain controlled by the UI.
    selected_group <- switch(basemap, osm="OpenStreetMap Streets", satellite="Esri Satellite",
      hybrid="Esri Hybrid imagery", terrain="Topographic Relief", "Esri Satellite")
    for (g in setdiff(base_groups, selected_group)) map <- map %>% leaflet::hideGroup(g)
    if (basemap == "hybrid") map <- map %>% leaflet::showGroup("Esri Hybrid road labels") else map <- map %>% leaflet::hideGroup("Esri Hybrid road labels")
    if (!isTRUE(input$route_show_labels)) map <- map %>% leaflet::hideGroup("Frosted labels")
    if (!isTRUE(input$route_pulsing)) map <- map %>% leaflet::hideGroup("Live radar pulse")
    if (!isTRUE(input$route_halos)) map <- map %>% leaflet::hideGroup("Catchment halos")

    # Fit route and uploaded coordinates together; guard against zero-width/height bounds.
    fit_lng <- if (!is.null(r)) suppressWarnings(as.numeric(r$coords[,1])) else numeric()
    fit_lat <- if (!is.null(r)) suppressWarnings(as.numeric(r$coords[,2])) else numeric()
    fit_lng <- c(fit_lng[is.finite(fit_lng)], coords$longitude)
    fit_lat <- c(fit_lat[is.finite(fit_lat)], coords$latitude)
    if (length(fit_lng) && length(fit_lat)) {
      dx <- diff(range(fit_lng, na.rm=TRUE)); dy <- diff(range(fit_lat, na.rm=TRUE))
      if (!is.finite(dx) || dx < 0.002) dx <- 0.02
      if (!is.finite(dy) || dy < 0.002) dy <- 0.02
      map <- map %>% leaflet::fitBounds(min(fit_lng,na.rm=TRUE)-dx*0.12,
        min(fit_lat,na.rm=TRUE)-dy*0.12, max(fit_lng,na.rm=TRUE)+dx*0.12,
        max(fit_lat,na.rm=TRUE)+dy*0.12)
    } else {
      map <- map %>% leaflet::setView(lng=79.1453, lat=12.9692, zoom=12)
    }
    map
  })

  # AI Simulation Lab: scenario impact and simple model comparison.
  simulation_data <- eventReactive(input$run_city_simulation, {
    req(input$sim_indicator)
    d <- rv$data
    d$date <- as.Date(d$date)
    d <- d[d$indicator == input$sim_indicator & is.finite(d$value) & !is.na(d$date), , drop = FALSE]
    validate(need(nrow(d) > 0, "No data for the selected indicator."))
    latest_by_location <- lapply(split(d, d$location), function(g) {
      g <- g[order(g$date), , drop = FALSE]
      latest <- tail(g$value, 1)
      recent <- if (nrow(g) >= 3) mean(tail(g$value, min(3, nrow(g))), na.rm = TRUE) else latest
      x <- as.numeric(g$date)
      linear <- latest
      if (nrow(g) >= 3 && length(unique(x)) >= 3) {
        fit <- tryCatch(stats::lm(value ~ x, data = data.frame(value = g$value, x = x)), error = function(e) NULL)
        if (!is.null(fit)) {
          pred <- tryCatch(as.numeric(stats::predict(fit, newdata = data.frame(x = max(x) + 30.4375 * input$sim_horizon))),
                           error = function(e) latest)
          if (length(pred) && is.finite(pred[1])) linear <- pred[1]
        }
      }
      moving <- recent
      smooth <- latest
      if (nrow(g) > 1) {
        alpha <- 0.35
        vals <- g$value
        for (j in 2:length(vals)) smooth <- alpha * vals[j] + (1 - alpha) * smooth
      }
      chosen <- switch(input$sim_model, "Moving average" = moving,
                       "Exponential smoothing" = smooth, linear)
      # Illustrative stress factors are only applied to compatible indicator groups.
      dimension <- as.character(tail(g$dimension, 1))
      scenario_factor <- 1 + input$sim_change / 100
      if (dimension == "Mobility") scenario_factor <- scenario_factor * (1 + input$sim_road_closure / 100)
      if (dimension == "Water" || dimension == "Environment") scenario_factor <- scenario_factor * (1 + input$sim_rainfall / 500)
      scenario_factor <- scenario_factor * (1 - input$sim_resource_cut / 500)
      data.frame(location = as.character(g$location[1]), dimension = dimension,
                 baseline = as.numeric(chosen), scenario = as.numeric(chosen * scenario_factor),
                 latest = as.numeric(latest), observations = nrow(g), stringsAsFactors = FALSE)
    })
    res <- do.call(rbind, latest_by_location)
    rownames(res) <- NULL
    res
  }, ignoreInit = TRUE)

  output$simulation_compare_plot <- renderPlotly({
    d <- simulation_data()
    plotdat <- rbind(
      data.frame(location = d$location, value = d$baseline, series = "Baseline"),
      data.frame(location = d$location, value = d$scenario, series = "What-if scenario")
    )
    plotly::plot_ly(plotdat, x = ~location, y = ~value, color = ~series,
                    type = "bar", barmode = "group",
                    hovertemplate = "%{x}<br>%{fullData.name}: %{y:.2f}<extra></extra>") %>%
      plotly::layout(xaxis = list(title = "", tickangle = -35),
                     yaxis = list(title = input$sim_indicator),
                     paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                     font = list(color = "#CBD5E1"),
                     legend = list(orientation = "h", y = 1.12))
  })
  output$simulation_summary <- renderUI({
    d <- simulation_data()
    change <- if (mean(abs(d$baseline), na.rm = TRUE) > 0)
      100 * mean(d$scenario - d$baseline, na.rm = TRUE) / mean(abs(d$baseline), na.rm = TRUE) else 0
    div(
      h4(paste(input$sim_indicator, "scenario")),
      p(paste("Model:", input$sim_model)),
      p(paste("Forecast horizon:", input$sim_horizon, "month(s)")),
      p(paste("Mean scenario shift:", round(change, 1), "%")),
      p(paste("Locations simulated:", nrow(d))),
      p("Scenario effects are simplified assumptions. They are not physical forecasts or causal estimates.")
    )
  })
  output$model_comparison_plot <- renderPlotly({
    d <- rv$data
    d$date <- as.Date(d$date)
    d <- d[d$indicator == input$sim_indicator & is.finite(d$value) & !is.na(d$date), , drop = FALSE]
    validate(need(nrow(d) > 2, "More history is needed for model comparison."))
    models <- c("Last value", "Moving average", "Linear trend")
    errors <- c()
    for (m in models) {
      actual <- predicted <- numeric()
      for (loc in unique(d$location)) {
        g <- d[d$location == loc, , drop = FALSE]
        g <- g[order(g$date), , drop = FALSE]
        if (nrow(g) >= 3) {
          train <- g[-nrow(g), , drop = FALSE]
          test <- tail(g$value, 1)
          pred <- switch(m,
            "Last value" = tail(train$value, 1),
            "Moving average" = mean(tail(train$value, min(3, nrow(train))), na.rm = TRUE),
            "Linear trend" = {
              x <- as.numeric(train$date)
              fit <- tryCatch(stats::lm(value ~ x, data = data.frame(value = train$value, x = x)), error = function(e) NULL)
              if (is.null(fit)) tail(train$value, 1) else as.numeric(predict(fit, newdata = data.frame(x = as.numeric(tail(g$date, 1)))))
            })
          actual <- c(actual, test); predicted <- c(predicted, pred)
        }
      }
      errors <- c(errors, if (length(actual)) sqrt(mean((actual - predicted)^2, na.rm = TRUE)) else NA_real_)
    }
    plotly::plot_ly(x = models, y = errors, type = "bar",
                    marker = list(color = c("#64748B", "#20D6C7", "#8B5CF6")),
                    hovertemplate = "%{x}<br>RMSE: %{y:.2f}<extra></extra>") %>%
      plotly::layout(xaxis = list(title = ""), yaxis = list(title = "Holdout RMSE (lower is better)"),
                     paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                     font = list(color = "#CBD5E1"))
  })

  # 3D digital twin view using real geographic coordinates and data-derived marker heights.
  output$digital_twin_plot <- renderPlotly({
    req(input$twin_indicator)
    d <- rv$data
    d <- d[d$indicator == input$twin_indicator & is.finite(d$value), , drop = FALSE]
    if (!is.null(input$twin_dimension) && input$twin_dimension != "All") {
      d <- d[d$dimension == input$twin_dimension, , drop = FALSE]
    }
    validate(need(nrow(d) > 0, "No values available for this selection."))
    agg <- aggregate(value ~ location, data = d, FUN = function(x) mean(x, na.rm = TRUE))
    agg <- merge(agg, rv$coordinates, by = "location", all.x = TRUE)
    agg <- agg[is.finite(agg$latitude) & is.finite(agg$longitude), , drop = FALSE]
    validate(need(nrow(agg) > 0, "Coordinates are unavailable for the selected locations."))
    rng <- range(agg$value, na.rm = TRUE)
    z <- if (diff(rng) < .Machine$double.eps) rep(10, nrow(agg)) else 5 + 45 * (agg$value - rng[1]) / diff(rng)
    label <- if (isTRUE(input$twin_labels)) agg$location else rep("", nrow(agg))
    plotly::plot_ly(agg, x = ~longitude, y = ~latitude, z = z,
      type = "scatter3d", mode = "markers+text",
      text = label, textposition = "top center",
      marker = list(size = 8, color = agg$value, colorscale = "Viridis",
                    showscale = TRUE, colorbar = list(title = input$twin_indicator)),
      customdata = agg$location,
      hovertemplate = "%{customdata}<br>Longitude: %{x:.4f}<br>Latitude: %{y:.4f}<br>Indicator: %{marker.color:.2f}<extra></extra>") %>%
      plotly::layout(scene = list(
        xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"),
        zaxis = list(title = "Relative intensity (not building height)"),
        bgcolor = "rgba(0,0,0,0)"),
        paper_bgcolor = "rgba(0,0,0,0)", font = list(color = "#CBD5E1"))
  })
  output$digital_twin_summary <- renderUI({
    d <- rv$data
    d <- d[d$indicator == input$twin_indicator, , drop = FALSE]
    div(h4("Prototype digital twin"),
        p(paste("Indicator:", input$twin_indicator)),
        p(paste("Locations represented:", length(unique(d$location)))),
        p("3D elevation encodes relative indicator intensity, not measured building height."),
        p("The coordinates are approximate demo coordinates and should be verified before operational use."))
  })

  # Resource allocation: transparent greedy / proportional heuristic.
  allocation_plan <- eventReactive(input$run_allocation, {
    req(input$allocation_indicator)
    d <- rv$data
    d <- d[d$indicator == input$allocation_indicator & is.finite(d$value), , drop = FALSE]
    validate(need(nrow(d) > 0, "No data for selected indicator."))
    agg <- aggregate(value ~ location, data = d, FUN = function(x) mean(x, na.rm = TRUE))
    agg <- merge(agg, rv$coordinates, by = "location", all.x = TRUE)
    rng <- range(agg$value, na.rm = TRUE)
    score <- if (diff(rng) < .Machine$double.eps) rep(50, nrow(agg)) else 100 * (agg$value - rng[1]) / diff(rng)
    if (input$allocation_indicator %in% c("Water Quality", "Green Cover", "Waste Segregation", "Road Quality", "Healthcare Access", "Digital Service Access")) score <- 100 - score
    agg$need_score <- round(score, 1)
    units <- as.integer(input$allocation_units)
    agg$units <- 0L
    if (input$allocation_strategy == "Need proportional") {
      weights <- pmax(0, agg$need_score)
      if (sum(weights) == 0) weights <- rep(1, nrow(agg))
      raw <- units * weights / sum(weights)
      agg$units <- floor(raw)
      remainder <- units - sum(agg$units)
      if (remainder > 0) agg$units[order(raw - floor(raw), decreasing = TRUE)[seq_len(remainder)]] <-
        agg$units[order(raw - floor(raw), decreasing = TRUE)[seq_len(remainder)]] + 1L
    } else if (input$allocation_strategy == "Balanced coverage") {
      agg$units <- rep(units %/% nrow(agg), nrow(agg))
      remainder <- units - sum(agg$units)
      if (remainder > 0) agg$units[seq_len(remainder)] <- agg$units[seq_len(remainder)] + 1L
    } else {
      agg <- agg[order(agg$need_score, decreasing = TRUE), , drop = FALSE]
      agg$units <- 0L
      agg$units[seq_len(min(units, nrow(agg)))] <- 1L
      if (units > nrow(agg)) agg$units[1] <- agg$units[1] + units - nrow(agg)
    }
    agg
  }, ignoreInit = TRUE)
  output$allocation_plot <- renderPlotly({
    d <- allocation_plan()
    plotly::plot_ly(d, x = ~reorder(location, units), y = ~units, type = "bar",
                    marker = list(color = "#20D6C7"),
                    hovertemplate = "%{x}<br>Assigned units: %{y}<extra></extra>") %>%
      plotly::layout(xaxis = list(title = "", tickangle = -30),
                     yaxis = list(title = "Teams / vehicles"),
                     paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                     font = list(color = "#CBD5E1"))
  })
  output$allocation_table <- DT::renderDT({
    d <- allocation_plan()
    DT::datatable(d[, c("location", "need_score", "units"), drop = FALSE],
      colnames = c("Location", "Need score", "Assigned units"), rownames = FALSE,
      options = list(pageLength = 6, searching = FALSE, dom = "tip"))
  })
  output$allocation_summary <- renderUI({
    d <- allocation_plan()
    div(p(paste("Total assigned:", sum(d$units))),
        p(paste("Locations covered:", sum(d$units > 0), "of", nrow(d))),
        p(paste("Strategy:", input$allocation_strategy)))
  })

  # Knowledge graph: data-derived bipartite relationships laid out deterministically.
  output$knowledge_graph_plot <- renderPlotly({
    req(input$graph_dimension)
    d <- rv$data
    if (!is.null(input$graph_dimension) && input$graph_dimension != "All") d <- d[d$dimension == input$graph_dimension, , drop = FALSE]
    if (!is.null(input$graph_location) && input$graph_location != "All") d <- d[d$location == input$graph_location, , drop = FALSE]
    validate(need(nrow(d) > 0, "No relationships match the current filters."))
    locs <- unique(as.character(d$location))
    inds <- unique(as.character(d$indicator))
    nodes <- data.frame(name = c(locs, if (isTRUE(input$graph_show_indicators)) inds else unique(as.character(d$dimension))),
                        type = c(rep("Location", length(locs)),
                                 rep(if (isTRUE(input$graph_show_indicators)) "Indicator" else "Dimension",
                                     length(if (isTRUE(input$graph_show_indicators)) inds else unique(as.character(d$dimension))))))
    nodes <- nodes[!duplicated(nodes$name), , drop = FALSE]
    n <- nrow(nodes)
    theta <- seq(0, 2*pi, length.out = n + 1)[seq_len(n)]
    nodes$x <- cos(theta); nodes$y <- sin(theta)
    edge_list <- list()
    if (isTRUE(input$graph_show_indicators)) {
      rel <- unique(d[, c("location", "indicator")])
      for (i in seq_len(nrow(rel))) {
        a <- match(rel$location[i], nodes$name); b <- match(rel$indicator[i], nodes$name)
        if (!is.na(a) && !is.na(b)) edge_list[[length(edge_list)+1]] <- c(a,b)
      }
    } else {
      rel <- unique(d[, c("location", "dimension")])
      for (i in seq_len(nrow(rel))) {
        a <- match(rel$location[i], nodes$name); b <- match(rel$dimension[i], nodes$name)
        if (!is.na(a) && !is.na(b)) edge_list[[length(edge_list)+1]] <- c(a,b)
      }
    }
    p <- plotly::plot_ly()
    if (length(edge_list)) {
      for (e in edge_list) p <- p %>% plotly::add_trace(x = c(nodes$x[e[1]], nodes$x[e[2]], NA),
        y = c(nodes$y[e[1]], nodes$y[e[2]], NA), type = "scatter", mode = "lines",
        line = list(color = "rgba(148,163,184,0.35)", width = 1), hoverinfo = "skip", showlegend = FALSE)
    }
    p %>% plotly::add_trace(data = nodes, x = ~x, y = ~y, type = "scatter", mode = "markers+text",
      text = ~name, textposition = "top center", color = ~type,
      marker = list(size = 15), hovertemplate = "%{text}<br>Node type: %{fullData.name}<extra></extra>") %>%
      plotly::layout(showlegend = TRUE, xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        margin = list(l=20,r=20,t=20,b=20), paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)", font = list(color="#CBD5E1"))
  })

  # Existing CSV export.
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("citypulse360_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(
        data_filtered(),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
}

# 8. START THE SHINY APPLICATION

shinyApp(ui = ui, server = server)
