# ═══════════════════════════════════════════════════════════════════════════════
#  Kaudulla NP — Combined 5-Tab Dashboard
#
#  Built from five source dashboards, combined into one tabsetPanel app.
#  Each source app is wrapped in a Shiny MODULE (its own namespace) so nothing
#  collides. The content of each app is preserved as-is; only input/output IDs
#  are namespaced via ns()/session, and shinyApp()/ui/server wrappers were
#  converted into module UI + server functions.
#
#  TABS (in requested order):
#    1. Dona & Recollared         (from Dona___recollared.R)
#    2. Density & Climate         (from density_Climate_dash.R)
#    3. Day / Night               (from day_night_hot_cool_final.R)
#    4. Vegetation Tracking       (from vegetation_tracking_dashboard_v2__3_.R)
#    5. Tracking                  (from tracking1.R)
#
#  Data files needed in the working directory:
#    kaudulla_elephants_clean_not_imputed.csv
#    kaudulla_elephants_clean.csv
#    POWER_Point_Hourly_kawudulla_new.csv
# ═══════════════════════════════════════════════════════════════════════════════

# ── Libraries (union of all five dashboards) ─────────────────────────────────
needed <- c("shiny","leaflet","plotly","dplyr","lubridate","readr","tidyr",
            "crosstalk","sf","geosphere","viridisLite","leaflet.extras2")
miss   <- setdiff(needed, rownames(installed.packages()))
if (length(miss)) install.packages(miss, repos = "https://cran.r-project.org")

library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(crosstalk)
library(sf)
library(geosphere)
library(viridisLite)
library(leaflet.extras2)

# Prevent sf's S4 'span' generic from masking shiny::span (htmltools tag builder)
span <- shiny::span


# ═══════════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════════
#  TAB 1 — DONA & RECOLLARED   (source: Dona___recollared.R)
# ═══════════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════════

# ── Constants ─────────────────────────────────────────────────────────────────
DR_KAUD_LAT  <- 8.168
DR_KAUD_LON  <- 80.913

# ── CSS ───────────────────────────────────────────────────────────────────────
dr_css <- "
/* Base */
.dr-scope *, .dr-scope *::before, .dr-scope *::after { box-sizing: border-box; margin: 0; padding: 0; }
.dr-scope {
  background: #f1f5f9;
  color: #1e293b;
  font-family: system-ui, -apple-system, sans-serif;
  font-size: 13px;
}

/* Outer flex */
.dr-scope #outer { display: flex; min-height: 100vh; }

/* Sidebar */
.dr-scope #sidebar {
  width: 230px;
  flex-shrink: 0;
  background: #ffffff;
  border-right: 1px solid #e2e8f0;
  padding: 14px;
  position: sticky;
  top: 0;
  height: 100vh;
  overflow-y: auto;
  box-shadow: 1px 0 4px #0000000a;
}
.dr-scope #sidebar .brand { font-size: 14px; font-weight: 700; color: #166534; margin-bottom: 4px; }
.dr-scope #sidebar .brand-sub { font-size: 10px; color: #64748b; margin-bottom: 16px; line-height: 1.5; }

/* Section headings in sidebar */
.dr-scope .sh {
  font-size: 10px; font-weight: 700; text-transform: uppercase;
  letter-spacing: .08em; color: #64748b;
  margin: 16px 0 6px; padding-top: 14px;
  border-top: 1px solid #f1f5f9;
}
.dr-scope .sh:first-of-type { border-top: none; padding-top: 0; }

/* Legend blocks in sidebar */
.dr-scope .leg-block {
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  border-radius: 6px;
  padding: 9px 10px;
  margin-top: 6px;
  font-size: 11px;
  line-height: 2;
}
.dr-scope .leg-block strong {
  display: block; font-size: 10px; text-transform: uppercase;
  letter-spacing: .06em; color: #64748b; margin-bottom: 4px;
}
.dr-scope .dot {
  display: inline-block;
  width: 11px; height: 11px;
  border-radius: 50%;
  margin-right: 5px;
  vertical-align: middle;
}

/* Main scrollable area */
.dr-scope #main { flex: 1; overflow-y: auto; padding: 10px; }

/* Panel cards */
.dr-scope .panel-card {
  background: #ffffff;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  overflow: hidden;
  margin-bottom: 12px;
  box-shadow: 0 1px 3px #0000000a;
}
.dr-scope .panel-hdr {
  background: #f8fafc;
  border-bottom: 1px solid #e2e8f0;
  padding: 8px 14px;
}
.dr-scope .panel-hdr h3 {
  font-size: 11px; font-weight: 700; text-transform: uppercase;
  letter-spacing: .07em; color: #166534; margin: 0 0 2px;
}
.dr-scope .panel-hdr .desc { font-size: 10px; color: #64748b; }
"

# ── Module UI ───────────────────────────────────────────────────────────────
dona_recollared_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(dr_css))),

    div(class = "dr-scope",
      div(id = "outer",

          # ── SIDEBAR ──
          div(id = "sidebar",
              div(class = "brand",   "\U0001F418 Kaudulla NP"),
              div(class = "brand-sub", "Individual Elephant Tracking"),

              # Custom Sidebar Legend
              div(class = "sh", "Legend: Elephant Identity"),
              div(class = "leg-block",
                  tags$strong("GPS Points Mapping"),
                  div(span(class="dot", style="background:#E31A1C"), "Recollared Female"),
                  div(span(class="dot", style="background:#984EA3"), "Dona"),
                  div(span(class="dot", style="background:#4DAF4A"), "Other Elephants")
              )
          ),

          # ── MAIN PANEL ──
          div(id = "main",
              div(class = "panel-card",
                  div(class = "panel-hdr",
                      tags$h3("Map 1 — GPS Points by Individual"),
                      div(class = "desc",
                          "Map displaying standard OSM terrain (green parks, blue water, yellow roads).")
                  ),
                  # The Map
                  leafletOutput(ns("elephantMap"), height = "800px")
              )
          )
      )
    )
  )
}

# ── Module Server ─────────────────────────────────────────────────────────────
dona_recollared_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # 1. Load and process the dataset
    elephant_data <- reactive({
      data <- read.csv("kaudulla_elephants_clean.csv", stringsAsFactors = FALSE)

      data <- data %>%
        filter(!is.na(lat) & !is.na(lon)) %>%
        mutate(
          display_category = case_when(
            tolower(name) == "recollared female" ~ "Recollared Female",
            tolower(name) == "dona"              ~ "Dona",
            TRUE                                 ~ "Other Elephants"
          ),
          point_color = case_when(
            display_category == "Recollared Female" ~ "#E31A1C",
            display_category == "Dona"              ~ "#984EA3",
            TRUE                                    ~ "#4DAF4A"
          )
        )
      return(data)
    })

    # 2. Render Leaflet Map
    output$elephantMap <- renderLeaflet({
      req(elephant_data())
      df <- elephant_data()

      leaflet(data = df, options = leafletOptions(zoomControl = TRUE)) %>%

        # --- UPDATED TO MATCH YOUR IMAGE ---
        # 1. Standard OpenStreetMap (This exactly matches the image you provided)
        addTiles(group = "OpenStreetMap (Default)") %>%
        # 2. Satellite Imagery (Optional layer for switching)
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        # 3. Faded Light map (Optional layer)
        addProviderTiles("CartoDB.Positron",  group = "Faded Light") %>%

        # Update the Layers Control menu
        addLayersControl(
          baseGroups = c("OpenStreetMap (Default)", "Satellite", "Faded Light"),
          position   = "bottomright",
          options    = layersControlOptions(collapsed = FALSE)
        ) %>%
        # -----------------------------------

      # Set starting view
      setView(lng = DR_KAUD_LON, lat = DR_KAUD_LAT, zoom = 12) %>%

        # Add the styled dots
        addCircleMarkers(
          lat         = ~lat,
          lng         = ~lon,
          radius      = 4,
          fillColor   = ~point_color,
          fillOpacity = 0.85,
          color       = "white", # Thin white border so dots stand out against the green/yellow map
          weight      = 0.5,
          popup       = ~paste0(
            "<b>", name, "</b><br>",
            date, " @ ", time, "<br>",
            "<span style='color:#64748b'>Category:</span>  ",
            "<b>", display_category, "</b>"
          )
        )
    })
  })
}


# ═══════════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════════
#  TAB 2 — DENSITY & CLIMATE   (source: density_Climate_dash.R)
# ═══════════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════════

# ── DATA (global, loaded once) ────────────────────────────────────────────────
message("Loading GPS data (density/climate)...")
dc_gps <- read_csv(
  "kaudulla_elephants_clean.csv",
  show_col_types = FALSE
) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(
    dt    = as.POSIXct(datetime, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    year  = year(dt),
    lat_g = round(lat, 3),
    lon_g = round(lon, 3)
  ) %>%
  filter(!is.na(year))

DC_FOCUS_YEARS <- c(2024L, 2025L, 2026L)
dc_gps_f <- dc_gps %>% filter(year %in% DC_FOCUS_YEARS)

DC_ELE_NAMES <- sort(unique(dc_gps_f$name))

# Tag every fix with the calendar year-month it belongs to.
dc_gps_ym <- dc_gps_f %>%
  mutate(
    year_month = as.Date(lubridate::floor_date(dt, "month")),
    ym_label   = format(year_month, "%b %Y")
  )

message("Loading climate (POWER) data for temperature/rainfall-class map...")
dc_power_lines_dpt <- readLines("POWER_Point_Hourly_kawudulla_new.csv")
dc_header_end_dpt   <- which(grepl("^YEAR,MO", dc_power_lines_dpt))[1] - 1
dc_weather_raw_dpt  <- read_csv("POWER_Point_Hourly_kawudulla_new.csv",
                                skip = dc_header_end_dpt, show_col_types = FALSE)

dc_weather_dpt <- dc_weather_raw_dpt %>%
  filter(T2M != -999) %>%
  mutate(period = ifelse(HR >= 6 & HR < 18, "Day", "Night"))

# ---- Categorical TEMPERATURE scale (no numeric legend) ------------------
dc_temp_breaks <- c(-Inf, 24, 27, 30, 33, Inf)
dc_temp_labels <- c("Cool (<24\u00B0C)", "Mild (24-27\u00B0C)", "Warm (27-30\u00B0C)",
                    "Hot (30-33\u00B0C)", "Very Hot (>33\u00B0C)")
dc_temp_colors <- setNames(
  c("#4A6FA5", "#7FA65C", "#E0A83E", "#D9723A", "#B33A3A"),
  dc_temp_labels
)

# ---- Categorical RAINFALL scale (no numeric legend) ----------------------
# PRECTOTCORR is NASA POWER's corrected precipitation rate, in mm/day.
dc_rain_breaks <- c(-Inf, 2.5, 25, 50, 100, 150, Inf)

dc_rain_labels <- c(
  "Dry / Very Light (<2.5 mm/day)",
  "Light (2.5 - 25 mm/day)",
  "Moderate (25 - 50 mm/day)",
  "Fairly Heavy (50 - 100 mm/day)",
  "Heavy (100 - 150 mm/day)",
  "Very Heavy (>150 mm/day)"
)

dc_rain_colors <- setNames(
  c("#E8D9A0", "#C2E699", "#78C679", "#31A354", "#006837", "#081D58"),
  dc_rain_labels
)
# Average TEMPERATURE per specific year-month x day/night period.
dc_weather_ym_temp <- dc_weather_dpt %>%
  group_by(YEAR, MO, period) %>%
  summarise(avg_value = mean(T2M, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    year_month = as.Date(sprintf("%d-%02d-01", YEAR, MO)),
    category   = cut(avg_value, breaks = dc_temp_breaks, labels = dc_temp_labels)
  ) %>%
  select(year_month, period, avg_value, category)

# Average RAINFALL per specific year-month x day/night period.
dc_weather_ym_rain <- dc_weather_dpt %>%
  group_by(YEAR, MO, period) %>%
  summarise(avg_value = mean(PRECTOTCORR, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    year_month = as.Date(sprintf("%d-%02d-01", YEAR, MO)),
    category   = cut(avg_value, breaks = dc_rain_breaks, labels = dc_rain_labels)
  ) %>%
  select(year_month, period, avg_value, category)

# Lookup used by the server to switch between the two metrics.
dc_metric_info <- list(
  temp = list(table = dc_weather_ym_temp, colors = dc_temp_colors, labels = dc_temp_labels,
              name = "Temperature", unit = "\u00B0C", fmt = 1),
  rainfall = list(table = dc_weather_ym_rain, colors = dc_rain_colors, labels = dc_rain_labels,
                  name = "Rainfall", unit = "mm/day", fmt = 1)
)

# Pick a "suitable" number of hexagon columns from the sample size.
dc_suitable_bins <- function(n) {
  if (n <= 1) return(5)
  max(6, min(40, round(sqrt(n))))
}

dc_filter_by_elephants_dpt <- function(df, selected) {
  if (is.null(selected) || length(selected) == 0 || "All Elephants" %in% selected) {
    df
  } else {
    df %>% filter(name %in% selected)
  }
}

# UTM zone 44N suits Sri Lanka - used only to build metrically-correct hexagons.
DC_LOCAL_CRS <- 32644

dc_add_basetiles_dpt <- function(map) {
  map %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addProviderTiles(providers$OpenStreetMap,     group = "Street") %>%
    addProviderTiles(providers$CartoDB.Positron,  group = "Light")
}

# ── CSS ────────────────────────────────────────────────────────────────────
dc_app_css <- "
.dc-scope *,.dc-scope *::before,.dc-scope *::after{box-sizing:border-box;margin:0;padding:0}
.dc-scope{background:#f1f5f9;color:#1e293b;font-family:system-ui,-apple-system,sans-serif;
          font-size:13px;overflow-x:hidden}
.dc-scope #page-hdr{background:#ffffff;border-bottom:2px solid #e2e8f0;padding:10px 16px;
          display:flex;align-items:center;gap:10px;box-shadow:0 1px 4px #0000000d}
.dc-scope #page-hdr h1{font-size:14px;font-weight:700;color:#166534;letter-spacing:.04em}
.dc-scope #page-hdr .sub{font-size:11px;color:#64748b}
.dc-scope .dash-panel{background:#ffffff;border:1px solid #e2e8f0;border-radius:8px;margin:6px;
            overflow:hidden;display:flex;flex-direction:column;box-shadow:0 1px 3px #0000000a}
.dc-scope .panel-title{font-size:11px;font-weight:700;text-transform:uppercase;
             letter-spacing:.07em;color:#166534;white-space:nowrap;padding:6px 2px}
.dc-scope .form-control,.dc-scope select{background:#ffffff!important;border:1px solid #cbd5e1!important;
                     color:#1e293b!important;font-size:11px!important;border-radius:4px;
                     padding:3px 7px;min-height:26px;height:auto}
.dc-scope label{color:#64748b!important;font-size:11px;margin-bottom:4px;display:inline-block;}
.dc-scope .shiny-input-container{margin-bottom:14px!important;margin-top:4px!important;}
.dc-scope .radio-inline{margin-right:10px!important}
.dc-scope .radio-inline label{color:#475569!important;font-size:11px!important}
/* Force the radio buttons to the left and align them */
.dc-scope .climate-radio-container .shiny-options-group {
    display: flex;
    flex-direction: row;
    gap: 15px; 
    margin-left: 0 !important;
    padding-left: 0 !important;
}

.dc-scope .climate-radio-container .radio {
    margin-top: 5px !important;
    margin-bottom: 10px !important;
    padding-left: 0 !important;
}

/* Align inputs and labels correctly */
.dc-scope .climate-radio-container label {
    display: flex !important;
    align-items: center;
    font-size: 11px !important;
    color: #1e293b !important;
}

.dc-scope .climate-radio-container input[type=\"radio\"] {
    margin-right: 5px !important;
    margin-top: 0 !important;
}
"

# ── Module UI ────────────────────────────────────────────────────────────────
density_climate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(dc_app_css))),

    div(class = "dc-scope",
      div(id = "page-hdr",
          tags$h1("\U0001F4CD Movement Density, Path & Climate"),
          span(class = "sub",
               "Hexbin fix density  .  categorical monthly temperature or rainfall with animated, numbered travel path")
      ),

      fluidRow(
        column(width = 3,
               div(class = "dash-panel", style = "padding: 10px;",
                   selectizeInput(
                     ns("dpt_elephants"), "Select elephant(s):",
                     choices  = c("All Elephants", DC_ELE_NAMES),
                     selected = "All Elephants",
                     multiple = TRUE,
                     options  = list(plugins = list("remove_button"))
                   ),
                   # Inside density_climate_ui
                   div(class = "climate-radio-container",
                       radioButtons(
                         ns("dpt_metric"), "Climate variable (map 2 + chart):",
                         choices  = c("Temperature" = "temp", "Rainfall" = "rainfall"),
                         selected = "temp", 
                         inline   = FALSE # Set to FALSE so we can control alignment with CSS
                       )
                   ),
                   radioButtons(
                     ns("dpt_period"), "Time of day:",
                     choices  = c("Day (06:00-18:00)" = "Day", "Night (18:00-06:00)" = "Night"),
                     selected = "Day"
                   ),
                   hr(),
                   uiOutput(ns("dpt_summary")),
                   hr(),
                   helpText("Use the layer switcher (top-right of each map) for Satellite / Street / Light. Hover markers for details; scroll to zoom, drag to pan.")
               )
        ),
        column(width = 9,
               fluidRow(
                 column(12, uiOutput(ns("dpt_connect_slider")))
               ),
               fluidRow(
                 # Add padding to the right side of the left map
                 column(6, style = "padding-right: 8px;",
                        div(class = "panel-title", "Hexbin density + monthly centroids + numbered travel path"),
                        leafletOutput(ns("dpt_density_map"), height = "640px")),
                 
                 # Add padding to the left side of the right map
                 column(6, style = "padding-left: 8px;",
                        uiOutput(ns("dpt_map2_title")),
                        leafletOutput(ns("dpt_temp_map"), height = "640px"))
               )
        )
      )
    )
  )
}

# ── Module Server ──────────────────────────────────────────────────────────────
density_climate_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Convenience accessor for the currently-selected metric's table/colors/etc.
    current_metric <- reactive({
      dc_metric_info[[input$dpt_metric]]
    })

    # ALL fixes (every year-month) for the selected elephant(s)
    dpt_elephant_data <- reactive({
      dc_gps_ym %>% dc_filter_by_elephants_dpt(input$dpt_elephants)
    })

    # One centroid PER YEAR-MONTH for the selected elephant(s)
    dpt_centroid_by_ym <- reactive({
      dpt_elephant_data() %>%
        group_by(year_month, ym_label) %>%
        summarise(lon = mean(lon), lat = mean(lat), n = n(), .groups = "drop") %>%
        arrange(year_month)
    })

    # Centroids + their temperature/rainfall class/colour + displacement km.
    dpt_path_data <- reactive({
      cm  <- dpt_centroid_by_ym()
      per <- input$dpt_period
      mi  <- current_metric()
      if (nrow(cm) == 0) return(cm)

      cm2 <- cm %>%
        left_join(mi$table %>% filter(period == per), by = "year_month") %>%
        mutate(
          category  = factor(as.character(category), levels = mi$labels),
          fillColor = unname(mi$colors[as.character(category)])
        )
      cm2$fillColor[is.na(cm2$fillColor)] <- "#999999"

      cm2$disp_km <- NA_real_
      if (nrow(cm2) > 1) {
        for (i in 2:nrow(cm2)) {
          cm2$disp_km[i] <- geosphere::distHaversine(
            c(cm2$lon[i - 1], cm2$lat[i - 1]),
            c(cm2$lon[i],     cm2$lat[i])
          ) / 1000
        }
      }
      cm2
    })

    # ---- Map 1: hex density + one centroid per year-month ----
    output$dpt_density_map <- renderLeaflet({
      df <- dpt_elephant_data()
      validate(need(nrow(df) > 0, "No GPS fixes for this elephant selection."))

      pts_wgs <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
      pts_utm <- st_transform(pts_wgs, DC_LOCAL_CRS)

      n_bins <- dc_suitable_bins(nrow(df))
      bb <- st_bbox(pts_utm)
      width_m <- as.numeric(bb["xmax"] - bb["xmin"])
      if (!is.finite(width_m) || width_m <= 0) width_m <- 500
      cellsize <- max(width_m / n_bins, 50)

      grid_utm <- st_make_grid(pts_utm, cellsize = cellsize, square = FALSE)
      hex_utm  <- st_sf(hex_id = seq_along(grid_utm), geometry = grid_utm)

      joined <- st_join(pts_utm, hex_utm)
      counts <- joined %>% st_drop_geometry() %>% count(hex_id, name = "n")

      hex_utm <- hex_utm %>% inner_join(counts, by = "hex_id")
      validate(need(nrow(hex_utm) > 0, "Not enough spatial spread to build a hex grid."))
      hex_wgs <- st_transform(hex_utm, 4326)

      pal <- colorNumeric(palette = viridisLite::magma(256), domain = hex_wgs$n)
      cm  <- dpt_centroid_by_ym()

      leaflet() %>%
        dc_add_basetiles_dpt() %>%
        addPolygons(data = hex_wgs, fillColor = ~pal(n), fillOpacity = 0.75,
                    color = "white", weight = 0.6,
                    label = ~paste0("Fixes: ", n),
                    group = "Hex density") %>%
        addLegend(pal = pal, values = hex_wgs$n, title = "Fixes / hex", position = "bottomleft") %>%
        addCircleMarkers(data = cm, lng = ~lon, lat = ~lat, radius = 6,
                         color = "black", weight = 1.5, fillColor = "#00E5FF", fillOpacity = 1,
                         label = ~paste0(ym_label, " centroid (n=", n, ")"),
                         group = "Monthly centroids") %>%
        addLayersControl(baseGroups = c("Satellite", "Street", "Light"),
                         overlayGroups = c("Hex density", "Monthly centroids", "Travel path"),
                         options = layersControlOptions(collapsed = FALSE),
                         position = "topright")
    })

    # ---- Play-button slider ----
    output$dpt_connect_slider <- renderUI({
      cm <- dpt_centroid_by_ym()
      n_steps <- max(nrow(cm) - 1, 0)
      mx <- max(n_steps, 1)

      step_labels <- cm$ym_label
      if (length(step_labels) == 0) step_labels <- "No data"
      while (length(step_labels) < mx + 1) {
        step_labels <- c(step_labels, step_labels[length(step_labels)])
      }
      labels_js <- paste0(
        "[", paste0('"', gsub('"', '\\\\"', step_labels), '"', collapse = ","), "]"
      )

      slider_id <- ns("dpt_connect_step")

      tagList(
        sliderInput(
          ns("dpt_connect_step"),
          "Play path (connects centroids in travel order):",
          min = 0, max = mx, value = n_steps, step = 1,
          animate = animationOptions(interval = 900, loop = FALSE),
          width = "100%"
        ),
        tags$script(HTML(sprintf('
          (function() {
            var labels = %s;
            var $el = $("#%s");
            function applyPrettify() {
              var slider = $el.data("ionRangeSlider");
              if (slider) {
                slider.update({
                  prettify: function(num) {
                    return labels[num] !== undefined ? labels[num] : num;
                  }
                });
              } else {
                setTimeout(applyPrettify, 50);
              }
            }
            applyPrettify();
          })();
        ', labels_js, slider_id)))
      )
    })

    # ---- Dynamic titles/legends that follow the Temperature/Rainfall toggle ----
    output$dpt_map2_title <- renderUI({
      mi <- current_metric()
      div(class = "panel-title", paste0("Monthly ", tolower(mi$name), " (categorical) + numbered travel path"))
    })

    # ---- Map 2: every year-month's centroid, categorical temperature OR rainfall fill ----
    output$dpt_temp_map <- renderLeaflet({
      per <- input$dpt_period
      mi  <- current_metric()
      cm2 <- dpt_path_data()
      validate(need(nrow(cm2) > 0, "No data available."))

      leaflet() %>%
        dc_add_basetiles_dpt() %>%
        addCircleMarkers(data = cm2, lng = ~lon, lat = ~lat, radius = 9,
                         color = "black", weight = 1, fillColor = ~fillColor, fillOpacity = 0.9,
                         label = ~paste0(ym_label, " (", per, "): ",
                                         ifelse(is.na(avg_value), "no data",
                                                paste0(round(avg_value, mi$fmt), " ", mi$unit, " - ", category))),
                         group = "Monthly climate") %>%
        addLegend(colors = mi$colors, labels = names(mi$colors),
                  title = paste0(mi$name, " class (", per, ")"), position = "bottomleft") %>%
        addLayersControl(baseGroups = c("Satellite", "Street", "Light"),
                         overlayGroups = c("Monthly climate", "Travel path"),
                         options = layersControlOptions(collapsed = FALSE),
                         position = "topright")
    })

    # ---- Numbered travel-path overlay, drawn on BOTH map 1 and map 2 ----
    observe({
      cm2 <- dpt_path_data()
      req(nrow(cm2) > 0)

      steps <- input$dpt_connect_step
      if (is.null(steps)) steps <- max(nrow(cm2) - 1, 0)
      steps <- min(steps, max(nrow(cm2) - 1, 0))

      draw_path <- function(map_id) {
        proxy <- leafletProxy(map_id) %>% clearGroup("Travel path")
        if (is.na(steps) || steps <= 0) return(invisible(NULL))

        for (i in seq_len(steps)) {
          p1 <- cm2[i, ]; p2 <- cm2[i + 1, ]
          seg <- data.frame(lon = c(p1$lon, p2$lon), lat = c(p1$lat, p2$lat))
          mid_lon <- (p1$lon + p2$lon) / 2
          mid_lat <- (p1$lat + p2$lat) / 2
          seg_color <- if (!is.na(p2$fillColor)) p2$fillColor else "#1e293b"

          proxy <- proxy %>%
            addPolylines(data = seg, lng = ~lon, lat = ~lat,
                         color = seg_color, weight = 3, opacity = 0.95,
                         group = "Travel path") %>%
            addLabelOnlyMarkers(
              lng = mid_lon, lat = mid_lat,
              label = as.character(i),
              labelOptions = labelOptions(
                noHide = TRUE, textOnly = TRUE, direction = "center",
                style = list(
                  "background"    = "#1e293b",
                  "color"         = "#ffffff",
                  "border-radius" = "50%",
                  "padding"       = "1px 6px",
                  "font-weight"   = "bold",
                  "font-size"     = "11px",
                  "box-shadow"    = "0 0 2px #fff"
                )
              ),
              group = "Travel path"
            )
        }
      }

      draw_path(ns("dpt_density_map"))
      draw_path(ns("dpt_temp_map"))
    })

    # ---- Sidebar summary text ----
    output$dpt_summary <- renderUI({
      df <- dpt_elephant_data()
      cm <- dpt_centroid_by_ym()
      mi <- current_metric()
      tagList(
        strong("Selection summary"), br(),
        paste0("Elephant(s): ",
               if ("All Elephants" %in% input$dpt_elephants || length(input$dpt_elephants) == 0)
                 "All Elephants" else paste(input$dpt_elephants, collapse = ", ")),
        br(),
        paste0("Total GPS fixes: ", nrow(df)), br(),
        paste0("Year-months with data: ", nrow(cm)), br(),
        if (nrow(cm) > 0)
          paste0("Range: ", dplyr::first(cm$ym_label), " to ", dplyr::last(cm$ym_label))
        else "",
        br(),
        paste0("Climate variable shown: ", mi$name, " (", input$dpt_period, ")")
      )
    })
  })
}


# ═══════════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════════
#  TAB 3 — DAY / NIGHT   (source: day_night_hot_cool_final.R)
# ═══════════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════════

# ── CLASSIFIERS ──────────────────────────────────────────────────────────────
dn_classify_hot_cool <- function(d) {
  d <- as.Date(d)
  dplyr::case_when(
    d <  as.Date("2024-11-01") ~ "Hot",
    d <  as.Date("2025-04-01") ~ "Cool",
    d <  as.Date("2025-11-01") ~ "Hot",
    d <  as.Date("2026-04-01") ~ "Cool",
    TRUE                       ~ "Hot"
  )
}

dn_classify_period <- function(d) {
  d <- as.Date(d)
  dplyr::case_when(
    d <  as.Date("2024-11-01") ~ "Hot 1 (Jun-Oct 2024)",
    d <  as.Date("2025-04-01") ~ "Cool 1 (Nov 2024 - Mar 2025)",
    d <  as.Date("2025-11-01") ~ "Hot 2 (Apr-Oct 2025)",
    d <  as.Date("2026-04-01") ~ "Cool 2 (Nov 2025 - Mar 2026)",
    TRUE                       ~ "Hot 3 (Apr-Jun 2026)"
  )
}

dn_classify_rain <- function(d) {
  d <- as.Date(d)
  dplyr::case_when(
    d <  as.Date("2024-10-01") ~ "Low",
    d <  as.Date("2024-12-01") ~ "Heavy",
    d <  as.Date("2025-01-01") ~ "Low",
    d <  as.Date("2025-02-01") ~ "Heavy",
    d <  as.Date("2025-04-01") ~ "Low",
    d <  as.Date("2025-06-01") ~ "Heavy",
    d <  as.Date("2025-10-01") ~ "Low",
    d <  as.Date("2025-12-01") ~ "Heavy",
    d <  as.Date("2026-01-01") ~ "Low",
    d <  as.Date("2026-02-01") ~ "Heavy",
    d <  as.Date("2026-04-01") ~ "Low",
    d <  as.Date("2026-06-01") ~ "Heavy",
    TRUE                       ~ "Low"
  )
}

# ── DATA LOADING ─────────────────────────────────────────────────────────────
message("Loading climate & rainfall data (day/night)...")
dn_clim_raw <- read_csv(
  "POWER_Point_Hourly_kawudulla_new.csv",
  skip = 12, show_col_types = FALSE
)

dn_clim_daily <- dn_clim_raw %>%
  mutate(date = make_date(YEAR, MO, DY)) %>%
  group_by(date) %>%
  summarise(T_avg = mean(T2M, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(T_avg)) %>%
  mutate(hot_cool = dn_classify_hot_cool(date))

dn_clim_daily_rain <- dn_clim_raw %>%
  mutate(date = make_date(YEAR, MO, DY)) %>%
  group_by(date) %>%
  summarise(P_avg = mean(PRECTOTCORR, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(P_avg)) %>%
  mutate(rain = dn_classify_rain(date))

# Extract hourly rainfall to merge directly with GPS points
dn_clim_hourly <- dn_clim_raw %>%
  mutate(dt_round = make_datetime(YEAR, MO, DY, HR, tz = "UTC")) %>%
  select(dt_round, rain_mm_hr = PRECTOTCORR)

message("Loading GPS data (day/night)...")
dn_gps <- read_csv(
  "kaudulla_elephants_clean.csv",
  show_col_types = FALSE
) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(
    dt        = as.POSIXct(datetime, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    dt_round  = floor_date(dt, "hour"),
    year      = year(dt),
    hour      = hour(dt),
    date      = as_date(dt),
    day_night = if_else(hour >= 6 & hour < 18, "Day", "Night"),
    hot_cool  = dn_classify_hot_cool(date),
    period    = dn_classify_period(date),
    rain      = dn_classify_rain(date)
  ) %>%
  filter(!is.na(year)) %>%
  left_join(dn_clim_hourly, by = "dt_round")

# ── Constants ─────────────────────────────────────────────────────────────────
DN_ELE_NAMES <- sort(unique(dn_gps$name))
DN_YEARS     <- as.character(sort(unique(dn_gps$year)))
DN_KAUD_LAT  <- 8.168;  DN_KAUD_LON <- 80.913

DN_PERIOD_5_COLS <- c(
  "Hot 1 (Jun-Oct 2024)"         = "#991b1b",
  "Cool 1 (Nov 2024 - Mar 2025)" = "#1e40af",
  "Hot 2 (Apr-Oct 2025)"         = "#ef4444",
  "Cool 2 (Nov 2025 - Mar 2026)" = "#93c5fd",
  "Hot 3 (Apr-Jun 2026)"         = "#fca5a5"
)

DN_RAIN_2_COLS <- c("Heavy" = "#dc2626", "Low" = "#16a34a")

DN_HOURLY_RAIN_COLS <- c(
  "> 0 - 2 mm/hr" = "#93c5fd",
  "2 - 5 mm/hr"   = "#3b82f6",
  "5 - 10 mm/hr"  = "#1d4ed8",
  "10 - 50 mm/hr" = "#1e3a8a",
  "> 50 mm/hr"    = "#312e81"
)

DN_RAIN_TRANSITIONS <- as.Date(c(
  "2024-10-01","2024-12-01","2025-01-01","2025-02-01",
  "2025-04-01","2025-06-01","2025-10-01","2025-12-01",
  "2026-01-01","2026-02-01","2026-04-01","2026-06-01"
))

DN_RAIN_BANDS <- list(
  list(x0="2024-06-01", x1="2024-10-01", fill="rgba(254,215,170,0.40)"),
  list(x0="2024-10-01", x1="2024-12-01", fill="rgba(147,197,253,0.40)"),
  list(x0="2024-12-01", x1="2025-01-01", fill="rgba(254,215,170,0.40)"),
  list(x0="2025-01-01", x1="2025-02-01", fill="rgba(147,197,253,0.40)"),
  list(x0="2025-02-01", x1="2025-04-01", fill="rgba(254,215,170,0.40)"),
  list(x0="2025-04-01", x1="2025-06-01", fill="rgba(147,197,253,0.40)"),
  list(x0="2025-06-01", x1="2025-10-01", fill="rgba(254,215,170,0.40)"),
  list(x0="2025-10-01", x1="2025-12-01", fill="rgba(147,197,253,0.40)"),
  list(x0="2025-12-01", x1="2026-01-01", fill="rgba(254,215,170,0.40)"),
  list(x0="2026-01-01", x1="2026-02-01", fill="rgba(147,197,253,0.40)"),
  list(x0="2026-02-01", x1="2026-04-01", fill="rgba(254,215,170,0.40)"),
  list(x0="2026-04-01", x1="2026-06-01", fill="rgba(147,197,253,0.40)"),
  list(x0="2026-06-01", x1="2026-07-05", fill="rgba(254,215,170,0.40)")
)

DN_TRANSITIONS <- as.Date(c("2024-11-01","2025-04-01","2025-11-01","2026-04-01"))

DN_HC_BANDS <- list(
  list(x0 = "2024-06-01", x1 = "2024-11-01", fill = "rgba(253,186,116,0.35)"),
  list(x0 = "2024-11-01", x1 = "2025-04-01", fill = "rgba(147,197,253,0.35)"),
  list(x0 = "2025-04-01", x1 = "2025-11-01", fill = "rgba(253,186,116,0.35)"),
  list(x0 = "2025-11-01", x1 = "2026-04-01", fill = "rgba(147,197,253,0.35)"),
  list(x0 = "2026-04-01", x1 = "2026-07-05", fill = "rgba(253,186,116,0.35)")
)

# ── CSS ──────────────────────────────────────────────────────────────────────
dn_css <- "
.dn-scope *, .dn-scope *::before, .dn-scope *::after { box-sizing: border-box; margin: 0; padding: 0; }
.dn-scope { background: #f1f5f9; color: #1e293b; font-family: system-ui, -apple-system, sans-serif; font-size: 13px; }
.dn-scope #outer { display: flex; min-height: 100vh; }
.dn-scope #sidebar { width: 250px; flex-shrink: 0; background: #ffffff; border-right: 1px solid #e2e8f0; padding: 14px; position: sticky; top: 0; height: 100vh; overflow-y: auto; box-shadow: 1px 0 4px #0000000a; }
.dn-scope #sidebar .brand { font-size: 14px; font-weight: 700; color: #166534; margin-bottom: 4px; }
.dn-scope #sidebar .brand-sub { font-size: 10px; color: #64748b; margin-bottom: 16px; line-height: 1.5; }
.dn-scope .sh { font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: .08em; color: #64748b; margin: 16px 0 6px; padding-top: 14px; border-top: 1px solid #f1f5f9; }
.dn-scope .sh:first-of-type { border-top: none; padding-top: 0; }
.dn-scope #sidebar .form-control, .dn-scope #sidebar select { background: #ffffff !important; border: 1px solid #cbd5e1 !important; color: #1e293b !important; font-size: 11px !important; border-radius: 4px; width: 100%; }
.dn-scope #sidebar label { color: #475569 !important; font-size: 11px; }
.dn-scope #sidebar .shiny-input-container { margin: 0 0 6px !important; }
.dn-scope .leg-block { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 6px; padding: 9px 10px; margin-top: 6px; font-size: 11px; line-height: 2; }
.dn-scope .leg-block strong { display: block; font-size: 10px; text-transform: uppercase; letter-spacing: .06em; color: #64748b; margin-bottom: 4px; }
.dn-scope .dot { display: inline-block; width: 11px; height: 11px; border-radius: 50%; margin-right: 5px; vertical-align: middle; }
.dn-scope .note-txt { font-size: 9px; color: #94a3b8; line-height: 1.5; margin-top: 6px; }
.dn-scope #main { flex: 1; overflow-y: auto; padding: 10px; }
.dn-scope .panel-card { background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; overflow: hidden; margin-bottom: 12px; box-shadow: 0 1px 3px #0000000a; }
.dn-scope .panel-hdr { background: #f8fafc; border-bottom: 1px solid #e2e8f0; padding: 8px 14px; }
.dn-scope .panel-hdr h3 { font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: .07em; color: #166534; margin: 0 0 2px; }
.dn-scope .panel-hdr .desc { font-size: 10px; color: #64748b; }
"

# ── Module UI ────────────────────────────────────────────────────────────────
day_night_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(dn_css))),

    div(class = "dn-scope",
      div(id = "outer",

          # ── SIDEBAR ──────────────────────────────────────────────────────────────
          div(id = "sidebar",
              div(class = "brand",   "\U0001F418 Kaudulla NP"),
              div(class = "brand-sub", "GPS movement patterns by time of day and climate period"),

              div(class = "sh", "Filter: Year"),
              selectInput(ns("yr"), NULL, choices = c("All", DN_YEARS), selected = "All", width = "100%"),

              div(class = "sh", "Filter: Elephants"),
              selectInput(ns("eles"), NULL, choices = c("All Elephants" = "ALL", setNames(DN_ELE_NAMES, DN_ELE_NAMES)), selected = "ALL", multiple = TRUE, width = "100%"),
              div(class = "note-txt", "Ctrl+click to pick multiple elephants. Select 'All Elephants' to show all."),

              # --- Map 1 Legend ---
              div(class = "sh", "Legend: Day vs Night"),
              div(class = "leg-block",
                  tags$strong("Map 1 — Time of Day"),
                  div(span(class="dot", style="background:#dc2626"), "Day  (06:00 \u2013 18:00)"),
                  div(span(class="dot", style="background:#2563eb"), "Night  (18:00 \u2013 06:00)")
              ),

              # --- Map 2 Data Layer ---
              div(class = "sh", "Map 2 — Data Layer"),
              radioButtons(ns("climate_tab"), NULL,
                           choices  = c("Temperature Period (Hot/Cool)" = "temp",
                                        "Rainfall Period (Heavy/Low)"   = "rain"
                                        ),
                           selected = "temp", width = "100%"
              ),

              # Conditional legends/filters for Map 2
              conditionalPanel(
                condition = "input.climate_tab === 'temp'", ns = ns,
                div(class = "sh", "Legend: Hot vs Cool"),
                div(class = "leg-block",
                    tags$strong("Map 2 & Chart — Temperature Period"),
                    div(span(class="dot", style="background:#dc2626"), "Hot period"),
                    div(span(class="dot", style="background:#2563eb"), "Cool period")
                )
              ),

              conditionalPanel(
                condition = "input.climate_tab === 'rain'", ns = ns,
                div(class = "sh", "Legend: Heavy vs Low Rain"),
                div(class = "leg-block",
                    tags$strong("Map 2 & Chart — Rainfall Period"),
                    div(span(class="dot", style="background:#dc2626"), "Heavy rain period"),
                    div(span(class="dot", style="background:#16a34a"), "Low rain period")
                )
              ),

              conditionalPanel(
                condition = "input.climate_tab === 'hourly'", ns = ns,
                div(class = "sh", "Filter: Exact Hourly Rain"),
                selectInput(ns("rain_thresh"), NULL,
                            choices = c(
                              "Light Rain (> 0 to 2 mm/hr)" = "light",
                              "Moderate Rain (2 to 5 mm/hr)" = "mod",
                              "Heavy Rain (5 to 10 mm/hr)" = "heavy",
                              "Extreme Rain (10 to 20 mm/hr)" = "extreme",
                              "Cloudburst (> 20 mm/hr)" = "ultra"
                            ),
                            selected = "mod",
                            multiple = TRUE,
                            width = "100%"),
                div(class = "note-txt", "Click to add multiple rain brackets. Press Backspace to remove."),
                div(class = "leg-block",
                    tags$strong("Map 2 — Hourly Intensity"),
                    div(span(class="dot", style="background:#93c5fd"), "> 0 - 2 mm/hr"),
                    div(span(class="dot", style="background:#3b82f6"), "2 - 5 mm/hr"),
                    div(span(class="dot", style="background:#1d4ed8"), "5 - 10 mm/hr"),
                    div(span(class="dot", style="background:#1e3a8a"), "10 - 20 mm/hr"),
                    div(span(class="dot", style="background:#312e81"), "> 20 mm/hr")
                )
              )
          ),

          # ── MAIN ─────────────────────────────────────────────────────────────────
          div(id = "main",

              div(class = "panel-card",
                  div(class = "panel-hdr",
                      tags$h3("Map 1 — GPS Points: Day vs Night"),
                      div(class = "desc", "All GPS fixes plotted simultaneously \u00b7 Red = Day (06:00\u201318:00) \u00b7 Blue = Night (18:00\u201306:00)")
                  ),
                  leafletOutput(ns("map_dn"), height = "440px")
              ),

              div(class = "panel-card",
                  div(class = "panel-hdr",
                      div(style = "margin-bottom:8px",
                          radioButtons(ns("climate_tab_map"), NULL,
                                       choices  = c("Temperature Period (Hot/Cool)" = "temp",
                                                    "Rainfall Period (Heavy/Low)"   = "rain",
                                                    "Hourly Rain Filter"            = "hourly"),
                                       selected = "temp", inline = TRUE)
                      ),
                      conditionalPanel(
                        condition = "input.climate_tab_map === 'temp'", ns = ns,
                        tags$h3("Map 2 — GPS Points: Hot vs Cool Period"),
                        div(class = "desc", "All GPS fixes \u00b7 Red = Hot period \u00b7 Blue = Cool period"),
                        div(style = "margin-top:6px",
                            radioButtons(ns("map2_mode"), NULL, choices = c("2 colours  (Hot / Cool)" = "two", "5 periods  (individual shades)" = "five"), selected = "two", inline = TRUE)
                        )
                      ),
                      conditionalPanel(
                        condition = "input.climate_tab_map === 'rain'", ns = ns,
                        tags$h3("Map 2 — GPS Points: Heavy vs Low Rain Period"),
                        div(class = "desc", "All GPS fixes \u00b7 Red = Heavy rain period \u00b7 Green = Low rain period")
                      ),
                      conditionalPanel(
                        condition = "input.climate_tab_map === 'hourly'", ns = ns,
                        tags$h3("Map 2 — GPS Points During Active Rainfall"),
                        div(class = "desc", "Filters the map to ONLY show locations where the hourly rainfall exceeded your selected threshold.")
                      )
                  ),
                  leafletOutput(ns("map_hc"), height = "440px")
              ),

              conditionalPanel(
                condition = "input.climate_tab_map === 'temp' || input.climate_tab_map === null", ns = ns,
                div(class = "panel-card",
                    div(class = "panel-hdr",
                        tags$h3("Temperature Time Series with Hot / Cool Period Boundaries"),
                        div(class = "desc", "Daily mean temperature (NASA POWER) \u00b7 Orange = Hot \u00b7 Blue = Cool \u00b7 Dashed lines = transition dates")
                    ),
                    plotlyOutput(ns("temp_ts"), height = "300px")
                )
              ),

              conditionalPanel(
                condition = "input.climate_tab_map === 'rain' || input.climate_tab_map === 'hourly'", ns = ns,
                div(class = "panel-card",
                    div(class = "panel-hdr",
                        tags$h3("Rainfall Time Series — Heavy vs Low Rain Periods"),
                        div(class = "desc", "Mean daily rainfall rate (NASA POWER) \u00b7 Blue shading = Heavy rain \u00b7 Orange shading = Low rain")
                    ),
                    plotlyOutput(ns("rain_ts"), height = "300px")
                )
              )
          )
      )
    )
  )
}

# ── Module Server ──────────────────────────────────────────────────────────────
day_night_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    gps_f <- reactive({
      df <- dn_gps
      if (!is.null(input$yr) && input$yr != "All") df <- df %>% filter(as.character(year) == input$yr)
      sels <- input$eles
      if (!is.null(sels) && length(sels) > 0 && !"ALL" %in% sels) df <- df %>% filter(name %in% sels)
      df
    })

    base_map <- function() {
      leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles("CartoDB.Positron",  group = "CartoDB Light") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "CartoDB Light", "Satellite"),
          position   = "bottomright",
          options    = layersControlOptions(collapsed = TRUE)
        ) %>%
        setView(lng = DN_KAUD_LON, lat = DN_KAUD_LAT, zoom = 13)
    }

    output$map_dn <- renderLeaflet(base_map())
    output$map_hc <- renderLeaflet(base_map())

    update_map <- function(map_id, df, col_var, col_map, legend_title) {
      proxy <- leafletProxy(map_id) %>% clearMarkers() %>% removeControl("map-legend")
      if (nrow(df) == 0) return(invisible(NULL))

      fills <- unname(col_map[as.character(df[[col_var]])])
      fills[is.na(fills)] <- "#94a3b8"

      proxy %>%
        addCircleMarkers(
          lat         = df$lat,
          lng         = df$lon,
          radius      = 4,
          fillColor   = fills,
          fillOpacity = 0.70,
          color       = "transparent",
          weight      = 0,
          popup       = paste0(
            "<b>", df$name, "</b><br>",
            format(df$dt, "%d %b %Y  %H:%M"), "<br>",
            "<span style='color:#64748b'>", legend_title, ":</span>  ",
            "<b>", df[[col_var]], "</b>"
          )
        ) %>%
        addLegend(
          layerId  = "map-legend", position = "bottomleft",
          colors   = unname(col_map), labels = names(col_map),
          title    = legend_title, opacity = 0.9
        )
    }

    observe({
      df <- gps_f()

      # Update Map 1
      update_map(ns("map_dn"), df, "day_night", c("Day" = "#dc2626", "Night" = "#2563eb"), "Time of Day")

      # Update Map 2 based on selection
      if (isTRUE(input$climate_tab_map == "hourly")) {
        cat_selected <- input$rain_thresh

        if (is.null(cat_selected) || length(cat_selected) == 0) {
          df_map2 <- df[0, ]
        } else {
          df_map2 <- df %>%
            filter(!is.na(rain_mm_hr), rain_mm_hr > 0) %>%
            mutate(
              temp_cat = case_when(
                rain_mm_hr > 20 ~ "ultra",
                rain_mm_hr > 10 ~ "extreme",
                rain_mm_hr > 5  ~ "heavy",
                rain_mm_hr > 2  ~ "mod",
                TRUE            ~ "light"
              )
            ) %>%
            filter(temp_cat %in% cat_selected)

          if(nrow(df_map2) > 0) {
            df_map2 <- df_map2 %>% mutate(
              rain_cat = case_when(
                temp_cat == "ultra"   ~ "> 20 mm/hr",
                temp_cat == "extreme" ~ "10 - 20 mm/hr",
                temp_cat == "heavy"   ~ "5 - 10 mm/hr",
                temp_cat == "mod"     ~ "2 - 5 mm/hr",
                temp_cat == "light"   ~ "> 0 - 2 mm/hr"
              ),
              rain_cat = factor(rain_cat, levels = names(DN_HOURLY_RAIN_COLS))
            )
          }
        }

        update_map(ns("map_hc"), df_map2, "rain_cat", DN_HOURLY_RAIN_COLS, "Hourly Rainfall")
      } else if (isTRUE(input$climate_tab_map == "rain")) {
        update_map(ns("map_hc"), df, "rain", DN_RAIN_2_COLS, "Rainfall Period")

      } else if (isTRUE(input$map2_mode == "five")) {
        update_map(ns("map_hc"), df, "period", DN_PERIOD_5_COLS, "Temperature Period")

      } else {
        update_map(ns("map_hc"), df, "hot_cool", c("Hot" = "#eab308", "Cool" = "#9333ea"), "Temperature Period")
      }

      updateRadioButtons(session, "climate_tab", selected = input$climate_tab_map)

    }) |> bindEvent(input$yr, input$eles, input$map2_mode, input$climate_tab_map, input$rain_thresh, ignoreNULL = FALSE, ignoreInit = FALSE)

    observe({ updateRadioButtons(session, "climate_tab_map", selected = input$climate_tab) }) |> bindEvent(input$climate_tab, ignoreInit = TRUE)

    output$temp_ts <- renderPlotly({
      shapes <- lapply(DN_HC_BANDS, function(b) { list(type="rect", xref="x", yref="paper", x0=b$x0, x1=b$x1, y0=0, y1=1, fillcolor=b$fill, line=list(width=0), layer="below") })
      vlines <- lapply(as.character(DN_TRANSITIONS), function(d) { list(type="line", xref="x", yref="paper", x0=d, x1=d, y0=0, y1=1, line=list(color="#94a3b8", width=1.2, dash="dash")) })
      all_shapes <- c(shapes, vlines)
      annots <- lapply(seq_along(DN_TRANSITIONS), function(i) {
        lbl <- c("\u2192 Cool","\u2192 Hot","\u2192 Cool","\u2192 Hot")[i]
        list(x=as.character(DN_TRANSITIONS[i]), y=1, yref="paper", text=lbl, showarrow=FALSE, font=list(size=9, color="#64748b"), xanchor="left", yanchor="top", xshift=4)
      })

      plot_ly() %>%
        add_trace(x=c(as.Date("2024-06-01")), y=c(NA), type="scatter", mode="lines", line=list(color="rgba(253,186,116,0.7)", width=8), name="Hot period", showlegend=TRUE) %>%
        add_trace(x=c(as.Date("2024-06-01")), y=c(NA), type="scatter", mode="lines", line=list(color="rgba(147,197,253,0.7)", width=8), name="Cool period", showlegend=TRUE) %>%
        add_trace(data=dn_clim_daily, x=~date, y=~T_avg, type="scatter", mode="lines", line=list(color="#1e293b", width=1.2), name="Mean daily temp (\u00b0C)", hovertemplate="<b>%{x|%d %b %Y}</b><br>Mean temp: <b>%{y:.2f} \u00b0C</b><extra></extra>", showlegend=TRUE) %>%
        layout(
          plot_bgcolor="#ffffff", paper_bgcolor="#f8fafc", shapes=all_shapes, annotations=annots,
          font=list(color="#1e293b", family="system-ui, sans-serif", size=11),
          xaxis=list(title="", type="date", gridcolor="#e2e8f0", tickfont=list(size=10, color="#64748b")),
          yaxis=list(title="Mean daily temperature (\u00b0C)", gridcolor="#e2e8f0", tickfont=list(size=10, color="#64748b"), range=list(21, 30.5)),
          legend=list(orientation="h", x=0.5, xanchor="center", y=1.12, bgcolor="transparent", borderwidth=0, font=list(size=10, color="#1e293b")),
          margin=list(t=30, l=60, r=20, b=30), hovermode="x unified", hoverlabel=list(bgcolor="#ffffff", bordercolor="#e2e8f0", font=list(color="#1e293b"))
        )
    })

    output$rain_ts <- renderPlotly({
      rain_shapes <- lapply(DN_RAIN_BANDS, function(b) { list(type="rect", xref="x", yref="paper", x0=b$x0, x1=b$x1, y0=0, y1=1, fillcolor=b$fill, line=list(width=0), layer="below") })
      rain_vlines <- lapply(as.character(DN_RAIN_TRANSITIONS), function(d) { list(type="line", xref="x", yref="paper", x0=d, x1=d, y0=0, y1=1, line=list(color="#94a3b8", width=1.2, dash="dash")) })
      all_shapes <- c(rain_shapes, rain_vlines)

      plot_ly() %>%
        add_trace(x=c(as.Date("2024-06-01")), y=c(NA), type="scatter", mode="lines", line=list(color="rgba(147,197,253,0.7)", width=8), name="Heavy rain period", showlegend=TRUE) %>%
        add_trace(x=c(as.Date("2024-06-01")), y=c(NA), type="scatter", mode="lines", line=list(color="rgba(254,215,170,0.7)", width=8), name="Low rain period", showlegend=TRUE) %>%
        add_bars(data=dn_clim_daily_rain, x=~date, y=~P_avg, marker=list(color="#1d4ed8", opacity=0.75), name="Mean daily rainfall (mm/day)", hovertemplate="<b>%{x|%d %b %Y}</b><br>Rainfall: <b>%{y:.2f} mm/day</b><extra></extra>", showlegend=TRUE) %>%
        layout(
          plot_bgcolor="#ffffff", paper_bgcolor="#f8fafc", shapes=all_shapes,
          font=list(color="#1e293b", family="system-ui, sans-serif", size=11),
          xaxis=list(title="", type="date", gridcolor="#e2e8f0", tickfont=list(size=10, color="#64748b")),
          yaxis=list(title="Mean daily rainfall rate (mm/day)", gridcolor="#e2e8f0", tickfont=list(size=10, color="#64748b"), range=list(0, 165)),
          legend=list(orientation="h", x=0.5, xanchor="center", y=1.12, bgcolor="transparent", borderwidth=0, font=list(size=10, color="#1e293b")),
          margin=list(t=30, l=60, r=20, b=30), hovermode="x unified", hoverlabel=list(bgcolor="#ffffff", bordercolor="#e2e8f0", font=list(color="#1e293b")),
          bargap = 0
        )
    })
  })
}


# ═══════════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════════
#  TAB 4 — VEGETATION TRACKING   (source: vegetation_tracking_dashboard_v2__3_.R)
# ═══════════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════════

# ── GLOBAL DATA ────────────────────────────────────────────────────────────────
message("Loading GPS data (vegetation)...")
vt_gps <- read_csv("kaudulla_elephants_clean.csv", show_col_types = FALSE) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(
    dt    = as.POSIXct(datetime, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    year  = year(dt),
    lat_g = round(lat, 3),
    lon_g = round(lon, 3)
  ) %>%
  filter(!is.na(year))

VT_FOCUS_YEARS <- c(2024L, 2025L, 2026L)
vt_gps_f <- vt_gps %>% filter(year %in% VT_FOCUS_YEARS)

# Fix counts for bottom bar chart
vt_fix_counts <- vt_gps_f %>%
  count(year, name, name = "fixes") %>%
  mutate(year = as.character(year))

# Hotspot pre-computation (all elephants combined, per year)
vt_hs_all <- vt_gps_f %>%
  group_by(year, lat_g, lon_g) %>%
  summarise(
    visits      = n(),
    n_elephants = n_distinct(name),
    elephants   = paste(sort(unique(name)), collapse = ", "),
    .groups     = "drop"
  )

# Hotspot per individual elephant
vt_hs_each <- vt_gps_f %>%
  group_by(year, name, lat_g, lon_g) %>%
  summarise(visits = n(), .groups = "drop")

vt_get_top_all <- function(n_top) {
  vt_hs_all %>%
    group_by(year) %>%
    slice_max(visits, n = n_top, with_ties = FALSE) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    rename(lat = lat_g, lon = lon_g)
}

vt_get_top_each <- function(nm, n_top) {
  vt_hs_each %>%
    filter(name == nm) %>%
    group_by(year) %>%
    slice_max(visits, n = n_top, with_ties = FALSE) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    rename(lat = lat_g, lon = lon_g)
}

# ── NDVI data ────────────────────────────────────────────────────────────────
vt_density_raw <- tribble(
  ~year, ~`Non-Vegetation`, ~`Shrubs & Degraded`, ~`Sparse Vegetation`,
         ~`Moderate Canopy`, ~`High Density Forest`,
  2018, 18.12, 1.01, 12.87, 68.00, 0.00,
  2019, 32.52, 1.97,  8.32, 57.20, 0.00,
  2020, 33.92, 1.49,  6.60, 57.98, 0.00,
  2021, 29.39, 1.90,  7.60, 60.17, 0.95,
  2022, 34.25, 2.49, 13.46, 49.80, 0.00,
  2023, 23.38, 7.15, 20.04, 49.38, 0.05,
  2024, 25.09, 3.34, 13.22, 58.04, 0.31,
  2025, 24.02, 4.07, 14.84, 56.72, 0.36,
  2026, 34.44, 1.25,  4.83, 59.46, 0.00
)

vt_health_raw <- tribble(
  ~year, ~`Non-Vegetation`, ~`Unhealthy Plant`, ~`Moderate Healthy`, ~`Very Healthy`,
  2018, 17.88, 14.11, 67.99, 0.00,
  2019, 30.26,  7.32, 62.42, 0.00,
  2020, 33.58,  8.43, 57.97, 0.00,
  2021, 26.80,  7.14, 66.05, 0.00,
  2022, 30.53, 11.07, 58.38, 0.00,
  2023, 16.60, 21.01, 62.39, 0.00,
  2024, 18.84, 13.95, 67.20, 0.00,
  2025, 21.31, 12.23, 66.46, 0.00,
  2026, 31.14,  5.84, 63.00, 0.00
)

# ── Colour palettes ──────────────────────────────────────────────────────────
VT_YEAR_COLS   <- c("2024" = "#ea580c", "2025" = "#2563eb", "2026" = "#16a34a")
VT_DENS_COLS   <- c("Non-Vegetation"      = "#92714a",
                    "Shrubs & Degraded"   = "#b5960f",
                    "Sparse Vegetation"   = "#6aab2e",
                    "Moderate Canopy"     = "#2d6e1f",
                    "High Density Forest" = "#14400d")
VT_HLTH_COLS   <- c("Non-Vegetation"  = "#92714a",
                    "Unhealthy Plant"  = "#dc2626",
                    "Moderate Healthy" = "#d97706",
                    "Very Healthy"     = "#16a34a")

# GPS dot colours per year (for the all-GPS-points map)
VT_GPS_YEAR_COLS <- c("2024" = "#f97316", "2025" = "#3b82f6", "2026" = "#22c55e")

# Per-elephant hotspot colours
VT_ELE_HS_COLS <- c(
  "2024" = "#7c3aed",   # violet
  "2025" = "#0891b2",   # cyan
  "2026" = "#be185d"    # pink
)

VT_ELE_NAMES <- sort(unique(vt_gps_f$name))
VT_KAUD_LAT  <- 8.175; VT_KAUD_LON <- 80.913

# Unique colour per elephant (for combined hotspot map dots + legend)
VT_ELE_COLORS <- c(
  "#e11d48","#7c3aed","#0891b2","#16a34a","#d97706",
  "#be185d","#1d4ed8","#15803d","#b45309","#6d28d9",
  "#0e7490","#dc2626","#065f46","#92400e"
)
names(VT_ELE_COLORS) <- VT_ELE_NAMES

# Triangle SVG icon factory (for all-elephant top-1 hotspot markers)
vt_make_triangle_icon <- function(color, size = 22) {
  svg <- paste0(
    'data:image/svg+xml;utf8,<svg xmlns="http://www.w3.org/2000/svg" width="',
    size,'\" height="',size,'">',
    '<polygon points="',size/2,',2 ',size-2,',',size-2,' 2,',size-2,'"',
    ' fill="',URLencode(color, reserved=TRUE),'"',
    ' stroke="white" stroke-width="2"/>',
    '</svg>'
  )
  makeIcon(iconUrl = svg, iconWidth = size, iconHeight = size,
           iconAnchorX = size/2, iconAnchorY = size-2)
}

# Monthly home-range radius (spread in km) pre-computed
vt_monthly_range <- vt_gps_f %>%
  mutate(month = month(dt)) %>%
  group_by(year, month, name) %>%
  summarise(
    lat_sd = sd(lat, na.rm = TRUE),
    lon_sd = sd(lon, na.rm = TRUE),
    n_pts  = n(),
    .groups = "drop"
  ) %>%
  filter(n_pts >= 5) %>%
  mutate(
    radius_km = sqrt(lat_sd^2 + lon_sd^2) * 111,
    month_lbl = month.abb[month],
    yr_chr    = as.character(year)
  )

# NDVI moderate canopy for 2024-2026 (monthly proxy via yearly value)
vt_mod_canopy <- data.frame(
  year      = c(2024, 2025, 2026),
  mod_canopy = c(58.04, 56.72, 59.46)
)

# ── CSS ──────────────────────────────────────────────────────────────────────
vt_css <- "
.vt-scope *,.vt-scope *::before,.vt-scope *::after{box-sizing:border-box;margin:0;padding:0}
.vt-scope{background:#f1f5f9;color:#1e293b;
          font-family:system-ui,-apple-system,sans-serif;font-size:13px;overflow-x:hidden}
.vt-scope #page-hdr{background:#ffffff;border-bottom:2px solid #e2e8f0;padding:10px 16px;
          display:flex;align-items:center;gap:10px;box-shadow:0 1px 4px #0000000d}
.vt-scope #page-hdr h1{font-size:15px;font-weight:700;color:#166534;letter-spacing:.04em}
.vt-scope #page-hdr .sub{font-size:11px;color:#64748b}
.vt-scope .dash-panel{background:#ffffff;border:1px solid #e2e8f0;border-radius:8px;margin:6px;
            overflow:hidden;display:flex;flex-direction:column;box-shadow:0 1px 3px #0000000a}

/* PANEL HEADER: Increased padding and gap to prevent element crowding */
.vt-scope .panel-hdr{background:#f8fafc;border-bottom:1px solid #e2e8f0;padding:10px 15px;
           display:flex !important;align-items:center !important;gap:15px !important;flex-wrap:wrap;flex-shrink:0}
           
.vt-scope .panel-title{font-size:11px;font-weight:700;text-transform:uppercase;
              letter-spacing:.07em;color:#166534;white-space:nowrap}
.vt-scope .panel-body{padding:10px;flex:1;min-height:0}
.vt-scope .row-2{display:grid;grid-template-columns:1fr 1fr;gap:0;padding:6px 0 0}

/* INPUT CONTAINERS: Ensures consistent vertical alignment */
.vt-scope .shiny-input-container { margin-bottom: 0px !important; display: inline-flex !important; align-items: center; }

/* RADIO BUTTONS: Explicit spacing */
.vt-scope .radio-inline { display: inline-flex !important; align-items: center; margin-right: 15px !important; margin-left: 5px !important; }
.vt-scope .radio-inline label { color: #1e293b !important; font-size: 11px !important; white-space: nowrap !important; font-weight: 600 !important; margin-bottom: 0 !important; margin-left: 4px !important; }

/* CHECKBOXES: Explicit spacing for years */
.vt-scope .checkbox-inline { display: inline-flex !important; align-items: center; margin-right: 10px !important; margin-bottom: 0 !important; }
.vt-scope .checkbox-inline label { color: #1e293b !important; font-size: 11px !important; font-weight: 600 !important; margin-bottom: 0 !important; margin-left: 4px !important; }

.vt-scope .form-control,.vt-scope select{background:#ffffff!important;border:1px solid #cbd5e1!important;
                      color:#1e293b!important;font-size:11px!important;border-radius:4px;
                      padding:3px 7px;height:26px}
.vt-scope label{color:#64748b!important;font-size:10px; margin-bottom: 0 !important;}

.vt-scope .note{font-size:9px;color:#94a3b8;padding:3px 12px 5px;font-style:italic;
      background:#f8fafc;border-top:1px solid #f1f5f9}
.vt-scope .leaflet-tooltip{background:#ffffffee;border:1px solid #e2e8f0;color:#1e293b;
                 font-size:11px;padding:5px 8px;border-radius:4px;box-shadow:0 2px 8px #0000001a}
.vt-scope .leaflet-tooltip-arrow{display:none}

/* Add space between the label and the first checkbox */
.vt-scope .year-checkbox-container .checkbox-inline:first-child {
    margin-left: 20px !important; /* Adjust 10px as needed */
}

/* Align radio buttons perfectly to the left edge of the column */
.tk-scope .align-left-radio .shiny-options-group {
    display: flex;
    flex-direction: row;
    gap: 20px;             /* Space between buttons */
    margin-left: 0 !important;
    padding-left: 0 !important;
}

/* Remove default Bootstrap padding */
.tk-scope .align-left-radio .radio {
    margin-top: 0 !important;
    margin-bottom: 5px !important;
    padding-left: 0 !important;
    display: flex;
    align-items: center;
}

/* Ensure the radio input and text label are properly aligned */
.tk-scope .align-left-radio label {
    display: flex !important;
    align-items: center;
    font-size: 11px !important;
    color: #1e293b !important;
    margin-bottom: 0 !important;
    cursor: pointer;
}

/* Remove default margin on the radio input itself */
.tk-scope .align-left-radio input[type=\"radio\"] {
    margin-right: 8px !important;
    margin-left: 0 ;
margin-top: 0 ;
position: relative !important;
}
"

# ── Module UI ────────────────────────────────────────────────────────────────
vegetation_tracking_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(vt_css))),

    div(class = "vt-scope",
      # ── Header ────────────────────────────────────────────────────────────────
      div(id = "page-hdr",
        tags$h1("\U0001F33F Kaudulla NP — Vegetation & GPS Tracking Dashboard"),
        span(class = "sub",
             "NDVI 2018-2026  \u00b7  GPS collar data 2024-2026  \u00b7  Hotspot analysis")
      ),

      # ── ROW 1: Vegetation chart | GPS tracking map ───────────────────────────
      div(class = "row-2",

        # LEFT: Vegetation chart
        div(class = "dash-panel",
          div(class = "panel-hdr",
            span(class = "panel-title", "Vegetation Coverage"),
            selectInput(ns("veg_type"), NULL,
              choices  = c("Density Classes" = "density", "Health Classes" = "health"),
              selected = "density", width = "150px"),
            radioButtons(ns("chart_style"), NULL,
              choices = c("Grouped" = "group", "Stacked" = "stack"),
              selected = "group", inline = TRUE),
            div(style = "margin-left:auto;font-size:9px;color:#94a3b8",
                "Focus: 2024 / 2025 / 2026")
          ),
          div(class = "panel-body", plotlyOutput(ns("veg_chart"), height = "340px")),
          div(class = "note",
              "High Density Forest < 0.4% all years — nearly absent from park.")
        ),

        # RIGHT: All GPS points coloured by year
        div(class = "dash-panel",
          div(class = "panel-hdr",
            span(class = "panel-title", "All GPS Tracking Points"),
            div(style = "display:flex;gap:4px;align-items:center",
              span(style = "font-size:10px;color:#64748b", "Years:"),
              
              div(class = "year-checkbox-container", 
                  checkboxGroupInput(ns("sel_years"), NULL,
                                     choices  = c("2024","2025","2026"),
                                     selected = c("2024","2025","2026"),
                                     inline   = TRUE)
              )
              ),
            div(style = "display:flex;gap:6px;align-items:center;flex-wrap:wrap",
              span(style = "font-size:10px;color:#64748b;white-space:nowrap", "Elephants:"),
              selectInput(ns("sel_elephant"), NULL,
                choices  = setNames(VT_ELE_NAMES, VT_ELE_NAMES),
                selected = VT_ELE_NAMES,
                multiple = TRUE,
                width    = "260px")
            )
          ),
          div(class = "panel-body", leafletOutput(ns("map_gps"), height = "340px")),
          div(class = "note",
              "Dots coloured by year: \U0001F7E0 2024  \U0001F535 2025  \U0001F7E2 2026. Click a dot for details.")
        )
      ),

      # ── ROW 2: Monthly home range | Per-elephant hotspots ────────
      div(class = "row-2",

        # LEFT: Monthly home range radius vs vegetation
        div(class = "dash-panel",
          div(class = "panel-hdr",
            span(class = "panel-title", "Monthly Home Range vs Vegetation"),
            div(style = "display:flex;gap:6px;align-items:center",
              span(style = "font-size:10px;color:#64748b", "Elephants:"),
              selectInput(ns("range_elephants"), NULL,
                choices  = setNames(VT_ELE_NAMES, VT_ELE_NAMES),
                selected = VT_ELE_NAMES,
                multiple = TRUE,
                width    = "220px")
            ),
            div(style = "display:flex;gap:4px;align-items:center",
              span(style = "font-size:10px;color:#64748b", "Years:"),
              checkboxGroupInput(ns("range_years"), NULL,
                choices  = c("2024","2025","2026"),
                selected = c("2024","2025","2026"),
                inline   = TRUE)
            ),
            div(style = "margin-left:auto;font-size:9px;color:#94a3b8",
                "Radius = spatial spread (km) per month")
          ),
          div(class = "panel-body", plotlyOutput(ns("plot_range"), height = "340px")),
          div(class = "note",
              "Lines = monthly home range radius per elephant per year.  ",
              "Shaded band = Moderate Canopy % (right axis) — shrinks when canopy contracts.")
        ),

        # RIGHT: Per-elephant hotspots
        div(class = "dash-panel",
          div(class = "panel-hdr",
            span(class = "panel-title", "Hotspot Trajectory & Per-Elephant Dots"),
            div(style = "display:flex;gap:6px;align-items:center;flex-wrap:wrap",
              span(style = "font-size:10px;color:#64748b;white-space:nowrap", "Elephants:"),
              selectInput(ns("ele_pick"), NULL,
                choices  = setNames(VT_ELE_NAMES, VT_ELE_NAMES),
                selected = VT_ELE_NAMES,
                multiple = TRUE,
                width    = "230px")
            ),
            div(style = "display:flex;gap:6px;align-items:center",
              span(style = "font-size:10px;color:#64748b", "Show top:"),
              selectInput(ns("top_n_each"), NULL,
                choices  = c("Top 1 (most visited)" = "1", "Top 5" = "5"),
                selected = "1", width = "155px")
            )
          ),
          div(class = "panel-body", leafletOutput(ns("map_combined"), height = "340px")),
          uiOutput(ns("combined_legend")),
          uiOutput(ns("ele_info"))
        )
      ),

      # ── BOTTOM ROW: GPS Fix Counts ────────────────────────────────────────────
      div(class = "bot-row",
        div(class = "dash-panel", style = "margin:0",
          div(class = "panel-hdr",
            span(class = "panel-title", "GPS Fix Counts per Elephant per Year"),
            div(style = "font-size:10px;color:#64748b;margin-left:auto",
                "Only years with actual recorded fixes shown per elephant")
          ),
          div(class = "panel-body", plotlyOutput(ns("fix_chart"), height = "230px"))
        )
      )
    )
  )
}

# ── Module Server ──────────────────────────────────────────────────────────────
vegetation_tracking_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Shared light layout helper ──────────────────────────────────────────
    light_layout <- function(p, ...) {
      p %>% layout(
        plot_bgcolor  = "#ffffff",
        paper_bgcolor = "#f8fafc",
        font       = list(color = "#1e293b", family = "system-ui, sans-serif", size = 11),
        xaxis      = list(gridcolor = "#e2e8f0", zerolinecolor = "#e2e8f0",
                          tickfont  = list(color = "#64748b", size = 10)),
        yaxis      = list(gridcolor = "#e2e8f0", zerolinecolor = "#e2e8f0",
                          tickfont  = list(color = "#64748b", size = 10)),
        legend     = list(bgcolor = "#ffffff", bordercolor = "#e2e8f0", borderwidth = 1,
                          font = list(size = 10, color = "#1e293b")),
        margin     = list(t = 10, l = 50, r = 10, b = 40),
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#e2e8f0",
                          font = list(color = "#1e293b")),
        ...
      )
    }

    # ── Base map builder ────────────────────────────────────────────────────
    base_map <- function() {
      leaflet() %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles("CartoDB.Positron",  group = "CartoDB Light") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "CartoDB Light", "Satellite"),
          position   = "bottomright",
          options    = layersControlOptions(collapsed = TRUE)
        ) %>%
        setView(lng = VT_KAUD_LON, lat = VT_KAUD_LAT, zoom = 13)
    }

    # ── VEGETATION CHART ──────────────────────────────────────────────────────
    output$veg_chart <- renderPlotly({
      if (input$veg_type == "density") {
        df   <- vt_density_raw %>% filter(year %in% VT_FOCUS_YEARS)
        cols <- VT_DENS_COLS
        cls  <- c("Non-Vegetation","Shrubs & Degraded","Sparse Vegetation",
                  "Moderate Canopy","High Density Forest")
      } else {
        df   <- vt_health_raw %>% filter(year %in% VT_FOCUS_YEARS)
        cols <- VT_HLTH_COLS
        cls  <- c("Non-Vegetation","Unhealthy Plant","Moderate Healthy","Very Healthy")
      }

      df_long <- df %>%
        pivot_longer(-year, names_to = "class", values_to = "pct") %>%
        mutate(class = factor(class, levels = cls), yr = factor(year))

      p <- plot_ly()
      for (cl in cls) {
        sub <- df_long %>% filter(class == cl)
        p <- add_bars(p,
          data = sub, x = ~yr, y = ~pct, name = cl,
          marker = list(color = cols[cl], opacity = 0.9,
                        line = list(width = 0.5, color = "#00000022")),
          hovertemplate = paste0("<b>", cl, "</b><br>Year: %{x}<br>",
                                 "Coverage: <b>%{y:.2f}%</b><extra></extra>")
        )
      }

      p %>%
        light_layout() %>%
        layout(
          barmode = input$chart_style,
          xaxis   = list(title = "Year", tickfont = list(color = "#475569", size = 11)),
          yaxis   = list(title = "Coverage (%)"),
          legend  = list(orientation = "h", x = 0, y = -0.28,
                         bgcolor = "transparent", borderwidth = 0,
                         font = list(size = 9.5, color = "#1e293b"))
        )
    })

    # ── GPS TRACKING MAP — all points coloured by year, filterable ────────────
    output$map_gps <- renderLeaflet({
      base_map() %>%
        setView(lng = VT_KAUD_LON, lat = VT_KAUD_LAT, zoom = 13)
    })

    observe({
      req(length(input$sel_years) > 0)

      sel_yrs  <- as.integer(input$sel_years)
      sel_eles <- if (length(input$sel_elephant) == 0) VT_ELE_NAMES else input$sel_elephant

      df <- vt_gps_f %>%
        filter(year %in% sel_yrs, name %in% sel_eles)

      proxy <- leafletProxy(ns("map_gps"), session) %>%
        clearMarkers() %>% clearControls()

      if (nrow(df) == 0) return()

      if (nrow(df) > 8000) df <- df %>% slice_sample(n = 8000)

      for (yr in sort(sel_yrs)) {
        sub <- df %>% filter(year == yr)
        if (nrow(sub) == 0) next
        col <- unname(VT_GPS_YEAR_COLS[as.character(yr)])
        popup_txt <- paste0(
          "<b>", sub$name, "</b><br>",
          "Year: <b>", yr, "</b><br>",
          "Lat: ", round(sub$lat, 5), "  Lon: ", round(sub$lon, 5), "<br>",
          format(sub$dt, "%Y-%m-%d %H:%M UTC")
        )
        proxy <- proxy %>%
          addCircleMarkers(
            data = sub, lat = ~lat, lng = ~lon,
            radius = 3, color = col, fillColor = col,
            stroke = FALSE, fillOpacity = 0.65,
            popup = popup_txt, group = as.character(yr)
          )
      }

      proxy %>%
        addLegend(
          position = "bottomleft",
          colors   = unname(VT_GPS_YEAR_COLS[as.character(sort(sel_yrs))]),
          labels   = as.character(sort(sel_yrs)),
          title    = "Year", opacity = 0.85
        )
    })

    # ── MONTHLY HOME RANGE vs VEGETATION ──────────────────────────────────────
    output$plot_range <- renderPlotly({
      sel      <- if (length(input$range_elephants) == 0) VT_ELE_NAMES else input$range_elephants
      sel_yrs  <- if (length(input$range_years) == 0) VT_FOCUS_YEARS else as.integer(input$range_years)
      df       <- vt_monthly_range %>% filter(name %in% sel, year %in% sel_yrs)
      validate(need(nrow(df) > 0, "No data for selected elephants / years."))

      df <- df %>% mutate(month_lbl = factor(month_lbl, levels = month.abb))

      p <- plot_ly()

      for (yr in sel_yrs) {
        mc  <- vt_mod_canopy$mod_canopy[vt_mod_canopy$year == yr]
        col <- unname(VT_YEAR_COLS[as.character(yr)])
        p <- p %>% add_trace(
          x = month.abb, y = rep(mc, 12),
          type = "scatter", mode = "lines",
          fill = "tozeroy",
          fillcolor = paste0(substr(col,1,7),"18"),
          line = list(color = paste0(substr(col,1,7),"55"), width = 1, dash = "dot"),
          name = paste0("Mod.Canopy ", yr, " (", mc, "%)"),
          yaxis = "y2",
          hovertemplate = paste0("Moderate Canopy ", yr, ": ", mc, "%<extra></extra>"),
          showlegend = TRUE
        )
      }

      for (nm in sel) {
        e_col <- unname(VT_ELE_COLORS[nm])
        for (yr in sel_yrs) {
          sub <- df %>% filter(name == nm, year == yr) %>% arrange(month)
          if (nrow(sub) < 2) next
          p <- p %>% add_trace(
            data = sub,
            x = ~month_lbl, y = ~radius_km,
            type = "scatter", mode = "lines+markers",
            line   = list(color = e_col, width = 1.8),
            marker = list(color = e_col, size = 5),
            name   = paste0(nm, " ", yr),
            legendgroup = nm,
            showlegend = (yr == min(sel_yrs[sel_yrs %in% unique(df$year[df$name == nm])])),
            hovertemplate = paste0("<b>", nm, " ", yr,
                                   "</b><br>Month: %{x}<br>Range: %{y:.2f} km<extra></extra>")
          )
        }
      }

      p %>%
        light_layout() %>%
        layout(
          xaxis  = list(title = "Month", categoryorder = "array",
                        categoryarray = month.abb),
          yaxis  = list(title = "Home Range Radius (km)", side = "left"),
          yaxis2 = list(title = "Moderate Canopy (%)", overlaying = "y",
                        side = "right", showgrid = FALSE,
                        tickfont = list(color = "#64748b")),
          legend = list(orientation = "v", x = 1.08, y = 1,
                        font = list(size = 9)),
          hovermode = "x unified",
          margin = list(t = 10, l = 50, r = 110, b = 40)
        )
    })

    # ── COMBINED MAP: triangles + per-elephant dots + arrows ──────────────────
    output$map_combined <- renderLeaflet(base_map())

    observe({
      req(input$top_n_each)
      n_each   <- as.integer(input$top_n_each)
      sel_eles <- if (length(input$ele_pick) == 0) VT_ELE_NAMES else input$ele_pick

      tri_pts <- vt_get_top_all(1)   # one point per year

      proxy <- leafletProxy(ns("map_combined"), session) %>%
        clearMarkers() %>% clearShapes() %>% clearControls()

      # ── Layer 1: triangles only ──
      yr_seq <- sort(unique(tri_pts$year))
      for (yr in yr_seq) {
        r      <- tri_pts %>% filter(year == yr)
        yr_col <- unname(VT_YEAR_COLS[as.character(yr)])
        tip    <- paste0(
          "<b>All-Elephant Top Hotspot — ", yr, "</b><br>",
          "Lat: ", r$lat, "N  Lon: ", r$lon, "E<br>",
          "Visits: <b>", r$visits, "</b><br>",
          r$elephants
        )
        proxy <- proxy %>%
          addMarkers(
            lat   = r$lat, lng = r$lon,
            icon  = vt_make_triangle_icon(yr_col, 26),
            popup = tip
          ) %>%
          addLabelOnlyMarkers(
            lat = r$lat + 0.0015, lng = r$lon,
            label = as.character(yr),
            labelOptions = labelOptions(
              noHide = TRUE, textOnly = TRUE, direction = "top",
              style  = list(
                color = yr_col, "font-weight" = "bold", "font-size" = "12px",
                "text-shadow" = "0 0 4px #fff, 0 0 4px #fff"
              )
            )
          )
      }

      # ── Layer 2: per-elephant dots + directional arrows between years ──
      for (nm in sel_eles) {
        e_col <- unname(VT_ELE_COLORS[nm])
        e_pts <- vt_get_top_each(nm, n_each)
        if (nrow(e_pts) == 0) next

        for (i in seq_len(nrow(e_pts))) {
          r   <- e_pts[i, ]
          tip <- paste0(
            "<b style='color:", e_col, "'>", nm, "</b>",
            " — ", r$year,
            if (r$rank == 1) "  \u2605 Most Visited" else paste0("  #", r$rank), "<br>",
            "Visits: <b>", r$visits, "</b>"
          )
          proxy <- proxy %>%
            addCircleMarkers(
              lat = r$lat, lng = r$lon,
              radius    = if (r$rank == 1) 9 else 6,
              color     = "#ffffff", weight = 1.5,
              fillColor = e_col, fillOpacity = 0.88,
              popup     = tip
            )
        }

        rank1_pts <- e_pts %>% filter(rank == 1) %>% arrange(year)
        yr_avail  <- sort(unique(rank1_pts$year))
        if (length(yr_avail) >= 2) {
          for (i in seq_len(length(yr_avail) - 1)) {
            from <- rank1_pts %>% filter(year == yr_avail[i])
            to   <- rank1_pts %>% filter(year == yr_avail[i + 1])
            if (nrow(from) == 0 || nrow(to) == 0) next

            dlat  <- to$lat - from$lat
            dlon  <- to$lon - from$lon
            dist  <- sqrt(dlat^2 + dlon^2)
            shrink <- min(0.0007, dist * 0.15)
            frac  <- shrink / dist

            lat_s <- from$lat + dlat * frac
            lon_s <- from$lon + dlon * frac
            lat_e <- to$lat   - dlat * frac
            lon_e <- to$lon   - dlon * frac

            proxy <- proxy %>%
              addPolylines(
                lat     = c(lat_s, lat_e),
                lng     = c(lon_s, lon_e),
                color   = e_col, weight = 2.2, opacity = 0.9,
                options = pathOptions(interactive = FALSE)
              )

            bearing  <- atan2(dlon, dlat)
            wing_len <- dist * 0.18
            angle1   <- bearing + (140 * pi / 180)
            angle2   <- bearing - (140 * pi / 180)
            mid_lat  <- (lat_s + lat_e) / 2
            mid_lon  <- (lon_s + lon_e) / 2
            w1_lat   <- mid_lat + wing_len * cos(angle1)
            w1_lon   <- mid_lon + wing_len * sin(angle1)
            w2_lat   <- mid_lat + wing_len * cos(angle2)
            w2_lon   <- mid_lon + wing_len * sin(angle2)

            proxy <- proxy %>%
              addPolylines(
                lat     = c(w1_lat, mid_lat, w2_lat),
                lng     = c(w1_lon, mid_lon, w2_lon),
                color   = e_col, weight = 2.2, opacity = 0.9,
                options = pathOptions(interactive = FALSE)
              )
          }
        }
      }

      # ── Dynamic map legend ──
      sel_legend_cols  <- unname(VT_ELE_COLORS[sel_eles])
      proxy %>%
        addLegend(
          position = "bottomleft",
          colors   = sel_legend_cols,
          labels   = sel_eles,
          title    = "\U0001F418 Elephant hotspot",
          opacity  = 0.9
        )

    }) |> bindEvent(input$ele_pick, input$top_n_each, ignoreInit = FALSE)

    # Legend strip below combined map
    output$combined_legend <- renderUI({
      tags$div(
        style = "padding:5px 12px;background:#f8fafc;border-top:1px solid #e2e8f0;
                 font-size:10px;color:#475569;display:flex;gap:14px;flex-wrap:wrap",
        tags$span(
          HTML("&#9650;"),
          tags$b(" Triangle"), " = All-elephant #1 hotspot per year (\u25b2 coloured by year — \U0001F7E0 2024  \U0001F535 2025  \U0001F7E2 2026)"
        ),
        tags$span(
          tags$span(style = "display:inline-block;width:10px;height:10px;border-radius:50%;
                             background:#888;border:1.5px solid #fff;margin-right:3px;vertical-align:middle"),
          "Dot + chevron on line = per-elephant most-visited spot; chevron arrow on line shows direction 2024 \u2192 2025 \u2192 2026 (unique colour per elephant)"
        )
      )
    })

    # Info strip below combined map
    output$ele_info <- renderUI({
      sel <- if (length(input$ele_pick) == 0) VT_ELE_NAMES else input$ele_pick
      tots <- vt_gps_f %>% filter(name %in% sel) %>%
        group_by(name) %>% summarise(n = n(), .groups = "drop") %>%
        mutate(lbl = paste0(name, " (", formatC(n, format="d", big.mark=","), ")"))
      tags$div(class = "note",
        paste0("Selected: ", paste(tots$lbl, collapse = "  \u00b7  "))
      )
    })

    # ── GPS FIX COUNTS CHART ──────────────────────────────────────────────────
    output$fix_chart <- renderPlotly({
      df        <- vt_fix_counts
      all_names <- sort(unique(df$name))

      p <- plot_ly()
      for (yr in c("2024","2025","2026")) {
        sub <- df %>% filter(year == yr)
        if (nrow(sub) == 0) next
        p <- add_bars(p,
          data = sub, x = ~name, y = ~fixes, name = yr,
          marker = list(color = VT_YEAR_COLS[yr], opacity = 0.88,
                        line = list(width = 0.5, color = "#00000022")),
          hovertemplate = paste0("<b>%{x}</b><br>", yr,
                                 ": <b>%{y}</b> fixes<extra></extra>")
        )
      }

      p %>%
        light_layout() %>%
        layout(
          barmode = "group",
          xaxis   = list(title = "", tickangle = -35,
                         categoryorder = "array", categoryarray = all_names,
                         tickfont = list(color = "#475569", size = 10)),
          yaxis   = list(title = "GPS fixes"),
          legend  = list(orientation = "h", x = 1, xanchor = "right", y = 1.12,
                         bgcolor = "transparent", borderwidth = 0,
                         font = list(size = 10, color = "#1e293b")),
          margin  = list(t = 5, l = 50, r = 10, b = 80)
        )
    })
  })
}


# ═══════════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════════
#  TAB 5 — TRACKING   (source: tracking1.R)
# ═══════════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════════

# ── CSS for dashboard ──────────────────────────────────────────────
tk_veg_css <- "
.tk-scope *,.tk-scope *::before,.tk-scope *::after{box-sizing:border-box;margin:0;padding:0}
.tk-scope{background:#f1f5f9;color:#1e293b;font-family:system-ui,-apple-system,sans-serif;
          font-size:13px;overflow-x:hidden}
.tk-scope #page-hdr{background:#ffffff;border-bottom:2px solid #e2e8f0;padding:10px 16px;
          display:flex;align-items:center;gap:10px;box-shadow:0 1px 4px #0000000d}
.tk-scope #page-hdr h1{font-size:14px;font-weight:700;color:#166534;letter-spacing:.04em}
.tk-scope #page-hdr .sub{font-size:11px;color:#64748b}
.tk-scope .dash-panel{background:#ffffff;border:1px solid #e2e8f0;border-radius:8px;margin:6px;
            overflow:hidden;display:flex;flex-direction:column;box-shadow:0 1px 3px #0000000a}
.tk-scope .panel-hdr{background:#f8fafc;border-bottom:1px solid #e2e8f0;padding:7px 12px;
           display:flex;align-items:center;gap:8px;flex-wrap:wrap;flex-shrink:0}
.tk-scope .panel-title{font-size:11px;font-weight:700;text-transform:uppercase;
             letter-spacing:.07em;color:#166534;white-space:nowrap}
.tk-scope .panel-body{padding:0;flex:1;min-height:0}
.tk-scope .form-control,.tk-scope select{background:#ffffff!important;border:1px solid #cbd5e1!important;
                     color:#1e293b!important;font-size:11px!important;border-radius:4px;
                     padding:3px 7px;height:26px}
.tk-scope label{color:#64748b!important;font-size:10px}
.tk-scope .radio-inline{margin-right:10px!important}
.tk-scope .radio-inline label{color:#475569!important;font-size:11px!important}
.tk-scope .leaflet-tooltip{background:#ffffffee;border:1px solid #e2e8f0;color:#1e293b;
                 font-size:11px;padding:5px 8px;border-radius:4px;box-shadow:0 2px 8px #0000001a}
.tk-scope .leaflet-tooltip-arrow{display:none}
.tk-scope .note{font-size:9px;color:#94a3b8;padding:3px 12px 5px;font-style:italic;
      background:#f8fafc;border-top:1px solid #f1f5f9}

/* Updated Alignment CSS */
.tk-scope .align-left-radio .shiny-options-group {display: flex !important;
    flex-wrap: nowrap !important;
    align-items: center;
    gap: 18px; 
}
.tk-scope .radio-group-container .shiny-options-group {
    display: flex;
    flex-direction: row;
    gap: 15px;             /* Controls space between items */
    margin-left: 0 !important;
    padding-left: 0 !important;
}
.tk-scope .align-left-radio .radio-inline {
    margin-left: 0 !important;
    padding-left: 0 !important;

.tk-scope .align-left-radio .radio-inline:first-child {
    padding-left: 0 !important;
}
"

# ── Module UI ────────────────────────────────────────────────────────────────
tracking_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(tk_veg_css))),

    div(class = "tk-scope",
        titlePanel("Elephant Tracker & Climate Dashboard"),
        br(),

        # Elephant Selector (all 13 elephants)
        fluidRow(
          column(width = 12,
                 selectInput(ns("elephant"), "Select Elephant(s):",
                             choices = c("Gothami", "recollared female", "female_1", "Mina", "Talatha",
                                         "Dona", "Dewmi", "Rahu", "Tara Devi", "Kasun",
                                         "Wilmini", "Pazhani", "Damien"),
                             selected = c("Gothami", "Talatha"),
                             multiple = TRUE,
                             width = "100%"))
        ),
        br(),

        # Top Row: Map (widened)
        fluidRow(
          column(width = 12,
                 h4("GPS Tracking Map (Temperature)"),
                 leafletOutput(ns("map"), height = 450))
        ),

        hr(),

        # Middle Row: Controls
        fluidRow(
          column(width = 2,
                 dateRangeInput(ns("date_range"), "Select Date Range:",
                                start = "2024-06-01", end = Sys.Date()),
                 checkboxInput(ns("show_imputed"), "Show Imputed Data Points", value = TRUE)),
          column(width = 2,
                 uiOutput(ns("temp_range_ui"))),
          column(width = 2,
                 radioButtons(ns("map_type"), "Map Type:",
                              choices = c("OpenStreetMap" = "OpenStreetMap",
                                          "Satellite" = "Esri.WorldImagery",
                                          "Dark" = "CartoDB.DarkMatter",
                                          "Light" = "CartoDB.Positron",
                                          "Topo" = "OpenTopoMap"),
                              selected = "OpenStreetMap")),
          column(width = 2,
                 helpText("Map Legend:"),
                 HTML("<div style='background: linear-gradient(to right, #FF6B6B, #FFD93D, #6BCB77); width: 100%; height: 10px; margin-bottom: 5px;'></div>"),
                 HTML("<span style='float: left; font-size: 12px;'>Low</span>"),
                 HTML("<span style='float: right; font-size: 12px;'>High</span><br><br>"),
                 HTML("<span style='color:blue; font-size: 16px;'>\u25cf</span><span style='color:black; font-size: 16px;'>\u25cf</span> Current (By Elephant)")),
          column(width = 2,
                 uiOutput(ns("time_slider_ui")))
        ),

        hr(),

        # Row: Climate Data Time Series Plot
        fluidRow(
          column(width = 12,
                 h4("Climate Data — Selected Date Range"),
                 
                 div(class = "align-left-radio",
                     radioButtons(ns("climate_type"), "Select Data Type:",
                                  choices = c("Temperature" = "temp", "Rainfall" = "rainfall",
                                              "Wind Speed" = "wind_speed"),
                                  selected = "temp", inline = FALSE) # Set inline = FALSE
                 ),
                 # These remain outside so they don't get the negative margin/alignment
                 helpText("Drag a box to highlight points."),
                 plotlyOutput(ns("climate_plot"), height = 380))
        )
    )
  )
}
       

# ── Module Server ──────────────────────────────────────────────────────────────
tracking_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helpers: human-readable label and unit for the selected climate variable
    climate_label <- function(type) {
      switch(type,
             temp = "Temperature",
             rainfall = "Rainfall",
             wind_speed = "Wind Speed",
             type)
    }
    climate_unit <- function(type) {
      switch(type,
             temp = "\u00b0C",
             rainfall = "mm",
             wind_speed = "m/s",
             "")
    }

    # Elephant color palette
    lonlat_palette_hex <- c(
      "#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231",
      "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe",
      "#008080", "#e6beff", "#9a6324", "#fffac8", "#800000"
    )
    ele_all_names <- c("Gothami", "recollared female", "female_1", "Mina", "Talatha",
                       "Dona", "Dewmi", "Rahu", "Tara Devi", "Kasun",
                       "Wilmini", "Pazhani", "Damien")
    lonlat_colors <- setNames(lonlat_palette_hex[seq_along(ele_all_names)], ele_all_names)

    # Load Elephant Data
    track_data <- reactive({
      csv_file <- "kaudulla_elephants_clean.csv"
      validate(need(file.exists(csv_file), paste0(csv_file, " not found.")))

      req(length(input$elephant) > 0)

      df <- read_csv(csv_file, show_col_types = FALSE)
      df <- df %>% filter(name %in% input$elephant)

      df$date <- as.Date(parse_date_time(df$date, orders = c("mdy", "ymd", "Ymd"), quiet = TRUE))
      df$datetime <- parse_date_time(paste(as.character(df$date), df$time),
                                     orders = c("Ymd HMS", "Ymd HM"), tz = "UTC", quiet = TRUE)
      df$datetime <- floor_date(df$datetime, unit = "hour")
      df
    })

    # Load Climate Data
    climate_data <- reactive({
      csv_file <- "POWER_Point_Hourly_kawudulla_new.csv"
      validate(need(file.exists(csv_file), paste0(csv_file, " not found.")))

      df <- read_csv(csv_file, skip = 12, show_col_types = FALSE)
      df <- df %>%
        rename(year = YEAR, month = MO, day = DY, hour = HR,
               temp = T2M, rainfall = PRECTOTCORR)

      wind_col <- intersect(c("WS10M", "WS2M", "WS50M"), names(df))
      if (length(wind_col) > 0) {
        df$wind_speed <- df[[wind_col[1]]]
      } else {
        df$wind_speed <- NA_real_
        warning("No recognized wind speed column (WS10M/WS2M/WS50M) found in climate CSV.")
      }

      df$date <- make_date(df$year, df$month, df$day)
      df$datetime <- floor_date(make_datetime(df$year, df$month, df$day, df$hour, tz = "UTC"), unit = "hour")
      df <- df %>% filter(!is.na(datetime))
      df
    })

    observe({
      df <- track_data()
      req(nrow(df) > 0)
      updateDateRangeInput(session, "date_range",
                           start = min(df$date, na.rm = TRUE),
                           end = max(df$date, na.rm = TRUE),
                           min = min(df$date, na.rm = TRUE),
                           max = max(df$date, na.rm = TRUE))
    })

    output$temp_range_ui <- renderUI({
      df <- climate_data()
      req(nrow(df) > 0)
      temp_min <- min(df$temp, na.rm = TRUE)
      temp_max <- max(df$temp, na.rm = TRUE)
      sliderInput(ns("temp_range"), "Temperature Range (\u00b0C):",
                  min = floor(temp_min), max = ceiling(temp_max),
                  value = c(floor(temp_min), ceiling(temp_max)), step = 1)
    })

    base_dataset <- reactive({
      t_df <- track_data() %>% filter(date >= input$date_range[1], date <= input$date_range[2])
      if (!input$show_imputed) t_df <- t_df %>% filter(imputed == FALSE)

      c_df <- climate_data() %>% dplyr::select(datetime, temp, rainfall, wind_speed)
      merged_df <- inner_join(t_df, c_df, by = "datetime") %>% arrange(datetime)

      validate(need(nrow(merged_df) > 0, "No matching tracking/climate data in this range."))
      return(merged_df)
    })

    filtered_dataset <- reactive({
      df <- base_dataset()
      req(nrow(df) > 0)
      req(input$temp_range)

      df_filtered <- df %>% filter(temp >= input$temp_range[1], temp <= input$temp_range[2])
      validate(need(nrow(df_filtered) > 0, "No data in selected temperature range."))
      return(df_filtered)
    })

    current_max_time <- reactive({
      df <- filtered_dataset()
      unique_times <- sort(unique(df$datetime))
      idx <- if (is.null(input$current_time)) length(unique_times) else input$current_time
      idx <- min(idx, length(unique_times))
      unique_times[idx]
    })

    shared_dataset <- reactive({
      df <- filtered_dataset()
      max_t <- current_max_time()
      df_sub <- df %>% filter(datetime <= max_t)
      SharedData$new(df_sub, key = ~datetime)
    })

    current_point <- reactive({
      df <- filtered_dataset()
      max_t <- current_max_time()
      df %>% filter(datetime <= max_t) %>% group_by(name) %>% slice_max(datetime, n=1) %>% ungroup()
    })

    output$time_slider_ui <- renderUI({
      df <- filtered_dataset()
      n_steps <- length(unique(df$datetime))
      sliderInput(ns("current_time"), "Timeline — drag or press \u25b6 Play",
                  min = 1, max = n_steps, value = 1, step = 1,
                  animate = animationOptions(interval = 1000, loop = FALSE), width = "100%")
    })

    # ── MAP RENDERING ──────────────────────────────────────────────────────────
    output$map <- renderLeaflet({
      df <- filtered_dataset() %>% filter(!is.na(lon), !is.na(lat))
      validate(need(nrow(df) > 0, "No track data."))

      leaflet() %>%
        addProviderTiles(input$map_type) %>%
        addMapPane("climate_dots", zIndex = 410) %>%
        addMapPane("track_lines", zIndex = 420) %>%
        addMapPane("current_dots", zIndex = 430) %>%
        fitBounds(lng1 = min(df$lon), lat1 = min(df$lat),
                  lng2 = max(df$lon), lat2 = max(df$lat))
    })

    observe({
      full_df <- filtered_dataset() %>% filter(!is.na(lon), !is.na(lat))
      req(nrow(full_df) > 0)

      max_t <- current_max_time()
      trail_df <- full_df %>% filter(datetime <= max_t)
      curr_df <- current_point() %>% filter(!is.na(lon), !is.na(lat))

      c_type <- input$climate_type

      pal <- colorNumeric(palette = c("#FF6B6B", "#FFD93D", "#6BCB77"),
                          domain = full_df[[c_type]], na.color = "transparent")

      proxy <- leafletProxy(ns("map"), session) %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls()

      # Legend
      proxy <- proxy %>%
        addLegend(pal = pal,
                  values = full_df[[c_type]],
                  title = paste0(climate_label(c_type), " (", climate_unit(c_type), ")"),
                  position = "bottomright")

      elephants <- unique(trail_df$name)
      marker_colors <- c("#0000FF", "#000000", "#800080", "#FF0000", "#006400", "#FF8C00",
                         "#FF1493", "#4B0082", "#2E8B57", "#DAA520", "#4682B4", "#D2691E", "#708090")

      for(i in seq_along(elephants)) {
        e <- elephants[i]
        e_color <- marker_colors[(i - 1) %% length(marker_colors) + 1]

        # ── Segment the track based on time gaps ────────────────────────
        e_df <- trail_df %>%
          filter(name == e) %>%
          arrange(datetime) %>%
          mutate(
            gap_hrs = as.numeric(difftime(datetime, lag(datetime), units = "hours")),
            is_break = coalesce(gap_hrs > 1.5, FALSE),
            seg_id = cumsum(is_break)
          )

        segments <- split(e_df, e_df$seg_id)
        for (seg in segments) {
          if(nrow(seg) > 1) {
            proxy <- proxy %>% addPolylines(data = seg, lng = ~lon, lat = ~lat,
                                            color = e_color, weight = 3, opacity = 1.0,
                                            options = pathOptions(pane = "track_lines"))
          }
        }

        hover_html <- lapply(paste0(
          "<b>Elephant:</b> ", e_df$name, "<br/>",
          "<b>Date:</b> ", e_df$date, "<br/>",
          "<b>Time:</b> ", e_df$time, "<br/>",
          "<b>Temp:</b> ", round(e_df$temp, 1), " &deg;C<br/>",
          "<b>Rainfall:</b> ", round(e_df$rainfall, 1), " mm<br/>",
          "<b>Wind Speed:</b> ", round(e_df$wind_speed, 1), " m/s"
        ), htmltools::HTML)

        proxy <- proxy %>% addCircleMarkers(data = e_df, lng = ~lon, lat = ~lat,
                                            radius = 4, color = ~pal(e_df[[c_type]]),
                                            stroke = FALSE, fillOpacity = 0.8,
                                            label = hover_html,
                                            labelOptions = labelOptions(style = list("padding" = "5px")),
                                            options = pathOptions(pane = "climate_dots"))

        e_curr <- curr_df %>% filter(name == e)
        if(nrow(e_curr) > 0) {
          curr_hover_html <- lapply(paste0(
            "<b>Current Location:</b> ", e_curr$name, "<br/>",
            "<b>Date:</b> ", e_curr$date, "<br/>",
            "<b>Time:</b> ", e_curr$time, "<br/>",
            "<b>Temp:</b> ", round(e_curr$temp, 1), " &deg;C<br/>",
            "<b>Rainfall:</b> ", round(e_curr$rainfall, 1), " mm<br/>",
            "<b>Wind Speed:</b> ", round(e_curr$wind_speed, 1), " m/s"
          ), htmltools::HTML)

          proxy <- proxy %>% addCircleMarkers(data = e_curr, lng = ~lon, lat = ~lat,
                                              radius = 9, color = "white", fillColor = e_color,
                                              stroke = TRUE, weight = 2, fillOpacity = 1,
                                              label = curr_hover_html,
                                              labelOptions = labelOptions(style = list("padding" = "5px")),
                                              options = pathOptions(pane = "current_dots"))
        }
      }
    })

    observeEvent(input$map_type, {
      leafletProxy(ns("map")) %>%
        clearTiles() %>%
        addProviderTiles(input$map_type)
    }, ignoreInit = TRUE)

    # ── PLOTS ──────────────────────────────────────────────────────────────────
    output$climate_plot <- renderPlotly({
      full_df <- filtered_dataset()
      validate(need(nrow(full_df) > 0, "No data available."))
      curr_climate <- current_point() %>% slice(1)

      if (input$climate_type == "temp") {
        plot_ly(shared_dataset(), x = ~datetime, y = ~temp, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#ff7f0e', width = 2), marker = list(size = 5, color = '#ff7f0e'),
                text = ~paste("<b>DateTime:</b>", format(datetime, "%Y-%m-%d %H:%M"), "<br><b>Temperature:</b>", round(temp, 2), "\u00b0C"), hoverinfo = "text", name = 'Temp') %>%
          add_markers(data = curr_climate, inherit = FALSE, x = ~datetime, y = ~temp, marker = list(size = 12, color = "white", line = list(color = "black", width = 2)), hoverinfo = "skip", showlegend = FALSE) %>%
          layout(xaxis = list(title = "DateTime", showgrid = TRUE, gridcolor = "#e0e0e0"), yaxis = list(title = "Temperature (\u00b0C)", showgrid = TRUE, gridcolor = "#e0e0e0"), margin = list(t = 10, b = 40, l = 50, r = 20), dragmode = "select", plot_bgcolor = "#f8f9fa", paper_bgcolor = "white") %>% highlight(on = "plotly_selected", off = "plotly_deselect", color = "red", opacity = 0.8)
      } else if (input$climate_type == "rainfall") {
        plot_ly(shared_dataset(), x = ~datetime, y = ~rainfall, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#2E86AB', width = 2), marker = list(size = 5, color = '#2E86AB'),
                text = ~paste("<b>DateTime:</b>", format(datetime, "%Y-%m-%d %H:%M"), "<br><b>Rainfall:</b>", round(rainfall, 2), "mm"), hoverinfo = "text", name = 'Rain') %>%
          add_markers(data = curr_climate, inherit = FALSE, x = ~datetime, y = ~rainfall, marker = list(size = 12, color = "white", line = list(color = "black", width = 2)), hoverinfo = "skip", showlegend = FALSE) %>%
          layout(xaxis = list(title = "DateTime", showgrid = TRUE, gridcolor = "#e0e0e0"), yaxis = list(title = "Rainfall (mm)", showgrid = TRUE, gridcolor = "#e0e0e0"), margin = list(t = 10, b = 40, l = 50, r = 20), dragmode = "select", plot_bgcolor = "#f8f9fa", paper_bgcolor = "white") %>% highlight(on = "plotly_selected", off = "plotly_deselect", color = "cyan", opacity = 0.8)
      } else {
        plot_ly(shared_dataset(), x = ~datetime, y = ~wind_speed, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#6BCB77', width = 2), marker = list(size = 5, color = '#6BCB77'),
                text = ~paste("<b>DateTime:</b>", format(datetime, "%Y-%m-%d %H:%M"), "<br><b>Wind Speed:</b>", round(wind_speed, 2), "m/s"), hoverinfo = "text", name = 'Wind Speed') %>%
          add_markers(data = curr_climate, inherit = FALSE, x = ~datetime, y = ~wind_speed, marker = list(size = 12, color = "white", line = list(color = "black", width = 2)), hoverinfo = "skip", showlegend = FALSE) %>%
          layout(xaxis = list(title = "DateTime", showgrid = TRUE, gridcolor = "#e0e0e0"), yaxis = list(title = "Wind Speed (m/s)", showgrid = TRUE, gridcolor = "#e0e0e0"), margin = list(t = 10, b = 40, l = 50, r = 20), dragmode = "select", plot_bgcolor = "#f8f9fa", paper_bgcolor = "white") %>% highlight(on = "plotly_selected", off = "plotly_deselect", color = "green", opacity = 0.8)
      }
    })


    # ── DYNAMIC TITLES (react to Select Data Type) ─────────────────────────────
    output$lat_lon_section_title <- renderUI({
      lbl <- climate_label(input$climate_type)
      h4(paste0(lbl, " vs Location — Does ", lbl, " Relate to Elephant Movement?"))
    })

    output$lat_plot_title <- renderUI({
      h5(paste0(climate_label(input$climate_type), " vs Latitude"))
    })

    output$lon_plot_title <- renderUI({
      h5(paste0(climate_label(input$climate_type), " vs Longitude"))
    })

    # ── CLIMATE vs LAT / CLIMATE vs LON SCATTER PLOTS ──────────────────────────
    output$temp_lat_plot <- renderPlotly({
      df <- filtered_dataset()
      max_t <- current_max_time()
      df_sub <- df %>% filter(datetime <= max_t)
      validate(need(nrow(df_sub) > 0, "No data available."))
      curr <- current_point()
      
      c_type <- input$climate_type
      lbl <- climate_label(c_type)
      unit <- climate_unit(c_type)
      y_vals <- df_sub[[c_type]]
      curr_y_vals <- curr[[c_type]]
      
      plot_ly(df_sub, x = ~lat, y = y_vals, color = ~name, type = 'scatter', mode = 'markers',
              marker = list(size = 6, opacity = 0.7),
              text = ~paste("<b>Elephant:</b>", name, "<br><b>Date:</b>", date,
                            "<br><b>Time:</b>", format(datetime, "%H:%M"),
                            "<br><b>Lat:</b>", round(lat, 5), paste0("<br><b>", lbl, ":</b>"), round(y_vals, 2), unit),
              hoverinfo = "text") %>%
        add_markers(data = curr, inherit = FALSE, x = ~lat, y = curr_y_vals,
                    marker = list(size = 12, color = "white", line = list(color = "black", width = 2)),
                    text = ~paste("<b>Current —</b>", name, "<br><b>Time:</b>", format(datetime, "%H:%M"),
                                  "<br><b>Lat:</b>", round(lat, 5), paste0("<br><b>", lbl, ":</b>"), round(curr_y_vals, 2), unit),
                    hoverinfo = "text", showlegend = FALSE) %>%
        layout(xaxis = list(title = "Latitude", showgrid = TRUE, gridcolor = "#e0e0e0"),
               yaxis = list(title = paste0(lbl, " (", unit, ")"), showgrid = TRUE, gridcolor = "#e0e0e0"),
               margin = list(t = 10, b = 40, l = 50, r = 20),
               plot_bgcolor = "#f8f9fa", paper_bgcolor = "white")
    })
    
    output$temp_lon_plot <- renderPlotly({
      df <- filtered_dataset()
      max_t <- current_max_time()
      df_sub <- df %>% filter(datetime <= max_t)
      validate(need(nrow(df_sub) > 0, "No data available."))
      curr <- current_point()
      
      c_type <- input$climate_type
      lbl <- climate_label(c_type)
      unit <- climate_unit(c_type)
      y_vals <- df_sub[[c_type]]
      curr_y_vals <- curr[[c_type]]
      
      plot_ly(df_sub, x = ~lon, y = y_vals, color = ~name, type = 'scatter', mode = 'markers',
              marker = list(size = 6, opacity = 0.7),
              text = ~paste("<b>Elephant:</b>", name, "<br><b>Date:</b>", date,
                            "<br><b>Time:</b>", format(datetime, "%H:%M"),
                            "<br><b>Lon:</b>", round(lon, 5), paste0("<br><b>", lbl, ":</b>"), round(y_vals, 2), unit),
              hoverinfo = "text") %>%
        add_markers(data = curr, inherit = FALSE, x = ~lon, y = curr_y_vals,
                    marker = list(size = 12, color = "white", line = list(color = "black", width = 2)),
                    text = ~paste("<b>Current —</b>", name, "<br><b>Time:</b>", format(datetime, "%H:%M"),
                                  "<br><b>Lon:</b>", round(lon, 5), paste0("<br><b>", lbl, ":</b>"), round(curr_y_vals, 2), unit),
                    hoverinfo = "text", showlegend = FALSE) %>%
        layout(xaxis = list(title = "Longitude", showgrid = TRUE, gridcolor = "#e0e0e0"),
               yaxis = list(title = paste0(lbl, " (", unit, ")"), showgrid = TRUE, gridcolor = "#e0e0e0"),
               margin = list(t = 10, b = 40, l = 50, r = 20),
               plot_bgcolor = "#f8f9fa", paper_bgcolor = "white")
    })
    
    output$temp_lat_cor <- renderPrint({
      df <- filtered_dataset()
      max_t <- current_max_time()
      df <- df %>% filter(datetime <= max_t)
      validate(need(nrow(df) > 1, "Not enough data to compute a correlation."))
      
      c_type <- input$climate_type
      lbl <- climate_label(c_type)
      y_vals <- df[[c_type]]
      
      overall_r <- cor(y_vals, df$lat, use = "complete.obs")
      cat(sprintf("Overall correlation (%s vs Latitude):  r = %.3f\n", lbl, overall_r))
      
      if (length(unique(df$name)) > 1) {
        per_elephant <- df %>%
          group_by(name) %>%
          summarise(r = suppressWarnings(cor(.data[[c_type]], lat, use = "complete.obs")), .groups = "drop")
        cat("Per elephant:\n")
        for (i in seq_len(nrow(per_elephant))) {
          cat(sprintf("  %-20s r = %.3f\n", per_elephant$name[i], per_elephant$r[i]))
        }
      }
    })
    
    output$temp_lon_cor <- renderPrint({
      df <- filtered_dataset()
      max_t <- current_max_time()
      df <- df %>% filter(datetime <= max_t)
      validate(need(nrow(df) > 1, "Not enough data to compute a correlation."))
      
      c_type <- input$climate_type
      lbl <- climate_label(c_type)
      y_vals <- df[[c_type]]
      
      overall_r <- cor(y_vals, df$lon, use = "complete.obs")
      cat(sprintf("Overall correlation (%s vs Longitude):  r = %.3f\n", lbl, overall_r))
      
      if (length(unique(df$name)) > 1) {
        per_elephant <- df %>%
          group_by(name) %>%
          summarise(r = suppressWarnings(cor(.data[[c_type]], lon, use = "complete.obs")), .groups = "drop")
        cat("Per elephant:\n")
        for (i in seq_len(nrow(per_elephant))) {
          cat(sprintf("  %-20s r = %.3f\n", per_elephant$name[i], per_elephant$r[i]))
        }
      }
    })
  })
}


# ═══════════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════════
#  TOP-LEVEL APP — 5 tabs
# ═══════════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════════

ui <- fluidPage(
  tags$style(HTML(".tab-content { padding-top: 6px; }")),
  titlePanel("Kaudulla NP"),

  tabsetPanel(
    id = "main_tabs",
    tabPanel("Dona & Recollared",   dona_recollared_ui("tab_dona")),
    tabPanel("Density & Climate",   density_climate_ui("tab_density")),
    tabPanel("Day / Night",         day_night_ui("tab_daynight")),
    tabPanel("Vegetation Tracking", vegetation_tracking_ui("tab_veg")),
    tabPanel("Tracking",            tracking_ui("tab_tracking"))
  )
)

server <- function(input, output, session) {
  dona_recollared_server("tab_dona")
  density_climate_server("tab_density")
  day_night_server("tab_daynight")
  vegetation_tracking_server("tab_veg")
  tracking_server("tab_tracking")
}

shinyApp(ui = ui, server = server)
