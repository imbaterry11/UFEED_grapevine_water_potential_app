# UFEED Grapevine Water Potential Explorer
# Run with: shiny::runApp("UFEED_water_potential_app")

required_packages <- c(
  "shiny", "dplyr", "lubridate", "readr", "leaflet", "plotly", "htmltools"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Please install the missing package(s) before running this app: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(shiny)
library(dplyr)
library(lubridate)
library(readr)
library(leaflet)
library(plotly)
library(htmltools)

# -----------------------------------------------------------------------------
# Data loading and preparation
# -----------------------------------------------------------------------------

DATA_URL <- Sys.getenv(
  "UFEED_DATA_URL",
  unset = "https://nextcloud.inrae.fr/public.php/dav/files/bpxB7kscDkz37fT/?accept=zip"
)

DATA_PATH <- Sys.getenv(
  "UFEED_DATA_PATH",
  unset = "all_data_application.csv"
)

CURRENT_SOURCE <- Sys.getenv(
  "UFEED_CURRENT_SOURCE",
  unset = "2026"
)

download.file(
  url = DATA_URL,
  destfile = DATA_PATH,
  mode = "wb",
  quiet = FALSE
)

format_time_label <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "Predawn / not applicable",
    TRUE ~ paste0(sprintf("%02d", as.integer(round(x))), ":00")
  )
}

mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

min_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else min(x)
}

max_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else max(x)
}

format_time_band <- function(min_time, max_time) {
  dplyr::case_when(
    is.na(min_time) & is.na(max_time) ~ "Predawn / not applicable",
    min_time == max_time ~ format_time_label(min_time),
    TRUE ~ paste0(format_time_label(min_time), "-", format_time_label(max_time))
  )
}

hex_to_rgba <- function(hex, alpha = 0.16) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d, %d, %d, %.2f)", rgb[1, 1], rgb[2, 1], rgb[3, 1], alpha)
}

load_ufeed_data <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Cannot find the data file: ", path,
      "\nPlace all_data_application.csv in the same folder as app.R, ",
      "or set the UFEED_DATA_PATH environment variable.",
      call. = FALSE
    )
  }
  
  dat <- readr::read_csv(path, show_col_types = FALSE)
  
  required_cols <- c(
    "Date", "lon", "lat", "y_pred", "ps_type",
    "time_measured", "T2M", "T2M_MAX", "T2M_MIN", "source"
  )
  
  missing_cols <- setdiff(required_cols, names(dat))
  if (length(missing_cols) > 0) {
    stop(
      "The input data is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!("y_pred_NN" %in% names(dat))) {
    dat$y_pred_NN <- NA_real_
  }
  
  dat %>%
    mutate(
      Date = as.Date(Date),
      lon = as.numeric(lon),
      lat = as.numeric(lat),
      y_pred = as.numeric(y_pred),
      y_pred_NN = as.numeric(y_pred_NN),
      ps_type = as.integer(ps_type),
      time_measured = as.numeric(time_measured),
      T2M = as.numeric(T2M),
      T2M_MAX = as.numeric(T2M_MAX),
      T2M_MIN = as.numeric(T2M_MIN),
      source = as.character(source),
      ps_label = case_when(
        ps_type == 0 ~ "Predawn water potential (pd)",
        ps_type == 1 ~ "Midday stem water potential (md)",
        TRUE ~ paste("Unknown type", ps_type)
      ),
      time_label = format_time_label(time_measured),
      location_id = paste0(round(lon, 6), " | ", round(lat, 6))
    ) %>%
    filter(!is.na(Date), !is.na(lon), !is.na(lat), !is.na(y_pred) | !is.na(y_pred_NN))
}

ufeed_data <- load_ufeed_data(DATA_PATH)

source_choices <- sort(unique(ufeed_data$source))
time_choices <- c(
  "Predawn / not applicable",
  sort(unique(ufeed_data$time_label[ufeed_data$time_label != "Predawn / not applicable"]))
)
time_choices <- time_choices[time_choices %in% unique(ufeed_data$time_label)]

x_limit_data <- ufeed_data %>% filter(source == CURRENT_SOURCE)
if (nrow(x_limit_data) == 0) {
  x_limit_data <- ufeed_data
}
x_min <- min(x_limit_data$Date, na.rm = TRUE)
x_max <- max(x_limit_data$Date, na.rm = TRUE)

all_locations <- ufeed_data %>%
  distinct(location_id, lon, lat)

default_location_id <- all_locations$location_id[22]

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  div(
    class = "app-shell",
    
    div(
      class = "topbar",
      div(
        class = "brand-block",
        div(
          class = "brand-logos",
          
          tags$a(
            href = "https://github.com/imbaterry11/UFEED",
            target = "_blank",
            rel = "noopener noreferrer",
            class = "brand-logo-link",
            tags$img(
              src = "UFEED.png",
              class = "brand-logo brand-logo-ufeed",
              alt = "UFEED"
            )
          ),
          
          tags$a(
            href = "https://www.inrae.fr/",
            target = "_blank",
            rel = "noopener noreferrer",
            class = "brand-logo-link",
            tags$img(
              src = "Logo-INRAE_Transparent.svg.png",
              class = "brand-logo brand-logo-inrae",
              alt = "INRAE"
            )
          ),
          
          tags$a(
            href = "https://egfv.bordeaux-aquitaine.hub.inrae.fr/",
            target = "_blank",
            rel = "noopener noreferrer",
            class = "brand-logo-link",
            tags$img(
              src = "Activites-scientifiques.png",
              class = "brand-logo brand-logo-egfv",
              alt = "UMR EGFV"
            )
          )
        ),
        div(
          div(class = "brand-title", "UFEED Grapevine Water Potential Explorer")
          # div(class = "brand-subtitle", "Grapevine water-potential explorer")
        )
      ),
      div(class = "today-pill", textOutput("today_text", inline = TRUE))
    ),
    
    div(
      class = "hero-card",
      div(
        class = "hero-copy",
        h1("Visualize grapevine water potential prediction"),
        p(
          "Explore predicted grapevine predawn and midday stem water potential by source, ",
          "model choice for the current season, and location. Click one map point to visualize ",
          "one location."
        )
      ),
      div(
        class = "metric-grid",
        div(class = "metric-card", div(class = "metric-value", textOutput("n_locations", inline = TRUE)), div(class = "metric-label", "Locations")),
        # div(class = "metric-card", div(class = "metric-value", textOutput("n_records", inline = TRUE)), div(class = "metric-label", "Records")),
        div(class = "metric-card", div(class = "metric-value", textOutput("axis_range", inline = TRUE)), div(class = "metric-label", "Temporal range"))
      )
    ),
    
    div(
      class = "layout-grid",
      
      tags$aside(
        class = "filters-panel",
        div(class = "panel-title", "Filters"),
        div(class = "panel-note", "Selections update both the map and water potential curve. Only one location can be selected."),
        
        selectizeInput(
          inputId = "source",
          label = "Data source",
          choices = source_choices,
          selected = source_choices,
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "Select one or more sources")
        ),
        
        radioButtons(
          inputId = "ps_type",
          label = "Water potential type",
          choices = c(
            "Predawn (pd)" = "0",
            "Midday stem (md)" = "1"
          ),
          selected = "1"
        ),
        
        radioButtons(
          inputId = "model_type",
          label = "Current-season model",
          choices = c(
            "Ensemble model" = "ensemble",
            "NN model" = "nn"
          ),
          selected = "ensemble"
        ),
        div(
          class = "panel-note",
          "The NN model theoretically has better extrapolation capacity under extreme conditions, but the ensemble model has higher accuracy in model training. This option only affects current-season data; historical data always uses the ensemble model."
        ),
        
        radioButtons(
          inputId = "show_historical",
          label = "Historical data",
          choices = c(
            "Show historical data" = "yes",
            "Hide historical data" = "no"
          ),
          selected = "yes"
        ),
        
        selectizeInput(
          inputId = "time_measured",
          label = "Measurement time band",
          choices = time_choices,
          selected = time_choices,
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "Select time(s) for the band")
        ),
        
        div(
          class = "button-row",
          actionButton("clear_locations", "Reset location", class = "ghost-button"),
          actionButton("reset_filters", "Reset filters", class = "primary-button"),
          downloadButton("download_all_data", "Download all data", class = "ghost-button")
        ),
        
        div(
          class = "selection-status",
          div(class = "status-label", "Current map selection"),
          textOutput("location_status")
        )
      ),
      
      tags$main(
        class = "content-panel",
        
        div(
          class = "viz-card map-card",
          div(
            class = "card-header-row",
            div(
              div(class = "card-title", "Location map"),
              div(class = "card-subtitle", "Click a point on the map to visualize data")
            ),
            div(class = "card-badge", textOutput("visible_points", inline = TRUE))
          ),
          leafletOutput("site_map", height = "520px")
        ),
        
        div(
          class = "viz-card plot-card",
          div(
            class = "card-header-row",
            div(
              div(class = "card-title", "Predicted water potential progression"),
              div(class = "card-subtitle", textOutput("plot_subtitle", inline = TRUE))
            ),
            div(class = "card-badge", "Forecast shaded")
          ),
          plotlyOutput("water_plot", height = "520px")
        )
      ),
      tags$footer(
        class = "app-footer",
        span("Beta version. If you have any questions, please email "),
        tags$a(
          href = "mailto:hongrui.wang@inrae.fr",
          "Hongrui Wang"
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  selected_locations <- reactiveVal(default_location_id)
  
  output$today_text <- renderText({
    paste("Today:", format(Sys.Date(), "%b %d, %Y"))
  })
  
  output$n_locations <- renderText({
    format(nrow(all_locations), big.mark = ",")
  })
  
  output$n_records <- renderText({
    format(nrow(ufeed_data), big.mark = ",")
  })
  
  output$axis_range <- renderText({
    paste0(format(x_min, "%b %d"), " - ", format(x_max, "%b %d"))
  })
  
  output$download_all_data <- downloadHandler(
    filename = function() {
      paste0("all_data_application_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      file.copy(DATA_PATH, file, overwrite = TRUE)
    },
    contentType = "text/csv"
  )
  
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "source", selected = source_choices)
    updateRadioButtons(session, "ps_type", selected = "1")
    updateRadioButtons(session, "model_type", selected = "ensemble")
    updateRadioButtons(session, "show_historical", selected = "yes")
    updateSelectizeInput(session, "time_measured", selected = time_choices)
    selected_locations(default_location_id)
  })
  
  observeEvent(input$clear_locations, {
    selected_locations(default_location_id)
  })
  
  filtered_for_controls <- reactive({
    req(input$source, input$time_measured, input$ps_type, input$show_historical)
    
    dat <- ufeed_data %>%
      filter(
        source %in% input$source,
        ps_type == as.integer(input$ps_type),
        time_label %in% input$time_measured
      )
    
    if (identical(input$show_historical, "no")) {
      dat <- dat %>% filter(source == CURRENT_SOURCE)
    }
    
    dat
  })
  
  visible_location_data <- reactive({
    filtered_for_controls() %>%
      distinct(location_id, lon, lat) %>%
      arrange(lat, lon)
  })
  
  observeEvent(input$site_map_marker_click, {
    click <- input$site_map_marker_click
    req(click$id)
    
    selected_locations(click$id)
  })
  
  observe({
    visible_ids <- visible_location_data()$location_id
    current <- selected_locations()
    
    if (length(visible_ids) == 0) {
      return()
    }
    
    if (length(current) != 1 || !(current %in% visible_ids)) {
      selected_locations(visible_ids[1])
    }
  })
  
  output$location_status <- renderText({
    selected_id <- selected_locations()[1]
    loc <- all_locations %>% filter(location_id == selected_id) %>% slice(1)
    
    if (nrow(loc) == 0) {
      "No matching location is available under the current filters."
    } else {
      paste0("Selected location: Lon ", round(loc$lon[1], 4), ", Lat ", round(loc$lat[1], 4), ".")
    }
  })
  
  output$visible_points <- renderText({
    paste(nrow(visible_location_data()), "locations")
  })
  
  output$site_map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomControl = FALSE,
        preferCanvas = TRUE
      )
    ) %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Satellite"
      ) %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        group = "Clean light"
      ) %>%
      
      addProviderTiles(
        providers$Esri.WorldTopoMap,
        group = "Terrain"
      ) %>%
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        group = "Dark"
      ) %>%
      addLayersControl(
        baseGroups = c(
          "Satellite",
          "Clean light",
          "Terrain",
          "Dark"
        ),
        options = layersControlOptions(
          collapsed = TRUE
        )
      ) %>%
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(
          metric = TRUE,
          imperial = FALSE
        )
      )
    # %>%
    #   addControl(
    #     html = htmltools::HTML(
    #       "<div class='map-layer-note'>
    #         <strong>Map layers</strong><br/>
    #         Switch between light, satellite, terrain, and dark views.
    #       </div>"
    #     ),
    #     position = "bottomright"
    #   )
  })
  
  observe({
    pts <- visible_location_data()
    selected <- selected_locations()
    
    proxy <- leafletProxy("site_map") %>%
      clearMarkers()
    
    if (nrow(pts) == 0) {
      proxy %>% addControl(
        html = "<div class='map-empty'>No locations match the current filters.</div>",
        position = "topright"
      )
      return()
    }
    
    pts <- pts %>%
      mutate(
        is_selected = location_id %in% selected,
        marker_radius = if_else(is_selected, 9, 5),
        fill_color = if_else(is_selected, "#B72E48", "#2563eb"),
        border_color = if_else(is_selected, "#F8B84E", "#ffffff"),
        marker_weight = if_else(is_selected, 2.5, 1.2),
        marker_opacity = if_else(is_selected, 0.96, 0.72),
        label_text = paste0("Lon ", round(lon, 4), ", Lat ", round(lat, 4)),
        popup_html = paste0(
          "<div class='popup-card'>",
          "<strong>Location</strong><br/>",
          "Lon: ", round(lon, 4), "<br/>",
          "Lat: ", round(lat, 4), "<br/>",
          "<span>Click to select this location</span>",
          "</div>"
        )
      )
    
    proxy <- proxy %>%
      addCircleMarkers(
        data = pts,
        lng = pts$lon,
        lat = pts$lat,
        layerId = pts$location_id,
        radius = pts$marker_radius,
        stroke = TRUE,
        weight = pts$marker_weight,
        color = pts$border_color,
        fillColor = pts$fill_color,
        fillOpacity = pts$marker_opacity,
        popup = pts$popup_html,
        label = pts$label_text,
        options = pathOptions(pane = "markerPane")
      )
    
    if (nrow(pts) == 1) {
      proxy %>% setView(lng = pts$lon[1], lat = pts$lat[1], zoom = 5)
    } else {
      proxy %>%
        fitBounds(
          lng1 = min(pts$lon, na.rm = TRUE),
          lat1 = min(pts$lat, na.rm = TRUE),
          lng2 = max(pts$lon, na.rm = TRUE),
          lat2 = max(pts$lat, na.rm = TRUE)
        )
    }
  })
  
  plot_raw_data <- reactive({
    req(input$model_type)
    selected <- selected_locations()[1]
    
    filtered_for_controls() %>%
      filter(location_id == selected) %>%
      filter(Date >= x_min, Date <= x_max) %>%
      mutate(
        use_nn_model = source == CURRENT_SOURCE & input$model_type == "nn" & !is.na(y_pred_NN),
        y_plot = if_else(use_nn_model, y_pred_NN, y_pred),
        model_label = case_when(
          source != CURRENT_SOURCE ~ "Historical data",
          use_nn_model ~ "NN model",
          TRUE ~ "Ensemble model"
        )
      ) %>%
      filter(!is.na(y_plot))
  })
  
  plot_data <- reactive({
    plot_raw_data() %>%
      group_by(Date, source, ps_label, model_label, location_id, lon, lat) %>%
      summarise(
        y_pred = mean(y_plot, na.rm = TRUE),
        y_min_band = min(y_plot, na.rm = TRUE),
        y_max_band = max(y_plot, na.rm = TRUE),
        time_min = min_or_na(time_measured),
        time_max = max_or_na(time_measured),
        T2M = mean_or_na(T2M),
        T2M_MAX = mean_or_na(T2M_MAX),
        T2M_MIN = mean_or_na(T2M_MIN),
        n_records = n(),
        n_times = n_distinct(time_label),
        .groups = "drop"
      ) %>%
      mutate(
        time_band_label = format_time_band(time_min,time_max)
      ) %>%
      arrange(source, Date)
  })
  
  output$plot_subtitle <- renderText({
    type_label <- if (identical(input$ps_type, "0")) "pd" else "md"
    model_label <- if (identical(input$model_type, "nn")) "current-season NN model" else "current-season ensemble model"
    history_label <- if (identical(input$show_historical, "yes")) "historical data shown" else "historical data hidden"
    selected_id <- selected_locations()[1]
    loc <- all_locations %>% filter(location_id == selected_id) %>% slice(1)
    
    if (nrow(loc) == 0) {
      paste("Selected location for", type_label, "with", model_label, "and", history_label)
    } else {
      paste0(
        "Selected location: Lon ", round(loc$lon[1], 4), ", Lat ", round(loc$lat[1], 4),
        " · ", type_label, " · ", model_label, " · ", history_label
      )
    }
  })
  
  output$water_plot <- renderPlotly({
    dat <- plot_data()
    
    validate(
      need(nrow(dat) > 0, "No data match the current selection.")
    )
    
    source_levels <- sort(unique(dat$source))
    source_palette <- c(
      "2026" = "#2563eb",
      "2025" = "#9333ea",
      "2015-2025 mean" = "#0f766e",
      "2016-2025 mean" = "#0f766e"
    )
    fallback_palette <- c("#2563eb", "#0f766e", "#9333ea", "#ea580c", "#475569", "#be123c")
    missing_sources <- setdiff(source_levels, names(source_palette))
    if (length(missing_sources) > 0) {
      source_palette[missing_sources] <- rep(fallback_palette, length.out = length(missing_sources))
    }
    
    source_dash_map <- setNames(rep("solid", length(source_levels)), source_levels)
    source_dash_map[source_levels != CURRENT_SOURCE] <- "dash"
    
    y_min <- min(dat$y_min_band, na.rm = TRUE)
    y_max <- max(dat$y_max_band, na.rm = TRUE)
    y_padding <- max(0.05, abs(y_max - y_min) * 0.08)
    y_min <- y_min - y_padding
    y_max <- y_max + y_padding
    
    today <- Sys.Date()
    p <- plot_ly()
    
    shade_start <- max(today, x_min)
    
    if (!is.na(shade_start) && shade_start <= x_max) {
      p <- p %>%
        add_trace(
          x = as.Date(c(shade_start, x_max, x_max, shade_start)),
          y = c(y_min, y_min, y_max, y_max),
          type = "scatter",
          mode = "none",
          fill = "toself",
          fillcolor = "rgba(15, 118, 110, 0.10)",
          line = list(color = "rgba(15, 118, 110, 0)"),
          text = rep("Forecast data", 4),
          hoverinfo = "text",
          name = "Forecast data",
          showlegend = FALSE,
          inherit = FALSE
        )
    }
    
    for (src in source_levels) {
      d <- dat %>% filter(source == src) %>% arrange(Date)
      line_color <- unname(source_palette[src])
      
      band_hover_text <- paste0(
        "<b>", htmlEscape(src), " selected-time band</b>",
        "<br>Date: ", format(d$Date, "%Y-%m-%d"),
        "<br>Water potential band: ", round(d$y_min_band, 3), " to ", round(d$y_max_band, 3),
        "<br>Mean predicted water potential: ", round(d$y_pred, 3),
        "<br>Time band: ", htmlEscape(d$time_band_label),
        "<br>Model: ", htmlEscape(d$model_label),
        "<br>Location: Lon ", round(d$lon, 4), ", Lat ", round(d$lat, 4)
      )
      
      p <- p %>%
        add_trace(
          x = c(d$Date, rev(d$Date)),
          y = c(d$y_min_band, rev(d$y_max_band)),
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = hex_to_rgba(line_color, 0.16),
          line = list(color = "rgba(0,0,0,0)", width = 0),
          text = c(band_hover_text, rev(band_hover_text)),
          hoverinfo = "text",
          name = paste(src, "selected-time band"),
          showlegend = FALSE,
          inherit = FALSE
        )
      
      hover_text <- paste0(
        "<b>", htmlEscape(unique(d$ps_label)[1]), "</b>",
        "<br>Date: ", format(d$Date, "%Y-%m-%d"),
        "<br>Mean predicted water potential: ", round(d$y_pred, 3),
        # "<br>Selected-time band: ", round(d$y_min_band, 3), " to ", round(d$y_max_band, 3),
        "<br>Source: ", htmlEscape(src),
        "<br>Time band: ", htmlEscape(d$time_band_label),
        "<br>Model: ", htmlEscape(d$model_label),
        "<br>Location: Lon ", round(d$lon, 4), ", Lat ", round(d$lat, 4),
        # "<br>Records used: ", d$n_records,
        "<br>T2M: ", ifelse(is.na(d$T2M), "NA", paste0(round(d$T2M, 1), " °C")),
        "<br>T2M max/min: ",
        ifelse(is.na(d$T2M_MAX), "NA", paste0(round(d$T2M_MAX, 1), "°C")),
        " / ",
        ifelse(is.na(d$T2M_MIN), "NA", paste0(round(d$T2M_MIN, 1), " °C"))
      )
      
      p <- p %>%
        add_trace(
          data = d,
          x = ~Date,
          y = ~y_pred,
          type = "scatter",
          mode = "lines",
          name = src,
          text = hover_text,
          hoverinfo = "text",
          line = list(
            color = line_color,
            width = 2.8,
            dash = unname(source_dash_map[src])
          ),
          legendgroup = src,
          inherit = FALSE
        )
    }
    
    shapes <- list()
    annotations <- list()
    
    if (!is.na(today) && today >= x_min && today <= x_max) {
      shapes <- append(shapes, list(list(
        type = "line",
        x0 = as.character(today),
        x1 = as.character(today),
        y0 = 0,
        y1 = 1,
        yref = "paper",
        line = list(color = "rgba(239, 68, 68, 0.95)", width = 2, dash = "dot")
      )))
      
      annotations <- append(annotations, list(list(
        x = as.character(today),
        y = 1,
        yref = "paper",
        text = "Today",
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "bottom",
        font = list(color = "black", size = 12)
      )))
    }
    
    p %>%
      layout(
        font = list(color = "black"),
        
        margin = list(l = 70, r = 28, t = 24, b = 70),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(248,250,252,0.75)",
        hovermode = "closest",
        
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.22,
          bgcolor = "rgba(255,255,255,0)",
          font = list(color = "black")
        ),
        
        xaxis = list(
          title = list(
            text = "Date",
            font = list(color = "black")
          ),
          range = as.character(c(x_min, x_max)),
          tickformat = "%b %d",
          tickfont = list(color = "black"),
          
          showgrid = TRUE,
          gridcolor = "rgba(148, 163, 184, 0.22)",
          
          showline = TRUE,
          linecolor = "black",
          linewidth = 1,
          mirror = FALSE,
          
          ticks = "outside",
          tickcolor = "black",
          
          zeroline = FALSE,
          automargin = TRUE
        ),
        
        yaxis = list(
          title = list(
            text = "Predicted grapevine water potential (MPa)",
            font = list(color = "black")
          ),
          range = c(y_min, y_max),
          tickfont = list(color = "black"),
          
          showgrid = TRUE,
          gridcolor = "rgba(148, 163, 184, 0.22)",
          
          showline = TRUE,
          linecolor = "black",
          linewidth = 1,
          mirror = FALSE,
          
          ticks = "outside",
          tickcolor = "black",
          
          zeroline = FALSE,
          automargin = TRUE
        ),
        
        shapes = shapes,
        annotations = annotations
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  
}

shinyApp(ui = ui, server = server)
