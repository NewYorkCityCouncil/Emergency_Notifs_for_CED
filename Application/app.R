# app.R

# --- 0. Libraries ---
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(knitr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(glue)
library(councilcount)
library(councildown)
library(councilverse)
library(shiny) # Make sure shiny is loaded

# --- 1. Initial Data Loading and Basic Prep (Run Once) ---

# IMPORTANT: Path to your data file
emergency_data_file <- "emergency_data_cleaned.xlsx"

if (!file.exists(emergency_data_file)) {
  stop(glue::glue("Error: Data file not found at '{emergency_data_file}'.
       Please place the file in the app directory or update the path."))
}

# Read the raw emergency data
emergency_data_raw <- read_excel(emergency_data_file)

# Basic cleaning and add Year column
emergency_data <- emergency_data_raw %>%
  filter(Council_District != "All Districts") %>%
  mutate(
    Council_District = as.character(Council_District),
    Year = year(Date_of_Incident),
    # Ensure Incident is character and handle potential NAs/empty strings
    Incident = as.character(Incident),
    Incident = ifelse(is.na(Incident) | Incident == "", "Unknown", Incident)
  ) %>%
  filter(Year %in% c(2023, 2024)) # Keep only relevant years initially

# Get unique incident types for the dropdown, adding "All Incidents"
incident_choices <- c("All Incidents", sort(unique(emergency_data$Incident)))

# Download NYC Council Districts boundaries (run once)
nyc_districts_url <- "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_City_Council_Districts/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson"
temp_geojson <- tempfile(fileext = ".geojson")
download_success <- FALSE
nyc_districts_base <- NULL # Initialize to NULL

tryCatch({
  download.file(nyc_districts_url, temp_geojson, mode = "wb", quiet = TRUE)
  nyc_districts_base <- st_read(temp_geojson) %>%
    mutate(district_id = as.character(CounDist)) # Add district_id here
  download_success <- TRUE
}, error = function(e) {
  message("Failed to download NYC Council Districts GeoJSON. Error: ", e$message)
})

if (!download_success && is.null(nyc_districts_base)) {
  # Try reading a local file if download failed and base is still NULL
  local_geojson_path <- "NYC_City_Council_Districts.geojson" # Example local file name
  if(file.exists(local_geojson_path)) {
    tryCatch({
      nyc_districts_base <- st_read(local_geojson_path) %>%
        mutate(district_id = as.character(CounDist))
      message("Successfully loaded local GeoJSON backup.")
      download_success <- TRUE # Treat as success for the app logic
    }, error = function(e_local){
      message("Failed to read local GeoJSON backup. Error: ", e_local$message)
    })
  }
}

# Final check if boundaries loaded
if (is.null(nyc_districts_base)) {
  stop("Could not load Council District boundaries (download failed and no local backup found/readable). App cannot continue.")
}


# Pre-calculate Council Member lookup (can be done once)
# Take the most recent non-NA name per district within the 2023-2024 timeframe
council_members_lookup <- emergency_data %>%
  filter(!is.na(Council_Member) & Council_Member != "") %>%
  arrange(Council_District, desc(Date_of_Incident)) %>%
  group_by(Council_District) %>%
  slice_head(n = 1) %>%
  select(Council_District, Council_Member) %>%
  ungroup() %>%
  # Ensure character type for joining
  mutate(Council_District = as.character(Council_District))


# --- 2. UI Definition ---
ui <- fluidPage(
  # Add custom CSS
  tags$head(
    tags$style(HTML("
      html, body {height: 100%; margin: 0; padding: 0;}
      .container-fluid {height: 100%; padding: 0;} /* Make fluidPage container take full height */
      #councilMap {height: 100vh !important;} /* Force map to take full viewport height */

      /* Optional: Style the text within the transparent panel for better readability */
      #controls h3, #controls h4, #controls label {
         /* text-shadow: 1px 1px 2px white, -1px -1px 2px white, 1px -1px 2px white, -1px 1px 2px white; */ /* Example: White outline */
         /* color: black; */ /* Ensure text color contrasts with map */
      }
       #controls h3 {
        /* font-weight: bold; */
       }

    "))
  ),
  
  # Map Output - Make this fill the background
  leafletOutput("councilMap", width="100%", height="100%"),
  
  # Absolute Panel for Controls (Overlays the Map)
  absolutePanel(
    id = "controls",
    fixed = TRUE,
    draggable = TRUE,
    top = 20,
    left = 20,
    right = "auto",
    bottom = "auto",
    width = 220,
    height = "auto",
    style = "z-index: 1000;", # Ensures controls stay on top
    
    h3("Interactive Emergency Incidents Map"),
    h4("Filter Options"),
    
    selectInput("selected_year", "Select Year:",
                choices = c("Overall (2023-2024)" = "Overall", "2023", "2024"),
                selected = "Overall"),
    
    selectInput("selected_incident", "Select Incident Type:",
                choices = incident_choices,
                selected = "All Incidents")
  )
)

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  # --- Reactive Data Filtering ---
  filtered_emergency_data <- reactive({
    req(input$selected_year, input$selected_incident)
    
    filtered <- emergency_data
    
    if (input$selected_year != "Overall") {
      filtered <- filtered %>% filter(Year == as.numeric(input$selected_year))
    }
    
    if (input$selected_incident != "All Incidents") {
      filtered <- filtered %>% filter(Incident == input$selected_incident)
    }
    
    if(nrow(filtered) == 0) {
      return(tibble(
        Council_District = character(), Incident = character(),
        Council_Member = character(), Date_of_Incident = as.POSIXct(character()),
        Year = integer()
      ))
    }
    return(filtered)
  })
  
  # --- Reactive Data Aggregation and Merging for Map ---
  reactive_map_data <- reactive({
    current_data <- filtered_emergency_data()
    req(nyc_districts_base)
    
    if (nrow(current_data) == 0) {
      map_data_agg <- nyc_districts_base %>%
        mutate(
          Total_Incidents = 0,
          Council_Member = left_join(., council_members_lookup, by = c("district_id" = "Council_District"))$Council_Member,
          Council_Member = ifelse(is.na(Council_Member), "Data Unavailable", Council_Member),
          TopIncidentsHTML = "<p>No incidents match filter.</p>"
        )
      return(map_data_agg)
    }
    
    district_summary <- current_data %>%
      group_by(Council_District) %>%
      summarize(Total_Incidents = n(), .groups = "drop")
    
    if (input$selected_incident == "All Incidents" && nrow(current_data) > 0) {
      top_incidents_filtered <- current_data %>%
        group_by(Council_District, Incident) %>%
        summarize(Count = n(), .groups = "drop") %>% group_by(Council_District) %>%
        arrange(Council_District, desc(Count)) %>% slice_head(n = 3) %>% ungroup()
      top_incidents_html <- top_incidents_filtered %>%
        group_by(Council_District) %>%
        summarise(
          TopIncidentsHTML = paste0("<ol>", paste0("<li>", htmlEscape(Incident), " (", Count, ")</li>", collapse = ""), "</ol>"),
          .groups = "drop" )
    } else if (nrow(current_data) > 0) {
      top_incidents_html <- current_data %>%
        group_by(Council_District) %>% summarise(Count = n(), .groups = "drop") %>%
        mutate(TopIncidentsHTML = paste0("<p><b>Selected:</b> ", htmlEscape(input$selected_incident), " (", Count, ")</p>")) %>%
        select(Council_District, TopIncidentsHTML)
    } else {
      top_incidents_html <- tibble(Council_District = character(), TopIncidentsHTML = character())
    }
    
    map_data_agg <- nyc_districts_base %>%
      left_join(district_summary, by = c("district_id" = "Council_District")) %>%
      left_join(council_members_lookup, by = c("district_id" = "Council_District")) %>%
      left_join(top_incidents_html, by = c("district_id" = "Council_District")) %>%
      mutate(
        Total_Incidents = ifelse(is.na(Total_Incidents), 0, Total_Incidents),
        Council_Member = ifelse(is.na(Council_Member), "Data Unavailable", Council_Member),
        TopIncidentsHTML = case_when(
          Total_Incidents == 0 ~ "<p>No incidents match filter.</p>",
          is.na(TopIncidentsHTML) ~ "<p>Incident data processing error.</p>", TRUE ~ TopIncidentsHTML
        )
      )
    return(map_data_agg)
  })
  
  # --- Render Leaflet Map ---
  output$councilMap <- renderLeaflet({
    map_data <- reactive_map_data()
    req(nrow(map_data) > 0)
    
    bins <- c(0, 5, 10, 20, 40, 60, Inf)
    labels <- c("0 - 5", "5 - 10", "10 - 20", "20 - 40", "40 - 60", "60 - Inf") # Match screenshot format
    
    max_incidents <- max(map_data$Total_Incidents, na.rm = TRUE)
    palette_domain <- if (max_incidents > 0) map_data$Total_Incidents else c(0, 1)
    
    pal <- councildown::colorBin(
      palette = "nycc_blue", domain = palette_domain, bins = bins,
      na.color = "#E0E0E0", # Slightly grey NA color might look better than pure white
      right = FALSE
    )
    
    popup_content <- lapply(seq_len(nrow(map_data)), function(i) {
      row <- map_data[i, ]
      year_text <- ifelse(input$selected_year == "Overall", "2023-2024", input$selected_year)
      incident_text <- ifelse(input$selected_incident == "All Incidents", "All Incidents", input$selected_incident)
      popup_title <- glue::glue("<h4>District {htmlEscape(row$district_id)} ({year_text})</h4>")
      popup_body <- paste0(
        "<p><b>Council Member:</b> ", htmlEscape(row$Council_Member), "</p>",
        "<p><b>Selected Filter:</b> ", htmlEscape(incident_text), "</p>",
        "<p><b>Total Incidents (matching filter):</b> ", row$Total_Incidents, "</p><hr>",
        "<b>Incident Details:</b>", row$TopIncidentsHTML
      )
      councilPopup(paste0(popup_title, popup_body))
    })
    
    # Create the map
    # CHANGED: Added attributionControl = FALSE
    leaflet(map_data, options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
      addCouncilStyle(add_dists = TRUE, dist_year = "2023") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Basemap") %>%
      addPolygons(
        fillColor = ~pal(Total_Incidents), weight = 1, opacity = 1, color = "white",
        dashArray = "3", fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3, color = "#666", dashArray = "", fillOpacity = 0.9, bringToFront = TRUE
        ),
        label = ~paste0("District ", district_id, ": ", Total_Incidents, " incidents"),
        popup = popup_content, group = "Districts"
      ) %>%
      addLegend_decreasing(
        position = "bottomright", pal = pal,
        title = glue::glue("Incidents ({ifelse(input$selected_year=='Overall', '23-24', input$selected_year)}, {str_trunc(input$selected_incident, 20)})"),
        values = ~Total_Incidents, opacity = 0.8, decreasing = TRUE,
        # Use custom labFormat to ensure labels match desired format
        labFormat = function(type, cuts, p) { labels }
      ) %>%
      htmlwidgets::onRender("
        function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this); }
      ") %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to NYC",
        onClick=JS("function(btn, map){ map.setView([40.7128, -74.0060], 10); }")
      )) %>%
      addLayersControl(
        baseGroups = c("Basemap"), overlayGroups = c("Districts"),
        options = layersControlOptions(collapsed = TRUE, position = "bottomleft")
      )
    # REMOVED: addControl(html="...", position="bottomright") line was deleted
    # Optional: Re-add source text if desired and not overlapping
    # %>% addSourceText(...)
    
  }) # End renderLeaflet
  
} # End server function

# --- 4. Run the App ---
shinyApp(ui = ui, server = server)