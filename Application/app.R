# app.R

# --- 0. Libraries ---
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2) # Required by councildown/verse, keep loaded
library(lubridate)
library(stringr)
library(knitr) # Required by councildown/verse, keep loaded
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(glue)
library(councilcount)
library(councildown)
library(councilverse)
library(shiny)
library(shinyjs)

# --- 1. Initial Data Loading and Basic Prep ---
emergency_data_file <- "emergency_data_cleaned.xlsx"
if (!file.exists(emergency_data_file)) { stop(glue::glue("Error: Data file not found at '{emergency_data_file}'.")) }
emergency_data_raw <- read_excel(emergency_data_file)

required_cols <- c("Council_District", "Date_of_Incident", "Incident", "Status", "Council_Member", "Borough")
if (!all(required_cols %in% names(emergency_data_raw))) { stop(glue::glue("Error: Missing required columns. Need: {paste(required_cols, collapse=', ')}")) }

emergency_data <- emergency_data_raw %>%
  mutate(
    Status = as.character(Status), Status = ifelse(is.na(Status) | Status == "", "Unknown", Status),
    Council_District = as.character(Council_District), Council_Member = as.character(Council_Member),
    Borough = as.character(Borough), Borough = ifelse(is.na(Borough) | Borough == "", "Unknown", toupper(Borough)),
    Incident = as.character(Incident), Incident = ifelse(is.na(Incident) | Incident == "", "Unknown", Incident),
    Year = year(Date_of_Incident)
  ) %>%
  filter(Council_District != "All Districts") %>%
  filter(Year %in% c(2023, 2024))

incident_choices <- c("All Incidents", sort(unique(emergency_data$Incident)))
status_choices <- c("All Statuses", "Concluded", "Other", "Pending")

# Load Council Districts Spatial Data
nyc_districts_url <- "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_City_Council_Districts/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson"
temp_geojson_dist <- tempfile(fileext = ".geojson"); download_success_dist <- FALSE; nyc_districts_base <- NULL
tryCatch({ download.file(nyc_districts_url, temp_geojson_dist, mode = "wb", quiet = TRUE); nyc_districts_base <- st_read(temp_geojson_dist) %>% mutate(district_id = as.character(CounDist)); download_success_dist <- TRUE; }, error = function(e) {})
if (!download_success_dist) { local_geojson_path_dist <- "NYC_City_Council_Districts.geojson"; if(file.exists(local_geojson_path_dist)) { tryCatch({ nyc_districts_base <- st_read(local_geojson_path_dist) %>% mutate(district_id = as.character(CounDist)); download_success_dist <- TRUE }, error = function(e_local){}) } }
if (is.null(nyc_districts_base)) { stop("FATAL ERROR: Could not load Council District boundaries.") }
target_crs <- st_crs(nyc_districts_base)

# Load Boroughs Spatial Data
borough_geojson_file <- "borough-boundaries.geojson"; nyc_boroughs <- NULL
if (file.exists(borough_geojson_file)) { tryCatch({ nyc_boroughs_raw <- st_read(borough_geojson_file); nyc_boroughs <- nyc_boroughs_raw %>% st_transform(crs = target_crs) %>% mutate( Borough = toupper(boro_name) ) %>% select(Borough, geometry); }, error = function(e) { nyc_boroughs <- NULL }) }

# Council Member Lookup
council_members_lookup <- emergency_data %>% filter(!is.na(Council_Member) & Council_Member != "") %>% arrange(Council_District, desc(Date_of_Incident)) %>% group_by(Council_District) %>% slice_head(n = 1) %>% select(Council_District, Council_Member) %>% ungroup() %>% mutate(Council_District = as.character(Council_District))

# --- 2. UI Definition ---
ui <- fluidPage(
  tags$head(tags$style(HTML("html, body {height: 100%; margin: 0; padding: 0;} .container-fluid {height: 100%; padding: 0;} #councilMap {height: 100vh !important;} #controls h3 { margin-top: 0; }"))),
  leafletOutput("councilMap", width="100%", height="100%"),
  absolutePanel(
    id = "controls", fixed = TRUE, draggable = TRUE,
    top = 20, left = 20, right = "auto", bottom = "auto",
    width = 300,
    style = "background-color: rgba(255, 255, 255, 0.85); padding: 15px; border-radius: 5px; z-index: 2000; max-height: 90vh;",
    h3("Interactive Emergency Incidents Map"),
    shinyjs::useShinyjs(),
    radioButtons("map_level", "Select Map Level:", choices = c("Districts", "Boroughs"), selected = "Districts"),
    h4("Filter Options"),
    selectInput("selected_year", "Select Year:", choices = c("Overall (2023-2024)" = "Overall", "2023", "2024"), selected = "Overall"),
    selectInput("selected_incident", "Select Incident Type:", choices = incident_choices, selected = "All Incidents"),
    selectInput("selected_status", "Select Incident Status:", choices = status_choices, selected = "All Statuses")
  )
)

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  # Enable/disable Borough option based on data load status
  observe({
    borough_data_loaded <- !is.null(nyc_boroughs) && inherits(nyc_boroughs, "sf") && nrow(nyc_boroughs) > 0
    if (!borough_data_loaded) {
      shinyjs::disable(selector = "#map_level input[value='Boroughs']")
      if(input$map_level == "Boroughs") { updateRadioButtons(session, "map_level", selected = "Districts") }
    } else {
      shinyjs::enable(selector = "#map_level input[value='Boroughs']")
    }
  })
  
  # --- Reactive Data Filtering (includes status filter) ---
  filtered_emergency_data <- reactive({
    req(input$selected_year, input$selected_incident, input$selected_status)
    data_subset <- emergency_data
    if (input$selected_year != "Overall") { year_num <- as.numeric(input$selected_year); data_subset <- data_subset %>% filter(Year == year_num) }
    if (input$selected_incident != "All Incidents") { selected_inc <- input$selected_incident; data_subset <- data_subset %>% filter(Incident == selected_inc) }
    if (input$selected_status != "All Statuses") { selected_stat <- input$selected_status; data_subset <- data_subset %>% filter(Status == selected_stat) }
    if (nrow(data_subset) == 0) { final_filtered_data <- tibble( Council_District = character(), Incident = character(), Council_Member = character(), Date_of_Incident = Sys.time()[0], Year = integer(), Status = character(), Borough=character() ) } else { final_filtered_data <- data_subset }
    return(final_filtered_data)
  })
  
  # --- Reactive Data Aggregation (handles level switching, calculates percentage, cached) ---
  reactive_map_data <- reactive({
    req(input$map_level, input$selected_year, input$selected_incident, input$selected_status)
    level <- input$map_level
    if(level == "Boroughs") { req(!is.null(nyc_boroughs), inherits(nyc_boroughs, "sf"), nrow(nyc_boroughs) > 0) } else { req(!is.null(nyc_districts_base)) }
    fully_filtered_data <- filtered_emergency_data(); req(fully_filtered_data)
    
    # Calculate Denominator Data (filters except status)
    partially_filtered_data <- emergency_data
    if (input$selected_year != "Overall") { year_num <- as.numeric(input$selected_year); partially_filtered_data <- partially_filtered_data %>% filter(Year == year_num) }
    if (input$selected_incident != "All Incidents") { selected_inc <- input$selected_incident; partially_filtered_data <- partially_filtered_data %>% filter(Incident == selected_inc) }
    
    aggregated_data <- NULL; base_shapes <- NULL
    
    # Aggregate by District Logic
    if (level == "Districts") {
      base_shapes <- nyc_districts_base; denominator_summary <- partially_filtered_data %>% group_by(Council_District) %>% summarize(Denominator_Total = n(), .groups = "drop")
      if (nrow(fully_filtered_data) == 0) { aggregated_data <- base_shapes %>% left_join(denominator_summary, by = c("district_id" = "Council_District")) %>% left_join(council_members_lookup, by = c("district_id" = "Council_District")) %>% mutate( Level_ID = district_id, Level_Name = paste("District", district_id), Council_Member_Name = ifelse(is.na(Council_Member), "Unavailable", Council_Member), Total_Incidents = 0, Denominator_Total = ifelse(is.na(Denominator_Total), 0, Denominator_Total), Percent_Selected_Status = 0, TopIncidentsHTML = "<p>None</p>" )
      } else { district_summary <- fully_filtered_data %>% group_by(Council_District) %>% summarize(Total_Incidents = n(), .groups = "drop"); if (input$selected_incident == "All Incidents") { if (!"Incident" %in% names(fully_filtered_data)) { ti_html <- fully_filtered_data %>% distinct(Council_District) %>% mutate(H = "<p>Err</p>") %>% select(Council_District, TopIncidentsHTML=H) } else { ti_filtered <- fully_filtered_data %>% filter(n() > 0) %>% group_by(Council_District, Incident) %>% summarize(N = n(), .groups = "drop") %>% group_by(Council_District) %>% arrange(desc(N)) %>% slice_head(n = 3) %>% ungroup(); ti_html <- ti_filtered %>% group_by(Council_District) %>% summarise( H = paste0("<ol>", paste0("<li>", htmlEscape(Incident), " (", N, ")</li>", collapse = ""), "</ol>"), .groups = "drop" ) %>% select(Council_District, TopIncidentsHTML=H) } } else { ti_html <- fully_filtered_data %>% group_by(Council_District) %>% summarise(N = n(), .groups = "drop") %>% mutate(H = paste0("<p><b>Selected:</b> ", htmlEscape(input$selected_incident), " (", N, ")</p>")) %>% select(Council_District, TopIncidentsHTML=H) }; aggregated_data <- base_shapes %>% left_join(district_summary, by = c("district_id" = "Council_District")) %>% left_join(denominator_summary, by = c("district_id" = "Council_District")) %>% left_join(council_members_lookup, by = c("district_id" = "Council_District")) %>% left_join(ti_html, by = c("district_id" = "Council_District")) %>% mutate( Level_ID = district_id, Level_Name = paste("District", district_id), Council_Member_Name = ifelse(is.na(Council_Member), "Unavailable", Council_Member), Total_Incidents = ifelse(is.na(Total_Incidents), 0, Total_Incidents), Denominator_Total = ifelse(is.na(Denominator_Total), 0, Denominator_Total), Percent_Selected_Status = ifelse(Denominator_Total > 0, round((Total_Incidents / Denominator_Total) * 100, 1), 0), TopIncidentsHTML = case_when( is.na(TopIncidentsHTML) & Total_Incidents > 0 ~ "<p>Details N/A</p>", Total_Incidents == 0 ~ "<p>None</p>", is.na(TopIncidentsHTML) ~ "<p>Err</p>", TRUE ~ TopIncidentsHTML ) ) }
    }
    # Aggregate by Borough Logic
    else if (level == "Boroughs") {
      base_shapes <- nyc_boroughs; denominator_summary <- partially_filtered_data %>% group_by(Borough) %>% summarize(Denominator_Total = n(), .groups = "drop")
      if (nrow(fully_filtered_data) == 0 || !"Borough" %in% names(fully_filtered_data)) { aggregated_data <- base_shapes %>% left_join(denominator_summary, by = "Borough") %>% mutate( Level_ID = Borough, Level_Name = tools::toTitleCase(tolower(Borough)), Council_Member_Name = NA_character_, Total_Incidents = 0, Denominator_Total = ifelse(is.na(Denominator_Total), 0, Denominator_Total), Percent_Selected_Status = 0, TopIncidentsHTML = "<p>None</p>" )
      } else { borough_summary <- fully_filtered_data %>% group_by(Borough) %>% summarize(Total_Incidents = n(), .groups = "drop"); if (input$selected_incident == "All Incidents") { if (!"Incident" %in% names(fully_filtered_data)) { ti_html <- fully_filtered_data %>% distinct(Borough) %>% mutate(H = "<p>Err</p>") %>% select(Borough, TopIncidentsHTML=H) } else { ti_filtered <- fully_filtered_data %>% filter(n() > 0) %>% group_by(Borough, Incident) %>% summarize(N = n(), .groups = "drop") %>% group_by(Borough) %>% arrange(desc(N)) %>% slice_head(n = 3) %>% ungroup(); ti_html <- ti_filtered %>% group_by(Borough) %>% summarise( H = paste0("<ol>", paste0("<li>", htmlEscape(Incident), " (", N, ")</li>", collapse = ""), "</ol>"), .groups = "drop" ) %>% select(Borough, TopIncidentsHTML=H) } } else { ti_html <- fully_filtered_data %>% group_by(Borough) %>% summarise(N = n(), .groups = "drop") %>% mutate(H = paste0("<p><b>Selected:</b> ", htmlEscape(input$selected_incident), " (", N, ")</p>")) %>% select(Borough, TopIncidentsHTML=H) }; aggregated_data <- base_shapes %>% left_join(borough_summary, by = "Borough") %>% left_join(denominator_summary, by = "Borough") %>% left_join(ti_html, by = "Borough") %>% mutate( Level_ID = Borough, Level_Name = tools::toTitleCase(tolower(Borough)), Council_Member_Name = NA_character_, Total_Incidents = ifelse(is.na(Total_Incidents), 0, Total_Incidents), Denominator_Total = ifelse(is.na(Denominator_Total), 0, Denominator_Total), Percent_Selected_Status = ifelse(Denominator_Total > 0, round((Total_Incidents / Denominator_Total) * 100, 1), 0), TopIncidentsHTML = case_when( is.na(TopIncidentsHTML) & Total_Incidents > 0 ~ "<p>Details N/A</p>", Total_Incidents == 0 ~ "<p>None</p>", is.na(TopIncidentsHTML) ~ "<p>Err</p>", TRUE ~ TopIncidentsHTML ) ) }
    } else { return(NULL) }
    
    # Check output structure and return
    required_output_cols <- c("Level_ID", "Level_Name", "Council_Member_Name", "Total_Incidents", "Percent_Selected_Status", "TopIncidentsHTML", "geometry")
    if(!is.null(aggregated_data) && inherits(aggregated_data, "sf") && all(required_output_cols %in% names(aggregated_data))) {
      return(aggregated_data)
    } else {
      return(NULL) # Return NULL if aggregation failed or structure incorrect
    }
  }) %>%
    bindCache(input$map_level, input$selected_year, input$selected_incident, input$selected_status) # Cache based on inputs
  
  # --- Render Initial Leaflet Map ---
  output$councilMap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
      councildown::addCouncilStyle(add_dists = TRUE, dist_year = "2023") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Basemap") %>%
      htmlwidgets::onRender(" function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this); } ") %>%
      addEasyButton(easyButton( icon="fa-globe", title="Zoom to NYC", onClick=JS("function(btn, map){ map.setView([40.7128, -74.0060], 10); }") )) %>%
      addLayersControl(
        baseGroups = c("Basemap"),
        overlayGroups = c("Data Layer"), # Data layer will be updated by proxy
        options = layersControlOptions(collapsed = TRUE, position = "bottomleft")
      )
  })
  
  # --- Observer for Dynamic Map Updates via Proxy ---
  observe({
    map_display_data <- reactive_map_data()
    req(map_display_data, inherits(map_display_data, "sf"))
    
    # Handle case where data has no rows after filtering/aggregation
    if (nrow(map_display_data) == 0) {
      leafletProxy("councilMap") %>%
        clearGroup("Data Layer") %>%
        clearControls() # Clear legend
      return()
    }
    
    # Prepare dynamic elements (bins, labels, palette, popups)
    bins_district <- c(0, 5, 10, 20, 40, 60, Inf); labels_district <- c("0 - 4", "5 - 9", "10 - 19", "20 - 39", "40 - 59", "60+")
    bins_borough <- c(0, 50, 100, 200, 300, 400, Inf); labels_borough <- c("0 - 49", "50 - 99", "100 - 199", "200 - 299", "300 - 399", "400+")
    if (input$map_level == "Districts") { current_bins <- bins_district; current_labels <- labels_district } else { current_bins <- bins_borough; current_labels <- labels_borough }
    
    max_incidents <- max(map_display_data$Total_Incidents, na.rm = TRUE); palette_domain <- if(max_incidents > 0 && !is.na(max_incidents)) map_display_data$Total_Incidents else c(0, 1)
    pal <- councildown::colorBin( palette = "nycc_blue", domain = palette_domain, bins = current_bins, na.color = "#E0E0E0", right = FALSE )
    
    level_text <- tools::toTitleCase(input$map_level); year_text_legend <- ifelse(input$selected_year=='Overall', '23-24', input$selected_year); incident_text_legend <- str_trunc(input$selected_incident, 20); legend_title <- glue::glue("{level_text} Incidents ({year_text_legend}, {incident_text_legend})")
    
    popup_content <- lapply(seq_len(nrow(map_display_data)), function(i) { row <- map_display_data[i, ]; year_text_popup <- ifelse(input$selected_year == "Overall", "2023-2024", input$selected_year); incident_text_popup <- ifelse(input$selected_incident == "All Incidents", "All Incidents", htmlEscape(input$selected_incident)); status_text_popup <- ifelse(input$selected_status == "All Statuses", "All", htmlEscape(input$selected_status)); popup_title <- glue::glue("<h4>{htmlEscape(row$Level_Name)} ({year_text_popup})</h4>"); popup_body_parts <- c( paste0("<p><b>Incident Filter:</b> ", incident_text_popup, "</p>"), paste0("<p><b>Status Filter:</b> ", status_text_popup, "</p>") ); if (input$map_level == "Districts") { popup_body_parts <- c(popup_body_parts, paste0("<p><b>Council Member:</b> ", htmlEscape(row$Council_Member_Name), "</p>")) }; popup_body_parts <- c(popup_body_parts, paste0("<p><b>Total Incidents (matching ALL filters):</b> ", row$Total_Incidents, "</p>") ); if (input$selected_status != "All Statuses") { if("Percent_Selected_Status" %in% names(row)){ percent_label <- glue("% {htmlEscape(input$selected_status)}"); popup_body_parts <- c(popup_body_parts, paste0("<p><b>", percent_label, " (of Year/Type Total):</b> ", sprintf("%.1f%%", row$Percent_Selected_Status), "</p>") ) } else { popup_body_parts <- c(popup_body_parts, "<p><i>Percentage error.</i></p>") } }; popup_body_parts <- c(popup_body_parts, "<hr>", "<b>Details:</b>", ifelse(!is.na(row$TopIncidentsHTML), row$TopIncidentsHTML, "<p>N/A</p>") ); popup_body <- paste(popup_body_parts, collapse=""); councilPopup(paste0(popup_title, popup_body)) })
    
    # Update map via proxy
    leafletProxy("councilMap", data = map_display_data) %>%
      clearGroup("Data Layer") %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(Total_Incidents), weight = 1, opacity = 1, color = "white",
        dashArray = "3", fillOpacity = 0.7, group = "Data Layer",
        highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.9, bringToFront = TRUE ),
        label = ~paste0(Level_Name, ": ", Total_Incidents, " incidents"),
        popup = popup_content
      ) %>%
      addLegend_decreasing(
        position = "bottomright", pal = pal, title = legend_title, values = ~Total_Incidents,
        opacity = 0.8, decreasing = TRUE, labFormat = function(type, cuts, p) { current_labels },
        layerId = "currentLegend"
      )
  })
  
} # End server function

# --- 4. Run the App ---
shinyApp(ui = ui, server = server)