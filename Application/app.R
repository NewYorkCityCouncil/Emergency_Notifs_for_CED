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
tryCatch({
  download.file(nyc_districts_url, temp_geojson, mode = "wb", quiet = TRUE)
  nyc_districts_base <- st_read(temp_geojson) %>%
    mutate(district_id = as.character(CounDist)) # Add district_id here
  download_success <- TRUE
}, error = function(e) {
  message("Failed to download NYC Council Districts GeoJSON. Error: ", e$message)
  # Optionally, try reading a local backup if available
  # nyc_districts_base <- st_read("path/to/local/backup.geojson") 
})

if (!download_success && !exists("nyc_districts_base")) {
  stop("Could not load Council District boundaries. App cannot continue.")
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
  titlePanel("NYC Council Districts: Interactive Emergency Incidents Map"),
  
  fluidRow(
    column(3,
           selectInput("selected_year", "Select Year:",
                       choices = c("Overall (2023-2024)" = "Overall", "2023", "2024"),
                       selected = "Overall")
    ),
    column(4, # Increased width slightly for potentially long incident names
           selectInput("selected_incident", "Select Incident Type:",
                       choices = incident_choices,
                       selected = "All Incidents")
    ),
    column(5 # Add an empty column for spacing or future elements
           
    )
  ),
  
  hr(), # Add a horizontal rule for separation
  
  # Map Output
  leafletOutput("councilMap", height = "75vh") # Adjusted height slightly
)

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  # --- Reactive Data Filtering ---
  filtered_emergency_data <- reactive({
    req(input$selected_year, input$selected_incident) # Ensure inputs are available
    
    filtered <- emergency_data # Start with the base data
    
    # Filter by Year
    if (input$selected_year != "Overall") {
      filtered <- filtered %>% filter(Year == as.numeric(input$selected_year))
    }
    
    # Filter by Incident Type
    if (input$selected_incident != "All Incidents") {
      filtered <- filtered %>% filter(Incident == input$selected_incident)
    }
    
    # Handle case where filter results in zero rows
    if(nrow(filtered) == 0) {
      # Return an empty dataframe with expected columns to avoid errors downstream
      # Adjust column names if your 'emergency_data' has different ones
      return(tibble(
        Council_District = character(), 
        Incident = character(),
        Council_Member = character(), # Include if used below
        Date_of_Incident = as.POSIXct(character()), # Include if used below
        Year = integer() # Include if used below
        # Add other necessary columns here...
      ))
    }
    
    return(filtered)
  })
  
  # --- Reactive Data Aggregation and Merging for Map ---
  reactive_map_data <- reactive({
    
    current_data <- filtered_emergency_data()
    
    # If no data after filtering, create an empty structure for the map
    if (nrow(current_data) == 0) {
      map_data_agg <- nyc_districts_base %>%
        mutate(
          Total_Incidents = 0,
          Council_Member = "Data Unavailable", # Default text
          TopIncidentsHTML = "<p>No incidents match filter.</p>" # Default text
        )
      return(map_data_agg)
    }
    
    # 1. Calculate total incidents by district for the filtered data
    district_summary <- current_data %>%
      group_by(Council_District) %>%
      summarize(Total_Incidents = n(), .groups = "drop")
    
    # 2. Get top 3 incident types by district for the filtered data
    # Only relevant if "All Incidents" is selected, otherwise it's just the selected one
    if (input$selected_incident == "All Incidents" && nrow(current_data) > 0) {
      top_incidents_filtered <- current_data %>%
        group_by(Council_District, Incident) %>%
        summarize(Count = n(), .groups = "drop") %>%
        group_by(Council_District) %>%
        arrange(Council_District, desc(Count)) %>%
        slice_head(n = 3) %>%
        ungroup()
      
      # Format top incidents as HTML list
      top_incidents_html <- top_incidents_filtered %>%
        group_by(Council_District) %>%
        summarise(
          TopIncidentsHTML = paste0(
            "<ol>",
            paste0("<li>", htmlEscape(Incident), " (", Count, ")</li>", collapse = ""),
            "</ol>"
          ),
          .groups = "drop"
        )
    } else if (nrow(current_data) > 0) {
      # If a specific incident is selected, the "top" list is just that incident's count
      top_incidents_html <- district_summary %>% # Use district_summary as it has the counts
        mutate(TopIncidentsHTML = paste0("<p><b>Selected:</b> ", htmlEscape(input$selected_incident), " (", Total_Incidents, ")</p>")) %>%
        select(Council_District, TopIncidentsHTML)
      
    } else {
      # Create an empty structure if no data, ensuring Council_District column exists
      top_incidents_html <- tibble(Council_District = character(), TopIncidentsHTML = character())
    }
    
    
    # 3. Merge aggregated data with district boundaries
    # Start with base districts, join summary, members, and top incidents HTML
    map_data_agg <- nyc_districts_base %>%
      left_join(district_summary, by = c("district_id" = "Council_District")) %>%
      left_join(council_members_lookup, by = c("district_id" = "Council_District")) %>%
      left_join(top_incidents_html, by = c("district_id" = "Council_District")) %>%
      mutate(
        # Handle NAs from joins - districts with no incidents in the filter get 0
        Total_Incidents = ifelse(is.na(Total_Incidents), 0, Total_Incidents),
        Council_Member = ifelse(is.na(Council_Member), "Data Unavailable", Council_Member),
        TopIncidentsHTML = case_when(
          # If total incidents is 0, override the TopIncidentsHTML
          Total_Incidents == 0 ~ "<p>No incidents match filter.</p>", 
          # If TopIncidentsHTML is NA but Total > 0 (shouldn't happen often with above logic, but safety)
          is.na(TopIncidentsHTML) ~ "<p>Incident data unavailable.</p>", 
          # Otherwise, use the calculated HTML
          TRUE ~ TopIncidentsHTML 
        )
      )
    
    return(map_data_agg)
  })
  
  # --- Render Leaflet Map ---
  output$councilMap <- renderLeaflet({
    
    map_data <- reactive_map_data()
    
    # Define bins (keep consistent for comparability across filters)
    bins <- c(0, 5, 10, 20, 40, 60, Inf) # Adjusted bins slightly for potentially lower counts
    labels <- c("0-5", "6-10", "11-20", "21-40", "41-60", "60+")
    
    # Create palette dynamically based on *current* data range
    # Handle case where max incidents is 0
    max_incidents <- max(map_data$Total_Incidents, na.rm = TRUE)
    palette_domain <- if (max_incidents > 0) map_data$Total_Incidents else c(0, 1) # Avoid error with all zeros
    
    pal <- councildown::colorBin(
      palette = "nycc_blue",
      domain = palette_domain,
      bins = bins,
      na.color = "#FFFFFF",
      right = FALSE
    )
    
    # Generate dynamic popup content
    popup_content <- lapply(seq_len(nrow(map_data)), function(i) {
      row <- map_data[i, ]
      
      # Determine title based on selection
      year_text <- ifelse(input$selected_year == "Overall", "2023-2024", input$selected_year)
      incident_text <- ifelse(input$selected_incident == "All Incidents", "All Incidents", input$selected_incident)
      
      popup_title <- glue::glue("<h4>District {htmlEscape(row$district_id)} ({year_text})</h4>")
      
      # Construct the body
      popup_body <- paste0(
        "<p><b>Council Member:</b> ", htmlEscape(row$Council_Member), "</p>",
        "<p><b>Selected Filter:</b> ", htmlEscape(incident_text), "</p>",
        "<p><b>Total Incidents (matching filter):</b> ", row$Total_Incidents, "</p>",
        "<hr>", # Separator
        "<b>Incident Details:</b>", # Changed title slightly
        row$TopIncidentsHTML # Use the pre-formatted HTML
      )
      
      # Combine using councilPopup
      councilPopup(paste0(popup_title, popup_body))
    })
    
    
    # Create the map
    leaflet(map_data) %>%
      addCouncilStyle(add_dists = TRUE, dist_year = "2023") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Basemap") %>%
      addPolygons(
        fillColor = ~pal(Total_Incidents),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0("District ", district_id, ": ", Total_Incidents, " incidents"),
        popup = popup_content, # Use the generated list of popups
        group = "Districts"
      ) %>%
      # Use a dynamic legend title
      addLegend_decreasing(
        position = "bottomright",
        pal = pal,
        title = glue::glue("Incidents ({ifelse(input$selected_year=='Overall', '23-24', input$selected_year)}, {str_trunc(input$selected_incident, 20)})"), # Shorten long incident names
        values = ~Total_Incidents,
        opacity = 0.8,
        decreasing = TRUE,
        labels = labels # Use the static labels matching the bins
      ) %>%
      addLayersControl(
        baseGroups = c("Basemap"),
        overlayGroups = c("Districts"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addSourceText(
        lng = -74.15,
        lat = 40.55,
        "Source: NYC Council Data Team (Emergency Incident Reports)"
      )
  }) # End renderLeaflet
  
} # End server function

# --- 4. Run the App ---
shinyApp(ui = ui, server = server)
