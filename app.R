# app.R

# --- 0. Libraries ---
# Make sure all required packages are installed:
# install.packages(c("shiny", "readxl", "dplyr", "tidyr", "lubridate", "stringr", 
#                    "sf", "leaflet", "htmltools", "htmlwidgets", 
#                    "councilcount", "councildown", "councilverse"))

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2) # While not used in the final map, might be needed by dependencies or future plots
library(lubridate)
library(stringr)
library(knitr) # While not used in the final map, might be needed by dependencies
library(sf)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(councilcount) 
library(councildown)
library(councilverse) # Used for saving, not interactive display, comment out for now if not needed for styling
library(glue) # Used implicitly by map saving, good to include if adding save features later

# --- 1. Data Loading and Preprocessing (Run Once on App Start) ---

# IMPORTANT: Change this path if needed. 
# Best practice: Place the Excel file in the same directory as app.R, 
# or in a subdirectory (e.g., 'data/') and use "data/emergency_data_cleaned.xlsx"
emergency_data_file <- "~/Documents/Emergency_Notifs_for_CED/data/output/emergency_data_cleaned.xlsx" # Assumes file is in the same directory as app.R

if (!file.exists(emergency_data_file)) {
  stop(glue::glue("Error: Data file not found at '{emergency_data_file}'. 
       Please place the file in the app directory or update the path."))
}

# Step 1: Read the emergency data
emergency_data_raw <- read_excel(emergency_data_file)

# Filter Out ALL CMs category and make sure Council_District is character
emergency_data <- emergency_data_raw %>%
  filter(Council_District != "All Districts") %>%
  mutate(Council_District = as.character(Council_District)) # Ensure character type early

# Step 2: Download NYC Council Districts boundaries
nyc_districts_url <- "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_City_Council_Districts/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson"
temp_geojson <- tempfile(fileext = ".geojson")
tryCatch({
  download.file(nyc_districts_url, temp_geojson, mode = "wb", quiet = TRUE)
  nyc_districts <- st_read(temp_geojson)
}, error = function(e) {
  stop("Failed to download or read NYC Council Districts GeoJSON. Check URL or internet connection. Error: ", e$message)
})

# Step 3: Process emergency data 
emergency_data <- emergency_data %>%
  mutate(Year = year(Date_of_Incident)) %>%
  filter(Year %in% c(2023, 2024))

# Calculate total incidents by district and year
district_year_summary <- emergency_data %>%
  group_by(Council_District, Year) %>%
  summarize(Total_Incidents = n(), .groups = "drop") %>%
  arrange(Council_District, Year)

# Get top 3 incident types by district
top_incidents_by_district <- emergency_data %>%
  group_by(Council_District, Incident) %>%
  summarize(Count = n(), .groups = "drop") %>%
  group_by(Council_District) %>%
  arrange(Council_District, desc(Count)) %>%
  slice_head(n = 3) %>%
  ungroup()

# Get council member names by district (taking the latest value if multiple exist)
council_members <- emergency_data %>%
  filter(!is.na(Council_Member)) %>% # Ensure we don't get NA if possible
  arrange(Council_District, desc(Date_of_Incident)) %>% # Take the most recent non-NA name
  group_by(Council_District) %>%
  slice_head(n = 1) %>%
  select(Council_District, Council_Member) %>%
  ungroup()

# Pre-calculate the HTML for the top 3 incidents list for each district
top_incidents_html <- top_incidents_by_district %>%
  group_by(Council_District) %>%
  summarise(
    TopIncidentsHTML = {
      # Create list items
      list_items <- paste0("<li>", htmlEscape(Incident), " (", Count, ")</li>", collapse = "")
      # Wrap in ordered list tags
      paste0("<ol>", list_items, "</ol>")
    }, 
    .groups = "drop"
  )

# Step 4: Merge the data with the district boundaries
district_year_wide <- district_year_summary %>%
  pivot_wider(
    names_from = Year,
    values_from = Total_Incidents,
    names_prefix = "Incidents_",
    values_fill = 0 # Fill missing years for a district with 0
  )

# Ensure district IDs are character in the spatial data
nyc_districts <- nyc_districts %>%
  mutate(district_id = as.character(CounDist))

# Perform the joins
nyc_map_data <- nyc_districts %>%
  left_join(district_year_wide, by = c("district_id" = "Council_District")) %>%
  left_join(council_members, by = c("district_id" = "Council_District")) %>%
  left_join(top_incidents_html, by = c("district_id" = "Council_District"))

# Calculate total incidents and handle potential NAs from joins
nyc_map_data <- nyc_map_data %>%
  mutate(
    Incidents_2023 = ifelse(is.na(Incidents_2023), 0, Incidents_2023),
    Incidents_2024 = ifelse(is.na(Incidents_2024), 0, Incidents_2024),
    Total_Incidents = Incidents_2023 + Incidents_2024,
    # Fill missing Council Member / Top Incidents with placeholders
    Council_Member = ifelse(is.na(Council_Member), "Data Unavailable", Council_Member),
    TopIncidentsHTML = ifelse(is.na(TopIncidentsHTML), "<p>No incident data available.</p>", TopIncidentsHTML)
  )

# Define bins and palette (using councildown)
bins <- c(0, 20, 40, 60, 80, 100, Inf)
pal <- councildown::colorBin(
  palette = "nycc_blue", 
  domain = nyc_map_data$Total_Incidents,
  bins = bins,
  na.color = "#FFFFFF", # Color for districts with no data (though we handle NAs above)
  right = FALSE
)
labels <- c("0-20", "21-40", "41-60", "61-80", "81-100", "100+")


# --- 2. UI Definition ---
ui <- fluidPage(
  titlePanel("NYC Council Districts: Emergency Incidents Map (2023-2024)"),
  
  # Map Output
  leafletOutput("councilMap", height = "85vh") # Use vh for relative height
)

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  # Render the leaflet map
  output$councilMap <- renderLeaflet({
    
    leaflet(nyc_map_data) %>%
      # Apply council style to the map object and load dependencies
      addCouncilStyle() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Basemap") %>%
      addPolygons(
        fillColor = ~pal(Total_Incidents),
        weight = 1, # Slightly thinner lines
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3, # Thicker highlight
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0("District ", district_id), # Simple label on hover
        popup = ~lapply(councildown::councilPopup( # Use council styled popup
          paste0(
            "<h4>Council District ", htmlEscape(district_id), "</h4>",
            "<p><b>Council Member:</b> ", htmlEscape(Council_Member), "</p>",
            "<p><b>Total Incidents:</b></p>",
            "<ul>",
            "<li>2023: ", Incidents_2023, "</li>",
            "<li>2024: ", Incidents_2024, "</li>",
            "<li><b>Total: ", Total_Incidents, "</b></li>",
            "</ul>",
            "<p><b>Top 3 Incident Types (2023-2024):</b></p>",
            TopIncidentsHTML # Use the pre-calculated HTML string
          )
        ), htmltools::HTML), # Ensure HTML is rendered correctly
        group = "Districts" # Assign to a group for potential layer control
      ) %>%
      councildown::addLegend_decreasing( # Use council styled legend
        position = "bottomright", # Changed position slightly
        pal = pal,
        title = "Total Incidents (2023-2024)",
        values = ~Total_Incidents, # Use formula notation
        opacity = 0.8,
        decreasing = FALSE, # Match the labels order (0-20 at bottom)
        labels = labels # Use the predefined labels
      ) %>%
      # Optional: Add layer control if you have multiple base maps or overlays
      addLayersControl(
        baseGroups = c("Basemap"),
        overlayGroups = c("Districts"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      # Add source text (optional, adjust coordinates if needed)
      councildown::addSourceText(
        lng = -74.15, # Adjusted slightly left
        lat = 40.55,  # Adjusted slightly down
        "Source: NYC Council Data Team (Emergency Incident Reports 2023-2024)"
      ) %>%
      # Optional: Add council district labels directly on the map
      # councildown::addCouncilStyle(add_dists = TRUE, dist_year = "2023") # Applies labels based on 2023 districts
      # Note: This might make the map cluttered. Consider using hover labels only.
      identity() # End pipe cleanly
    
  }) # End renderLeaflet
  
} # End server function

# --- 4. Run the App ---
shinyApp(ui = ui, server = server)