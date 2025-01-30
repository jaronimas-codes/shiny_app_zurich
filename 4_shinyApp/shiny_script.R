library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(sf)  # Use sf instead of rgdal
library(here)

getwd()
# Gather data

# Define file paths using 'here()'
zurich_json_data <- st_read(here("1_data/json/data/stzh.adm_stadtkreise_v.json"))

# Master, Rent, and Burglary data
data <- read.csv(here("4_shinyApp/data/master_final.csv"))
data_ab <- read.csv(here("4_shinyApp/data/ab_final.csv"))
data_burglary <- read.csv(here("4_shinyApp/data/burglary_final.csv"))
data_rent <- read.csv(here("4_shinyApp/data/rent_final.csv"))

# Extra data of center points in each neighborhood
center_data <- read.csv(here("4_shinyApp/data/kreis_center_coords.csv"))

tmp_all <- data_burglary %>%
  left_join(center_data, by = "neighbourhood_group") %>%
  left_join(data_rent, by = "neighbourhood_group") %>%
  mutate(FactsRent = "Average Rent")

# Color Palette
palette <- colorFactor("Accent", domain = unique(data$room_type[!is.na(data$room_type)]))

# shiny app start
ui <- fluidPage(
  titlePanel("Interactive Map with Leaflet"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxGroupInput("district", "Select District", choices = unique(tmp_all$neighbourhood_group), selected = c("Kreis 1", "Kreis 5", "Kreis 4")),
      checkboxGroupInput("categories", "Select Room Type", choices = unique(data$room_type), selected = unique(data$room_type)),
      checkboxGroupInput("burglary", "Select Burglary Rate", choices = unique(tmp_all$FactsEnglish), selected = unique(tmp_all$FactsEnglish)),
      checkboxGroupInput("rent", "Select Rent Rate", choices = unique(tmp_all$FactsRent), selected = unique(tmp_all$FactsRent))
    ),
    mainPanel(
      width = 9,
      leafletOutput("map"),
      uiOutput("dynamic_columns")
    )
  )
)

server <- function(input, output, session) {
  # Filter data dynamically
  filtered_data <- reactive({
    data %>%
      filter(
        room_type %in% input$categories,
        neighbourhood_group %in% input$district
      )
  })
  
  filtered_burglary <- reactive({
    tmp_all %>%
      filter(
        FactsEnglish %in% input$burglary,
        neighbourhood_group %in% input$district
      )
  })
  
  filtered_rent <- reactive({
    tmp_all %>%
      filter(
        FactsRent %in% input$rent,
        neighbourhood_group %in% input$district
      )
  })
  
  # Summary section
  unique_neighbourhoods <- reactive({
    unique(filtered_data()$neighbourhood_group)
  })
  
  output$dynamic_columns <- renderUI({
    columns <- lapply(unique_neighbourhoods(), function(kreis_item) {
      filtered_data_kreis <- filtered_data()[filtered_data()$neighbourhood_group == kreis_item, ]
      filtered_rent_kreis <- filtered_rent()[filtered_rent()$neighbourhood_group == kreis_item, ]
      filtered_burglary_kreis <- filtered_burglary()[filtered_burglary()$neighbourhood_group == kreis_item, ]
      
      summary <- paste(
        kreis_item,
        "Listings Total:", nrow(filtered_data_kreis),
        "Average Rent: ", ifelse(nrow(filtered_rent_kreis) > 0, mean(filtered_rent_kreis$avg.price, na.rm = TRUE), "N/A"),
        "Average Burglary: ", ifelse(nrow(filtered_burglary_kreis) > 0, round(mean(filtered_burglary_kreis$BurglariesPerResident, na.rm = TRUE), 3), "N/A"),
        sep = "\n"
      )
      
      column(width = 3, renderPrint({ cat(summary) }))
    })
    
    do.call(tagList, columns)
  })
  
  # Map rendering
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(lat = 47.37, lng = 8.543073, zoom = 12) %>%
      addPolygons(data = zurich_json_data, color = "darkgrey", weight = 1) %>%
      addCircles(
        lat = filtered_data()$latitude, lng = filtered_data()$longitude,
        color = palette(filtered_data()$room_type),
        label = paste(filtered_data()$room_type, "CHF :", filtered_data()$price)
      ) %>%
      addCircles(
        lat = filtered_rent()$center_lat, lng = filtered_rent()$center_longitude,
        label = paste("Average rent per District: ", filtered_rent()$avg.price),
        radius = filtered_rent()$avg.price^2 / 4000, color = "blue",
        stroke = TRUE, fill = FALSE, weight = 1.5
      ) %>%
      addCircles(
        lat = filtered_burglary()$center_lat, lng = filtered_burglary()$center_longitude,
        label = paste("Burglary rate per Resident: ", round(filtered_burglary()$BurglariesPerResident * 100, 2)),
        radius = filtered_burglary()$BurglariesPerResident * 40000,
        color = "red", stroke = TRUE, fill = FALSE, weight = 1.5
      )
  })
}

shinyApp(ui, server)
