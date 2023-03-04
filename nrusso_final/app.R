library(leaflet.providers)
library(maps)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(stringr)
library(tools)
library(shiny)
library(shinythemes)
library(sf)
library(geojsonsf)

df <- read_csv('IncHousing.csv')

#reading in gejson us map for base leaflet
usmap  <- geojson_sf('https://rstudio.github.io/leaflet/json/us-states.geojson')

#joining csv to spatial data
left_join(usmap, df, by = 'name') -> mapData

# list_df <- list(usmap,df)
# mapData <- list_df %>% reduce(full_join, by='name')
# mapData <- sf::st_as_sf(mapData)

#another way to load data I didn't use
#https://api.tiles.mapbox.com/v4/{id}/{z}/{x}/{y}{r}.png?access_token={accessToken}

#defining bins and palettew for 
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = mapData$density, bins = bins)


# Define UI 
ui <- navbarPage("Inclusionary Housing Policies in the US",
                 theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              
                            ),
                            # Map Panel
                            mainPanel(
                              # Using Shiny JS
                              shinyjs::useShinyjs(),
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Page
                              leafletOutput("map")
                            )
                          )
                 )#,
                 # # Data Table Pannel
                 # tabPanel("Data",
                 #          fluidPage(
                 #            wellPanel(DT::dataTableOutput("table"))
                 #          )
                 # )
)
#info for this comes form https://rstudio.github.io/leaflet/choropleths.html
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(mapData) %>%
      setView(-96, 37.8, 4) %>%
      # addProviderTiles("MapBox", options = providerTileOptions(
      #   id = "mapbox.light",
      #   accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = "red",
        weight = 2,
        # opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        stroke = TRUE,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        popup = ~paste0("<b>", name, "</b>"), group = "mapData", layerId = ~State, fill = FALSE,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)