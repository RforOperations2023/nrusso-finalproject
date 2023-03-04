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
library(htmltools)

df <- read_csv('IncHousing.csv')
#reading in geojson us map for base leaflet
usmap  <- geojson_sf('https://rstudio.github.io/leaflet/json/us-states.geojson')
#joining csv to spatial data
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000)
#subsetting and combining csv to shapefile for later subsetting
agg_tbl <- df %>%
  group_by(name) %>%
  summarise(total_units=sum(TotalAffordableUnits,na.rm=TRUE),
            total_progs = n())
left_join(usmap, agg_tbl, by = 'name') -> mapData
mapData[is.na(mapData)] <- 0
pal <- colorBin("YlOrRd", domain = mapData$total_progs, bins = bins)


# Define UI 
ui <- navbarPage("Inclusionary Housing Policies in the US",
                 theme = shinytheme("cerulean"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("unitsORprograms",
                                          "select data to view:",
                                          choices = c("Total Affordable Units"
                                                      = "TotalAffordableUnits",
                                                      "Number of Programs" 
                                                      = "name"),
                                          selected = "name"
                                          )),
                            # Map Panel
                            mainPanel(
                              leafletOutput("map")
                            )
                          )
                 ),
                 # Data Table Panel
                 tabPanel("Data",
                          fluidPage(
                            wellPanel(DT::dataTableOutput("table"))
                          )
                 ),
                 tabPanel("Scatterplot",
                   sidebarLayout(
                   sidebarPanel(
                     selectInput("scatterplotx",
                                  "select data to view for scatterplot:",
                                  choices = c("Total Affordable Rental Units"
                                              = "TotalAffordableRentalUnits",
                                              "Total Affordable Ownership Units" 
                                              = "TotalAffordableOwnershipUnits",
                                              "Total Affordable nits" 
                                              = "TotalAffordableUnits"),
                                  selected = "TotalAffordableRentalUnits"
                     ),
                     selectInput("scatterploty",
                                 "select data to view for scatterplot:",
                                 choices = c("Total Affordable Rental Units"
                                             = "TotalAffordableRentalUnits",
                                             "Total Affordable Ownership Units" 
                                             = "TotalAffordableOwnershipUnits",
                                             "Total Affordable Units" 
                                             = "TotalAffordableUnits"),
                                 selected = "TotalAffordableUnits"
                     )),
                   # plot Panel
                   mainPanel(
                     plotlyOutput("scatter")
                   )
                   )
                 )
)
#info for this comes form https://rstudio.github.io/leaflet/choropleths.html
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(mapData) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal(total_progs),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "3",
        fillOpacity = 0.7,
        stroke = TRUE,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#663",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        popup = ~paste0("# of programs:", total_progs,"<hr>","State:", name), 
          group = "total_progs", layerId = ~name, fill = TRUE,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
       addLegend(pal = pal, values = ~total_progs, opacity = 0.9, title = "legend",
                 position = "bottomright")
    
  })
  
  output$table <- DT::renderDataTable(

      DT::datatable(data = df[,c( "ProgramName", "City", "State",
                                      "County", "Program Applicable Areas",
                                  "Minimum Set-aside")], 
                    options = list(pageLength = 50), 
                    rownames = FALSE))
  
  output$scatter <- renderPlotly({
    dat <- subset(df, !is.na(TotalAffordableUnits))
    dat <- subset(df, !is.na( TotalAffordableOwnershipUnits))
    dat <- subset(df, !is.na( TotalAffordableRentalUnits))
    plt <- dat[, c("TotalAffordableUnits", "TotalAffordableOwnershipUnits",
                    "TotalAffordableRentalUnits")]
    plt[is.na(plt)] <- 0
    
    ggplotly(
      ggplot()) +
        geom_point(data = plt, aes_string(x = input$scatterplotx, y = input$scatterploty,
                              color = 'blue'))
    
  })

  #I had to remove this code because it also did not function with my 
  #leaflet proxy
#reactive subsetting function based on radio button inputs to change map values
  # total_counts <- reactive({
  #   if(input$unitsORprograms == "Number of Programs") {
  #   
  #     new <- c("name", "total_progs", "geometry")
  #     
  #     return(new)
  #     
  #   # emptydf <- data.frame()
  #   # agg_tbl <- df %>%
  #   #   group_by(name) %>%
  #   #   summarise(total_count = n())
  #   # left_join(usmap, agg_tbl, by = 'name') -> mapData
  #   # mapData[is.na(mapData)] <- 0
  #   # emptydf <- rbind(emptydf, mapData)
  #   # return(emptydf)
  #     }
  # #
  #     else {
  #       new1 <- c("name", "total_units", "geometry")
  #       # group_by(name) %>%
  #       #   summarise(total_count = sum(TotalAffordableUnits, na.rm = TRUE))
  #       # left_join(usmap, agg_tbl, by = 'name') -> mapData
  #       # mapData[is.na(mapData)] <- 0
  #       # emptydf <- rbind(emptydf, mapData)
  #       # return(emptydf)
  #       return(new1)
  #       }
  #       
  # 
  # })
  
#could not figure out how to get observes to work
  # observe({
  #   new <- total_counts()
  #   
  #   leafletProxy("leaflet", data = new) %>%
  #     clearGroup(group = "total_progs") %>%
  #     addPolygons(
  #       fillColor = ~pal(toal_progs),
  #       weight = 1,
  #       opacity = 1,
  #       color = "black",
  #       dashArray = "3",
  #       fillOpacity = 0.7,
  #       stroke = TRUE,
  #       highlightOptions = highlightOptions(
  #         weight = 5,
  #         color = "#663",
  #         dashArray = "",
  #         fillOpacity = 0.7,
  #         bringToFront = TRUE),
  #       popup = ~htmlEscape(name), 
  #       group = "total_progs", layerId = ~name, fill = TRUE,
  #       labelOptions = labelOptions(
  #         style = list("font-weight" = "normal", padding = "3px 8px"),
  #         textsize = "15px",
  #         direction = "auto")) #%>%
  #     # addLegend(pal = pal, values = ~total_progs, opacity = 0.7, title = NULL,
  #     #           position = "bottomright")
  # })
  }
  


# Run the application 
shinyApp(ui = ui, server = server)