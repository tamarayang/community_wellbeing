library(leaflet)
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(h2("Mapping the Community Well-Being Index :)", align = "center"),  h1("A YEETeam Endeavour")),
  
  # Sidebar layout with input and output definitions ----
  
      
      # br() element to introduce extra vertical spacing ----
      br(),
    
    # Main panel for displaying outputs ----
    mainPanel( p("A YEETeam Endeavour"),
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("CWB", leafletOutput("mymap")),
                  tabPanel("Population", leafletOutput("mymap1")),
                  tabPanel("Income", leafletOutput("mymap2"))
      )
      
    )
  )


# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$mymap <- output$mymap <- renderLeaflet({leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
      addWMSTiles(
        "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
        layers = c("1-degree grid", "5-degree grid"),
        options = WMSTileOptions(format = "image/png8", transparent = TRUE),
        attribution = NULL,group = 'Graticules') %>%
      setView(lng = -100.487632, lat = 60.246243, zoom = 3) %>%
      hideGroup(c("Place names")) %>%
      addAwesomeMarkers(map, data = new_data_high, lng = new_data_high$xcoord, lat = new_data_high$ycoord, icon = icons2,
                        popup = ~paste0( csd_name,  ' <br> <br> ', "Population: ", census_pop, '<br> <br>',"Income: ", income, '<br> <br>', "Well-Being: ", cwb)) %>%
      addAwesomeMarkers(map, data = new_data_low, lng = new_data_low$xcoord, lat = new_data_low$ycoord, icon = icons,
                        popup = ~paste0(csd_name,  ' <br> <br> ', "Population: ", census_pop, '<br> <br>',"Income: ", income, '<br> <br>', "Well-Being: ", cwb))
  })
  
  # Generate a summary of the data ----
  output$mymap1 <- output$mymap1 <- renderLeaflet({leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
      addWMSTiles(
        "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
        layers = c("1-degree grid", "5-degree grid"),
        options = WMSTileOptions(format = "image/png8", transparent = TRUE),
        attribution = NULL,group = 'Graticules') %>%
      setView(lng = -100.487632, lat = 60.246243, zoom = 3) %>%
      hideGroup(c("Place names")) %>%
      addAwesomeMarkers(map, data = new_data_, lng = new_data_$xcoord, lat = new_data_$ycoord, icon = icons2,
                        popup = ~paste0(csd_name,  ' <br> <br> ', census_pop, '<br> <br>', income, '<br> <br>', "Well-Being: ", cwb),
                        popupOptions = popupOptions(maxHeight = "200"
                                                    , maxWidth = "300")) %>%
      addAwesomeMarkers(map, data = new_data_low, lng = new_data_low$xcoord, lat = new_data_low$ycoord, icon = icons,
                        popup = ~paste0( csd_name,  ' <br> <br> ', census_pop, '<br> <br>', income, '<br> <br>', "Well-Being: ", cwb, '<br> <br>'),
                        popupOptions = popupOptions(maxHeight = "200"
                                                    , maxWidth = "300"))
  })
  
  # Generate an HTML table view of the data ----
  
  output$mymap2 <- output$mymap2 <- renderLeaflet({leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
      addWMSTiles(
        "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
        layers = c("1-degree grid", "5-degree grid"),
        options = WMSTileOptions(format = "image/png8", transparent = TRUE),
        attribution = NULL,group = 'Graticules') %>%
      setView(lng = -100.487632, lat = 60.246243, zoom = 3) %>%
      hideGroup(c("Place names")) %>%
      addAwesomeMarkers(map, data = new_data_high, lng = new_data_high$xcoord, lat = new_data_high$ycoord, icon = icons,
                        popup = ~paste0(csd_name,  ' <br> <br> ', census_pop, '<br> <br>', income, '<br> <br>', "Well-Being: ", cwb),
                        popupOptions = popupOptions(maxHeight = "200"
                                                    , maxWidth = "300")) %>%
      addAwesomeMarkers(map, data = new_data_low, lng = new_data_low$xcoord, lat = new_data_low$ycoord, icon = icons2,
                        popup = ~paste0(csd_name,  ' <br> <br> ', census_pop, '<br> <br>', income, '<br> <br>', "Well-Being: ", cwb),
                        popupOptions = popupOptions(maxHeight = "200"
                                                    , maxWidth = "300"))
  })
  
  
  
  
}
shinyApp(ui, server)
