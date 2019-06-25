

new_data_ <- yeet_teamt_TableToExcel
new_data_2 <- cwb2016
new_data_ <- new_data_ %>% 
  mutate("xcoord" = POINT_X, POINT_X = NULL, "ycoord" = POINT_Y, POINT_Y = NULL )
new_data_$CSDUID <- as.numeric(new_data_$CSDUID)
new_data_1 <- new_data_ %>% select(CSDUID, xcoord, ycoord)
new_data_1$CSDUID <- as.numeric(new_data_1$CSDUID)
new_data_ <- new_data_1 %>% left_join(new_data_2, by = "CSDUID")
new_data_ <- new_data_[complete.cases(new_data_), ]
new_data_high <- new_data_ %>% filter(cwb > "88")
new_data_low <- new_data_ %>% filter(cwb < "41")




#This is where we start

library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


ui <- fluidPage(titlePanel("Mapping the Community Well-Being Index :)"),
  leafletOutput("mymap"),
  leafletOutput("mymap1"),
  p(),
  sidebarLayout(sidebarPanel("census_pop", "Population", min = 250, max = 2731571,
                             value = c(25, 40)),
    mainPanel(tabsetPanel(type = "tabs",
                          tabPanel("Plot"),
                          tabPanel("Summary"),
                          tabPanel("Table")
  ))))

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'green',
  library = 'ion'
)

icons2 <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'red',
  library = 'ion'
)

server <- {
  
output$mymap <- output$mymap <- renderLeaflet({leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL,group = 'Graticules') %>%
    setView(lng = -100.487632, lat = 60.246243, zoom = 3) %>%
    hideGroup(c("Place names")) %>%
  addAwesomeMarkers(map, data = new_data_high, lng = new_data_high$xcoord, lat = new_data_high$ycoord, icon = icons,
                    popup = ~paste0( 'blabla. <br> <br> blabla'),
                    popupOptions = popupOptions(maxHeight = "200"
                                                , maxWidth = "300")) %>%
    addAwesomeMarkers(map, data = new_data_low, lng = new_data_low$xcoord, lat = new_data_low$ycoord, icon = icons2,
                      popup = ~paste0( csd_name,  ' <br> <br> ', census_pop)
                      )  })

  }

shinyApp(ui, server)



