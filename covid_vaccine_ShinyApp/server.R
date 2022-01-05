# COVID vaccination

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  
  # observe({
  #   updateSelectInput(session, "county", "Select or type in one county", 
  #                     choices = us_county_covid[us_county_covid$STATE_NAME == input$state,]$NAME)
  # })
  
  initial_lat = 39.8283
  initial_lng = -98.5795
  initial_zoom = 4
  
  
  observe({
    input$reset_button
    leafletProxy("map") %>% setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom)
  })
  
  bins <- c(0, 25, 50, 75, 100)
  mypal <- colorBin("Blues", domain = us_county_covid$series_complete_pop_pct, bins = bins)
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik")%>%
      setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
      addPolygons(
        data = us_county_covid,
        fillColor = ~mypal(us_county_covid$series_complete_pop_pct),
        color ="black",
        stroke = T,
        smoothFactor = 0.2,
        fillOpacity = 0.5,
        weight = 1,
        highlightOptions = highlightOptions(fillColor = "black",
                                            bringToFront = TRUE),
        label = labels
      ) %>%
      addLegend(
        position = "topright",
        pal= mypal,
        values = us_county_covid$series_complete_pop_pct,
        title = "Vaccination Percentage",
        opacity = 1
      ) %>% 
      addControl(
        actionButton("reset_button", "Reset"),
        position="topleft")
    
  })
  
})
