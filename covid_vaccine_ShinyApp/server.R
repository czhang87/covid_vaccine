# COVID vaccination

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  
  # # update the county drop down menu based on the selection of the state drop down menu
  # observe({
  #   updateSelectInput(session, "county", "Select or type in one county",
  #                     choices = us_county_covid[us_county_covid$STATE_NAME == input$state,]$NAME)
  # })
  
  
  # Initial view
  initial_lat = 39.8283
  initial_lng = -98.5795
  initial_zoom = 4
  
  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik")%>%
      setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom)
  })
  
  # Reset button in the map
  observe({
    input$reset_button
    leafletProxy("map") %>% setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom)
  })
  
  # Choropleth based on data types using leafletProxy in observe({})
  observe({
    
    # filter data based on metro status
    data_filtered <- us_county_covid %>%
      filter(metro_status %in% input$metro)
    
    # filter data based on state
    if("United States" %in% input$state){
      data_filtered <- data_filtered
    }
    else{
      data_filtered <- data_filtered %>%
        filter(STATE_NAME %in% input$state)
    }
    

    
    # labels for the map popup
    labels <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "At lease one dose:", data_filtered$administered_dose1_pop_pct, "%<br/>",
      "Fully vaccinated:", data_filtered$series_complete_pop_pct,"%<br/>",
      "Booster dose:", data_filtered$booster_doses_pop_pct,"%<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # Vaccination
    
    if ( input$data_type == "Vaccination"){
      
      # At least one dose
      if (input$vaccination_status == 'At Lease One Dose'){
        bins <- c(0, 25, 50, 75, 100)
        mypal <- colorBin("Blues", domain = data_filtered$administered_dose1_pop_pct, bins = bins)
        
        #leafletProxy map
        leafletProxy("map", data = data_filtered )%>%
          clearControls() %>% # clear the map legend
          clearShapes() %>% # clear polygons fill color
          addControl(
            actionButton("reset_button", "Reset"),
            position="topleft") %>% # add Reset View button in the map
          addPolygons(
            data = data_filtered,
            fillColor = ~mypal(data_filtered$administered_dose1_pop_pct),
            color ="black",
            stroke = T,
            smoothFactor = 0.2,
            fillOpacity = 0.75,
            weight = 1,
            highlightOptions = highlightOptions(fillColor = "black",
                                                bringToFront = TRUE),
            label = labels
          ) %>%
          addLegend(
            position = "topright",
            pal= mypal,
            values = data_filtered$administered_dose1_pop_pct,
            title = "Vaccination Percentage",
            opacity = 1
          )
      }
      
      # Fully vaccinated
      else if (input$vaccination_status == 'Fully Vaccinated'){
        
        # palette
        bins <- c(0, 25, 50, 75, 100)
        mypal <- colorBin("Blues", domain = data_filtered$series_complete_pop_pct, bins = bins)
        
        #leafletProxy map
        leafletProxy("map", data = data_filtered )%>%
          clearControls() %>% # clear the map legend
          clearShapes() %>% # clear polygons fill color
          addControl(
            actionButton("reset_button", "Reset"),
            position="topleft") %>% # add Reset View button in the map
          addPolygons(
            data = data_filtered,
            fillColor = ~mypal(data_filtered$series_complete_pop_pct),
            color ="black",
            stroke = T,
            smoothFactor = 0.2,
            fillOpacity = 0.75,
            weight = 1,
            highlightOptions = highlightOptions(fillColor = "black",
                                                bringToFront = TRUE),
            label = labels
          ) %>%
          addLegend(
            position = "topright",
            pal= mypal,
            values = data_filtered$series_complete_pop_pct,
            title = "Vaccination Percentage",
            opacity = 1
          )
      }
      
      # Booster dose
      else{
        
        # palette
        bins <- c(0, 25, 50, 75, 100)
        mypal <- colorBin("Blues", domain = data_filtered$booster_doses_pop_pct, bins = bins)
        
        #leafletProxy map
        leafletProxy("map", data = data_filtered )%>%
          clearControls() %>% # clear the map legend
          clearShapes() %>% # clear polygons fill color
          addControl(
            actionButton("reset_button", "Reset"),
            position="topleft") %>% # add Reset View button in the map
          addPolygons(
            data = data_filtered,
            fillColor = ~mypal(data_filtered$booster_doses_pop_pct),
            color ="black",
            stroke = T,
            smoothFactor = 0.2,
            fillOpacity = 0.75,
            weight = 1,
            highlightOptions = highlightOptions(fillColor = "black",
                                                bringToFront = TRUE),
            label = labels
          ) %>%
          addLegend(
            position = "topright",
            pal= mypal,
            values = data_filtered$booster_doses_pop_pct,
            title = "Vaccination Percentage",
            opacity = 1
          )
      }
      
      
    }
    
    # Hopitalization
    else if (input$data_type == "Hospitalizations per 100k Last 7 Days"){
      
      # palette
      bins <- c(0, 25, 50, 75, 100)
      mypal <- colorBin("Reds", domain = data_filtered$conf_covid_admit_100k_last_7, bins = bins)
      
      #leafletProxy map
      leafletProxy("map", data = data_filtered) %>%
        clearControls() %>%
        clearShapes() %>%
        addControl(
          actionButton("reset_button", "Reset"),
          position="topleft") %>%
        addPolygons(
          data = data_filtered,
          fillColor = ~mypal(data_filtered$conf_covid_admit_100k_last_7),
          color ="black",
          stroke = T,
          smoothFactor = 0.2,
          fillOpacity = 0.75,
          weight = 1,
          highlightOptions = highlightOptions(fillColor = "black",
                                              bringToFront = TRUE),
          label = labels
        ) %>%
        addLegend(
          position = "topright",
          pal= mypal,
          values = data_filtered$conf_covid_admit_100k_last_7,
          title = "Hospitalizations per 100k Last 7 Days",
          opacity = 1
        )
    }
  })
  
  
  
})
