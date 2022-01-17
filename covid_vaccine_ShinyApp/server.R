# COVID in the U.S.

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  
  # update the county drop down menu based on the selection of the state drop down menu
  observe({
    updateSelectInput(session, "county",
                      choices = us_county_covid[us_county_covid$STATE_NAME == input$state_rank,]$NAME)
  })
  
  # Reset Input Button
  observeEvent(input$reset_input, {
    
    updateSelectInput(session, "data_type", choices = choices_data_type)
    updateRadioButtons(session, "vaccination_status", choices = choices_vaccination_status)
    updateSelectInput(session, "xvariable", choices = choices_xvariable)
    updateSelectInput(session, "yvariable",choices = choices_yvariable, selected = "test_positivity_rate_last_7_d")
    updateSelectInput(session, "hue", choices = hue_labels)
    updatePickerInput(session, "table_columns_selected", selected = table_columns, choices = table_columns,
                      choicesOpt = list(
                        style = rep(("color: black; background: white;"),20)))
    updateSelectInput(session, "state", selected = "United States", choices = choices_state)
    updateCheckboxGroupInput(session, "metro", selected = choices_metro, choices = choices_metro)
    updateNumericRangeInput(session, "population", value = c(min_pop,max_pop))
    
  })
  
  # MAP TAB
  
  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik")%>%
      # addTiles() %>% 
      setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom)
  })
  
  # Reset button in the map
  observe({
    input$reset_map
    leafletProxy("map") %>% setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom)
  })
  
  
  observe({
    
    # Choropleth based on data types using leafletProxy in observe({})
    # filter data based on metro status
    data_filtered <- us_county_covid %>%
      filter(metro_status %in% input$metro) %>% 
      filter(between(POPULATION, input$population[1], input$population[2]))
    
    # filter data based on state
    if("United States" %in% input$state){
      data_filtered <- data_filtered
    }
    else{
      data_filtered <- data_filtered %>%
        filter(STATE_NAME %in% input$state)
    }
    
    # labels of vaccination percentage for the map popup
    labels_all_ages <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Population:", format(data_filtered$POPULATION, big.mark = ",", scientific = F), "<br/>",
      "At lease one dose:", data_filtered$administered_dose1_pop_pct, "%<br/>",
      "Fully vaccinated:", data_filtered$series_complete_pop_pct,"%<br/>",
      "Booster dose:", data_filtered$booster_doses_pop_pct,"%<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$svi_category,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    labels_5plus <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Population:", format(data_filtered$POPULATION, big.mark = ",", scientific = F), "<br/>",
      "At lease one dose:", data_filtered$administered_dose1_recip_5pluspop_pct, "%<br/>",
      "Fully vaccinated:", data_filtered$series_complete_5pluspop_pct,"%<br/>",
      "Booster dose:", data_filtered$booster_doses_pop_pct,"%<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$svi_category,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    labels_12plus <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Population:", format(data_filtered$POPULATION, big.mark = ",", scientific = F), "<br/>",
      "At lease one dose:", data_filtered$administered_dose1_recip_12pluspop_pct, "%<br/>",
      "Fully vaccinated:", data_filtered$series_complete_12pluspop,"%<br/>",
      "Booster dose:", data_filtered$booster_doses_pop_pct,"%<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$svi_category,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    labels_18plus <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Population:", format(data_filtered$POPULATION, big.mark = ",", scientific = F), "<br/>",
      "At lease one dose:", data_filtered$administered_dose1_recip_18pluspop_pct, "%<br/>",
      "Fully vaccinated:", data_filtered$series_complete_18pluspop,"%<br/>",
      "Booster dose:", data_filtered$booster_doses_18pluspop_pct,"%<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$svi_category,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    labels_65plus <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Population:", format(data_filtered$POPULATION, big.mark = ",", scientific = F), "<br/>",
      "At lease one dose:", data_filtered$administered_dose1_recip_65pluspop_pct, "%<br/>",
      "Fully vaccinated:", data_filtered$series_complete_65pluspop,"%<br/>",
      "Booster dose:", data_filtered$booster_doses_65pluspop_pct,"%<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$svi_category,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of cases
    labels_cases <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Population:", format(data_filtered$POPULATION, big.mark = ",", scientific = F), "<br/>",
      "Cases per 100k Last 7 Days:", data_filtered$Cases_per_100k_last_7_days,"<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$svi_category,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of test positivity
    labels_tests <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Population:", format(data_filtered$POPULATION, big.mark = ",", scientific = F), "<br/>",
      "Test Positivity Rate Last 7 Days:", data_filtered$test_positivity_rate_last_7_d,"<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$svi_category,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of hospitalization
    labels_hospitalizations <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Population:", format(data_filtered$POPULATION, big.mark = ",", scientific = F), "<br/>",
      "Hospitalizations per 100k Last 7 Days:", data_filtered$conf_covid_admit_100k_last_7,"<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$svi_category,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of deaths
    labels_deaths <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Population:", format(data_filtered$POPULATION, big.mark = ",", scientific = F), "<br/>",
      "Deaths per 100k Last 7 Days:", data_filtered$Deaths_per_100k_last_7_days,"<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$svi_category,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of vaccine hesitancy
    labels_hesitancy <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Population:", format(data_filtered$POPULATION, big.mark = ",", scientific = F), "<br/>",
      "COVID-19 Vaccine Hesitancy Percentage:", data_filtered$estimated_hesitant,"<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$svi_category,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of SVI
    labels_svi <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Population:", format(data_filtered$POPULATION, big.mark = ",", scientific = F), "<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$svi_category,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of COVID-19 Vaccine Coverage Index
    labels_cvac <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Population:", format(data_filtered$POPULATION, big.mark = ",", scientific = F), "<br/>",
      "COVID-19 Vaccine Coverage Index: ", data_filtered$ability_to_handle_a_covid,"<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$svi_category,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # Legend titles
    titles_cases<- "Cases per 100k Last 7 Days"
    titles_tests <- "Test Positivity Rate Last 7 Days"
    titles_hospitalizations <- "Hospitalizations per 100k Last 7 Days"
    titles_deaths <- "Deaths per 100k Last 7 Days"
    titles_vaccination <- "Vaccination Percentage"
    titles_hesitancy <- "COVID-19 Vaccine Hesitancy Percentage"
    titles_svi <- "CDC Social Vulnerability Index"
    titles_cvac <- "COVID-19 Vaccine Coverage Index"
    
    # bins
    bins_cases <- round(quantile(unique(us_county_covid$Cases_per_100k_last_7_days), c(0,0.25,0.5,0.75,1), na.rm = T),0)
    bins_tests <- c(0, 25, 50, 75, 100)
    bins_hospitalizations <- round(quantile(unique(us_county_covid$conf_covid_admit_100k_last_7), c(0,0.25,0.5,0.75,1), na.rm = T),0)
    bins_deaths <- round(quantile(unique(us_county_covid$Deaths_per_100k_last_7_days), c(0,0.25,0.5,0.75,1), na.rm = T),0)
    bins_vaccination <- c(0, 25, 50, 75, 100)
    bins_hesitancy <- round(quantile(unique(us_county_covid$estimated_hesitant), c(0,0.25,0.5,0.75,1), na.rm = T),0)
    bins_svi <- c(0, 0.25, 0.5, 0.75, 1)
    bins_cvac <- c(0, 0.25, 0.5, 0.75, 1)
    
    # Data types
    
    # Vaccination
    if ( input$data_type == "Vaccination Percentage"){
      
      # At least one dose
      if (input$vaccination_status == 'At Lease One Dose'){
        
        if (input$age == "All Age Groups"){
          
          # palette
          mypal <- colorBin("YlGnBu", domain = data_filtered$administered_dose1_pop_pct, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
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
              label = labels_all_ages
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$administered_dose1_pop_pct,
              title = titles_vaccination,
              opacity = 1
            )
        } 
        
        else if (input$age == "≥ 5 Years"){
          # palette
          mypal <- colorBin("YlGnBu", domain = data_filtered$administered_dose1_recip_5pluspop_pct, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
              position="topleft") %>% # add Reset View button in the map
            addPolygons(
              data = data_filtered,
              fillColor = ~mypal(data_filtered$administered_dose1_recip_5pluspop_pct),
              color ="black",
              stroke = T,
              smoothFactor = 0.2,
              fillOpacity = 0.75,
              weight = 1,
              highlightOptions = highlightOptions(fillColor = "black",
                                                  bringToFront = TRUE),
              label = labels_5plus
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$administered_dose1_recip_5pluspop_pct,
              title = titles_vaccination,
              opacity = 1
            )
        } 
        
        else if (input$age == "≥ 12 Years"){
          
          # palette
          mypal <- colorBin("YlGnBu", domain = data_filtered$administered_dose1_recip_12pluspop_pct, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
              position="topleft") %>% # add Reset View button in the map
            addPolygons(
              data = data_filtered,
              fillColor = ~mypal(data_filtered$administered_dose1_recip_12pluspop_pct),
              color ="black",
              stroke = T,
              smoothFactor = 0.2,
              fillOpacity = 0.75,
              weight = 1,
              highlightOptions = highlightOptions(fillColor = "black",
                                                  bringToFront = TRUE),
              label = labels_12plus
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$administered_dose1_recip_12pluspop_pct,
              title = titles_vaccination,
              opacity = 1
            )
        } 
        
        else if(input$age == "≥ 18 Years"){
          # palette
          mypal <- colorBin("YlGnBu", domain = data_filtered$administered_dose1_recip_18pluspop_pct, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
              position="topleft") %>% # add Reset View button in the map
            addPolygons(
              data = data_filtered,
              fillColor = ~mypal(data_filtered$administered_dose1_recip_18pluspop_pct),
              color ="black",
              stroke = T,
              smoothFactor = 0.2,
              fillOpacity = 0.75,
              weight = 1,
              highlightOptions = highlightOptions(fillColor = "black",
                                                  bringToFront = TRUE),
              label = labels_18plus
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$administered_dose1_recip_18pluspop_pct,
              title = titles_vaccination,
              opacity = 1
            )
        } 
        
        else {
          # palette
          mypal <- colorBin("YlGnBu", domain = data_filtered$administered_dose1_recip_65pluspop_pct, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
              position="topleft") %>% # add Reset View button in the map
            addPolygons(
              data = data_filtered,
              fillColor = ~mypal(data_filtered$administered_dose1_recip_65pluspop_pct),
              color ="black",
              stroke = T,
              smoothFactor = 0.2,
              fillOpacity = 0.75,
              weight = 1,
              highlightOptions = highlightOptions(fillColor = "black",
                                                  bringToFront = TRUE),
              label = labels_65plus
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$administered_dose1_recip_65pluspop_pct,
              title = titles_vaccination,
              opacity = 1
            )
        }
      }
      
      # Fully vaccinated
      else if (input$vaccination_status == 'Fully Vaccinated'){
        
        if (input$age == "All Age Groups"){
          
          # palette
          
          mypal <- colorBin("YlGnBu", domain = data_filtered$series_complete_pop_pct, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
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
              label = labels_all_ages
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$series_complete_pop_pct,
              title = titles_vaccination,
              opacity = 1
            )
        } 
        
        else if (input$age == "≥ 5 Years"){
          # palette
          mypal <- colorBin("YlGnBu", domain = data_filtered$series_complete_5pluspop_pct, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
              position="topleft") %>% # add Reset View button in the map
            addPolygons(
              data = data_filtered,
              fillColor = ~mypal(data_filtered$series_complete_5pluspop_pct),
              color ="black",
              stroke = T,
              smoothFactor = 0.2,
              fillOpacity = 0.75,
              weight = 1,
              highlightOptions = highlightOptions(fillColor = "black",
                                                  bringToFront = TRUE),
              label = labels_5plus
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$series_complete_5pluspop_pct,
              title = titles_vaccination,
              opacity = 1
            )
        } 
        
        else if (input$age == "≥ 12 Years"){
          
          # palette
          mypal <- colorBin("YlGnBu", domain = data_filtered$series_complete_12pluspop, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
              position="topleft") %>% # add Reset View button in the map
            addPolygons(
              data = data_filtered,
              fillColor = ~mypal(data_filtered$series_complete_12pluspop),
              color ="black",
              stroke = T,
              smoothFactor = 0.2,
              fillOpacity = 0.75,
              weight = 1,
              highlightOptions = highlightOptions(fillColor = "black",
                                                  bringToFront = TRUE),
              label = labels_12plus
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$series_complete_12pluspop,
              title = titles_vaccination,
              opacity = 1
            )
        } 
        
        else if(input$age == "≥ 18 Years"){
          # palette
          mypal <- colorBin("YlGnBu", domain = data_filtered$series_complete_18pluspop, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
              position="topleft") %>% # add Reset View button in the map
            addPolygons(
              data = data_filtered,
              fillColor = ~mypal(data_filtered$series_complete_18pluspop),
              color ="black",
              stroke = T,
              smoothFactor = 0.2,
              fillOpacity = 0.75,
              weight = 1,
              highlightOptions = highlightOptions(fillColor = "black",
                                                  bringToFront = TRUE),
              label = labels_18plus
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$series_complete_18pluspop,
              title = titles_vaccination,
              opacity = 1
            )
        } 
        
        else {
          # palette
          mypal <- colorBin("YlGnBu", domain = data_filtered$series_complete_65pluspop, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
              position="topleft") %>% # add Reset View button in the map
            addPolygons(
              data = data_filtered,
              fillColor = ~mypal(data_filtered$series_complete_65pluspop),
              color ="black",
              stroke = T,
              smoothFactor = 0.2,
              fillOpacity = 0.75,
              weight = 1,
              highlightOptions = highlightOptions(fillColor = "black",
                                                  bringToFront = TRUE),
              label = labels_65plus
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$series_complete_65pluspop,
              title = titles_vaccination,
              opacity = 1
            )
        }
      }
      
      # Booster dose
      else{
        
        if(input$age == "≥ 18 Years"){
          # palette
          mypal <- colorBin("YlGnBu", domain = data_filtered$booster_doses_18pluspop_pct, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
              position="topleft") %>% # add Reset View button in the map
            addPolygons(
              data = data_filtered,
              fillColor = ~mypal(data_filtered$booster_doses_18pluspop_pct),
              color ="black",
              stroke = T,
              smoothFactor = 0.2,
              fillOpacity = 0.75,
              weight = 1,
              highlightOptions = highlightOptions(fillColor = "black",
                                                  bringToFront = TRUE),
              label = labels_18plus
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$booster_doses_18pluspop_pct,
              title = titles_vaccination,
              opacity = 1
            )
        } 
        
        else if (input$age == "≥ 65 Years") {
          # palette
          mypal <- colorBin("YlGnBu", domain = data_filtered$booster_doses_65pluspop_pct, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
              position="topleft") %>% # add Reset View button in the map
            addPolygons(
              data = data_filtered,
              fillColor = ~mypal(data_filtered$booster_doses_65pluspop_pct),
              color ="black",
              stroke = T,
              smoothFactor = 0.2,
              fillOpacity = 0.75,
              weight = 1,
              highlightOptions = highlightOptions(fillColor = "black",
                                                  bringToFront = TRUE),
              label = labels_65plus
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$booster_doses_65pluspop_pct,
              title = titles_vaccination,
              opacity = 1
            )
        }
        
        else {
          
          # All age groups
          # palette
          
          mypal <- colorBin("YlGnBu", domain = data_filtered$booster_doses_pop_pct, bins = bins_vaccination)
          
          #leafletProxy map
          leafletProxy("map", data = data_filtered )%>%
            clearControls() %>% # clear the map legend
            clearShapes() %>% # clear polygons fill color
            addControl(
              actionBttn("reset_map", "", icon = icon("globe-americas")),
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
              label = labels_all_ages
            ) %>%
            addLegend(
              position = "topright",
              pal= mypal,
              values = data_filtered$booster_doses_pop_pct,
              title = titles_vaccination,
              opacity = 1
            )
        }
        
      }
      
    }
    
    # Cases 
    else if(input$data_type == "Cases per 100k Last 7 Days"){
      
      # palette
      
      mypal <- colorBin("YlOrRd", domain = data_filtered$Cases_per_100k_last_7_days, bins = bins_cases)
      
      #leafletProxy map
      leafletProxy("map", data = data_filtered) %>%
        clearControls() %>%
        clearShapes() %>%
        addControl(
          actionBttn("reset_map", "", icon = icon("globe-americas")),
          position="topleft") %>%
        addPolygons(
          data = data_filtered,
          fillColor = ~mypal(data_filtered$Cases_per_100k_last_7_days),
          color ="black",
          stroke = T,
          smoothFactor = 0.2,
          fillOpacity = 0.75,
          weight = 1,
          highlightOptions = highlightOptions(fillColor = "black",
                                              bringToFront = TRUE),
          label = labels_cases
        ) %>%
        addLegend(
          position = "topright",
          pal= mypal,
          values = data_filtered$Cases_per_100k_last_7_days,
          title = titles_cases,
          opacity = 1
        )
    }
    
    # Tests
    else if (input$data_type == "Test Positivity Rate Last 7 Days"){
      
      # palette
      
      mypal <- colorBin("YlOrRd", domain = data_filtered$test_positivity_rate_last_7_d, bins = bins_tests)
      
      #leafletProxy map
      leafletProxy("map", data = data_filtered) %>%
        clearControls() %>%
        clearShapes() %>%
        addControl(
          actionBttn("reset_map", "", icon = icon("globe-americas")),
          position="topleft") %>%
        addPolygons(
          data = data_filtered,
          fillColor = ~mypal(data_filtered$test_positivity_rate_last_7_d),
          color ="black",
          stroke = T,
          smoothFactor = 0.2,
          fillOpacity = 0.75,
          weight = 1,
          highlightOptions = highlightOptions(fillColor = "black",
                                              bringToFront = TRUE),
          label = labels_tests
        ) %>%
        addLegend(
          position = "topright",
          pal= mypal,
          values = data_filtered$test_positivity_rate_last_7_d,
          title = titles_tests,
          opacity = 1
        )
    }
    
    # Hospitalization 
    else if (input$data_type == "Hospitalizations per 100k Last 7 Days"){
      
      # palette
      
      mypal <- colorBin("YlOrRd", domain = data_filtered$conf_covid_admit_100k_last_7, bins = bins_hospitalizations)
      
      #leafletProxy map
      leafletProxy("map", data = data_filtered) %>%
        clearControls() %>%
        clearShapes() %>%
        addControl(
          actionBttn("reset_map", "", icon = icon("globe-americas")),
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
          label = labels_hospitalizations
        ) %>%
        addLegend(
          position = "topright",
          pal= mypal,
          values = data_filtered$conf_covid_admit_100k_last_7,
          title = titles_hospitalizations,
          opacity = 1
        )
    }
    
    # Deaths
    else if(input$data_type == "Deaths per 100k Last 7 Days"){
      
      # palette
      
      mypal <- colorBin("YlOrRd", domain = data_filtered$Deaths_per_100k_last_7_days, bins = bins_deaths)
      
      #leafletProxy map
      leafletProxy("map", data = data_filtered) %>%
        clearControls() %>%
        clearShapes() %>%
        addControl(
          actionBttn("reset_map", "", icon = icon("globe-americas")),
          position="topleft") %>%
        addPolygons(
          data = data_filtered,
          fillColor = ~mypal(data_filtered$Deaths_per_100k_last_7_days),
          color ="black",
          stroke = T,
          smoothFactor = 0.2,
          fillOpacity = 0.75,
          weight = 1,
          highlightOptions = highlightOptions(fillColor = "black",
                                              bringToFront = TRUE),
          label = labels_deaths
        ) %>%
        addLegend(
          position = "topright",
          pal= mypal,
          values = data_filtered$Deaths_per_100k_last_7_days,
          title = titles_deaths,
          opacity = 1
        )
    }
    
    # Vaccine Hesitancy
    else if(input$data_type == "Vaccination Hesitancy"){
      # palette
      
      mypal <- colorBin("Reds", domain = data_filtered$estimated_hesitant, bins = bins_hesitancy)
      
      #leafletProxy map
      leafletProxy("map", data = data_filtered) %>%
        clearControls() %>%
        clearShapes() %>%
        addControl(
          actionBttn("reset_map", "", icon = icon("globe-americas")),
          position="topleft") %>%
        addPolygons(
          data = data_filtered,
          fillColor = ~mypal(data_filtered$estimated_hesitant),
          color ="black",
          stroke = T,
          smoothFactor = 0.2,
          fillOpacity = 0.75,
          weight = 1,
          highlightOptions = highlightOptions(fillColor = "black",
                                              bringToFront = TRUE),
          label = labels_hesitancy
        ) %>%
        addLegend(
          position = "topright",
          pal= mypal,
          values = data_filtered$estimated_hesitant,
          title = titles_hesitancy,
          opacity = 1
        )
      
    }
    
    
    # CDC SVI
    else if (input$data_type == "CDC Social Vulnerability Index") {
      
      # palette
      
      mypal <- colorBin("Purples", domain = data_filtered$social_vulnerability_index, bins = bins_svi)
      
      #leafletProxy map
      leafletProxy("map", data = data_filtered) %>%
        clearControls() %>%
        clearShapes() %>%
        addControl(
          actionBttn("reset_map", "", icon = icon("globe-americas")),
          position="topleft") %>%
        addPolygons(
          data = data_filtered,
          fillColor = ~mypal(data_filtered$social_vulnerability_index),
          color ="black",
          stroke = T,
          smoothFactor = 0.2,
          fillOpacity = 0.75,
          weight = 1,
          highlightOptions = highlightOptions(fillColor = "black",
                                              bringToFront = TRUE),
          label = labels_svi
        ) %>%
        addLegend(
          position = "topright",
          pal= mypal,
          values = data_filtered$social_vulnerability_index,
          title = titles_svi,
          opacity = 1
        )
    }
    
    # COVID-19 Vaccine Coverage Index
    else {
      
      # palette
      
      mypal <- colorBin("Oranges", domain = data_filtered$ability_to_handle_a_covid, bins = bins_cvac)
      
      #leafletProxy map
      leafletProxy("map", data = data_filtered) %>%
        clearControls() %>%
        clearShapes() %>%
        addControl(
          actionBttn("reset_map", "", icon = icon("globe-americas")),
          position="topleft") %>%
        addPolygons(
          data = data_filtered,
          fillColor = ~mypal(data_filtered$ability_to_handle_a_covid),
          color ="black",
          stroke = T,
          smoothFactor = 0.2,
          fillOpacity = 0.75,
          weight = 1,
          highlightOptions = highlightOptions(fillColor = "black",
                                              bringToFront = TRUE),
          label = labels_cvac
        ) %>%
        addLegend(
          position = "topright",
          pal= mypal,
          values = data_filtered$ability_to_handle_a_covid,
          title = titles_cvac,
          opacity = 1
        )
    }
    
    # ANALYSIS TAB
    
    # Plot titles and labels
    output$scatter_title <- renderPrint({
      
      correlation <- cor(data_filtered[[input$yvariable]], 
                         data_filtered[[input$xvariable]], 
                         use="complete.obs") %>% round(3)
      
      HTML(paste0("<b>Correlation Between ", 
                  names(switch_labels[which(switch_labels == input$yvariable)]), 
                  " and ", 
                  names(switch_labels[which(switch_labels == input$xvariable)]), 
                  ": ", 
                  correlation,
                  "</b> "))
    })
    
    label_xvar<-HTML(paste0(names(switch_labels[which(switch_labels == input$xvariable)])))
    label_yvar<-HTML(paste0(names(switch_labels[which(switch_labels == input$yvariable)])))
    label_selected_var <- HTML(paste0(names(switch_labels[which(switch_labels == input$selected_variable)])))
    label_category <- names(hue_labels[which(hue_labels == input$hue)])
    
    # Filter data
    data_filtered <- data_filtered %>% 
      filter(administered_dose1_pop_pct>0,
             series_complete_pop_pct>0,
             booster_doses_pop_pct>0,
             !(is.na(svi_category))
      )
    
    # Scatter plot
    scatter <-  data_filtered %>% 
      ggplot(aes_string(x = input$xvariable, y = input$yvariable, color = input$hue)) +
      geom_point(aes(size= POPULATION), alpha = 0.5)+
      geom_smooth(method = lm, formula = y~x)+
      theme_bw()+
      theme(axis.text = black.bold.plain.11.text,
            axis.title = black.bold.plain.14.text,
            legend.position="bottom",
            legend.box = "vertical",
            legend.title=black.bold.plain.11.text,
            legend.text = black.bold.plain.11.text,
            strip.text = white.bold.plain.14.text,
            strip.background = element_rect(fill = "#2596be"))+ 
      scale_size_continuous(labels= comma, name = "Population")+
      facet_grid(reformulate(input$hue))+
      labs(x=label_xvar,
           y=label_yvar,
           col=label_category
      )+
      scale_y_continuous(labels = comma)+
      scale_x_continuous(labels = comma)
    
    output$scatter <- renderPlot({
      scatter
    })
    
    # Correlation heat map
    p.mat = as_tibble(data_filtered) %>% 
      select(Cases_per_100k_last_7_days,
             test_positivity_rate_last_7_d,
             conf_covid_admit_100k_last_7,
             pct_icu_covid,
             pct_vent_covid,
             Deaths_per_100k_last_7_days,
             administered_dose1_pop_pct,
             series_complete_pop_pct,
             booster_doses_pop_pct,
             estimated_hesitant,
             social_vulnerability_index,
             ability_to_handle_a_covid) %>% 
      drop_na() %>% 
      cor_pmat()
    
    corr_heatmap<- as_tibble(data_filtered) %>% 
      select(Cases_per_100k_last_7_days,
             test_positivity_rate_last_7_d,
             conf_covid_admit_100k_last_7,
             pct_icu_covid,
             pct_vent_covid,
             Deaths_per_100k_last_7_days,
             administered_dose1_pop_pct,
             series_complete_pop_pct,
             booster_doses_pop_pct,
             estimated_hesitant,
             social_vulnerability_index,
             ability_to_handle_a_covid) %>% 
      drop_na() %>% 
      cor() %>% 
      ggcorrplot(title = "Correlation Between Variables",
                 legend.title = "Correlation",
                 lab=T, type = "lower", hc.order = T, p.mat=p.mat, insig = "blank")+
      scale_x_discrete(labels=labels_corr)+
      scale_y_discrete(labels=labels_corr)+
      theme(text = element_text(size = 18),
            axis.text = black.bold.plain.18.text)
    
    output$corr_heatmap <- renderPlot({
      corr_heatmap
    })
    
    # Inequality Barchart of selected variable and hue
    inequality_bar <-if(input$mean_median=="Mean"){
      as_tibble(data_filtered) %>%
        group_by(!! sym(input$hue)) %>% 
        summarise(median_x = mean(!! sym(input$selected_variable))) %>% 
        ggplot(aes_string(x=input$hue,y="median_x", fill = input$hue))+
        geom_bar(stat = "identity")+
        theme_bw()+
        theme(legend.position  = "none",
              plot.title = black.bold.plain.18.text,
              axis.text = black.bold.plain.11.text,
              axis.title = black.bold.plain.14.text)+
        labs(title= label_selected_var,
             x=label_category,
             y=paste0(label_selected_var," (Mean)")
        )+
        coord_flip()+
        scale_y_continuous(labels = comma)
    }
    else{
      as_tibble(data_filtered) %>%
        group_by(!! sym(input$hue)) %>% 
        summarise(median_x = median(!! sym(input$selected_variable))) %>% 
        ggplot(aes_string(x=input$hue,y="median_x", fill = input$hue))+
        geom_bar(stat = "identity")+
        theme_bw()+
        theme(legend.position  = "none",
              plot.title = black.bold.plain.18.text,
              axis.text = black.bold.plain.11.text,
              axis.title = black.bold.plain.14.text)+
        labs(title= label_selected_var,
             x=label_category,
             y=paste0(label_selected_var," (Median)")
        )+
        coord_flip()+
        scale_y_continuous(labels = comma)
    }
    
    output$inequality_bar <- renderPlot({
      inequality_bar
    })
    
    
    # Barchart of population
    popbar<-if(input$mean_median=="Mean"){
      as_tibble(data_filtered) %>%
        filter(!is.na(!! sym(input$hue))) %>% 
        group_by(!! sym(input$hue)) %>% 
        summarise(median_pop = mean(POPULATION)) %>% 
        ggplot(aes_string(x=input$hue,y="median_pop", fill = input$hue))+
        geom_bar(stat = "identity")+
        theme_bw()+
        theme(legend.position  = "none",
              plot.title = black.bold.plain.18.text,
              axis.text = black.bold.plain.11.text,
              axis.title = black.bold.plain.14.text)+
        labs(title="Population",
             x=label_category,
             y=paste0("Population", " (Mean)")
        )+
        coord_flip()+
        scale_y_continuous(labels = comma)
    }
    else{
      as_tibble(data_filtered) %>%
        filter(!is.na(!! rlang::sym(input$hue))) %>% 
        group_by(!! rlang::sym(input$hue)) %>% 
        summarise(median_pop = median(POPULATION)) %>% 
        ggplot(aes_string(x=input$hue,y="median_pop", fill = input$hue))+
        geom_bar(stat = "identity")+
        theme_bw()+
        theme(legend.position  = "none",
              plot.title = black.bold.plain.18.text,
              axis.text = black.bold.plain.11.text,
              axis.title = black.bold.plain.14.text)+
        labs(title="Population",
             x=label_category,
             y=paste0("Population", " (Median)")
        )+
        coord_flip()+
        scale_y_continuous(labels = comma)
    }
    
    output$popbar <- renderPlot({
      popbar
    })
    
    # Value boxes-----------------------------------------
    output$box_case <- renderValueBox({
      valueBox(
        paste0(data_filtered %>% 
                 filter(STATE_NAME==input$state_rank, 
                        NAME==input$county) %>% 
                 pull(Cases_per_100k_last_7_days)), 
        "Cases per 100k Last 7 Days", 
        icon = icon("head-side-cough"),
        color = "red"
      )
    })
    
    output$box_test <- renderValueBox({
      valueBox(
        paste0(data_filtered %>% 
                 filter(STATE_NAME==input$state_rank, 
                        NAME==input$county) %>% 
                 pull(test_positivity_rate_last_7_d), "%"), 
        "Test Positivity Rate Last 7 Days", 
        icon = icon("vials"),
        color = "red"
      )
    })
    
    output$box_hospitalization <- renderValueBox({
      valueBox(
        paste0(data_filtered %>% 
                 filter(STATE_NAME==input$state_rank, 
                        NAME==input$county) %>% 
                 pull(conf_covid_admit_100k_last_7)), 
        "Hospitalizations per 100k Last 7 Days", 
        icon = icon("hospital"),
        color = "red"
      )
    })
    
    output$box_inpatient <- renderValueBox({
      valueBox(
        paste0(data_filtered %>% 
                 filter(STATE_NAME==input$state_rank, 
                        NAME==input$county) %>% 
                 pull(pct_inpatient), "%"), 
        "Inpatient Beds Occupied by All Patients", 
        icon = icon("hospital"),
        color = "red"
      )
    })
    
    output$box_icu <- renderValueBox({
      valueBox(
        paste0(data_filtered %>%
                 filter(STATE_NAME==input$state_rank,
                        NAME==input$county) %>%
                 pull(pct_icu_covid), "%"),
        "ICU Occupied by COVID Patients",
        icon = icon("procedures"),
        color = "red"
      )
    })
    
    output$box_staffed_icu <- renderValueBox({
      valueBox(
        paste0(data_filtered %>%
                 filter(STATE_NAME==input$state_rank,
                        NAME==input$county) %>%
                 pull(pct_staffed_icu), "%"),
        "ICU Occupied by All Patients",
        icon = icon("procedures"),
        color = "red"
      )
    })
    
    output$box_vent <- renderValueBox({
      valueBox(
        paste0(data_filtered %>%
                 filter(STATE_NAME==input$state_rank,
                        NAME==input$county) %>%
                 pull(pct_vent_covid), "%"),
        "Ventilator Used by COVID Patients",
        icon = icon("lungs"),
        color = "red"
      )
    })
    
    output$box_death <- renderValueBox({
      valueBox(
        paste0(data_filtered %>%
                 filter(STATE_NAME==input$state_rank,
                        NAME==input$county) %>%
                 pull(Deaths_per_100k_last_7_days)),
        "Deaths per 100k Last 7 Days",
        icon = icon("skull-crossbones"),
        color = "red"
      )
    })
    
    output$box_1dose <- renderValueBox({
      valueBox(
        paste0(data_filtered %>%
                 filter(STATE_NAME==input$state_rank,
                        NAME==input$county) %>%
                 pull(administered_dose1_pop_pct), "%"),
        "At Least One Dose in All Age Groups",
        icon = icon("syringe"),
        color = "blue"
      )
    })
    
    output$box_2doses <- renderValueBox({
      valueBox(
        paste0(data_filtered %>%
                 filter(STATE_NAME==input$state_rank,
                        NAME==input$county) %>%
                 pull(series_complete_pop_pct), "%"),
        "Fully Vaccinated in All Age Groups",
        icon = icon("syringe"),
        color = "blue"
      )
    })
    
    output$box_booster <- renderValueBox({
      valueBox(
        paste0(data_filtered %>%
                 filter(STATE_NAME==input$state_rank,
                        NAME==input$county) %>%
                 pull(booster_doses_pop_pct), "%"),
        "Booster (or Additional) Dose in All Age Groups",
        icon = icon("syringe"),
        color = "blue"
      )
    })
    
    output$box_hesitancy <- renderValueBox({
      valueBox(
        paste0(data_filtered %>%
                 filter(STATE_NAME==input$state_rank,
                        NAME==input$county) %>%
                 pull(estimated_hesitant), "%"),
        "COVID-19 Vaccine Hesitancy",
        icon = icon("question"),
        color = "purple"
      )
    })
    
    output$box_svi <- renderValueBox({
      valueBox(
        paste0(data_filtered %>%
                 filter(STATE_NAME==input$state_rank,
                        NAME==input$county) %>%
                 pull(svi_category)),
        "CDC Social Vulnerability Index",
        icon = icon("house-damage"),
        color = "purple"
      )
    })
    
    output$box_cvac <- renderValueBox({
      valueBox(
        paste0(data_filtered %>%
                 filter(STATE_NAME==input$state_rank,
                        NAME==input$county) %>%
                 pull(cvac_category)),
        "COVID-19 Vaccine Coverage Index",
        icon = icon("syringe"),
        color = "purple"
      )
    })
    
    # Rank Barchart of selected variable
    rank_state<- if(input$mean_median=="Mean"){
      as_tibble(data_filtered) %>%
        group_by(STATE_NAME) %>% 
        summarise(median_x = mean(!! sym(input$selected_variable))) %>% 
        ggplot(aes(x=reorder(STATE_NAME, median_x),y=median_x, fill = STATE_NAME))+
        geom_bar(stat = "identity")+
        theme_bw()+
        theme(legend.position  = "none",
              plot.title = black.bold.plain.18.text,
              axis.text = black.bold.plain.14.text,
              axis.title = black.bold.plain.18.text)+
        labs(title = paste0(label_selected_var," by State"),
             x="",
             y=names(switch_labels[which(switch_labels == input$selected_variable)]))+
        coord_flip()+
        scale_y_continuous(sec.axis = sec_axis(~ ., labels = comma),labels = comma )
    }
    else{
      as_tibble(data_filtered) %>%
        group_by(STATE_NAME) %>% 
        summarise(median_x = median(!! sym(input$selected_variable))) %>% 
        ggplot(aes(x=reorder(STATE_NAME, median_x),y=median_x, fill = STATE_NAME))+
        geom_bar(stat = "identity")+          
        theme_bw()+
        theme(legend.position  = "none",
              plot.title = black.bold.plain.18.text,
              axis.text = black.bold.plain.14.text,
              axis.title = black.bold.plain.18.text)+
        labs(title = paste0(label_selected_var," by State"),
             x="",
             y=names(switch_labels[which(switch_labels == input$selected_variable)]))+
        coord_flip()+
        scale_y_continuous(labels = comma)
    }
    
    output$rank_state <- renderPlot({
      rank_state
    })
    
    rank_county<- 
      as_tibble(data_filtered) %>%
      filter(STATE_NAME %in% input$state_rank) %>% 
      ggplot(aes(x=reorder(NAME,!! sym(input$selected_variable)), 
                 y=!! sym(input$selected_variable), 
                 fill = NAME))+
      geom_bar(stat = "identity")+
      theme_bw()+
      theme(legend.position  = "none",
            plot.title = black.bold.plain.18.text,
            axis.text = black.bold.plain.14.text,
            axis.title = black.bold.plain.18.text)+
      labs(title = paste0(label_selected_var," in ", input$state_rank," by County"),
           x="",
           y=label_selected_var)+
      coord_flip()+
      scale_y_continuous(sec.axis = sec_axis(~ ., labels = comma),labels = comma )
    output$rank_county <- renderPlot({
      rank_county
    })
    
    #TABLE TAB
    
    # Customized data table
    output$datatable_customized<- renderDataTable(
      as_tibble(data_filtered)[, input$table_columns_selected],
      options = list(
        pageLength=10, scrollX='400px'),
      filter = 'top'
    )
    
    # Full data table
    output$datatable_full<- renderDataTable(
      as_tibble(us_county_covid),
      options = list(
        pageLength=10, scrollX='400px'),
      filter = 'top'
    )
    
    # Download buttons
    output$download_customized_datatable <- downloadHandler(
      filename = function(){'us_county_covid_customized.csv'},
      content = function(fname) {
        write_csv(as_tibble(st_set_geometry(data_filtered, NULL))[, input$table_columns_selected], fname)
      }
    )
    
    output$download_full_datatable <- downloadHandler(
      filename = function(){'us_county_covid.csv'},
      content = function(fname) {
        write_csv(as_tibble(st_set_geometry(us_county_covid, NULL))[,], fname)
      }
    )
    
    download_plot <- function(exportname, plot) {
      downloadHandler(
        filename = function() {
          paste(exportname, Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot, device = "png", width = 8)
        }
      )
    }
    
    output$download_scatter <- download_plot('scatter', scatter)
    output$download_corr_heatmap <- download_plot('corr_heatmap', corr_heatmap)
    output$download_inequality_bar <- download_plot('inequality_bar',inequality_bar)
    output$download_popbar <- download_plot('popbar', popbar)
    output$download_rank_state <- download_plot('rank_state', rank_state)
    output$download_rank_county <- download_plot('rank_county', rank_county)
    
  })
})
