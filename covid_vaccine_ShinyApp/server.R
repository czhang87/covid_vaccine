# COVID in the U.S.

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  
  # # update the county drop down menu based on the selection of the state drop down menu
  # observe({
  #   updateSelectInput(session, "county", "Select or type in one county",
  #                     choices = us_county_covid[us_county_covid$STATE_NAME == input$state,]$NAME)
  # })
  
  # Reset Input Button
  observeEvent(input$reset_input, {
    updateNumericRangeInput(session, "population", value = c(min_pop,max_pop))
    updateSelectInput(session, "data_type", choices = c('Cases per 100k Last 7 Days',
                                                        'Test Positivity Rate Last 7 Days',
                                                        'Hospitalizations per 100k Last 7 Days', 
                                                        'Deaths per 100k Last 7 Days',
                                                        'Vaccination Percentage',
                                                        'Vaccination Hesitancy', 
                                                        'CDC Social Vulnerability Index',
                                                        'COVID-19 Vaccine Coverage Index')
    )
    updateRadioButtons(session, "vaccination_status", choices = c('At Lease One Dose', 
                                                                  'Fully Vaccinated', 
                                                                  'Booster (or Additional) Dose')
    )
    updateSelectInput(session, "data_type", choices = c('Cases per 100k Last 7 Days',
                                                        'Test Positivity Rate Last 7 Days',
                                                        'Hospitalizations per 100k Last 7 Days', 
                                                        'Deaths per 100k Last 7 Days',
                                                        'Vaccination Percentage',
                                                        'Vaccination Hesitancy', 
                                                        'CDC Social Vulnerability Index',
                                                        'COVID-19 Vaccine Coverage Index')
    )
    updateSelectInput(session, "xvariable", choices = c("Cases per 100k Last 7 Days"="Cases_per_100k_last_7_days",
                                                        "Test Positivity Rate Last 7 Days" = "test_positivity_rate_last_7_d",
                                                        "Hospitalizations per 100k Last 7 Days" = "conf_covid_admit_100k_last_7",
                                                        "Percentage of ICU Occupied by COVID patients" = "pct_icu_covid",
                                                        "Percentage of Ventilator Used by COVID patients" = "pct_vent_covid",
                                                        "Deaths per 100k Last 7 Days" = "Deaths_per_100k_last_7_days",
                                                        "At Least One Dose in All Age Groups"="administered_dose1_pop_pct",
                                                        "Fully Vaccinated in All Age Groups" = "series_complete_pop_pct",
                                                        "Booster (or Additional) Dose in All Age Groups" = "booster_doses_pop_pct",
                                                        "COVID-19 Vaccine Hesitancy Percentage" = "estimated_hesitant",
                                                        "CDC Social Vulnerability Index" = "social_vulnerability_index",
                                                        "COVID-19 Vaccine Coverage Index" = "ability_to_handle_a_covid")
    )
    updateSelectInput(session, "yvariable",choices = c("Cases per 100k Last 7 Days"="Cases_per_100k_last_7_days",
                                                       "Test Positivity Rate Last 7 Days" = "test_positivity_rate_last_7_d",
                                                       "Hospitalizations per 100k Last 7 Days" = "conf_covid_admit_100k_last_7",
                                                       "Percentage of ICU Occupied by COVID patients" = "pct_icu_covid",
                                                       "Percentage of Ventilator Used by COVID patients" = "pct_vent_covid",
                                                       "Deaths per 100k Last 7 Days" = "Deaths_per_100k_last_7_days",
                                                       "At Least One Dose in All Age Groups"="administered_dose1_pop_pct",
                                                       "Fully Vaccinated in All Age Groups" = "series_complete_pop_pct",
                                                       "Booster (or Additional) Dose in All Age Groups" = "booster_doses_pop_pct",
                                                       "COVID-19 Vaccine Hesitancy Percentage" = "estimated_hesitant",
                                                       "CDC Social Vulnerability Index" = "social_vulnerability_index",
                                                       "COVID-19 Vaccine Coverage Index" = "ability_to_handle_a_covid")
    )
    updateSelectInput(session, "hue", choices = c("Metropolitan Status"="metro_status",
                                                  "CDC Social Vulnerability Index" = "svi_category",
                                                  "COVID-19 Vaccine Coverage Index" = "cvac_category"
    ))
    updatePickerInput(session, "table_columns_selected", choices = table_columns)
    # updateSelectInput(session, "state", )
    # updateSelectInput(session, "xvariable", )
    # updateSelectInput(session, "xvariable", )
    # updateSelectInput(session, "xvariable", )
    # updateSelectInput(session, "xvariable", )
    # updateSelectInput(session, "xvariable", )
    
  })
  
  # MAP TAB
  
  # Initial view
  initial_lat = 39.8283
  initial_lng = -98.5795
  initial_zoom = 4
  
  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik")%>%
      # addTiles() %>% 
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
      "At lease one dose:", data_filtered$administered_dose1_pop_pct, "%<br/>",
      "Fully vaccinated:", data_filtered$series_complete_pop_pct,"%<br/>",
      "Booster dose:", data_filtered$booster_doses_pop_pct,"%<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$social_vulnerability_index,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    labels_5plus <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "At lease one dose:", data_filtered$administered_dose1_recip_5pluspop_pct, "%<br/>",
      "Fully vaccinated:", data_filtered$series_complete_5pluspop_pct,"%<br/>",
      "Booster dose:", data_filtered$booster_doses_pop_pct,"%<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$social_vulnerability_index,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    labels_12plus <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "At lease one dose:", data_filtered$administered_dose1_recip_12pluspop_pct, "%<br/>",
      "Fully vaccinated:", data_filtered$series_complete_12pluspop,"%<br/>",
      "Booster dose:", data_filtered$booster_doses_pop_pct,"%<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$social_vulnerability_index,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    labels_18plus <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "At lease one dose:", data_filtered$administered_dose1_recip_18pluspop_pct, "%<br/>",
      "Fully vaccinated:", data_filtered$series_complete_18pluspop,"%<br/>",
      "Booster dose:", data_filtered$booster_doses_18pluspop_pct,"%<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$social_vulnerability_index,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    labels_65plus <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "At lease one dose:", data_filtered$administered_dose1_recip_65pluspop_pct, "%<br/>",
      "Fully vaccinated:", data_filtered$series_complete_65pluspop,"%<br/>",
      "Booster dose:", data_filtered$booster_doses_65pluspop_pct,"%<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$social_vulnerability_index,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of cases
    labels_cases <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Cases per 100k Last 7 Days:", data_filtered$Cases_per_100k_last_7_days,"<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$social_vulnerability_index,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of test positivity
    labels_tests <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Test Positivity Rate Last 7 Days:", data_filtered$test_positivity_rate_last_7_d,"<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$social_vulnerability_index,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of hospitalization
    labels_hospitalizations <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Hospitalizations per 100k Last 7 Days:", data_filtered$conf_covid_admit_100k_last_7,"<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$social_vulnerability_index,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of deaths
    labels_deaths <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Deaths per 100k Last 7 Days:", data_filtered$Deaths_per_100k_last_7_days,"<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$social_vulnerability_index,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of vaccine hesitancy
    labels_hesitancy <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "COVID-19 Vaccine Hesitancy Percentage:", data_filtered$estimated_hesitant,"<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$social_vulnerability_index,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of SVI
    labels_svi <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$social_vulnerability_index,"<br/>"
    ) %>% lapply(htmltools::HTML)
    
    # labels of COVID-19 Vaccine Coverage Index
    labels_cvac <- paste(
      "<strong>",data_filtered$NAME,", ", data_filtered$STATE_NAME, "</strong><br/>",
      "COVID-19 Vaccine Coverage Index: ", data_filtered$ability_to_handle_a_covid,"<br/>",
      "Metro status: ", data_filtered$metro_status,"<br/>",
      "CDC Social Vulnerability Index: ", data_filtered$social_vulnerability_index,"<br/>"
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
    
    # data types
    
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
              actionButton("reset_button", "Reset"),
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
              actionButton("reset_button", "Reset"),
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
              actionButton("reset_button", "Reset"),
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
              actionButton("reset_button", "Reset"),
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
              actionButton("reset_button", "Reset"),
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
              actionButton("reset_button", "Reset"),
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
              actionButton("reset_button", "Reset"),
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
              actionButton("reset_button", "Reset"),
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
              actionButton("reset_button", "Reset"),
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
              actionButton("reset_button", "Reset"),
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
          actionButton("reset_button", "Reset"),
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
          actionButton("reset_button", "Reset"),
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
          actionButton("reset_button", "Reset"),
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
          actionButton("reset_button", "Reset"),
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
          actionButton("reset_button", "Reset"),
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
          actionButton("reset_button", "Reset"),
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
    
    # reactive labels for boxes
    output$scatter_title <- renderPrint({
      
      correlation <- cor(data_filtered[[input$yvariable]], 
                         data_filtered[[input$xvariable]], 
                         use="complete.obs") %>% round(3)
      
      HTML(paste0("<b>Correlation between ", 
                  names(switch_labels[which(switch_labels == input$yvariable)]), 
                  " and ", 
                  names(switch_labels[which(switch_labels == input$xvariable)]), 
                  ": ", 
                  correlation,
                  "</b> "))
    })
    
    output$yboxplot_title <- renderPrint({
      HTML(paste0("<b>", names(switch_labels[which(switch_labels == input$yvariable)]),"<b>"))
    })
    
    output$xboxplot_title <- renderPrint({
      HTML(paste0("<b>", names(switch_labels[which(switch_labels == input$xvariable)]),"<b>"))
    })
    
    # Scatter plot
    data_filtered <- data_filtered %>% 
      filter(administered_dose1_pop_pct>0,
             series_complete_pop_pct>0,
             booster_doses_pop_pct>0,
             !(is.na(svi_category))
      )
      
    output$scatter <- renderPlot({
      p <- data_filtered %>% 
        ggplot(aes_string(x = input$xvariable, y = input$yvariable, color = input$hue)) +
        geom_point(aes(size= POPULATION), alpha = 0.5)+
        geom_smooth(method = lm, formula = my.formula)+
        theme_bw()+ # remove the background
        theme(legend.position="bottom")+ 
        scale_size_continuous(labels= comma, name = "Population")
      
      p+ facet_grid(reformulate(input$hue))+
        labs(x=names(switch_labels[which(switch_labels == input$xvariable)]),
             y=names(switch_labels[which(switch_labels == input$yvariable)]),
             col=names(hue_labels[which(hue_labels == input$hue)])
        )
      
    })
    
    
    # boxplot of y axis
    
    output$yboxplot <-renderPlotly({
      data_filtered %>% 
        ggplot(aes_string(x=input$hue,y=input$yvariable, fill = input$hue))+
        geom_boxplot()+
        theme_bw()+
        theme(legend.position  = "none")+ # remove figure legend
        labs(x=names(hue_labels[which(hue_labels == input$hue)]),
             y=names(switch_labels[which(switch_labels == input$yvariable)]))+ 
        coord_flip() # flip to horizontal boxplot
    })
    
    #boxplot of x axis
    
    output$xboxplot <-renderPlotly({
      data_filtered %>% 
        ggplot(aes_string(x=input$hue,y=input$xvariable, fill = input$hue))+
        geom_boxplot()+
        theme_bw()+
        theme(legend.position  = "none")+ # remove figure legend
        labs(x=names(hue_labels[which(hue_labels == input$hue)]),
             y=names(switch_labels[which(switch_labels == input$xvariable)]))+ 
        coord_flip() # flip to horizontal boxplot
    })
    
    #TABLE TAB
    
    # Customized data table
    output$datatable_customized<- renderDataTable(
      as.tibble(data_filtered)[, input$table_columns_selected],
      options = list(
        pageLength=10, scrollX='400px'),
      filter = 'top'
    )
    
    # Full data table
    output$datatable_full<- renderDataTable(
      as.tibble(us_county_covid),
      options = list(
        pageLength=10, scrollX='400px'),
      filter = 'top'
    )
    
    # Download buttons
    output$download_customized_datatable <- downloadHandler(
      filename = function(){'us_county_covid_customized.csv'},
      content = function(fname) {
        write.csv(as.tibble(data_filtered)[, input$table_columns_selected], fname)
      }
    )
    
    output$download_full_datatable <- downloadHandler(
      filename = function(){'us_county_covid.csv'},
      content = function(fname) {
        write.csv(as.tibble(us_county_covid %>% select(-geometry)), fname)
      }
    )
    
  })
})
