# COVID in the U.S.

# Define UI for application 
shinyUI(

  dashboardPage(

    
    dashboardHeader(
      title="COVID-19 in the U.S."
    ),
    
    dashboardSidebar(
      
      sidebarMenu(
        id ="tabs",
        
        menuItem(
          "Map",
          tabName = "map",
          icon = icon("map-marked-alt")
        ),
        menuItem(
          "Analysis",
          tabName = "analysis",
          icon = icon('chart-area')
        ),
        menuItem(
          "Table",
          tabName = "table",
          icon = icon('table')
        ),
        menuItem(
          "About",
          tabName = "about",
          icon = icon('info')
        ),
        
        # Reset input button
        actionBttn(
          inputId = "reset_input",
          label = "Reset Input"),
        
        # conditional panel of map tab
        conditionalPanel(
          condition =  "input.tabs == 'map'",
          selectInput(
            inputId = "data_type",
            label = "Select the Data Type",
            choices = c('Cases per 100k Last 7 Days',
                        'Test Positivity Rate Last 7 Days',
                        'Hospitalizations per 100k Last 7 Days', 
                        'Deaths per 100k Last 7 Days',
                        'Vaccination Percentage',
                        'Vaccination Hesitancy', 
                        'CDC Social Vulnerability Index',
                        'COVID-19 Vaccine Coverage Index')
          ),
          
          # conditional panel of vaccination percentage
          conditionalPanel(
            condition = "input.data_type == 'Vaccination Percentage'",
            radioButtons(
              inputId = "vaccination_status",
              label = "Select the Vaccination Status",
              choices = c('At Lease One Dose', 'Fully Vaccinated', 'Booster (or Additional) Dose')
            ),
            selectInput(
              inputId = "age",
              label = "Vaccination Age Group",
              choices = c("All Age Groups","≥ 5 Years", "≥ 12 Years", "≥ 18 Years", "≥ 65 Years")
            )
          )
        ),
        
        # conditional panel for analysis
        conditionalPanel(
          condition = "input.tabs == 'analysis'",
          
          selectInput(
            inputId = "xvariable",
            label = "Select a Variable For The X-axis",
            choices = c("Cases per 100k Last 7 Days"="Cases_per_100k_last_7_days",
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
                        "COVID-19 Vaccine Coverage Index" = "ability_to_handle_a_covid"
            )
          ),
          selectInput(
            inputId = "yvariable",
            label = "Select a Variable For The Y-axis",
            choices = c("Cases per 100k Last 7 Days"="Cases_per_100k_last_7_days",
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
                        "COVID-19 Vaccine Coverage Index" = "ability_to_handle_a_covid"
            )
          ),
          selectInput(
            inputId = "hue",
            label = "Select a Comparison Category",
            choices = c("Metropolitan Status"="metro_status",
                        "CDC Social Vulnerability Index" = "svi_category",
                        "COVID-19 Vaccine Coverage Index" = "cvac_category"
            )
          )
        ),
        
        # conditional panel of table
        conditionalPanel(
          condition = "input.tabs == 'table'",
          pickerInput(
            inputId = "table_columns_selected",
            label = "Select Table Columns",
            choices = table_columns,
            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
            selected = table_columns,
            multiple = T,
            choicesOpt = list(
              style = rep(("color: black; background: white;"),20))
          )
        ),
        
        # panel for all tabs
        selectInput(
          inputId = "state",
          label = "Select or Type in One or Multiple states",
          choices = c("United States", us_county_covid %>%
                        pull(STATE_NAME) %>%
                        unique() %>%
                        sort()),
          selected = "United States",
          multiple = T
        ),
        
        checkboxGroupInput(
          inputId = "metro",
          label = "Select the Metropolitan Status",
          choices = c("Metro" , "Non-metro"),
          selected = c("Metro" , "Non-metro")
        ),
        numericRangeInput(
          inputId = "population",
          label = "Enter the Population",
          min = min_pop,
          max = max_pop,
          value = c(min_pop,max_pop),
          step = 5000
        )
      )
    ),
    
    dashboardBody(
      
      tabItems(
        
        # Map tab
        tabItem(
          tabName = "map",
          tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),# increase the height of the map
          leafletOutput("map")
          
        ),
        
        # Analysis tab
        
        tabItem(
          tabName = "analysis",
          h1("Analysis of Current COVID-19 Data in The U.S. by County"),
          fluidRow(
            column(
              width = 12,
              box(
                title = uiOutput("scatter_title"), width = NULL, status = "primary",solidHeader = T,
                plotOutput("scatter")
              )
              
            )
            # ,
            # column(
            #   width = 6
            # 
            # )
          ),
          fluidRow(
            column(
              width = 6,
              box(
                title = uiOutput("yboxplot_title"), width = NULL, status = "primary",solidHeader = T,
                plotlyOutput("yboxplot")
              )
            ),
            column(
              width = 6,
              box(
                title = uiOutput("xboxplot_title"), width = NULL, status = "primary",solidHeader = T,
                plotlyOutput("xboxplot")
              )
            )
          )
        ),
        
        # Table tab
        tabItem(
          tabName = "table",
          
          tabBox(
            title = "Data Table for Download",
            width = 12,
            # The id lets us use input$tabset_datatable on the server to find the current tab
            id = "tabset_datatable",
            tabPanel("Customized", 
                     downloadButton('download_customized_datatable', 'Download'),
                     dataTableOutput("datatable_customized")
            ),
            tabPanel("Full", 
                     downloadButton('download_full_datatable', 'Download'),
                     dataTableOutput("datatable_full")
            )
          )
        ),
        
        # About tab
        tabItem(
          tabName = "about",
          h1("About")
        )
        
      )
      
    )
  )
)
