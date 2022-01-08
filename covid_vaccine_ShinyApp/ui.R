# COVID vaccination

# Define UI for application 
shinyUI(
  
  dashboardPage(
    dashboardHeader(
      title="COVID-19 in the U.S."
    ),
    
    dashboardSidebar(
      
      sidebarMenu(
        
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
          "About",
          tabName = "about",
          icon = icon('info')
        ),
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
        radioButtons(
          inputId = "vaccination_status",
          label = "Select the Vaccination Status",
          choices = c('At Lease One Dose', 'Fully Vaccinated', 'Booster (or Additional) Dose')
        ),
        selectInput(
          inputId = "age",
          label = "Vaccination Age Group",
          choices = c("All Age Groups","≥ 5 Years", "≥ 12 Years", "≥ 18 Years", "≥ 65 Years")
        ),
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
        )
        # selectInput(
        #   inputId = "county",
        #   label = "Select or type in one county",
        #   choices = NULL
        # ),
        
        
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "map",
          tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),# increase the height of the map
          leafletOutput("map")
          
        ),
        tabItem(
          tabName = "analysis",
          h1("Analysis")
        ),
        tabItem(
          tabName = "about",
          h1("About")
        )
      )
      
    )
  )
)
