# COVID vaccination

# Define UI for application 
shinyUI(
  
  dashboardPage(
    dashboardHeader(
      title="COVID-19 Vaccination"
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
          choices = c('Vaccination',
                      'Cases per 100k Last 7 Days',
                      'Hospitalizations per 100k Last 7 Days', 
                      'Deaths per 100k Last 7 Days',
                      'Vaccination Hesitancy', 
                      'CDC Social Vulnerability Index')
        ),
        radioButtons(
          inputId = "vaccination_status",
          label = "Select the Vaccination Status",
          choices = c('At Lease One Dose', 'Fully Vaccinated', 'Booster (or Additional) Dose')
        ),
        checkboxGroupInput(
          inputId = "metro",
          label = "Select the Metropolitan Status",
          choices = c( "Metropolitan Counties" , "Non-Metropolitan Counties"),
          selected = c( "Metropolitan Counties" , "Non-Metropolitan Counties")
        ),
        selectInput(
          inputId = "state",
          label = "Select or type in one state",
          choices = c(us_county_covid %>%
                        pull(STATE_NAME) %>%
                        unique() %>%
                        sort())
        ),
        selectInput(
          inputId = "county",
          label = "Select or type in one county",
          choices = NULL
        ),
        selectInput(
          inputId = "age",
          label = "Select one age group",
          choices = c("≥ 5 Years", "≥ 12 Years", "≥ 18 Years", "≥ 65 Years")
        )
        
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
