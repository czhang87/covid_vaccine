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
        conditionalPanel(
          condition = "input.tabs == 'analysis'",
          selectInput(
            inputId = "county",
            label = "Select or type in one county",
            choices = NULL
          )
          ,
          selectInput(
            inputId = "xvariable",
            label = "Select a Variable For The X-axis",
            choices = c("At Least One Dose in All Age Groups"="administered_dose1_pop_pct",
                        "At Least One Dose in ≥ 5 Years" = "administered_dose1_recip_5pluspop_pct")
            
                        # "At Least One Dose in ≥ 5 Years",
                        # "At Least One Dose in ≥ 12 Years",
                        # "At Least One Dose in ≥ 18 Years",
                        # "At Least One Dose in ≥ 65 Years",
                        # "Fully Vaccinated in All Age Groups",
                        # "Fully Vaccinated in ≥ 5 Years",
                        # "Fully Vaccinated in ≥ 12 Years",
                        # "Fully Vaccinated in ≥ 18 Years",
                        # "Fully Vaccinated in ≥ 65 Years",
                        # "Booster (or Additional) Dose in All Age Groups",
                        # "Booster (or Additional) Dose in ≥ 5 Years",
                        # "Booster (or Additional) Dose in ≥ 12 Years",
                        # "Booster (or Additional) Dose in ≥ 18 Years",
                        # "Booster (or Additional) Dose in ≥ 65 Years")
          )
        ),
        checkboxGroupInput(
          inputId = "metro",
          label = "Select the Metropolitan Status",
          choices = c("Metro" , "Non-metro"),
          selected = c("Metro" , "Non-metro")
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
          h1("Analysis of current COVID-19 data in the U.S."),
          fluidRow(
            column(
              width = 6,
              box(
                title = "Box title", width = NULL, status = "primary",
                plotlyOutput("scatter_cases")
              ),
              box(
                title = "Box title", width = NULL, status = "primary",
                "Box content"
              ),
              box(
                title = "Box title", width = NULL, status = "primary",
                "Box content"
              )
            )
            ,
            column(
              width = 6,
              box(
                title = "Box title", width = NULL, status = "primary",
                "Box content"
              ),
              box(
                title = "Box title", width = NULL, status = "primary",
                "Box content"
              ),
              box(
                title = "Box title", width = NULL, status = "primary",
                "Box content"
              )
            )
          )
        ),
        tabItem(
          tabName = "about",
          h1("About")
        )
      )
      
    )
  )
)
