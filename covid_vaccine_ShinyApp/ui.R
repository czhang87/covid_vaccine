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
            choices = choices_data_type
          ),
          
          # conditional panel of vaccination percentage
          conditionalPanel(
            condition = "input.data_type == 'Vaccination Percentage'",
            radioButtons(
              inputId = "vaccination_status",
              label = "Select the Vaccination Status",
              choices = choices_vaccination_status
            ),
            selectInput(
              inputId = "age",
              label = "Vaccination Age Group",
              choices = choices_age
            )
          )
        ),
        
        # conditional panel for analysis
        conditionalPanel(
          condition = "input.tabs == 'analysis'",
          
          selectInput(
            inputId = "xvariable",
            label = "Select a Variable For The X-axis",
            choices = choices_xvariable
          ),
          selectInput(
            inputId = "yvariable",
            label = "Select a Variable For The Y-axis",
            choices = choices_yvariable
          ),
          selectInput(
            inputId = "hue",
            label = "Select a Comparison Category",
            choices = hue_labels
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
          choices = choices_state,
          selected = "United States",
          multiple = T
        ),
        
        checkboxGroupInput(
          inputId = "metro",
          label = "Select the Metropolitan Status",
          choices = choices_metro,
          selected = choices_metro
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
          leafletOutput("map") %>% withSpinner(color="#0dc5c1")
          
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
                plotOutput("scatter") %>% withSpinner(color="#0dc5c1")
              )
              
            )
          ),
          fluidRow(
            column(
              width = 6,
              box(
                title = uiOutput("yboxplot_title"), width = NULL, status = "primary",solidHeader = T,
                plotlyOutput("yboxplot") %>% withSpinner(color="#0dc5c1")
              )
            ),
            column(
              width = 6,
              box(
                title = uiOutput("xboxplot_title"), width = NULL, status = "primary",solidHeader = T,
                plotlyOutput("xboxplot") %>% withSpinner(color="#0dc5c1")
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
                     dataTableOutput("datatable_customized") %>% withSpinner(color="#0dc5c1")
            ),
            tabPanel("Full", 
                     downloadButton('download_full_datatable', 'Download'),
                     dataTableOutput("datatable_full") %>% withSpinner(color="#0dc5c1")
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
