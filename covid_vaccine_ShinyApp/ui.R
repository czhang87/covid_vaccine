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
        conditionalPanel(
          condition = "input.tabs!='about'",
          actionBttn(
            inputId = "reset_input",
            label = "Reset Input")
        ),
        
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
          condition = "input.tabs=='analysis' && input.tabset_analysis == 'Correlation'",
          selectInput(
            inputId = "xvariable",
            label = "Select a Variable For The X-axis",
            choices = choices_xvariable),
          selectInput(
            inputId = "yvariable",
            label = "Select a Variable For The Y-axis",
            choices = choices_yvariable,
            selected  = "test_positivity_rate_last_7_d")
        ),
        
        conditionalPanel(
          condition = "input.tabs=='analysis'&& (input.tabset_analysis == 'Inequality'||input.tabset_analysis == 'Rank')",
          selectInput(
            inputId = "selected_variable",
            label = "Select a Variable",
            choices = choices_xvariable)
          ),
        
        conditionalPanel(
          condition = "input.tabs=='analysis'&&(input.tabset_analysis == 'Correlation'||input.tabset_analysis == 'Inequality')",
          selectInput(
            inputId = "hue",
            label = "Select a Comparison Category",
            choices = hue_labels)
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
        
        # Conditional panel except About tab
        conditionalPanel(
          condition = "input.tabs != 'about'&&input.tabset_analysis!='Rank'",
            selectInput(
              inputId = "state",
              label = "Select or Type in One or Multiple states",
              choices = choices_state,
              selected = "United States",
              multiple = T)
        ),
        
        # Conditional panel for Rank
        conditionalPanel(
          condition = "input.tabs=='analysis'&&input.tabset_analysis == 'Rank'",
          selectInput(
            inputId = "state_rank",
            label = "Select or Type in One State",
            choices = choices_state_rank),
          selectInput(
            inputId = "county",
            label = "Select a County",
            choices = NULL)
        ),
        
        # Conditional panel except About tab
        conditionalPanel(
          condition = "input.tabs != 'about'",
          checkboxGroupInput(
            inputId = "metro",
            label = "Select the Metropolitan Status",
            choices = choices_metro,
            selected = choices_metro),
          numericRangeInput(
            inputId = "population",
            label = "Filter Counties by Population",
            min = min_pop,
            max = max_pop,
            value = c(min_pop,max_pop),
            step = 5000)
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
            tabBox(
              title = "Data Analysis",
              id = "tabset_analysis", height = "1500px", width = 12,
              
              tabPanel("Correlation", 
                       fluidRow(
                         width=12,
                         box(
                           title = uiOutput("scatter_title"), width = 12, height = 1270, status = "primary", solidHeader = T,
                           plotOutput("scatter") %>% withSpinner(color="#0dc5c1"),
                           plotOutput("corr_heatmap", width = 800, height = 800)%>% withSpinner(color="#0dc5c1")
                           
                         )
                       )
              ),
              tabPanel("Inequality", 
                       fluidRow(
                         column(
                           width = 6,
                           box(
                             title = uiOutput("xboxplot_title"), width = NULL, status = "primary",solidHeader = T,
                             plotOutput("xbar") %>% withSpinner(color="#0dc5c1")
                           )
                         ),
                         column(
                           width = 6,
                           box(
                             title = "Median Population", width = NULL, status = "primary",solidHeader = T,
                             plotOutput("popbar") %>% withSpinner(color="#0dc5c1")
                           )
                         )
                       )
              ),
              tabPanel("Rank", "Rank analysis")
              )
            )
        ),
        
        # Table tab
        tabItem(
          tabName = "table",
          
          tabBox(
            title = "Data Table for Download",
            width = 12,
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
