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
            choices = choices_xvariable),
          selectInput(
            inputId = "mean_median",
            label = "Select Mean or Median for Data Aggreggation",
            choices = c("Mean", "Median")
          )
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
          condition = "input.tabs == 'table'&&input.tabset_datatable=='Customized'",
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
          condition = "input.tabs != 'about'&&!(input.tabs == 'analysis'&&input.tabset_analysis=='Rank')",
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
      add_busy_spinner(spin = "fading-circle"),
      
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
          fluidRow(
            tabBox(
              title = "Data Analysis",
              id = "tabset_analysis", height = "600px", width = 12,
              
              tabPanel("Correlation", 
                         div(
                           style = "position: absolute; left: 4em; top: 4em;",
                           dropdown(
                             downloadButton(outputId = "download_scatter", label = "Download Plot"),
                             size = "xs",
                             icon = icon("download", class = "opt"), 
                             up = TRUE
                           )
                         ),
                       br(),
                       br(),
                         plotOutput("scatter") ,
                       br(),
                       br(),
                       div(
                         style = "position: absolute; left: 4em; top: 36em;",
                         dropdown(
                           downloadButton(outputId = "download_corr_heatmap", label = "Download Plot"),
                           size = "xs",
                           icon = icon("download", class = "opt"), 
                           up = TRUE
                         )
                       ),
                         plotOutput("corr_heatmap", width = 800, height = 800)
                       
              ),
              tabPanel("Inequality",
                       div(
                         style = "position: absolute; left: 4em; top: 4em;",
                         dropdown(
                           downloadButton(outputId = "download_inequality_bar", label = "Download Plot"),
                           size = "xs",
                           icon = icon("download", class = "opt"), 
                           up = TRUE
                         )
                       ),
                       div(
                         style = "position: absolute; left: 37em; top: 4em;",
                         dropdown(
                           downloadButton(outputId = "download_popbar", label = "Download Plot"),
                           size = "xs",
                           icon = icon("download", class = "opt"), 
                           up = TRUE
                         )
                       ),
                       column(
                           width = 6,
                           br(),
                           br(),
                           plotOutput("inequality_bar") 
                         ),
                         column(
                           width = 6,
                           br(),
                           br(),
                           plotOutput("popbar") 
                         )
                       
              ),
              tabPanel("Rank",
                       h2(uiOutput("title_value_box")),
                       fluidRow(
                         valueBoxOutput("box_case",3),
                         valueBoxOutput("box_test",3),
                         valueBoxOutput("box_hospitalization",3),
                         valueBoxOutput("box_inpatient",3)
                       ),
                       fluidRow(
                         valueBoxOutput("box_icu",3),
                         valueBoxOutput("box_staffed_icu",3),
                         valueBoxOutput("box_vent",3),
                         valueBoxOutput("box_death",3)
                       ),
                       fluidRow(
                         valueBoxOutput("box_1dose",3),
                         valueBoxOutput("box_2doses",3),
                         valueBoxOutput("box_booster",3),
                         valueBoxOutput("box_hesitancy",3)
                         ),
                       fluidRow(
                         valueBoxOutput("box_svi",width = 6),
                         valueBoxOutput("box_cvac",width = 6)),
                       fluidRow(
                         tabBox(
                           title = "County and State Rank",
                           id="rank_state_county",
                           width = 12,
                           tabPanel("County",
                                    div(
                                      style = "position: absolute; left: 4em; top: 4em;",
                                      dropdown(
                                        downloadButton(outputId = "download_rank_county", label = "Download Plot"),
                                        size = "xs",
                                        icon = icon("download", class = "opt"), 
                                        up = TRUE
                                      )
                                    ),
                                    br(),
                                    br(),
                                    plotOutput("rank_county", width = 900, height = 1200) 
                           ),
                           tabPanel("State",
                                    div(
                                      style = "position: absolute; left: 4em; top: 4em;",
                                      dropdown(
                                        downloadButton(outputId = "download_rank_state", label = "Download Plot"),
                                        size = "xs",
                                        icon = icon("download", class = "opt"), 
                                        up = TRUE
                                      )
                                    ),
                                    br(),
                                    br(),
                                    plotOutput("rank_state", width = 900, height = 800)
                           )
                         )
                       )
              )
            )
          )
        ),
        
        # Table tab
        tabItem(
          tabName = "table",
          
          tabBox(
            title = "Data Table for Analysis and Download",
            width = 12,
            id = "tabset_datatable",
            tabPanel("Customized", 
                     downloadButton('download_customized_datatable_csv', 'CSV'),
                     downloadButton('download_customized_datatable_xlsx', 'Excel'),
                     br(),
                     br(),
                     br(),
                     dataTableOutput("datatable_customized") 
            ),
            tabPanel("Full", 
                     downloadButton('download_full_datatable_csv', 'CSV'),
                     downloadButton('download_full_datatable_xlsx', 'Excel'),
                     br(),
                     br(),
                     br(),
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
