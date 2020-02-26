library(shiny)
library(shinydashboard)
library(plotly)
library(shinyalert)
source('demographics.R')
source('measurements.R')
source('data.R')
source('tab-modules.R')

shinyUI(
  dashboardPage(
    
    # header
    dashboardHeader(title = "AWI-Gen 1 QC"),
    
    
    # side bar
    dashboardSidebar(
      
      # importing dataset
      helpText("Import dataset here. Make sure it is a comma separated file"),
      fileInput("file", "Upload the csv here", accept = c(".csv")),
      br(),
      tags$hr(),
      
      # side bar menu items
      sidebarMenu(
        
        # data entry window
        menuItem("Home", tabName = "home"),
        # data here..
        menuItem("Data", tabName = "data"),
        # Instruments here ...
        menuItem("Demographic Information", tabName = "demographics"),
        menuItem("Behaviour Lifestyle", tabName = "lifestyle"),
        menuItem("Health history", tabName = "health"),
        menuItem("Family history", tabName = "family"),
        menuItem("Exposure", tabName = "exposure"),
        menuItem("Infection history", tabName = "infection"),
        menuItem("Cardiovascular Disease Risk",  tabName = "cardiovascular"),
        menuItem("Measurements", tabName = "measurements"),
        menuItem("Calculations", tabName = "calcultations")
      )
      
      
    ),
    
    
    # body
    dashboardBody(
      # tab items
      tabItems(
        tabItem( tabName = "home", div(uiOutput("launch_summary"), align="center")),
        
        # data ________________________________________________________________________________________________________
        tabItem( tabName = "data",
                 sectionLabels(),
                 # top row
                 fluidRow(
                   
                   column(6,
                          tabBox(height = "500px", width = "250px",
                                 tabPanel("Variables",
                                          helpText("Select one or 3 variables here"),
                                          uiOutput("launch_columns"),
                                          hr(),
                                          selectInput("a_dataInput","Choose dataset here:", c(site_data))
                                 ),
                                 tabPanel("Crosstabs",
                                          div(style = 'overflow-y:scroll;height:500px;',
                                              verbatimTextOutput("a_crosstab_summary"))
                                 ),
                                 tabPanel("Plot", plotOutput("a_bar_plot"))
                                 
                          )
                   ),
                   column(6,
                          tabBox(height = "500px", width = "250px",
                                 tabPanel("Variables",
                                          box(title = "Select numeric here ", status = "primary", solidHeader = T,
                                              uiOutput("num_columns"),
                                              selectInput("awigen2", "Choose variable",sort(group_by)))
                                 ),
                                 tabPanel("Missing", tableOutput("a_missing")),
                                 tabPanel("Not Missing", tableOutput("a_not_missing")),
                                 tabPanel("Mean", dataTableOutput("a_stats_mean")),
                                 tabPanel("Median", dataTableOutput("a_stats_median")),
                                 tabPanel("Summary", verbatimTextOutput("summary_of_selected_awigen")),
                                 tabPanel("Outliers",
                                          div(style = 'overflow-y:scroll;height:5000px;',
                                              dataTableOutput("a_return_outliers")
                                          )
                                 )
                                 
                          )
                          
                   )
                 ),
                 # bottom row
                 fluidRow(
                   column(6, 
                          tabBox(height = "500px",width = "250px",
                                 tabPanel("Codebook",
                                          div(style = 'overflow-y:scroll;height:400px;',
                                              verbatimTextOutput("aCodebook"))
                                 )
                                 
                          )
                          
                   ),
                   column(6,
                          tabBox(height = "500px",width = "250px",
                                 tabPanel("Plot", plotOutput("plot_awigen"))
                          )
                   )
                 )
                 
        ),
        
        # demographics ------------------------------------------------------------------------------
        tabItem( tabName = "demographics",
                 sectionLabels(),
                 # top row
                 fluidRow(
                   
                   column(6,
                          tabBox(height = "500px",width = "250px",
                                 tabPanel("Variables",
                                          helpText("Select one or 3 variables here"),
                                          selectInput("d_categorical1","Choose:", c(demographics_cat_cols),multiple = TRUE),
                                          hr(),
                                          selectInput("d_dataInput","Choose dataset here:", c(site_data))
                                 ),
                                 tabPanel("Crosstabs",
                                          div(style = 'overflow-y:scroll;height:500px;',
                                              verbatimTextOutput("d_crosstab_summary"))
                                 ),
                                 tabPanel("Plot", plotOutput("demographics_bar_plot"))
                                 
                          )
                   ),
                   column(6,
                          tabBox(height = "500px", width = "250px",
                                 tabPanel("Variables",
                                          box(title = "Select numeric here ", status = "primary", solidHeader = T,
                                              selectInput("demo1", "Choose variable",sort(demographics_num_cols)),
                                              selectInput("demo2", "Choose variable",sort(group_by)))
                                          ),
                                 tabPanel("Missing", tableOutput("d_missing")),
                                 tabPanel("Not Missing", tableOutput("d_not_missing")),
                                 tabPanel("Mean", dataTableOutput("d_stats_mean")),
                                 tabPanel("Median", dataTableOutput("d_stats_median")),
                                 tabPanel("Summary", verbatimTextOutput("summary_of_selected_demography")),
                                 tabPanel("Outliers",
                                          div(style = 'overflow-y:scroll;height:500px;',
                                              dataTableOutput("d_return_outliers")
                                          )
                                 )
                                 
                          )
                          
                   )
                 ),
                 # bottom row
                 fluidRow(
                   column(6, 
                          tabBox(height = "500px",width = "250px",
                                 tabPanel("Codebook",
                                          div(style = 'overflow-y:scroll;height:400px;',
                                              verbatimTextOutput("dCodebook"))
                                 )
      
                                 )
                          
                   ),
                   column(6,
                          tabBox(height = "500px",width = "250px",
                                 tabPanel("Plot", plotOutput("plot_demographics"))
                          )
                   )
                 )
                 
        ),
        
        # lifestyle ______________________________________________________________________________________
        tabItem( tabName = "lifestyle",
                 sectionLabels(),
                 fluidRow(
                   h2(" lifestyle  here ..")
                 )
                 
        ),
        
        # health
        tabItem( tabName = "health",
                 sectionLabels(),
                 fluidRow(
                   h2(" health here ..")
                 )
                 
        ),
        
        # family
        tabItem( tabName = "family",
                 sectionLabels(),
                 fluidRow(
                   h2(" family here ..")
                 )
                 
        ),
        
        # exposure
        tabItem( tabName = "exposure",
                 sectionLabels(),
                 fluidRow(
                   h2("exposure here ..")
                 )
                 
        ),
        
        # infection
        tabItem( tabName = "infection",
                 sectionLabels(),
                 fluidRow(
                   h2("infection here ..")
                 )
                 
        ),
        
        # cardiovascular
        tabItem( tabName = "cardiovascular",
                 sectionLabels()
                 
                 
        ),
        
        # measurements
        tabItem( tabName = "measurements",
                 # variable sections
                 sectionLabels(),
                 # top row
                 fluidRow(
                   
                   column(6,
                          tabBox(height = "500px",width = "250px",
                                 tabPanel("Variables",
                                          helpText("Select one or 3 variables here"),
                                          selectInput("m_categorical1","Choose:", c(measurements_cat_cols),multiple = TRUE),
                                          hr(),
                                          selectInput("dataInput","Choose dataset here:", c(site_data))
                                 ),
                                 tabPanel("Crosstabs",
                                          div(style = 'overflow-y:scroll;height:500px;',
                                              verbatimTextOutput("crosstab_summary"))
                                 ),
                                 tabPanel("Plot", plotOutput("measurement_bar_plot"))
                                 
                          )
                   ),
                   column(6,
                          tabBox(height = "500px", width = "250px",
                                 tabPanel("Variables",
                                          box(title = "Select numeric here ", status = "primary", solidHeader = T,
                                              selectInput("measure1", "Choose variable",sort(measurements_num_cols)),
                                              selectInput("measure2", "Choose variable",sort(group_by)))
                                          ),
                                 tabPanel("Missing", tableOutput("missing")),
                                 tabPanel("Not Missing", tableOutput("not_missing")),
                                 tabPanel("Mean", dataTableOutput("stats_mean")),
                                 tabPanel("Median", dataTableOutput("stats_median")),
                                 tabPanel("Summary", verbatimTextOutput("summary_of_selected_measurement")),
                                 tabPanel("Outliers",
                                          div(style = 'overflow-y:scroll;height:500px;',
                                              dataTableOutput("return_outliers")
                                          )
                                 )
                                 
                          )
                          
                   )
                 ),
                 # bottom row
                 fluidRow(
                   column(6, 
                          tabBox(height = "500px",width = "250px",
                                 tabPanel("Codebook",
                                          div(style = 'overflow-y:scroll;height:400px;',
                                              verbatimTextOutput("mCodebook"))
                                          )
                          )
                   ),
                   column(6,
                          tabBox(height = "500px",width = "250px",
                                 tabPanel("Plot", plotOutput("plot_measurements"))
                          )
                   )
                 )
        ),
        
        # calcultations
        tabItem( tabName = "calcultations",
                 sectionLabels(),
                 fluidRow(
                   h2("calculations here ..")
                 )
                 
        )
      )
    )
  )
)
