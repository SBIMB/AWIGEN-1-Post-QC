library(shiny)
library(shinydashboard)
library(plotly)
library(shinyalert)
source('measurements.R')

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
        
        # data here..
        menuItem("Data", tabName = "imported_data"),
        
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
        # data
        tabItem( tabName = "imported_data",
                 fluidRow(style='height:40vh',
                          column(width=6,
                                 uiOutput("launch_summary"),
                                 uiOutput("launch_columns")
                          ),
                          column(width=6,
                                 fluidRow(
                                   plotOutput("chart_preview")
                                   
                                 ),
                                 fluidRow(
                                   
                                   tableOutput("descriptive")
                                 )
                                 
                          )
                          
                 )
                 
        ),
        
        # demographics
        tabItem( tabName = "demographics",
                 fluidRow(
                   h2(" demo here ..")
                 )
                 
        ),
        
        # lifestyle
        tabItem( tabName = "lifestyle",
                 fluidRow(
                   h2(" lifestyle  here ..")
                 )
                 
        ),
        
        # health
        tabItem( tabName = "health",
                 fluidRow(
                   h2(" health here ..")
                 )
                 
        ),
        
        # family
        tabItem( tabName = "family",
                 fluidRow(
                   h2(" family here ..")
                 )
                 
        ),
        
        # exposure
        tabItem( tabName = "exposure",
                 fluidRow(
                   h2("exposure here ..")
                 )
                 
        ),
        
        # infection
        tabItem( tabName = "infection",
                 fluidRow(
                   h2("infection here ..")
                 )
                 
        ),
        
        # cardiovascular
        tabItem( tabName = "cardiovascular",
                 fluidRow(
                   h2(" cardio here ..")
                 )
                 
        ),
        
        # measurements
        tabItem( tabName = "measurements",
                 # variable sections
                 fluidRow(
                   box(title="Categoricals", width = 6, background = "black"),
                   box(title = "Numericals", width = 6, background = "light-blue")
                 ),
                 # top row
                 fluidRow(
                   
                   column(6,
                          tabBox(height = "600px",width = "250px",
                                 tabPanel("Variables",
                                            selectInput("m_categorical1","Choose:", c(measurements_cat_cols),multiple = TRUE),
                                            hr(),
                                            selectInput("m_categorical2","Choose:", c(group_by))
                                 ),
                                 tabPanel("Crosstabs",
                                          div(style = 'overflow-y:scroll;height:500px;',
                                              verbatimTextOutput("crosstab_summary"))
                                 ),
                                 tabPanel("Plot", plotOutput("measurement_bar_plot")),
                                  tabPanel("Per site")
                                 
                          )
                   ),
                   column(6,
                          tabBox(height = "600px", width = "250px",
                                 tabPanel("Variables", uiOutput("measurement_columns")),
                                 tabPanel("Missing", tableOutput("missing")),
                                 tabPanel("Not Missing", tableOutput("not_missing")),
                                 tabPanel("Plot", plotOutput("plot_measurements")),
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
                          tabBox(height = "600px",width = "250px",
                                 tabPanel("Select columns", "3")
                          )
                   ),
                   column(6,
                          tabBox(height = "600px",width = "250px",
                                 tabPanel("Columns")
                          )
                   )
                 )
        ),
        
        # calcultations
        tabItem( tabName = "calcultations",
                 fluidRow(
                   h2("calculations here ..")
                 )
                 
        )
      )
    )
  )
)
