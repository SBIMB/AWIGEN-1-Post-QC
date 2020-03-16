library(shiny)
library(shinydashboard)
library(plotly)
library(shinyalert)
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
        menuItem("Home", tabName = "home"), # This menu Item displays all the important messages to user
        # data here..
        menuItem("Data", tabName = "data")
        
      )
      
      
    ),
    
    
    # body
    dashboardBody(
      # tab items
      tabItems(
        tabItem( tabName = "home", div(uiOutput("launch_summary"), align="center")),
        
        # data: This is the main screen where users search for variables for analyis
        # There are two sections: Categorical and Numerical sections.
        # All column enteries MUST belong to any of those sections.
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
                 
        )
      )
    )
  )
)
