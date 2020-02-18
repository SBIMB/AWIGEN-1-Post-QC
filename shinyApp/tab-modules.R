library(shiny)
library(shinydashboard)

sectionLabels <- function(){
  fluidRow(
    box(title="Categoricals", width = 6, background = "black"),
    box(title = "Numericals", width = 6, background = "light-blue")
  )
}

tabItemRender <- function(fn){
  tagList(
    
    fn,
    # top row
    fluidRow(
      column(6, 
             tabBox(height = "600px",width = "250px", "...")),
      column(6, 
             tabBox(height = "600px",width = "250px", "..."))
    ),
    # bottom row
    fluidRow(
      column(6, 
             tabBox(height = "600px",width = "250px", "...")),
      column(6, 
             tabBox(height = "600px",width = "250px", "..."))
    )
    
  )

}

# tabItem(tabName = "measurements",
#          # variable sections
#          sectionLabels(),
#          # top row
#          fluidRow(
#            
#            column(6,
#                   tabBox(height = "600px",width = "250px",
#                          tabPanel("Variables",
#                                   helpText("Select one or 3 variables here"),
#                                   selectInput("m_categorical1","Choose:", c(measurements_cat_cols),multiple = TRUE),
#                                   hr(),
#                                   selectInput("m_categorical2","Choose:", c(group_by))
#                          ),
#                          tabPanel("Crosstabs",
#                                   div(style = 'overflow-y:scroll;height:500px;',
#                                       verbatimTextOutput("crosstab_summary"))
#                          ),
#                          tabPanel("Plot", plotOutput("measurement_bar_plot")),
#                          tabPanel("Per site")
#                          
#                   )
#            ),
#            column(6,
#                   tabBox(height = "600px", width = "250px",
#                          tabPanel("Variables", uiOutput("measurement_columns")),
#                          tabPanel("Missing", tableOutput("missing")),
#                          tabPanel("Not Missing", tableOutput("not_missing")),
#                          tabPanel("Plot", plotOutput("plot_measurements")),
#                          tabPanel("Mean", dataTableOutput("stats_mean")),
#                          tabPanel("Median", dataTableOutput("stats_median")),
#                          tabPanel("Summary", verbatimTextOutput("summary_of_selected_measurement")),
#                          tabPanel("Outliers",
#                                   div(style = 'overflow-y:scroll;height:500px;',
#                                       dataTableOutput("return_outliers")
#                                   )
#                          )
#                          
#                   )
#                   
#            )
#          ),
#          # bottom row
#          fluidRow(
#            column(6, 
#                   tabBox(height = "600px",width = "250px",
#                          tabPanel("Variable", "Still under development")
#                   )
#            ),
#            column(6,
#                   tabBox(height = "600px",width = "250px",
#                          tabPanel("Variable")
#                   )
#            )
#          )
# )
# 
