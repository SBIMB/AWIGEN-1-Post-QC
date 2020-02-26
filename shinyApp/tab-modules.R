library(shiny)
library(shinydashboard)


# set section labels for all tab items
sectionLabels <- function(){
  fluidRow(
    box(title="Categoricals", width = 6, background = "black"),
    box(title = "Numericals", width = 6, background = "light-blue")
  )
}