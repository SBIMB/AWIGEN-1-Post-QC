library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(randomcoloR)
library(dplyr)
library(plotly)
library(expss)
source('measurements.R')
source('plots.R')

# allow bigger files to be imported 
options(shiny.maxRequestSize=50*1024^2)

shinyServer(
  function(input, output){
    
    # read the dataset
    data <- reactive({
      awigenFile <- input$file
      if(is.null(awigenFile)){return()}
      read.csv(awigenFile$datapath, header = TRUE, sep = "," , stringsAsFactors=TRUE)
    })
    
    about <- reactive({
      if(is.null(data())){return()}
      df <- data()
      r <- nrow(df) # number of rows
      c <- ncol(df) # number of columns
      paste("Dataset has",r, "rows and",c,"columns", sep = " ")
    })
    
    # display data sections
    output$launch_summary <- renderUI({
      if(is.null(data()))
        h5("Powered by", tags$img(src='logo.jpg', Height=300, width=300), align = "center")
      else
       box(title = "Summary", status = "primary", solidHeader = T,
       renderText(about()))
      })
    
    # display columns to select
    output$launch_columns <- renderUI({
      if(is.null(data())){return()}
      box(title = "Select columns", status = "primary", solidHeader = T,
            selectInput("var", "Choose variable",sort(names(data()))))
    })
    
    # show plot for selected column
    output$chart_preview <- renderPlot({
      if(is.null(data())){return()}
      df <- data()
      selectedColumn <- input$var
      bar_plot_1(df, selectedColumn)
    })
    
    # render table of stats
    output$descriptive <- renderTable({
      if(is.null(data())){return()}
      df <- data()
      selectedColumn <- input$var
      table(df[,selectedColumn])
    })
    
    # measurements ___________
    # crosstab section
    # crostab tables
    output$crosstab_summary <- renderPrint({
      if(is.null(data())){return()}
      df <- data()
      selection <- input$m_categorical1
      if (!is.null(selection)) {
        if(length(selection)==1){
          cro(df[,selection[1]])
        }else if(length(selection)==2){
          cro(df[,selection[1]], df[,selection[2]])
        }else if(length(selection)==3){
          cro(df[,selection[1]], df[,selection[2]], df[,selection[3]])
        }else{
          print("Cannot print")
        }
        
      }
      
    })
    # crosstab plots
    # bar plot for measurements
    output$measurement_bar_plot <- renderPlot({
      if(is.null(data())){return()}
      df <- data()
      selection <- input$m_categorical1
      if (!is.null(selection)) {
        if(length(selection)==1){
          bar_plot_1(df, selection[1])
        }else if(length(selection)==2){
          bar_plot_2(df, selection[1], selection[2])
        }else if(length(selection)==3){
          bar_plot_3(df, selection[1], selection[2], selection[3])
        }else{
          shinyalert("Oops!", "Try not more than 2 variables", type = "error")
        }
        
      }
    })
    
    # numericals
    # get distribution of hiv numeric variables
    output$measurement_columns <- renderUI({
      
      if(is.null(data())){return()}
      box(title = "Select numeric here ", status = "primary", solidHeader = T,
          selectInput("measure1", "Choose variable",sort(measurements_num_cols)),
          selectInput("measure2", "Choose variable",sort(group_by)))
    })
    
    # reactives for plotting measurements
    # measures with -999
    with_999 <- reactive({
      if(is.null(data())){return()}
      df <- data()
      num_col <- input$measure1
      sortbyColumn <- input$measure2
      df[ which(df[, num_col] == -999), ]
    })
    
    # print number of missing values per category
    output$missing <- renderTable({
      if(is.null(data())){return()}
      num_col <- input$measure1
      sortbyColumn <- input$measure2
      as.data.frame(table(with_999()[, sortbyColumn], dnn = list(num_col)), responseName = "Count")
    })
    
    # measures with no missing values
    no_999 <- reactive({
      if(is.null(data())){return()}
      df <- data()
      num_col <- input$measure1
      sortbyColumn <- input$measure2
      df[ which(df[, num_col] != -999), ]
    })
    
    # print number of non missing values per category
    output$not_missing <- renderTable({
      if(is.null(data())){return()}
      num_col <- input$measure1
      sortbyColumn <- input$measure2
      as.data.frame(table(no_999()[, sortbyColumn], dnn = list(num_col)), responseName = "Count")
    })
    
    # means for measures
    meanForMeasures <- reactive({
      if(is.null(data())){return()}
      df <- data()
      num_col <- input$measure1
      sortbyColumn <- input$measure2
      data <- do.call("ddply",list(no_999(), sortbyColumn, summarize, aw_std.mean = call("mean",as.symbol(num_col),na.rm=TRUE)))
      colnames(data) <- c(num_col, "Mean")
      data
    })
    
    # median for measures
    medianForMeasures <- reactive({
      if(is.null(data())){return()}
      df <- data()
      num_col <- input$measure1
      sortbyColumn <- input$measure2
      data <- do.call("ddply",list(no_999(), sortbyColumn, summarize, aw_std.median = call("median",as.symbol(num_col),na.rm=TRUE)))
      colnames(data) <- c(num_col, "Median")
      data
    })
    
    # render statistics
    #n return the summary also
    output$stats_mean <- renderDataTable({
      meanForMeasures()
    })
    
    output$stats_median <- renderDataTable({
      medianForMeasures()
    })
    
    output$summary_of_selected_measurement <- renderPrint({
      if(is.null(data())){return()}
      df <- no_999()
      num_col <- input$measure1
      sortbyColumn <- input$measure2
      summary(df[,num_col])
    })
    
    
    # outliers for measures
    outliersForMeasures <- reactive({
      if(is.null(data())){return()}
      dataf <- no_999()
      num_col <- input$measure1
      outliers <- boxplot(dataf[, num_col], plot=FALSE)$out
      no_999_outliers <- dataf[which(dataf[, num_col] %in% outliers),]
    })
    
    # return the oupliers 
    output$return_outliers <- renderDataTable({
      if(is.null(data())){return()}
      num_col <- input$measure1
      sortbyColumn <- input$measure2
      dataf <- outliersForMeasures()
      select(dataf, sortbyColumn, num_col)
      
    })
    
    # render plot of measurements
    output$plot_measurements <- renderPlot({
      if(is.null(data())){return()}
      no_999 <- no_999()
      num_col <- input$measure1
      sortbyColumn <- input$measure2
      
      # make the chosen column a factor
      no_999[,sortbyColumn] <- as.factor(no_999[,sortbyColumn])
      
      # get groupby column to be used as fill in the plot
      fillColumn <- no_999[, sortbyColumn]
      
      # randomly generate the colors to be used in the plot based on the mean categories
      nColors <- nrow(meanForMeasures())
      paletteColors <- distinctColorPalette(nColors)

      # define the theme -- static
      theme_prefered <-  theme(
        plot.title = element_text(color="black", size=18),
        axis.title.x = element_text(color="black", size=18),
        axis.title.y = element_text(color="black", size=18),
        axis.text = element_text(color="black", size=16),
        legend.text = element_text(color="black", size=18),
        legend.title = element_text(color="black", size=18),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      )
      
      ggplot(no_999, aes(x=no_999[,num_col], color=sortbyColumn, fill=fillColumn)) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 30)+
        geom_density(alpha=0.6)+
        scale_color_manual(values=paletteColors)+
        scale_fill_manual(values=paletteColors)+
        labs(title= paste(num_col, "histogram", sep=" "), x=num_col, y = "Density")+
        theme_prefered
      
    })
    
    
    
    
    
    # others ..
    
    
  }
)