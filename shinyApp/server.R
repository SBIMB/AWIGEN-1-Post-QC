library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(randomcoloR)
library(dplyr)
library(plotly)
library(expss)
library(hash)
source('demographics.R')
source('measurements.R')
source('data.R')
source('tab-modules.R')
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
        box(title = "Summary", status = "primary", solidHeader = T, renderText(about()), align = "center")
    })
    
    # display columns to select
    output$launch_columns <- renderUI({
      if(is.null(data())){return()}
      box(title = "Select columns", status = "primary", solidHeader = T,
          selectInput("a_categorical1", "Choose variable", sort(names(data())), multiple = TRUE))
    })
    
    # numeric
    output$num_columns <- renderUI({
      if(is.null(data())){return()}
          selectInput("awigen1", "Choose variable", sort(names(data())), multiple = TRUE)
    })
    
    
    # awigen data section ---------------------------------------------------------------------------------------
    # crosstab section
    # choose dataset
    a_selectedDataset <- reactive({
      if(is.null(data())){return()}
      df <- data()
      site <- "site"
      switch(input$a_dataInput,
             "All" = df,
             "Agincourt" = df[ which(df[,site] == 1),],
             "Digkale" = df[ which(df[,site] == 2),],
             "Nairobi" = df[ which(df[,site] == 3),],
             "Nanoro" = df[ which(df[,site] == 4),],
             "Navrongo" =  df[ which(df[,site] == 5),],
             "Soweto" = df[ which(df[,site] == 6),]
      )
    })
    
    # crostab tables
    output$a_crosstab_summary <- renderPrint({
      if(is.null(data())){return()}
      
      df <- a_selectedDataset()
      selection <- input$a_categorical1
      if (!is.null(selection)) {
        if(length(selection)==1){
          cro(df[,selection[1]],)
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
    output$a_bar_plot <- renderPlot({
      if(is.null(data())){return()}
      df <- data()
      selection <- input$a_categorical1
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
    
    # show the codebook
    output$aCodebook <- renderPrint({
      if(is.null(data())){return()}
      selection <- input$a_categorical1
      if (!is.null(selection)) {
        if(length(selection)==1){
          print(paste("------",selection[1],"--------"))
          print(awigen_hash[[selection[1]]])
        }else if(length(selection)==2){
          print(paste("------",selection[1],"--------"))
          print(awigen_hash[[selection[1]]])
          print(paste("------",selection[2],"--------"))
          print(awigen_hash[[selection[2]]])
        }else if(length(selection)==3){
          print(paste("------",selection[1],"--------"))
          print(awigen_hash[[selection[1]]])
          print(paste("------",selection[2],"--------"))
          print(awigen_hash[[selection[2]]])
          print(paste("------",selection[3],"--------"))
          print(awigen_hash[[selection[3]]])
        }else{
          Print("Oops!")
        }
      }
    })
    
    # numericals
    # reactives for plotting measurements
    # measures with -999
    a_with_999 <- reactive({
      if(is.null(data())){return()}
      df <- data()
      num_col <- input$awigen1
      df[ which(df[, num_col] == -999), ]
    })
    
    # print number of missing values per category
    output$a_missing <- renderTable({
      if(is.null(data())){return()}
      num_col <- input$awigen1
      sortbyColumn <- input$awigen2
      as.data.frame(table(a_with_999()[, sortbyColumn], dnn = list(num_col)), responseName = "Count")
    })
    
    # measures with no missing values
    a_no_999 <- reactive({
      if(is.null(data())){return()}
      df <- data()
      num_col <- input$awigen1
      #sortbyColumn <- input$awigen1
      df[ which(df[, num_col] != -999), ]
    })
    
    # print number of non missing values per category
    output$d_not_missing <- renderTable({
      if(is.null(data())){return()}
      num_col <- input$awigen1
      sortbyColumn <- input$awigen2
      as.data.frame(table(a_no_999()[, sortbyColumn], dnn = list(num_col)), responseName = "Count")
    })
    
    # means for measures
    a_meanForMeasures <- reactive({
      if(is.null(data())){return()}
      df <- data()
      num_col <- input$awigen1
      sortbyColumn <- input$awigen2
      data <- do.call("ddply",list(a_no_999(), sortbyColumn, summarize, aw_std.mean = call("mean",as.symbol(num_col),na.rm=TRUE)))
      colnames(data) <- c(num_col, "Mean")
      data
    })
    
    # median for measures
    a_medianForMeasures <- reactive({
      if(is.null(data())){return()}
      df <- data()
      num_col <- input$awigen1
      sortbyColumn <- input$awigen2
      data <- do.call("ddply",list(a_no_999(), sortbyColumn, summarize, aw_std.median = call("median",as.symbol(num_col),na.rm=TRUE)))
      colnames(data) <- c(num_col, "Median")
      data
    })
    
    # render statistics
    #n return the summary also
    output$a_stats_mean <- renderDataTable({
      a_meanForMeasures()
    })
    
    output$a_stats_median <- renderDataTable({
      a_medianForMeasures()
    })
    
    output$summary_of_selected_awigen <- renderPrint({
      if(is.null(data())){return()}
      df <- a_no_999()
      num_col <- input$awigen1
      #sortbyColumn <- input$demo2
      summary(df[,num_col])
    })
    
    
    # outliers for measures
    a_outliersForMeasures <- reactive({
      if(is.null(data())){return()}
      dataf <- a_no_999()
      num_col <- input$awigen1
      outliers <- boxplot(dataf[, num_col], plot=FALSE)$out
      dataf[which(dataf[, num_col] %in% outliers),]
    })
    
    # return the outliers 
    output$a_return_outliers <- renderDataTable({
      if(is.null(data())){return()}
      num_col <- input$demo1
      sortbyColumn <- input$demo2
      dataf <- a_outliersForMeasures()
      select(dataf, sortbyColumn, num_col)
      
    })
    
    # render plot of measurements
    output$plot_awigen <- renderPlot({
      if(is.null(data())){return()}
      no_999 <- a_no_999()
      num_col <- input$awigen1
      sortbyColumn <- input$awigen2
      
      # make the chosen column a factor
      no_999[,sortbyColumn] <- as.factor(no_999[,sortbyColumn])
      
      # get groupby column to be used as fill in the plot
      fillColumn <- no_999[, sortbyColumn]
      
      # randomly generate the colors to be used in the plot based on the mean categories
      nColors <- nrow(a_meanForMeasures())
      paletteColors <- distinctColorPalette(nColors)
      
      ggplot(no_999, aes(x=no_999[,num_col], color=sortbyColumn, fill=fillColumn)) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 30)+
        geom_density(alpha=0.6)+
        scale_color_manual(values=paletteColors)+
        scale_fill_manual(values=paletteColors)+
        labs(title= paste(num_col, "histogram", sep=" "), x=num_col, y = "Density")+
        theme_prefered
      
    })
    
    
    
    
    
    # demographics --------------------------------------------------------------------------
    # crosstab section
    # choose dataset
    d_selectedDataset <- reactive({
      if(is.null(data())){return()}
      df <- data()
      site <- "site"
      switch(input$d_dataInput,
             "All" = df,
             "Agincourt" = df[ which(df[,site] == 1),],
             "Digkale" = df[ which(df[,site] == 2),],
             "Nairobi" = df[ which(df[,site] == 3),],
             "Nanoro" = df[ which(df[,site] == 4),],
             "Navrongo" =  df[ which(df[,site] == 5),],
             "Soweto" = df[ which(df[,site] == 6),]
      )
    })
    
    # crostab tables
    output$d_crosstab_summary <- renderPrint({
      if(is.null(data())){return()}
      
      df <- d_selectedDataset()
      selection <- input$d_categorical1
      if (!is.null(selection)) {
        if(length(selection)==1){
          cro(df[,selection[1]],)
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
    output$demographics_bar_plot <- renderPlot({
      if(is.null(data())){return()}
      df <- data()
      selection <- input$d_categorical1
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
    
    # show the codebook
    output$dCodebook <- renderPrint({
      if(is.null(data())){return()}
      selection <- input$d_categorical1
      if (!is.null(selection)) {
        if(length(selection)==1){
          print(paste("------",selection[1],"--------"))
          print(demographics_hash[[selection[1]]])
        }else if(length(selection)==2){
          print(paste("------",selection[1],"--------"))
          print(demographics_hash[[selection[1]]])
          print(paste("------",selection[2],"--------"))
          print(demographics_hash[[selection[2]]])
        }else if(length(selection)==3){
          print(paste("------",selection[1],"--------"))
          print(demographics_hash[[selection[1]]])
          print(paste("------",selection[2],"--------"))
          print(demographics_hash[[selection[2]]])
          print(paste("------",selection[3],"--------"))
          print(demographics_hash[[selection[3]]])
        }else{
          Print("Oops!")
        }
      }
    })
    
    # numericals
    # get distribution of hiv numeric variables
    # reactives for plotting measurements
    # measures with -999
    d_with_999 <- reactive({
      if(is.null(data())){return()}
      df <- data()
      num_col <- input$demo1
      sortbyColumn <- input$demo2
      df[ which(df[, num_col] == -999), ]
    })
    
    # print number of missing values per category
    output$d_missing <- renderTable({
      if(is.null(data())){return()}
      num_col <- input$demo1
      sortbyColumn <- input$demo2
      as.data.frame(table(d_with_999()[, sortbyColumn], dnn = list(num_col)), responseName = "Count")
    })
    
    # measures with no missing values
    d_no_999 <- reactive({
      if(is.null(data())){return()}
      df <- data()
      num_col <- input$demo1
      sortbyColumn <- input$demo2
      df[ which(df[, num_col] != -999), ]
    })
    
    # print number of non missing values per category
    output$d_not_missing <- renderTable({
      if(is.null(data())){return()}
      num_col <- input$demo1
      sortbyColumn <- input$demo2
      as.data.frame(table(d_no_999()[, sortbyColumn], dnn = list(num_col)), responseName = "Count")
    })
    
    # means for measures
    d_meanForMeasures <- reactive({
      if(is.null(data())){return()}
      df <- data()
      num_col <- input$demo1
      sortbyColumn <- input$demo2
      data <- do.call("ddply",list(d_no_999(), sortbyColumn, summarize, aw_std.mean = call("mean",as.symbol(num_col),na.rm=TRUE)))
      colnames(data) <- c(num_col, "Mean")
      data
    })
    
    # median for measures
    d_medianForMeasures <- reactive({
      if(is.null(data())){return()}
      df <- data()
      num_col <- input$demo1
      sortbyColumn <- input$demo2
      data <- do.call("ddply",list(d_no_999(), sortbyColumn, summarize, aw_std.median = call("median",as.symbol(num_col),na.rm=TRUE)))
      colnames(data) <- c(num_col, "Median")
      data
    })
    
    # render statistics
    #n return the summary also
    output$d_stats_mean <- renderDataTable({
      d_meanForMeasures()
    })
    
    output$d_stats_median <- renderDataTable({
      d_medianForMeasures()
    })
    
    output$summary_of_selected_demography <- renderPrint({
      if(is.null(data())){return()}
      df <- d_no_999()
      num_col <- input$demo1
      sortbyColumn <- input$demo2
      summary(df[,num_col])
    })
    
    
    # outliers for measures
    d_outliersForMeasures <- reactive({
      if(is.null(data())){return()}
      dataf <- d_no_999()
      num_col <- input$demo1
      outliers <- boxplot(dataf[, num_col], plot=FALSE)$out
      dataf[which(dataf[, num_col] %in% outliers),]
    })
    
    # return the outliers 
    output$d_return_outliers <- renderDataTable({
      if(is.null(data())){return()}
      num_col <- input$demo1
      sortbyColumn <- input$demo2
      dataf <- d_outliersForMeasures()
      select(dataf, sortbyColumn, num_col)
      
    })
    
    # render plot of measurements
    output$plot_demographics <- renderPlot({
      if(is.null(data())){return()}
      no_999 <- d_no_999()
      num_col <- input$demo1
      sortbyColumn <- input$demo2
      
      # make the chosen column a factor
      no_999[,sortbyColumn] <- as.factor(no_999[,sortbyColumn])
      
      # get groupby column to be used as fill in the plot
      fillColumn <- no_999[, sortbyColumn]
      
      # randomly generate the colors to be used in the plot based on the mean categories
      nColors <- nrow(d_meanForMeasures())
      paletteColors <- distinctColorPalette(nColors)
      
      ggplot(no_999, aes(x=no_999[,num_col], color=sortbyColumn, fill=fillColumn)) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 30)+
        geom_density(alpha=0.6)+
        scale_color_manual(values=paletteColors)+
        scale_fill_manual(values=paletteColors)+
        labs(title= paste(num_col, "histogram", sep=" "), x=num_col, y = "Density")+
        theme_prefered
      
    })
    
    
    
    
    
    
    
    # measurements --------------------------------------------------------------------------
    # crosstab section
    # choose dataset
    selectedDataset <- reactive({
      if(is.null(data())){return()}
      df <- data()
      site <- "site"
      switch(input$dataInput,
             "All" = df,
             "Agincourt" = df[ which(df[,site] == 1),],
             "Digkale" = df[ which(df[,site] == 2),],
             "Nairobi" = df[ which(df[,site] == 3),],
             "Nanoro" = df[ which(df[,site] == 4),],
             "Navrongo" =  df[ which(df[,site] == 5),],
             "Soweto" = df[ which(df[,site] == 6),]
      )
    })
    
    # crostab tables
    output$crosstab_summary <- renderPrint({
      if(is.null(data())){return()}
      
      df <- selectedDataset()
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
    
    # show the codebook
    output$mCodebook <- renderPrint({
      if(is.null(data())){return()}
      selection <- input$m_categorical1
      if (!is.null(selection)) {
        if(length(selection)==1){
          print(paste("------",selection[1],"--------"))
          print(measurements_hash[[selection[1]]])
        }else if(length(selection)==2){
          print(paste("------",selection[1],"--------"))
          print(measurements_hash[[selection[1]]])
          print(paste("------",selection[2],"--------"))
          print(measurements_hash[[selection[2]]])
        }else if(length(selection)==3){
          print(paste("------",selection[1],"--------"))
          print(measurements_hash[[selection[1]]])
          print(paste("------",selection[2],"--------"))
          print(measurements_hash[[selection[2]]])
          print(paste("------",selection[3],"--------"))
          print(measurements_hash[[selection[3]]])
        }else{
          Print("Oops!")
        }
    }
  })
    
    # numericals
    # get distribution of hiv numeric variables
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
      dataf[which(dataf[, num_col] %in% outliers),]
    })
    
    # return the outliers 
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