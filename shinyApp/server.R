library(shiny)
library(shinydashboard)
library(ggplot2)

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
    
    #sites in the file
    # output$agincourt <- data()[ which(data()[,site == 1]),]
    # output$digkale <- data()[ which(data()[,site == 2]),]
    # output$nairobi <- data()[ which(data()[,site == 3]),]
    # output$nanoro <- data()[ which(data()[,site == 4]),]
    # output$navrongo <- data()[ which(data()[,site == 5]),]
    # output$soweto <- data()[ which(data()[,site == 6]),]

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
      bb <- barplot(table(df[,selectedColumn]), 
                    width = 0.85, 
                    ylim=c(0,2700), 
                    main = selectedColumn, 
                    ylab = "Count")
      text(x = bb, 
           y = table(df[,selectedColumn]), 
           label = table(df[,selectedColumn]), 
           pos = 3, cex = 0.8, 
           col = "black")
      
    })
    
    # render table of stats
    output$descriptive <- renderTable({
      if(is.null(data())){return()}
      df <- data()
      selectedColumn <- input$var
      table(df[,selectedColumn])
    })
    
    # measurements
    output$hiv_columns <- renderUI({
       hiv_cols <- c("tested_hiv_qc",
                     "hiv_status_qc",
                     "hiv_positive_qc",
                     "hiv_medication_qc",
                     "traditional_med_hiv_qc",
                     "agree_hivtest",
                     "result_hiv_qc")
      
      if(is.null(data())){return()}
      box(title = "Select columns for crosstabs", status = "primary", solidHeader = T,
          selectInput("hiv_var1", "Choose variable",sort(hiv_cols)),
          selectInput("hiv_var2", "Choose variable",sort(hiv_cols)),
          radioButtons("chosenSite", label = h3("Choose site"),
                       choices = list("Agincourt" = 1, 
                                      "Digkale" = 2, 
                                      "Nairobi" = 3,
                                      "Nanoro" = 4,
                                      "Navrongo" =5,
                                      "Soweto" = 6,
                                      "All sites" = 7))
          )
    })
    
    # filter rows according to selected site
    # returns site data
    datasetInput <- reactive({
      if(is.null(data())){return()}
      df <- data()
      cl <- "site"
      clickedBtn <- input$chosenSite
      
       switch (clickedBtn,
         "1" = df[which(df[, cl] == 1),],
         "2" = df[which(df[, cl] == 2),],
         "3" = df[which(df[, cl] == 3),],
         "4" = df[which(df[, cl] == 4),],
         "5" = df[which(df[, cl] == 5),],
         "6" = df[which(df[, cl] == 6),],
         "7" = data()
       )
    })
    
    # get crosstabs    
    output$crosstabs <- renderTable({
      if(is.null(data())){return()}
      
      selectedVar1 <- input$hiv_var1
      selectedVar2 <- input$hiv_var2
      dff <- datasetInput()
      table(dff[, selectedVar1], dff[, selectedVar2])
      
    })
    
    # get distribution of hiv numeric variables
    output$measurement_columns <- renderUI({
      num_cols <- c("standing_height_qc",
                      "weight_qc",
                      "waist_circumference_qc",
                      "hip_circumference_qc",
                      "bp_sys_average_qc",
                      "bp_dia_average_qc",
                      "pulse_average_qc",
                      "visceral_fat_qc",
                      "subcutaneous_fat_qc",
                      "min_cimt_right",
                      "max_cimt_right",
                      "mean_cimt_right",
                      "mean_cimt_right_qc",
                      "min_cimt_left",
                      "max_cimt_left",
                      "mean_cimt_left",
                      "mean_cimt_left_qc",
                      "fasting_confirmation_qc",
                      "glucose_qc",
                      "s_creatinine_qc",
                      "insulin_qc",
                      "hdl_qc",
                      "ldl_qc",
                      "cholesterol_1_qc",
                      "triglycerides_qc",
                      "ur_creatinine_qc",
                      "ur_albumin_qc",
                      "ur_protein_qc")
      
      sortby <- c("sex",
                  "country",
                  "site")
      
      if(is.null(data())){return()}
      box(title = "Select numeric here ", status = "primary", solidHeader = T,
          selectInput("measure1", "Choose variable",sort(num_cols)),
          selectInput("measure2", "Choose variable",sort(sortby)))
    })
    
    # render plot of measurements
    
    num_var_summary <- function(df, num_col, site_names){
      
      # checking -999 values: missing values
      with_999 <- df[ which(df[, num_col] == -999), ]
      print("Printing the number of missing values per site")
      print(data.frame(table(with_999[, site_names])))
      
      
      # removing -999 values
      no_999 <- df[ which(df[, num_col] != -999), ]
      print("Printing the count of non missing values per site")
      print(data.frame(table(no_999[, site_names])))
      print(summary(no_999[, site_names]))
      
      
      # getting the mean per site
      mu_mean <- do.call("ddply",list(no_999, site_names, summarize, aw_std.mean = call("mean",as.symbol(num_col),na.rm=TRUE)))
      print("printing the means")
      print(head(mu_mean))
      
      
      # getting the medians per site
      mu_median <- do.call("ddply",list(no_999, site_names, summarize, aw_std.median = call("median",as.symbol(num_col),na.rm=TRUE)))
      print("printing the medians per site")
      print(head(mu_median))
      
      # getting the outliers
      outliers <- boxplot(no_999[, num_col], plot=FALSE)$out
      no_999_outliers <- no_999[which(no_999[, num_col] %in% outliers),]
      print("printing the outliers")
      print(data.frame(table(no_999_outliers[, site_names])))
      theme_prefered = theme(
        plot.title = element_text(color="black", size=18),
        axis.title.x = element_text(color="black", size=18),
        axis.title.y = element_text(color="black", size=18),
        axis.text = element_text(color="black", size=16),
        legend.text = element_text(color="black", size=18),
        legend.title = element_text(color="black", size=18),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      )
      
      plt <- ggplot(no_999, aes(x=no_999[,num_col], color=site_names, fill=site_names)) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 30)+
        geom_density(alpha=0.6)+
        geom_vline(data=mu_mean, aes(xintercept=aw_std.mean, color=site_names),
                   linetype="dashed")+
        geom_vline(data=mu_median, aes(xintercept=aw_std.median, color=site_names),
                   linetype="solid")+
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#de97da", "#e3948d", "#ecedb7"))+
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#de97da", "#e3948d", "#ecedb7"))+
        labs(title= paste(num_col, "histogram", sep=" "), x=num_col, y = "Density")+
        theme_prefered
      
      print(plt)
      
    }
    # others ..
    
    
  }
)