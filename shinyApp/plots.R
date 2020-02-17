library(ggplot2)
library(dplyr)
library(RColorBrewer)

# plot for one variable
bar_plot_1 <- function(df, selectedColumn){
  bp <- barplot(table(df[,selectedColumn]), 
                width = 0.5, 
                ylim=c(0,12050), 
                main = selectedColumn, 
                ylab = "Count")
  text(x = bp, 
       y = table(df[,selectedColumn]), 
       label = table(df[,selectedColumn]), 
       pos = 3, cex = 0.8, 
       col = "black")

}

# plot for two categorical variable
bar_plot_2 <- function(df, selectedColumn1, selectedColumn2){
  tt <- table(df[,selectedColumn1], df[,selectedColumn2])
   bp <- barplot(tt,
          col = brewer.pal(6,"Set1"),
          beside = T,
          ylim=c(0,7000),
          legend = row.names(tt),
          ylab = selectedColumn1, 
          xlab = selectedColumn2,
          las = 1)
   text(x = bp, 
        y = table(df[,selectedColumn1],df[,selectedColumn2]),
        label = table(df[,selectedColumn1],df[,selectedColumn2]), 
        pos = 4, 
        cex = 0.8, 
        col = "black",
        xpd=NA)
   
  
}

# more than 3 variables
bar_plot_3 <- function(df, selectedColumn1, selectedColumn2, selectedColumn3){
  tt <- ftable(df[, selectedColumn1], df[, selectedColumn2], df[, selectedColumn3])
  bp <- barplot(tt,
          col = brewer.pal(6,"Set1"),
          beside = T,
          ylim=c(0,7000),
          legend = row.names(tt),
          las = 1)
  
  text(x = bp, 
       y = tt,
       label = tt, 
       pos = 4, 
       cex = 0.8, 
       col = "black",
       xpd=NA)
}
