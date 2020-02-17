library(ggplot2)
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
  barplot(tt, ylab = selectedColumn1, xlab = selectedColumn2)
  
}

