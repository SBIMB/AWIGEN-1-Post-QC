library(ggplot2)
library(plyr)
#import data from imac
#awigen <- read.csv("~/Documents/Development/AWIGEN-1-Post-QC/data/raw/all_sites_v2.5.3.23.csv", header=TRUE)

# reading data from my pc
awigen <- read.csv("~/Development/AWIGEN-1-Post-QC/data/raw/all_sites_v2.5.3.23.csv", header=TRUE)

# Assign real site names
site_labels <- c("Agincourt", "Digkale", "Nairobi", "Nanoro", "Navrongo", "Soweto")
awigen$site_names <- site_labels[awigen$site]

# filtering out rows per site
agincourt <- awigen[ which(awigen$site == 1),]
digkale <- awigen[ which(awigen$site == 2),]
nairobi <- awigen[ which(awigen$site == 3),]
nanoro <- awigen[ which(awigen$site == 4),]
navrongo <- awigen[ which(awigen$site == 5),]
soweto <- awigen[ which(awigen$site == 6),]

measurememts <- c("standing_height_qc",
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


hiv_cols <- c("tested_hiv_qc",
              "hiv_status_qc",
              "hiv_positive_qc",
              "hiv_medication_qc",
              "traditional_med_hiv_qc",
              "agree_hivtest",
              "result_hiv_qc")

# create age groups 
agincourt$age_group <- cut(agincourt$age, 
                           breaks=c(40, 61, 71, max(agincourt$age)), 
                           right = FALSE, 
                           labels = c("40-60","61-70","71 plus"))


# code for testing age groups created 
# ages <- agincourt %>%
#   select(age, age_group)
# change the age value to see where it belongs
# testing <- ages[ which(ages$age== 71),]
# View(testing)

# categorizing soweto data
soweto_sweet <- soweto[ which(soweto$cohort_id_c == "SWEET"),]
soweto_bara <- soweto[ which(soweto$cohort_id_c == "BARA"),]
soweto_men<- soweto[ which(soweto$cohort_id_c == ""),]

# get the crosstabs for hiv
hiv_cat_freq <- function(df, column_list){
  for (cl in column_list) {
    print(cl)
    df_table <- table(df[, cl])
    print(df_table)
    
  }
}

hiv_cat_freq(soweto_bara, hiv_cols)

# comparing two categorical columns
table(soweto_men$hiv_positive_qc, soweto_men$agree_hivtest_qc)

# checking if the ids from the dataset below belong to soweto
library(xlsx)
copy_sweet <- read_excel("~/Downloads/Copy of Sweet HIV Testing (2).xlsx")
ids <- copy_sweet$`Study ID`
bara <- soweto_bara[soweto_bara$study_id %in% ids,  ]
sweet <- soweto_sweet[soweto_sweet$study_id %in% ids,  ]
men <- soweto_men[soweto_men$study_id %in% ids,  ]

num_var_summary <- function(df, num_col, site_names){

  # checking -999 values: missing values
  with_999 <- df[ which(df[, num_col] == -999), ]
  print("Printing the number of missing values per site")
  print(data.frame(table(with_999[, site_names])))
  print(" ")
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
  
  # plotting using ggplot
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

num_var_summary(awigen, "weight_qc", "site_names")

