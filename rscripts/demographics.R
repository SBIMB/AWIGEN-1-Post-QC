#library
library(ggplot2)
#import data from imac
#awigen <- read.csv("~/Documents/Development/AWIGEN-1-Post-QC/data/raw/all_sites_v2.5.3.23.csv", header=TRUE)

# reading data from my pc
awigen <- read.csv("~/Development/AWIGEN-1-Post-QC/data/raw/all_sites_v2.5.3.23.csv", header=TRUE)


# filtering out rows per site
agincourt <- awigen[ which(awigen$site == 1),]
digkale <- awigen[ which(awigen$site == 2),]
nairobi <- awigen[ which(awigen$site == 3),]
nanoro <- awigen[ which(awigen$site == 4),]
navrongo <- awigen[ which(awigen$site == 5),]
soweto <- awigen[ which(awigen$site == 6),]

demographics_cat <- c(
  "site",
  "region_qc",
  "sex",
  "country_qc",
  "home_language",
  "ethnicity",
  "father_ethnicity",
  "father_language",
  "pat_gfather_ethnicity",
  "pat_gfather_language",
  "pat_gmother_language",
  "pat_gmother_ethnicity",
  "mother_ethnicity",
  "mother_language",
  "mat_gfather_ethnicity",
  "mat_gfather_language",
  "mat_gmother_ethnicity",
  "mat_gmother_language",
  "siblings",
  "children",
  "pregnant_qc",
  "regular_periods",
  "last_period_qc",
  "months_since_last_period_c_qc",
  "menopause_status_c_qc",
  "menopause_manual",
  "marital_status_qc",
  "partnership_status_c_qc",
  "highest_level_of_education_qc",
  "occupation_qc",
  "employment_status_c_qc",
  "electricity",
  "solar_energy",
  "power_generator",
  "alternative_power_source",
  "television",
  "radio",
  "motor_vehicle",
  "motorcycle",
  "bicycle",
  "refrigerator",
  "washing_machine",
  "sewing_machine",
  "telephone",
  "mobile_phone",
  "microwave",
  "dvd_player",
  "satellite_tv_or_dstv",
  "computer_or_laptop",
  "internet_by_computer",
  "internet_by_mobile_phone",
  "electric_iron",
  "fan",
  "electric_or_gas_stove",
  "kerosene_stove",
  "plate_gas",
  "electric_plate",
  "torch",
  "gas_lamp",
  "kerosene_lamp_with_glass",
  "toilet_facilities",
  "portable_water",
  "grinding_mill",
  "tableh",
  "sofa_set",
  "wall_clock",
  "bed",
  "mattress",
  "blankets",
  "cattle",
  "other_livestock",
  "poultry",
  "tractor",
  "plough",
  "ses_site_quintile_c_qc",
  "ses_quintile_qc")

demographics_num <- c("age",
                      'number_of_brothers',
                      'number_of_sisters',
                      'number_of_sons',
                      'number_of_daughters',
                      "number_of_pregnancies_qc",
                      "number_of_live_births_qc",
                      "months_last_period_c",
                      "years_highest_education_qc",
                      "household_size_qc",
                      "number_of_rooms_qc",
                      "number_of_bedrooms_qc",
                      "num_in_house_c_qc",
                      "people_to_rooms_density_c_qc",
                      "people_to_bedrooms_density_c_qc",
                      "household_density_c","ses_c_qc",
)

# get the crosstabs for demographics
demo_cat_freq <- function(df, column_list){
  for (cl in column_list) {
    print(cl)
    df_table <- table(df[, cl])
    print(df_table)

  }
}

demo_cat_freq(awigen, demographics_cat)

bb <- barplot(table(awigen$country_qc), width = 0.85, 
  main = "Sample Sizes of Various Fitness Traits", 
  ylab = "Frequency")
text(x = bb, y = dat$freqs, label = dat$freqs, pos = 3, cex = 0.8, col = "red")

ggplot(awigen, aes(x = country_qc, y = age, fill = sex)) + geom_col(position = "dodge")

