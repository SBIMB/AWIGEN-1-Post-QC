library(ggplot2)
#import data from imac
#awigen <- read.csv("~/Documents/Development/AWIGEN-1-Post-QC/data/raw/all_sites_v2.5.3.23.csv", header=TRUE)

# reading data from my pc
awigen <- read.csv("~/Development/AWIGEN-1-Post-QC/data/raw/all_sites_v2.5.3.23.csv", header=TRUE)

measurememts <- ("standing_height_qc",
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
                  "tested_hiv_qc",
                  "hiv_status_qc",
                  "hiv_positive_qc",
                  "hiv_medication_qc",
                  "traditional_med_hiv_qc",
                  "agree_hivtest",
                  "result_hiv_qc",
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
