library(hash)

# choose dataset
site_data <- c("All", "Agincourt", "Digkale", "Nairobi", "Nanoro", "Navrongo", "Soweto")


# choose numeric variable
measurements_num_cols <- c("standing_height_qc",
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

# choose categorical variable
measurements_cat_cols <- c("tested_hiv_qc",
                            "hiv_status_qc",
                            "hiv_positive_qc",
                            "hiv_medication_qc",
                            "traditional_med_hiv_qc",
                            "agree_hivtest",
                            "result_hiv_qc",
                            "hiv_final_status_c")


# choose groupby attribute
group_by <- c("sex",
            "country",
            "site")


# refer to the codebook labels
measurements_hash <- hash()
ls_1 <- list("1" = "Yes", "0" = "No", "2" = "Don't know",
        "3" = "Refuse to answer", "-999" = "Missing")

ls_2 <- list("1" = "Yes", "0" = "No", "2" = "Don't know",
             "3" = "Refuse to answer", "-999" = "Missing", "-555"="Not Applicable")

ls_3 <- list("1" = "Yes", "0" = "No", "2" = "NA", "-999" = "Missing")

measurements_hash[["tested_hiv_qc"]] = ls_1
measurements_hash[["hiv_status_qc"]] = ls_2
measurements_hash[["hiv_positive_qc"]] = ls_2
measurements_hash[["hiv_medication_qc"]] = ls_2
measurements_hash[["traditional_med_hiv_qc"]] = ls_2
measurements_hash[["agree_hivtest"]] = list("1" = "Yes", "0" = "No")
measurements_hash[["result_hiv_qc"]] = ls_3
measurements_hash[["hiv_final_status_c"]] = ls_3

