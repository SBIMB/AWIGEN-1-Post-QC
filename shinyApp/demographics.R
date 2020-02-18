library(hash)

# numerics
demographics_cat_cols <- c("cohort_id_c",
                           "site",
                           "region_qc",
                           "enrolment_date",
                           "sex",
                           "country_c",
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
                           "siblings_qc",
                           "children_qc",
                           "pregnant_qc",
                           "regular_periods",
                           "last_period_qc",
                           "months_since_last_period_c_qc",
                           "months_last_period_c",
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
                           "ses_c_qc",
                           "ses_site_quintile_c_qc",
                           "ses_quintile_qc")


# categoricals
demographics_num_cols <- c("number_of_brothers_qc",
                           "number_of_sisters_qc",
                           "number_of_sons_qc",
                           "number_of_daughters_qc",
                           "number_of_pregnancies_qc",
                           "number_of_live_births_qc",
                           "years_highest_education_qc",
                           "age",
                           "household_size_qc",
                           "number_of_rooms_qc",
                           "number_of_bedrooms_qc",
                           "num_in_house_c_qc",
                           "people_to_rooms_density_c_qc",
                           "people_to_bedrooms_density_c_qc",
                           "household_density_c")

# codebook
demographics_hash <- hash()

demographics_hash[["site"]] = list("1"= "Agincourt",
                                   "2" = "Dikgale",
                                   "3" = "Nairobi",
                                   "4" = "Nanoro",
                                   "5" = "Navrongo",
                                   "6" = "SOWETO")

demographics_hash[["sex"]] = list("0"= "Female", "1" = "Male")
demographics_hash[["siblings_qc"]] = list("0"="Yes", "1"="No", "-999"="Missing")
demographics_hash[["children_qc"]] = list("0"="Yes", "1"="No", "-999"="Missing")


ls <- list("1"="Yes", "0"="No", "-999"="Missing", "-555"="Not applicable")
demographics_hash[["pregnant_qc"]] = ls
demographics_hash[["regular_periods"]] = ls


demographics_hash[["menopause_status_c_qc"]] = c("1-Pre-menopausal",
                                            "2-Peri-menopausal",
                                            "3-Post-menopausal",
                                            "4-No answer or missing data",
                                            "5-Cannot Stage",
                                            "6-Response Invalid",
                                            "-999-Missing",
                                            "-555-Not Applicable")

demographics_hash[["menopause_manual"]] = c("1-Pre-menopausal",
                                                 "2-Peri-menopausal",
                                                 "3-Post-menopausal",
                                                 "4-No answer or missing data",
                                                 "5-Cannot Stage",
                                                 "6-Response Invalid")

demographics_hash[["marital_status_qc"]] = c("1-Married",
                                              "2-Living Together",
                                              "3-Never married or co-habited",
                                              "4-Divorced with living partner",
                                              "5-Separated with living partner",
                                              "6-partner deceased",
                                              "-999-Missing")

demographics_hash[["partnership_status_c_qc"]] = c("0-Never married or co-habited",
                                              "1-Currently Married or living together",
                                              "2-Once married or co-habited",
                                              "-999-Missing")

demographics_hash[["highest_level_of_education_qc"]] = c("0 -No formal education",
                                                    "1 - primary",
                                                    "2 - secondary",
                                                    "3 - tertiary",
                                                    "-999 - Missing")

demographics_hash[["occupation_qc"]] = c("0 - self-employed",
                                          "1 - formal full-time",
                                          "2 - part-time",
                                          "3 - informal",
                                          "4 - unemployed",
                                          "-999 - Missing")

demographics_hash[["employment_status_c_qc"]] = c("0 - Not Employed",
                                                  "1 - Employed",
                                                  "-999 - Missing")
household <- c("1-Yes",
              "0-No",
              "2-Don't Know",
              "Blank-not answered")

demographics_hash[["electricity"]] = household
demographics_hash[["solar_energy"]] = household
demographics_hash[["power_generator"]] = household
demographics_hash[["alternative_power_source"]] = household
demographics_hash[["television"]] = household
demographics_hash[["radio"]] = household
demographics_hash[["motor_vehicle"]] = household
demographics_hash[["motorcycle"]] = household
demographics_hash[["bicycle"]] = household
demographics_hash[["refrigerator"]]
demographics_hash[["washing_machine"]] = household
demographics_hash[["sewing_machine"]] = household
demographics_hash[["telephone"]] = household
demographics_hash[["mobile_phone"]] = household
demographics_hash[["microwave"]] = household
demographics_hash[["dvd_player"]] = household
demographics_hash[["satellite_tv_or_dstv"]] = household
demographics_hash[["computer_or_laptop"]] = household
demographics_hash[["internet_by_computer"]] = household
demographics_hash[["internet_by_mobile_phone"]] = household
demographics_hash[["electric_iron"]] = household
demographics_hash[["fan"]] = household
demographics_hash[["electric_or_gas_stove"]] = household
demographics_hash[["kerosene_stove"]] = household
demographics_hash[["plate_gas"]] = household
demographics_hash[["electric_plate"]] = household
demographics_hash[["torch"]] = household
demographics_hash[["gas_lamp"]] = household
demographics_hash[["kerosene_lamp_with_glass"]] = household
demographics_hash[["toilet_facilities"]] = household
demographics_hash[["portable_water"]] = household
demographics_hash[["grinding_mill"]] = household
demographics_hash[["tableh"]] = household
demographics_hash[["sofa_set"]] = household
demographics_hash[["wall_clock"]] = household
demographics_hash[["bed"]] = household
demographics_hash[["mattress"]] = household
demographics_hash[["blankets"]] = household
demographics_hash[["cattle"]] = household
demographics_hash[["other_livestock"]] = household
demographics_hash[["poultry"]] = household
demographics_hash[["tractor"]] = household
demographics_hash[["plough"]] = household


language = c("1	Afrikaans",
"2-English",
"3-isiNdebele",
"4-isiXhosa",
"5-isiZulu",
"6-Sesotho",
"7-Sepedi",
"8-Setswana",
"9-siSwati",
"10-Tshivenda",
"11-Xitsonga",
"12-Shona",
"14-Embu",
"15-Kalenjin",
"16-Kamba",
"17-Kikuyu",
"18-Kisii",
"19-Luhya",
"20-Luo",
"21-Maasai",
"22-Meru",
"23-Mijikenda",
"24-Somali",
"25-Swahili",
"26-Taita",
"27-Taveta",
"28-Nankam",
"29-Kassem",
"30-Buli",
"31-Sisali",
"32-Kusal",
"33-Gruni",
"34-Dagbani",
"35-Dagaare",
"36-Mamprusi",
"37-Moore",
"38-Gourounsi",
"39-Fulani",
"40-Gourmatchema",
"41-Dioula",
"42-Bissa",
"43-Dagara",
"44-Francais",
"98-Other",
"99-Unknown",
"10-Missing")
demographics_hash[["home_language"]] = language
demographics_hash[["father_language"]] = language
demographics_hash[["pat_gfather_language"]] = language
demographics_hash[["pat_gmother_language"]] = language
demographics_hash[["mother_language"]] = language
demographics_hash[["mat_gfather_language"]] = language
demographics_hash[["mat_gmother_language"]] = language


ethnicities <- c("1	Zulu",
"2-Xhosa",
"3-Ndebele",
"4-Sotho",
"5-Venda",
"6-Tsonga",
"7-Tswana",
"8-BaPedi",
"9-Zimbabwean",
"10-Embu",
"11-Kalenjin",
"12-Kamba",
"13-Kikuyu",
"14-Kisii",
"15-Luhya",
"16-Luo",
"17-Maasai",
"18-Meru",
"19-Mijikenda",
"20-Somali",
"21-Swahili",
"22-Taita",
"23-Taveta",
"24-Kassena",
"25-Nankana",
"26-Bulsa",
"27-Dagaati",
"28-Sisala",
"29-Dagomba",
"30-Kusasi",
"31-Mampruga",
"32-Frafra",
"33-Mossi",
"34-Gourounsi",
"35-Peulh",
"36-Gourmatche",
"37-Dioula",
"38-Bissa",
"39-Dagara",
"40-Swati",
"99-Unknown",
"98-Other",
"100-Missing")
demographics_hash[["ethnicity"]] = ethnicities
demographics_hash[["father_ethnicity"]] = ethnicities
demographics_hash[["pat_gfather_ethnicity"]] = ethnicities
demographics_hash[["pat_gmother_ethnicity"]] = ethnicities
demographics_hash[["mother_ethnicity"]] = ethnicities
demographics_hash[["mat_gfather_ethnicity"]] = ethnicities
demographics_hash[["mat_gmother_ethnicity"]] = ethnicities
