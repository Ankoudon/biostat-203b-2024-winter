library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)
library(gridExtra)
library(gt)
library(gtsummary)



mimic_icu_cohort <- readRDS("./mimic_icu_cohort.rds") 

setwd("../")

# path to the service account token 
satoken <- "biostat-203b-2024-winter-313290ce47a6.json"
# BigQuery authentication using service account
bq_auth(path = satoken)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)

mimic_icu_cohort_pre <- mimic_icu_cohort |>
  mutate(
    first_careunit = fct_lump(first_careunit, n = 4),
    last_careunit = fct_lump(last_careunit, n = 4),
    admission_type = fct_lump(admission_type, n = 4),
    admission_location = fct_lump(admission_location, n = 3),
    discharge_location = fct_lump(discharge_location, n = 4),
    los_long = if_else(los >= 2, TRUE, FALSE),
    # Change later!
    race = if_else(
      str_detect(race, "ASIAN"), "ASIAN", if_else(
        str_detect(race, "WHITE"), "WHITE", if_else(
          str_detect(race, "BLACK"), "BLACK", if_else(
            str_detect(race, "HISPANIC"), "HISPANIC", "OTHER"))))) |>
  select(first_careunit, last_careunit, los, admission_type, admission_location,
         discharge_location,insurance, language, marital_status, 
         race, los_long, hospital_expire_flag, gender, dod, sodium, chloride, 
         creatinine, potassium, glucose, hematocrit, wbc, bicarbonate, 
         temperature_fahrenheit, non_invasive_blood_pressure_diastolic,
         respiratory_rate, non_invasive_blood_pressure_systolic, heart_rate,
         age_at_intime)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_table <- mimic_icu_cohort_pre %>%
  select(insurance, marital_status, race, gender, age_at_intime) %>%
  summarise_all(Mode) |> gt() |>
  tab_header(
    title = "Summary of Most Frequent Values",
    subtitle = "Calculated using Mode function"
  ) 

mean_table <- mimic_icu_cohort_pre %>%
  select(creatinine,
         potassium,
         sodium,
         chloride,
         bicarbonate,
         hematocrit,
         wbc,
         glucose) %>%
  
  summarise_all(mean) |> gt() |>
  tab_header(
    title = "Summary of Meam Values"
  ) 


patient_id = 10013310
icustays <- tbl(con_bq, "icustays") |>
  filter(subject_id == patient_id) |>
  select(hadm_id, stay_id) |>
  distinct() 


transfers <- tbl(con_bq, "d_items") |>
  filter(abbreviation == "NBPd" | abbreviation == "NBPs" |
           abbreviation == "HR" | abbreviation == "Temperature F" |
           abbreviation == "RR") |>
  select(itemid, abbreviation)

labevents <-tbl(con_bq, "chartevents") |>
  select(subject_id, itemid, charttime, hadm_id, valuenum) |>
  filter(subject_id == patient_id) |>
  semi_join(transfers, by = "itemid") |>
  left_join(icustays, by = c("hadm_id" = "hadm_id")) |>
  left_join(transfers, by = c("itemid" = "itemid")) |>
  collect() |>
  # Fix the timezone issue
  mutate(charttime = with_tz(charttime, "UTC")) |>
  arrange(stay_id, charttime)


patients <- tbl(con_bq, "patients") |>
  filter(subject_id %in% patient_id) |>
  collect()

patients <- tbl(con_bq, "patients") |>
  collect()

patient_id = 10013310
patient_race <- tbl(con_bq, "admissions") |>
  filter(subject_id  == patient_id) |>
  select(race) |>
  distinct() |>
  pull() |>
  tolower() 


patient_diagnoses <- tbl(con_bq, "diagnoses_icd") |>
  left_join( tbl(con_bq, "d_icd_diagnoses"),
             by = c("icd_code", "icd_version")) |>
  filter(subject_id %in% patient_id) |>
  arrange(hadm_id, seq_num) |>
  select(long_title, hadm_id) |>
  groupby(hadm_id) |>
  slice_min(n = 3)


patient_diagnoses <- tbl(con_bq, "diagnoses_icd") |>
  collect()

d_icd_diagnoses <- tbl(con_bq, "d_icd_diagnoses") |>
  collect()

patient_diagnoses <- patient_diagnoses |>
  left_join(tbl(con_bq, "d_icd_diagnoses"),
            by = c("icd_code", "icd_version")) 


d_icd_diagnoses <- tbl(con_bq, "d_icd_diagnoses") |>
  collect()

patient_diagnoses <- patient_diagnoses |>
  left_join(d_icd_diagnoses,
            by = c("icd_code", "icd_version")) |>
  filter(subject_id ==  patient_id) |>
  arrange(hadm_id, seq_num) |>
  select(long_title, hadm_id) 

diagnose_1 <- tolower(patient_diagnoses$long_title[1])
diagnose_2 <- tolower(patient_diagnoses$long_title[2])
diagnose_3 <- tolower(patient_diagnoses$long_title[3])


icustays <- tbl(con_bq, "icustays") |> 
  collect()

patient_id = 10013310
icustays <- icustays |>
  filter(subject_id == patient_id) |>
  select(hadm_id, stay_id) |>
  distinct() 

labevents <-tbl(con_bq, "chartevents") |>
  select(subject_id, itemid, charttime, hadm_id, valuenum) |>
  filter(subject_id == patient_id) |>
  semi_join(abb_item, by = "itemid") |>
  left_join(icustays, by = c("hadm_id" = "hadm_id")) |>
  left_join(abb_item, by = c("itemid" = "itemid")) |>
  collect() |>
  # Fix the timezone issue
  mutate(charttime = with_tz(charttime, "UTC")) |>
  arrange(stay_id, charttime)
