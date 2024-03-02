library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)

setwd("~/Desktop/ucla_assignment/Winter_2024/203B/203b-hw/hw4/mimiciv_shiny")

# path to the service account token 
satoken <- "../biostat-203b-2024-winter-313290ce47a6.json"
# BigQuery authentication using service account
bq_auth(path = satoken)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)

dbListTables(con_bq)

patient_id = 10013310

# Check this later
patients <- tbl(con_bq, "patients") |>
  filter(subject_id %in% patient_id) |>
  collect()

patient_gender = patients$gender[1]
patient_age = patients$anchor_age[1] 

patient_race <- tbl(con_bq, "admissions") |>
  filter(subject_id %in% patient_id) |>
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
  slice_head(n = 3) 

diagnose_1 <- tolower(patient_diagnoses$long_title[1])
diagnose_2 <- tolower(patient_diagnoses$long_title[2])
diagnose_3 <- tolower(patient_diagnoses$long_title[3])

transfers <- tbl(con_bq, "transfers") |>
  filter(subject_id %in% patient_id) |>
  select(careunit, intime, outtime) |>
  collect() |>
  # Fix the timezone issue
  mutate(intime = with_tz(intime, "UTC"),
         outtime = with_tz(outtime, "UTC"))

# Get the number of the distinct careunit
careunit_num <- transfers |>
  select(careunit) |>
  distinct() |>
  nrow()

adt <- transfers |>
  mutate(y_type = "ADT",
         line_size = if_else(
           # Detect ICU or CCU
           str_detect(careunit, "CCU") | str_detect(careunit, "ICU"),
           8, 3)) |>
  # Make the length of careunit less than 35
  mutate(careunit = str_sub(careunit, 1, 35)) |>
  na.omit() |>
  arrange(intime) 

labevents <- tbl(con_bq, "labevents") |>
  filter(subject_id %in% patient_id) |>
  select(charttime) |>
  collect() |>
  as_tibble() |>
  mutate(y_type = "Lab") |>
  # Fix the timezone issue
  mutate(charttime = with_tz(charttime, "UTC")) |>
  arrange(charttime)

procedures <- tbl(con_bq, "procedures_icd") |>
  filter(subject_id %in% patient_id) |>
  left_join(tbl(con_bq, "d_icd_procedures"),
            by = c("icd_code", "icd_version")) |>
  collect() |>
  mutate(y_type = "Procedure",
         # Fix the timezone issue
         chartdate = with_tz(chartdate, "UTC")) |>
  # Fix the date format
  mutate(chartdate = as.POSIXct(chartdate, format = "%Y-%m-%d %H:%M:%S")) |>
  # Make the length of long_title less than 49
  mutate(long_title = str_sub(long_title, 1, 49))

# Get the number of the distinctive procedures
long_title_num <- procedures |>
  select(long_title) |>
  distinct() |>
  nrow()

ggplot() +
  geom_point(data = labevents, aes(x = charttime, y = y_type),
             shape = 3, color = "black", size = 2) +
  geom_segment(data = adt,
               aes(x = intime, xend = outtime, y = y_type, yend = y_type,
                   color = careunit), linewidth = adt$line_size) +
  geom_point(data = procedures, aes(x = chartdate,
                                    y = y_type,
                                    shape = long_title), size = 3) +
  scale_shape_manual(values = seq(1, long_title_num)) +
  theme_bw() + 
  theme(legend.position = "bottom", legend.box = "vertical",
        axis.title.y = element_blank()) +
  guides(linewidth = "none",
         color = guide_legend(order = 2, title = "Care Unit"),
         shape = guide_legend(order = 1, title = "Procedure", ncol = 2)) +
  labs(title = paste0("Patient ",
                      patient_id, ", ",
                      patient_gender, ", ",
                      patient_age, " ",
                      "years old, ", patient_race),
       x = "Calendar Time",
       subtitle = paste0(diagnose_1, "\n", diagnose_2, "\n", diagnose_3)) +
  scale_y_discrete(limits = c("Procedure", "Lab", "ADT"))


