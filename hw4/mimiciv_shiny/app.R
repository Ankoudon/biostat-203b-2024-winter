# Written by Hiroyasu Ando
# UID: 605948443
# Date: 20240308

rm(list = ls())

library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)
library(gridExtra)
library(gt)
library(gtsummary)
library(shiny)

# Function to calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

mimic_icu_cohort <- readRDS("./mimic_icu_cohort.rds") 

setwd("../")

satoken <- "biostat-203b-2024-winter-313290ce47a6.json"
bq_auth(path = satoken)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter")

mimic_icu_cohort_pre <- mimic_icu_cohort |>
  mutate(
    first_careunit = fct_lump(first_careunit, n = 4),
    last_careunit = fct_lump(last_careunit, n = 4),
    admission_type = fct_lump(admission_type, n = 4),
    admission_location = fct_lump(admission_location, n = 3),
    discharge_location = fct_lump(discharge_location, n = 4),
    los_long = if_else(los >= 2, TRUE, FALSE),
    race = fct_collapse(
      race, 
      ASIAN = mimic_icu_cohort$race[str_detect(mimic_icu_cohort$race, "ASIAN")],
      WHITE = mimic_icu_cohort$race[str_detect(mimic_icu_cohort$race, "WHITE")],
      BLACK = mimic_icu_cohort$race[str_detect(mimic_icu_cohort$race, "BLACK")],
      HISPANIC = mimic_icu_cohort$race[str_detect(mimic_icu_cohort$race,
                                                  "HISPANIC")],
      other_level = "Other")) |>
  select(first_careunit, last_careunit, los, admission_type, admission_location,
         discharge_location,insurance, language, marital_status, 
         race, los_long, hospital_expire_flag, gender, dod, sodium, chloride, 
         creatinine, potassium, glucose, hematocrit, wbc, bicarbonate, 
         temperature_fahrenheit, non_invasive_blood_pressure_diastolic,
         respiratory_rate, non_invasive_blood_pressure_systolic, heart_rate,
         age_intime, subject_id) 

d_items <- tbl(con_bq, "d_items") |>
  filter(abbreviation == "NBPd" | abbreviation == "NBPs" |
           abbreviation == "HR" | abbreviation == "Temperature F" |
           abbreviation == "RR") |>
  select(itemid, abbreviation) |>
  collect()

patients_tbl <- tbl(con_bq, "patients") |>
  select(subject_id, anchor_age, gender) |>
  collect()
  
a <- mimic_icu_cohort_pre |>
  ggplot(aes(x = age_intime, y = los)) +
  geom_point() +
  labs(x = "Age at Intime", y = "Length of Stay at ICU",
       title = "Age at Intime") +
  theme(axis.text.x = element_text(size = 5.5))

b <- mimic_icu_cohort_pre |>
  ggplot(aes(x = insurance, y = los)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "insurance", y = "Length of Stay at ICU",
       title = "Insurance") +
  coord_cartesian(ylim = c(0, 11)) +
  theme(axis.text.x = element_text(size = 5.5))

c <- mimic_icu_cohort_pre |>
  ggplot(aes(x = race, y = los)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "race", y = "Length of Stay at ICU",
       title = "Race") +
  coord_cartesian(ylim = c(0, 11)) +
  theme(axis.text.x = element_text(size = 5.5))

d <- mimic_icu_cohort_pre |>
  ggplot(aes(x = gender, y = los)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "gender", y = "Length of Stay at ICU",
       title = "Gender") +
  coord_cartesian(ylim = c(0, 11)) +
  theme(axis.text.x = element_text(size = 5.5))

e <- mimic_icu_cohort_pre |>
  ggplot(aes(x = marital_status, y = los)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "marital_status", y = "Length of Stay at ICU",
       title = "Marital status") +
  coord_cartesian(ylim = c(0, 11)) +
  theme(axis.text.x = element_text(size = 5.5))

lab_los <- mimic_icu_cohort |>
  select(los,
         creatinine,
         potassium,
         sodium,
         chloride,
         bicarbonate,
         hematocrit,
         wbc,
         glucose) |>
  pivot_longer(cols = - los, names_to = "lab", values_to = "value") |>
  ggplot(aes(x = value, y = los)) +
  geom_point() +
  facet_wrap(~ lab, scales = "free") +
  labs(title = paste0(
    "Length of ICU stay vs ",
    "Last available lab measurements before ICU stay"),
    x = "Lab value",
    y = "Length of ICU stay")

vital_los <- mimic_icu_cohort |>
  select(los, heart_rate, non_invasive_blood_pressure_systolic,
         non_invasive_blood_pressure_diastolic, temperature_fahrenheit,
         respiratory_rate) |>
  rename("NBPd" = non_invasive_blood_pressure_diastolic,
         "NBPs" = non_invasive_blood_pressure_systolic,
         "HR" = heart_rate,
         "Temperature F" = temperature_fahrenheit,
         "RR" = respiratory_rate) |>
  pivot_longer(cols = - los, names_to = "vital",
               values_to = "value") |>
  ggplot(aes(x = value, y = los)) +
  geom_point() +
  facet_wrap(~ vital, scales = "free") +
  labs(title = paste0("Length of ICU stay vs ",
                      "First vital measurement within the ICU stay"),
       x = "Vital value",
       y = "Length of ICU stay")

ui <- fluidPage(
  
  titlePanel("203B-HW4-Shiny App"),
  
  tabsetPanel(
    tabPanel("Patient characteristics",
             sidebarPanel(
               selectInput("v_patient", "Summary",
                           c("demographics" = "demographics",
                             "lab measurements" = "lab",
                             "vitals" = "vitals"))),
             mainPanel(
               gt_output("tbl_patient"),
               plotOutput("plot_patient"))),
    tabPanel("Patient's ADT and ICU stay information",
             selectizeInput("numeric_input", "SUBJECT ID:",
                            choices = NULL,
                            selected = 10013310, multiple = FALSE),
             mainPanel(
               plotOutput("plot_adt"),
               plotOutput("plot_icu")))))

server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'numeric_input',
                       choices = mimic_icu_cohort_pre$subject_id, server = TRUE)
  
  v_1_gg <- function(type) {
    if (type == "demographics") {
      grid.arrange(a, b, c, d, e, ncol = 3)
      } else if (type == "lab") {
        lab_los
        } else if (type == "vitals") {
          vital_los}}
  
  v_1_tbl <- function(type) {
    
    if (type == "demographics") {
      
      mode_table <- mimic_icu_cohort_pre |>
        select(insurance,
               marital_status,
               race,
               gender,
               age_intime) |>
        summarise_all(Mode) |>
        gt() |>
        tab_header(title = "Summary of Most Frequent Values")
      
      } else if (type == "lab") {
        mean_table <- mimic_icu_cohort_pre |>
          select(creatinine,
                 potassium,
                 sodium,
                 chloride,
                 bicarbonate,
                 hematocrit,
                 wbc,
                 glucose) |>
          na.omit() |>
          summarise_all(~ round(mean(., na.rm = TRUE), digits = 1)) |>
          gt() |>
          tab_header(title = "Summary of Meam Values")
        
        } else if (type == "vitals") {
          mean_table <- mimic_icu_cohort_pre |>
            select(
              heart_rate, non_invasive_blood_pressure_systolic,
              non_invasive_blood_pressure_diastolic, temperature_fahrenheit,
              respiratory_rate) |>
            rename("NBPd" = non_invasive_blood_pressure_diastolic,
                   "NBPs" = non_invasive_blood_pressure_systolic,
                   "HR" = heart_rate,
                   "Temperature F" = temperature_fahrenheit,
                   "RR" = respiratory_rate) |>
            na.omit() |>
            summarise_all(~ round(median(., na.rm = TRUE), digits = 1)) |>
            gt() |>
            tab_header(
              title = "Summary of Median Values")}}
  
  output$plot_patient <- renderPlot({
    v_1_gg(input$v_patient)})
  
  output$tbl_patient <- render_gt({
    v_1_tbl(input$v_patient)})
  
  gg_adt <- function(sub_id) {
    patient_id = sub_id
    if (patient_id %in% patients_tbl$subject_id) {
      patients <- patients_tbl |>
        filter(subject_id == patient_id)
      
      patient_gender = patients$gender[1]
      patient_age = patients$anchor_age[1]
      
      patient_race <- tbl(con_bq, "admissions") |>
        filter(subject_id == patient_id) |>
        select(race) |>
        distinct() |>
        collect() |>
        pull() |>
        tolower()
      
      patient_diagnoses <- tbl(con_bq, "diagnoses_icd") |>
        left_join(tbl(con_bq, "d_icd_diagnoses"),
                  by = c("icd_code", "icd_version")) |>
        filter(subject_id == patient_id) |>
        arrange(hadm_id, seq_num) |>
        select(long_title, hadm_id) |>
        collect() |>
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
        mutate(chartdate = as.POSIXct(chartdate,
                                      format = "%Y-%m-%d %H:%M:%S")) |>
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
             subtitle = paste0(
               diagnose_1, "\n", diagnose_2, "\n", diagnose_3)) +
        scale_y_discrete(limits = c("Procedure", "Lab", "ADT"))
      
      } else {
        ggplot() +
          geom_blank() +
          theme_bw() +
          labs(title = "NULL")}}
  
  output$plot_adt <- renderPlot({
    gg_adt(as.integer(input$numeric_input))})
  
  gg_icu <- function(patient_id) {
    
    #if (patient_id %in% patients_tbl$subject_id & nrow(icustays) > 0) {
    if (patient_id %in% patients_tbl$subject_id) {
      
      icustays <- tbl(con_bq, "icustays") |>
        filter(subject_id == patient_id) |>
        select(hadm_id, stay_id) |>
        collect() |>
        distinct()
      
      labevents <- tbl(con_bq, "chartevents") |>
        select(subject_id, itemid, charttime, hadm_id, valuenum) |>
        filter(subject_id == patient_id) |>
        collect() |>
        semi_join(d_items, by = "itemid") |>
        left_join(icustays, by = c("hadm_id" = "hadm_id")) |>
        left_join(d_items, by = c("itemid" = "itemid")) |>
        # Fix the timezone issue
        mutate(charttime = with_tz(charttime, "UTC")) |>
        arrange(stay_id, charttime)
      
      ggplot() +
        geom_line(data = labevents,
                  mapping = aes(x = charttime, y = valuenum,
                                color = as.factor(abbreviation))) +
        geom_point(data = labevents,
                   mapping = aes(x = charttime, y = valuenum,
                                 color = as.factor(abbreviation)), size = 1) +
        facet_grid(abbreviation ~ stay_id, scales = "free") +
        theme_light() +
        guides(color = "none",
               x = guide_axis(n.dodge = 2)) +
        theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
        scale_x_datetime(labels = scales::date_format("%b %d %H:%M")) +
        labs(title = paste0("Patient ", patient_id, " ICU stays - Vitals"))
      
      } else {
        ggplot() +
          geom_blank() +
          theme_bw() +
          labs(title = "NULL")}}
  
  output$plot_icu <- renderPlot({
    gg_icu(as.integer(input$numeric_input))})}

# Run the application 

shinyApp(ui = ui, server = server)

