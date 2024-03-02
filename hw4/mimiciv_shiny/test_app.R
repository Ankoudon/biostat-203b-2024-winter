library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)


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
# Fix age_at_intime


ui <- fluidPage(
  
  titlePanel("203b-hw4-Shiny app"),
  
  tabsetPanel(
    
    tabPanel("Tab 1",
             
             selectInput("demo", "DEMOGRAPHICS:", 
                         c("race" = "race",
                           "insurance" = "insurance",
                           "marital status" = "marital_status",
                           "gender" = "gender",
                           "age at intime" = "age_at_intime"), selected = "None"),
             
             selectInput("lab", "LAB MEASUREMENTS:",
                         c("creatinine" = "creatinine",
                           "potassium" = "potassium",
                           "sodium" = "sodium",
                           "chloride" = "chloride", 
                           "bicarbonate" = "bicarbonate",
                           "hematocrit" = "hematocrit",
                           "wbc" = "wbc",
                           "glucose" = "glucose")),
             
             selectInput("vitals", "VITALS:",
                         c("NBPd" = "non_invasive_blood_pressure_diastolic",
                           "NBPs" = "non_invasive_blood_pressure_systolic",
                           "HR" = "heart_rate",
                           "Temperature F" = "temperature_fahrenheit",
                           "RR" = "respiratory_rate")),
             
             mainPanel(
               
               plotOutput("plot_demo"),
               plotOutput("plot_lab"),
               plotOutput("plot_vitals")
               
             )
    ),
    
    tabPanel("Tab 2",
             
             numericInput("numeric_input", "Enter SUBJECT ID:", value = 10013310),
    
    mainPanel(
      
      plotOutput("plot_adt")
      
    ))
    
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  demo_gg <- function(demo_type) {
    
    if (demo_type == "age_at_intime") {
      mimic_icu_cohort_pre |>
        ggplot(aes(x = age_at_intime, y = los)) +
        geom_point() +
        labs(x = "Age at Intime", y = "Length of Stay at ICU",
             title = "DEMOGRAPHICS: Age at Intime")
    
    } else {
      
      mimic_icu_cohort_pre |>
        ggplot(aes(x = !!sym(demo_type), y = los)) +
        geom_boxplot() +
        labs(x = demo_type, y = "Length of Stay at ICU",
             title = paste("DEMOGRAPHICS: ", toupper(demo_type)))
        
    }}
  
  output$plot_demo <- renderPlot({
    
    demo_gg(input$demo)
    
  })
  
  lab_gg <- function(lab_type) {
    
    mimic_icu_cohort_pre |>
      
      ggplot(aes(x = !!sym(lab_type), y = los)) +
      geom_point() +
      labs(x = lab_type, y = "Length of Stay at ICU",
           title = paste("LAB MEASUREMENTS: ", toupper(lab_type)))
    
    } 
  
  output$plot_lab <- renderPlot({
    lab_gg(input$lab)
  })
  
  vitals_gg <- function(vitals_type) {
    
    mimic_icu_cohort_pre |>
      
      ggplot(aes(x = !!sym(vitals_type), y = los)) +
      geom_point() +
      labs(x = vitals_type, y = "Length of Stay at ICU",
           title = paste("VITALS: ", toupper(vitals_type)))
    
  } 
  
  output$plot_vitals <- renderPlot({
    lab_gg(input$vitals)
    
  })
  
  gg_adt <- function(sub_id) {
    
    patient_id = sub_id
    
    # Check this later
    patients <- tbl(con_bq, "patients") |>
      filter(subject_id %in% patient_id) |>
      collect()
    
    patient_gender = patients$gender[1]
    patient_age = patients$anchor_age[1] 
    
    patient_race <- tbl(con_bq, "admissions") |>
      filter(subject_id %in% patient_id) |>
      select(race) |>
      collect() |>
      distinct() |>
      pull() |>
      tolower() 
    
    patient_diagnoses <- tbl(con_bq, "diagnoses_icd") |>
      left_join( tbl(con_bq, "d_icd_diagnoses"),
                 by = c("icd_code", "icd_version")) |>
      filter(subject_id %in% patient_id) |>
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
    
  }
  
  output$plot_adt <- renderPlot({
    
    gg_adt(input$numeric_input)
    
  })
  
} 

# Run the application 

shinyApp(ui = ui, server = server)
