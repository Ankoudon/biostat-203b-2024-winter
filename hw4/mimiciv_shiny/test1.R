library(shiny)
library(tidyverse)

setwd("./")

mimic_icu_cohort <- readRDS("./mimic_icu_cohort.rds") 

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


ui <- fluidPage(

    titlePanel("203b-hw4-Shiny app"),
    
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("v1", "Variable1:", 
                  c("demographics" = "demographics",
                    "lab measurements" = "lab",
                    "vitals" = "vitals"), selected = "demographics"),
      
      selectInput("v2", "Variable2:",
                  c("race" = "race",
                    "insurance" = "insurance",
                    "marital status" = "marital_status",
                    "gender" = "gender",
                    "age at intime" = "age_at_intime"), selected = "race"),
    ),
    
    mainPanel(
      tableOutput("view"),
      plotOutput("plot")
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  data_gg <- function(v1, v2) {
    
    if (v1 == "demographics") {
      
      if (v2 == "age_at_intime") {
        
        mimic_icu_cohort_pre |>
          ggplot(aes(x = age_at_intime, y = los)) +
          geom_point()
        
      } else if (v2 != "None") {
        
        mimic_icu_cohort_pre |>
          ggplot(aes(x = !!sym(v2), y = los)) +
          geom_boxplot()
        
      }
      
    } else if (v1 == "lab") {
      
      if (v2 != "None") {
        
        mimic_icu_cohort_pre |>
        ggplot(aes(x = !!sym(v2), y = los)) +
        geom_point() 
        
        }
      
    } else if (v1 == "vitals") {
      
      if (v2 != "None") {
      
      mimic_icu_cohort_pre |>
        ggplot(aes(x = !!sym(v2), y = los)) +
        geom_point()
        
      }
      
    }
  }
  
  output$plot <- renderPlot({
    
    if (input$v1 == "None" | input$v2 == "None") {
      
      return()
      
    } else {
      data_gg(input$v1, input$v2)
    }

    
  })
  
  
  data_sum <- function(v1) {
  
  mimic_icu_cohort_pre |> group_by(!!sym(v1)) |>
  
  summarise(
  n = n(),
  mean_los = mean(los),
  median_los = median(los),
  max_los = max(los),
  min_los = min(los),
  sd_los = sd(los),
  iqr_los = IQR(los)
  )
    }
  
  
  
  #output$view <- renderTable({
  #data_sum(input$v1)
  #})
  
  observeEvent(input$v1, {
    
    if (input$v1 == "lab") {
      
      other_choices <- c("creatinine" = "creatinine",
                         "potassium" = "potassium",
                         "sodium" = "sodium",
                         "chloride" = "chloride", 
                         "bicarbonate" = "bicarbonate",
                         "hematocrit" = "hematocrit",
                         "wbc" = "wbc",
                         "glucose" = "glucose")
      
      updateSelectInput(session, "v2", choices = other_choices, selected = "creatinine")
      
    } else if (input$v1 == "demographics") {
      
      other_choices <-  c("race" = "race",
                          "insurance" = "insurance",
                          "marital status" = "marital_status",
                          "gender" = "gender",
                          "age at intime" = "age_at_intime")
        
      updateSelectInput(session, "v2", choices = other_choices, selected = "race")
      
    } else if (input$v1 == "vitals") {
      
      other_choices <-  c("NBPd" = "non_invasive_blood_pressure_diastolic",
                          "NBPs" = "non_invasive_blood_pressure_systolic",
                          "HR" = "heart_rate",
                          "Temperature F" = "temperature_fahrenheit",
                          "RR" = "respiratory_rate")
      
      updateSelectInput(session, "v2", choices = other_choices, selected = "non_invasive_blood_pressure_diastolic")
      
      }
    
    })
  
  
  
}

# Run the application 
  
  shinyApp(ui = ui, server = server)
