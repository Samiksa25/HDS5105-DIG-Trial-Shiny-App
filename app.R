
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(survival)

# -----------------------------
# 1. LOAD DATA
# -----------------------------

DIG <- readRDS("Data/DIG_clean.rds")

# -----------------------------
# 2. DASHBOARD DESIGN
# -----------------------------

ui <- dashboardPage(
  
  skin = "purple",
  
  dashboardHeader(
    title = "DIG Trial Explorer",
    titleWidth = 280
  ),
  
  dashboardSidebar(
    width = 280,
    
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Patients", tabName = "patients", icon = icon("users")),
      menuItem("Did it help?", tabName = "outcomes", icon = icon("chart-bar")),
      menuItem("Who benefited?", tabName = "benefits", icon = icon("user-check"))
    ),
    
    hr(),
    
    h4("Patient Filters", style = "padding-left: 15px;"),
    
    selectInput(
      "treatment",
      "Treatment group",
      choices = c("All Patients", "Placebo", "Digoxin"),
      selected = "All Patients"
    ),
    
    selectInput(
      "sex",
      "Sex",
      choices = c("All", "Male", "Female"),
      selected = "All"
    ),
    
    selectInput(
      "race",
      "Race code",
      choices = c("All", sort(unique(DIG$RACE[!is.na(DIG$RACE)]))),
      selected = "All"
    ),
    
    sliderInput(
      "age",
      "Patient age range",
      min = floor(min(DIG$AGE, na.rm = TRUE)),
      max = ceiling(max(DIG$AGE, na.rm = TRUE)),
      value = c(
        floor(min(DIG$AGE, na.rm = TRUE)),
        ceiling(max(DIG$AGE, na.rm = TRUE))
      )
    ),
    
    hr(),
    
    selectInput(
      "view_mode",
      "Viewing mode",
      choices = c("Simple", "Expert"),
      selected = "Simple"
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f5f3fa;
        }
        .box {
          border-radius: 8px;
        }
        .main-header .logo {
          font-weight: bold;
        }
      "))
    ),
    
    tabItems(
      
      # -----------------------------
      # OVERVIEW
      # -----------------------------
      
      tabItem(
        tabName = "overview",
        
        h2("DIG Trial – Clinical Overview"),
        
        p("Explore patient characteristics and treatment outcomes from the DIG clinical trial."),
        
        fluidRow(
          valueBoxOutput("total_patients", width = 3),
          valueBoxOutput("placebo_patients", width = 3),
          valueBoxOutput("digoxin_patients", width = 3),
          valueBoxOutput("death_patients", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Patient Age Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("age_overview")
          ),
          
          box(
            title = "Patients by Treatment",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("treatment_overview")
          )
        )
      ),
      
      # -----------------------------
      # PATIENT DEMOGRAPHICS
      # -----------------------------
      
      tabItem(
        tabName = "patients",
        
        h2("Patient Demographics"),
        
        fluidRow(
          box(
            title = "Age Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("age_plot")
          ),
          
          box(
            title = "Sex Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("sex_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Race Code Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("race_plot")
          ),
          
          box(
            title = "Patient Data",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("patient_table")
          )
        )
      ),
      
      # -----------------------------
      # TREATMENT OUTCOMES
      # -----------------------------
      
      tabItem(
        tabName = "outcomes",
        
        h2("Did the Treatment Help?"),
        
        fluidRow(
          box(
            title = "Death by Treatment Group",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("death_plot")
          ),
          
          box(
            title = "Hospitalisation by Treatment Group",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("hospital_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Outcome Summary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("outcome_table")
          )
        )
      ),
      
      # -----------------------------
      # WHO BENEFITED
      # -----------------------------
      
      tabItem(
        tabName = "benefits",
        
        h2("Who Benefited from Treatment?"),
        
        fluidRow(
          box(
            title = "Death Rate by Age Group and Treatment",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("age_death_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Clinical Characteristics",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("clinical_table")
          )
        )
      )
    )
  )
)

# -----------------------------
# 3. SERVER
# -----------------------------

server <- function(input, output, session) {
  
  # Apply filters to the data
  
  filtered_data <- reactive({
    
    data <- DIG
    
    if (input$treatment != "All Patients") {
      data <- data %>%
        filter(TRTMT_label == input$treatment)
    }
    
    if (input$sex != "All") {
      data <- data %>%
        filter(SEX_label == input$sex)
    }
    
    if (input$race != "All") {
      data <- data %>%
        filter(RACE == as.numeric(input$race))
    }
    
    data <- data %>%
      filter(
        AGE >= input$age[1],
        AGE <= input$age[2]
      )
    
    data
  })
  
  # -----------------------------
  # OVERVIEW CARDS
  # -----------------------------
  
  output$total_patients <- renderValueBox({
    valueBox(
      nrow(filtered_data()),
      "Total Patients",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$placebo_patients <- renderValueBox({
    valueBox(
      sum(filtered_data()$TRTMT_label == "Placebo", na.rm = TRUE),
      "Placebo Patients",
      icon = icon("user"),
      color = "purple"
    )
  })
  
  output$digoxin_patients <- renderValueBox({
    valueBox(
      sum(filtered_data()$TRTMT_label == "Digoxin", na.rm = TRUE),
      "Digoxin Patients",
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$death_patients <- renderValueBox({
    valueBox(
      sum(filtered_data()$DEATH == 1, na.rm = TRUE),
      "Deaths",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  # -----------------------------
  # OVERVIEW CHARTS
  # -----------------------------
  
  output$age_overview <- renderPlot({
    
    ggplot(filtered_data(), aes(x = AGE)) +
      geom_histogram(
        bins = 25,
        fill = "#7651A8",
        color = "white"
      ) +
      labs(x = "Age (years)", y = "Number of patients") +
      theme_minimal()
  })
  
  output$treatment_overview <- renderPlot({
    
    ggplot(
      filtered_data(),
      aes(x = TRTMT_label, fill = TRTMT_label)
    ) +
      geom_bar() +
      scale_fill_manual(
        values = c("Placebo" = "#7651A8", "Digoxin" = "#45A77B")
      ) +
      labs(x = "Treatment", y = "Number of patients") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # -----------------------------
  # PATIENT DEMOGRAPHICS
  # -----------------------------
  
  output$age_plot <- renderPlot({
    
    ggplot(filtered_data(), aes(x = AGE)) +
      geom_histogram(
        bins = 25,
        fill = "#7651A8",
        color = "white"
      ) +
      labs(x = "Age (years)", y = "Patients") +
      theme_minimal()
  })
  
  output$sex_plot <- renderPlot({
    
    ggplot(filtered_data(), aes(x = SEX_label, fill = SEX_label)) +
      geom_bar() +
      scale_fill_manual(
        values = c("Male" = "#7651A8", "Female" = "#45A77B")
      ) +
      labs(x = "Sex", y = "Patients") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$race_plot <- renderPlot({
    
    ggplot(
      filtered_data(),
      aes(x = factor(RACE), fill = factor(RACE))
    ) +
      geom_bar() +
      labs(x = "Race code", y = "Patients", fill = "Race code") +
      theme_minimal()
  })
  
  output$patient_table <- renderDT({
    
    datatable(
      filtered_data() %>%
        select(ID, AGE, SEX, RACE, TRTMT_label, BMI, SYSBP, DIABP),
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  # -----------------------------
  # TREATMENT OUTCOMES
  # -----------------------------
  
  output$death_plot <- renderPlot({
    
    plot_data <- filtered_data() %>%
      group_by(TRTMT_label) %>%
      summarise(
        Death_Rate = mean(DEATH == 1, na.rm = TRUE) * 100,
        .groups = "drop"
      )
    
    ggplot(
      plot_data,
      aes(x = TRTMT_label, y = Death_Rate, fill = TRTMT_label)
    ) +
      geom_col() +
      scale_fill_manual(
        values = c("Placebo" = "#7651A8", "Digoxin" = "#45A77B")
      ) +
      labs(x = "Treatment", y = "Deaths (%)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$hospital_plot <- renderPlot({
    
    plot_data <- filtered_data() %>%
      group_by(TRTMT_label) %>%
      summarise(
        Hospitalisation_Rate = mean(HOSP == 1, na.rm = TRUE) * 100,
        .groups = "drop"
      )
    
    ggplot(
      plot_data,
      aes(x = TRTMT_label, y = Hospitalisation_Rate, fill = TRTMT_label)
    ) +
      geom_col() +
      scale_fill_manual(
        values = c("Placebo" = "#7651A8", "Digoxin" = "#45A77B")
      ) +
      labs(x = "Treatment", y = "Hospitalisation (%)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$outcome_table <- renderDT({
    
    result <- filtered_data() %>%
      group_by(TRTMT_label) %>%
      summarise(
        Patients = n(),
        Deaths = sum(DEATH == 1, na.rm = TRUE),
        Death_Rate = round(mean(DEATH == 1, na.rm = TRUE) * 100, 2),
        Hospitalisations = sum(HOSP == 1, na.rm = TRUE),
        Hospitalisation_Rate = round(mean(HOSP == 1, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      )
    
    datatable(result, options = list(dom = "t", scrollX = TRUE))
  })
  
  # -----------------------------
  # AGE GROUP COMPARISON
  # -----------------------------
  
  output$age_death_plot <- renderPlot({
    
    plot_data <- filtered_data() %>%
      mutate(
        Age_Group = cut(
          AGE,
          breaks = c(0, 50, 60, 70, 80, Inf),
          labels = c("Under 50", "50-59", "60-69", "70-79", "80+"),
          right = FALSE
        )
      ) %>%
      group_by(Age_Group, TRTMT_label) %>%
      summarise(
        Death_Rate = mean(DEATH == 1, na.rm = TRUE) * 100,
        .groups = "drop"
      )
    
    ggplot(
      plot_data,
      aes(
        x = Age_Group,
        y = Death_Rate,
        fill = TRTMT_label
      )
    ) +
      geom_col(position = "dodge") +
      scale_fill_manual(
        values = c("Placebo" = "#7651A8", "Digoxin" = "#45A77B")
      ) +
      labs(
        x = "Age group",
        y = "Deaths (%)",
        fill = "Treatment"
      ) +
      theme_minimal()
  })
  
  # -----------------------------
  # CLINICAL CHARACTERISTICS
  # -----------------------------
  
  output$clinical_table <- renderDT({
    
    result <- filtered_data() %>%
      group_by(TRTMT_label) %>%
      summarise(
        Patients = n(),
        Average_Age = round(mean(AGE, na.rm = TRUE), 1),
        Average_BMI = round(mean(BMI, na.rm = TRUE), 1),
        Average_Systolic_BP = round(mean(SYSBP, na.rm = TRUE), 1),
        Average_Diastolic_BP = round(mean(DIABP, na.rm = TRUE), 1),
        Average_Heart_Rate = round(mean(HEARTRTE, na.rm = TRUE), 1),
        .groups = "drop"
      )
    
    datatable(result, options = list(dom = "t", scrollX = TRUE))
  })
}

# -----------------------------
# 4. RUN APPLICATION
# -----------------------------

shinyApp(ui = ui, server = server)
