library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
dig <- read.csv("Data/DIG-1.csv")
dig$group <- factor(dig$TRTMT, levels = c(0, 1), labels = c("Placebo", "Digoxin"))


ui <- page_navbar(
  nav_panel(title = "Overview", p("First page content.")),
  nav_panel(title = "Patients", selectInput(inputId = "patient_chart", label = "What would you like to see?", choices = c("Age", "BMI", "Heart rate", "Blood pressure", "Race", "Medical history")), plotOutput(outputId = "age_plot")),
  nav_panel(title = "Did it help?", p("Third page content.")),
  nav_panel(title = "Who benefited?", p("Fourth page content.")),
  title = "DIG Trial explorer",
  sidebar = sidebar(
    checkboxGroupInput(inputId = "sex" , label = "Sex", choices = c("Male" = 1, "Female" = 2), selected = c(1, 2)),
    checkboxGroupInput(inputId = "race", label = "Race", choices = c("White" = 1, "Non-white" = 2), selected = c(1, 2)),
    sliderInput(
      inputId = "age",
      label = "Age",
      min = 21,
      max = 90,
      value = c(21, 90)
    )
  )

)
server <- function(input, output) {
  output$age_plot <- renderPlot({
    filtered <- dig %>% filter(SEX %in% input$sex, RACE %in% input$race, AGE >= input$age[1], AGE <= input$age[2])
    if (input$patient_chart == "Age") {
      Age <- filtered %>%
        ggplot( aes(x=AGE, fill=group)) +
        geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 5) +
        scale_fill_manual(values=c("#6e03ab", "#0ac219")) +
        theme_ipsum() +
        coord_cartesian(xlim = c(20, 95)) +
        labs(title = "How old were the patients?",
           x = "Age (years)",
           y = "Number of patients",
           fill = "")
       Age
    } else if (input$patient_chart == "BMI"){
      Bmi <- filtered %>%
        ggplot( aes(x=BMI, fill=group)) +
        geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 2, na.rm = TRUE) +
        scale_fill_manual(values=c("#6e03ab", "#0ac219")) +
        theme_ipsum() +
        coord_cartesian(xlim = c(10, 60)) +
        labs(title = "Body weight of the patients",
             x = "BMI (kg/m²)",
             y = "Number of patients",
             fill = "")
      Bmi
    } else if (input$patient_chart == "Heart rate"){ 
      HR <- filtered %>%
        ggplot( aes(x=HEARTRTE, fill=group)) +
        geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 5, na.rm = TRUE) +
        scale_fill_manual(values=c("#6e03ab", "#0ac219")) +
        theme_ipsum() +
        coord_cartesian(xlim = c(30, 160)) +
        labs(title = "What were the patients' heart rates?",
             x = "Heart rate (beats/min)",
             y = "Number of patients",
             fill = "")
      HR
    }else if (input$patient_chart == "Blood pressure"){
      BP <- ggplot(filtered) +
        geom_histogram( aes(x = SYSBP, y = after_stat(count), fill= group), color="#e9ecef",alpha = 0.6, position = 'identity', binwidth = 10, na.rm = TRUE) +
        geom_histogram( aes(x = DIABP, y = -after_stat(count), fill= group), color="#e9ecef", alpha = 0.6, position = 'identity', binwidth = 10, na.rm = TRUE) +
        theme_ipsum() +
        labs(title = "Blood pressure was almost identical in both groups",
             x = "Blood pressure(mmHg)",
             y = "Number of patients",
             fill = "") +
        scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
        annotate("label", x = 190, y = 300, label = "Systolic") +
        annotate("label", x = 40, y = -300, label = "Diastolic") +
        scale_y_continuous(labels = abs)
      BP
    }else if (input$patient_chart == "Race"){
      filtered$race <- factor(filtered$RACE, levels = c(1, 2), labels = c("White", "Non-white"))
      Race <- ggplot(filtered, aes(x = race, fill = group)) +
        geom_bar(position = "dodge", color = "#e9ecef", width = 0.4) +
        coord_flip() +
        scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
        theme_ipsum() + theme(legend.position = "bottom") +
        guides(fill = guide_legend(reverse = TRUE)) +
        labs(title = "Ethnic background of the patients",
             x = "",
             y = "Number of patients",
             fill = "")
      Race
    }else if (input$patient_chart == "Medical history"){
      history <- filtered %>%
        group_by(group) %>%
        summarise(
          `Prior heart attack`  = mean(PREVMI,   na.rm = TRUE) * 100,
          `Angina`              = mean(ANGINA,   na.rm = TRUE) * 100,
          `Diabetes`            = mean(DIABETES, na.rm = TRUE) * 100,
          `High blood pressure` = mean(HYPERTEN, na.rm = TRUE) * 100
        )
      history_long <- history %>%
        pivot_longer(cols = -group, names_to = "condition", values_to = "percent")
      
      History <- ggplot(history_long, aes(x = reorder(condition, percent), y = percent, fill = group)) +
        geom_col(position = "dodge", color = "#e9ecef", width = 0.4) +
        coord_flip() +
        scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
        theme_ipsum() +
        theme(legend.position = "bottom") +
        guides(fill = guide_legend(reverse = TRUE)) +
        labs(title = "Medical history of the patients",
             x = "",
             y = "Percentage of patients (%)",
             fill = "")
      History
    }
      
       })
  
}

shinyApp(ui = ui, server = server)