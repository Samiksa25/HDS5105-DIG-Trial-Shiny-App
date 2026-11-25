library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
dig <- read.csv("Data/DIG-1.csv")
dig$group <- factor(dig$TRTMT, levels = c(0, 1), labels = c("Placebo", "Digoxin"))
dig$age_group <- cut(dig$AGE, breaks = c(0, 60, 70, 100), labels = c("Under 60", "60–70", "Over 70"), right = FALSE)
dig$diabetes <- factor(dig$DIABETES, levels = c(0, 1), labels = c("No diabetes", "Diabetes"))


ui <- page_navbar(
  nav_panel(title = "Overview", p("First page content.")),
  nav_panel(title = "Patients", selectInput(inputId = "patient_chart", label = "What would you like to see?", choices = c("Age", "BMI", "Heart rate", "Blood pressure", "Race", "Medical history")), plotOutput(outputId = "age_plot")),
  nav_panel(title = "Did it help?", selectInput(inputId = "effect_chart", label = "What would you like to see?", choices = c("Overall result", "Over time")), plotOutput(outputId = "effect_plot")),
  nav_panel(title = "Who benefited?", selectInput(inputId = "benefit_chart", label = "What would you like to see?", choices = c("By age", "By diabetes")), plotOutput(outputId = "benefit_plot")),
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
        History  }
       })
  output$effect_plot <- renderPlot({
    filtered <- dig %>% filter(SEX %in% input$sex, RACE %in% input$race, AGE >= input$age[1], AGE <= input$age[2])
    if (input$effect_chart == "Overall result") {
      effect <- filtered %>%
        group_by(group) %>%
        summarise(
          `Died` = mean(DEATH, na.rm = TRUE) * 100,
          `Died or hospitalised for heart failure` = mean(DWHF, na.rm = TRUE) * 100
        )
      effect_long <- effect %>%
        pivot_longer(cols = -group, names_to = "outcome", values_to = "percent")
      Outcome <- ggplot(effect_long, aes(x = outcome, y = percent, fill = group)) +
        geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_dodge(width = 0.6), vjust = -0.5) +
        geom_bar(position = position_dodge(width = 0.6), stat = "identity", color = "#e9ecef", width = 0.4) +
        scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
        theme_ipsum() +
        theme(legend.position = "bottom") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        labs(title = "Same survival, fewer hospital stays",
             x = "",
             y = "Percentage of patients (%)",
             fill = "")
      Outcome
    } else if (input$effect_chart == "Over time") {
      by_year <- filtered %>%
        group_by(group) %>%
        summarise(
          `After 1 year`  = mean(DWHF == 1 & DWHFDAYS <= 365, na.rm = TRUE) * 100,
          `After 2 years` = mean(DWHF == 1 & DWHFDAYS <= 730, na.rm = TRUE) * 100
        )
      by_year_long <- by_year %>%
        pivot_longer(cols = -group, names_to = "time", values_to = "percent")
      Time <- ggplot(by_year_long, aes(x = time, y = percent, fill = group)) +
        geom_bar(position = position_dodge(width = 0.6), stat = "identity", color = "#e9ecef", width = 0.6) +
        geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_dodge(width = 0.6), vjust = -0.5) +
        scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_ipsum() +
        theme(legend.position = "bottom") +
        labs(title = "Did digoxin help?",
             subtitle = "Died or hospitalised for heart failure, over time",
             x = "",
             y = "Percentage of patients (%)",
             fill = "")
      Time
    }
  })
  output$benefit_plot <- renderPlot({
    filtered <- dig %>% filter(SEX %in% input$sex, RACE %in% input$race, AGE >= input$age[1], AGE <= input$age[2]) 
    if (input$benefit_chart == "By age") {
      benefit <- filtered %>%
        group_by(age_group, group, .drop = FALSE) %>%
        summarise(percent = mean(DWHF, na.rm = TRUE) * 100, .groups = "drop")
      Benefit <- ggplot(benefit, aes(x = age_group, y = percent, fill = group)) +
        geom_bar(position = position_dodge(width = 0.6), stat = "identity", color = "#e9ecef", width = 0.6) +
        geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_dodge(width = 0.6), vjust = -0.5) +
        scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        scale_x_discrete(drop = FALSE) +
        theme_ipsum() +
        theme(legend.position = "bottom") +
        labs(title = "Who benefited most?",
             subtitle = "Died or hospitalised for heart failure, by age group",
             x = "",
             y = "Percentage of patients (%)",
             fill = "")
      Benefit
    } else if (input$benefit_chart == "By diabetes") {
      benefit_diab <- filtered %>%
        group_by(diabetes, group) %>%
        summarise(percent = mean(DWHF, na.rm = TRUE) * 100, .groups = "drop")
      
      Benefit_diab <- ggplot(benefit_diab, aes(x = diabetes, y = percent, fill = group)) +
        geom_bar(position = position_dodge(width = 0.6), stat = "identity", color = "#e9ecef", width = 0.6) +
        geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_dodge(width = 0.6), vjust = -0.5) +
        scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_ipsum() +
        theme(legend.position = "bottom") +
        labs(title = "Who benefited most?",
             subtitle = "Died or hospitalised for heart failure, by diabetes",
             x = "",
             y = "Percentage of patients (%)",
             fill = "")
      Benefit_diab
    }
  })
}

shinyApp(ui = ui, server = server)