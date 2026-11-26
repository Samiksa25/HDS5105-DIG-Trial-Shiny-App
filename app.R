
# ============================================================
# DIG TRIAL EXPLORER
# ============================================================

# 1. Load packages
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(bsicons)


# 2. Load and prepare data
# Prefer the cleaned dataset created by 01_data_preprocessing.R.
# Fall back to the original CSV if the cleaned file is not available.
if (file.exists("Data/DIG_clean.rds")) {
  dig <- readRDS("Data/DIG_clean.rds")
} else {
  dig <- read.csv("Data/DIG-1.csv")
}

dig$group <- factor(
  dig$TRTMT,
  levels = c(0, 1),
  labels = c("Placebo", "Digoxin")
)

dig$age_group <- cut(
  dig$AGE,
  breaks = c(0, 60, 70, 100),
  labels = c("Under 60", "60–70", "Over 70"),
  right = FALSE
)

dig$diabetes <- factor(
  dig$DIABETES,
  levels = c(0, 1),
  labels = c("No diabetes", "Diabetes")
)


# ============================================================
# 3. USER INTERFACE
# ============================================================

ui <- page_navbar(
  
  # ----------------------------------------------------------
  # OVERVIEW TAB
  # ----------------------------------------------------------
  
  nav_panel(
    title = "Overview",
    
    div(
      class = "overview-wrap",
      
      div(
        style = "text-align: center; padding: 40px 0 24px;",
        
        h1(
          "Digoxin: an old medicine, a fair test",
          style = "font-weight: 800; letter-spacing: 1px; margin-bottom: 16px;"
        ),
        
        p(
          "See who took part, what changed, and who benefited most.",
          style = "color: #6b7280; font-size: 18px;"
        )
      ),
      
      hr(
        style = "width: 100%; height: 12px; border: none;
        border-radius: 7px; opacity: 1; margin: 0 0 40px;
        background: linear-gradient(to right, #0b5cbf, rgba(11, 92, 191, 0));"
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        gap = "40px",
        
        tags$img(
          src = "banner.png",
          style = "width: 100%; height: 100%; max-height: 440px;
          object-fit: cover; border-radius: 12px;"
        ),
        
        layout_column_wrap(
          width = 1,
          gap = "24px",
          
          value_box(
            title = "Patients in the trial",
            value = "6,800",
            showcase = bs_icon("people"),
            theme = "primary"
          ),
          
          value_box(
            title = "Heart failure hospital stays",
            value = "35% → 27%",
            showcase = bs_icon("hospital"),
            theme = "primary"
          ),
          
          value_box(
            title = "Lower risk of hospital stay",
            value = "23%",
            showcase = bs_icon("heart-pulse"),
            theme = "primary"
          )
        )
      )
    )
  ),
  
  
  # ----------------------------------------------------------
  # PATIENTS TAB
  # ----------------------------------------------------------
  
  nav_panel(
    title = "Patients",
    
    selectInput(
      inputId = "patient_chart",
      label = "What would you like to see?",
      choices = c(
        "Age",
        "BMI",
        "Heart rate",
        "Blood pressure",
        "Race",
        "Medical history"
      )
    ),
    
    plotOutput("age_plot")
  ),
  
  
  # ----------------------------------------------------------
  # DID IT HELP TAB
  # ----------------------------------------------------------
  
  nav_panel(
    title = "Did it help?",
    
    selectInput(
      inputId = "effect_chart",
      label = "What would you like to see?",
      choices = c("Overall result", "Over time")
    ),
    
    plotOutput("effect_plot")
  ),
  
  
  # ----------------------------------------------------------
  # WHO BENEFITED TAB
  # ----------------------------------------------------------
  
  nav_panel(
    title = "Who benefited?",
    
    selectInput(
      inputId = "benefit_chart",
      label = "What would you like to see?",
      choices = c("By age", "By diabetes")
    ),
    
    plotOutput("benefit_plot")
  ),
  
  
  # ----------------------------------------------------------
  # EXPERT MODE SWITCH
  # ----------------------------------------------------------
  
  nav_item(
    div(
      style = "position: absolute; right: 40px; top: 50%;
      transform: translateY(-50%); margin-top: 8px;",
      
      input_switch(
        id = "expert_mode",
        label = "Expert",
        width = "auto"
      )
    )
  ),
  
  
  # ----------------------------------------------------------
  # EXPERT ANALYSIS TAB
  # ----------------------------------------------------------
  
  nav_panel(
    title = "Expert analysis",
    conditionalPanel(
      condition = "input.expert_mode == true",
      h2("Expert Analysis Dashboard"),
      p("Detailed analysis of the DIG trial. Explore the five sections below.",
        style = "color: #6b7280; margin-bottom: 20px;"),
      tabsetPanel(
        id = "expert_tabs",
        tabPanel("1. Patient characteristics",
                 br(),
                 layout_columns(
                   col_widths = c(6, 6),
                   card(card_header("Patient profile by treatment group"),
                        tableOutput("expert_characteristics")),
                   card(card_header("Demographic breakdown"),
                        tableOutput("expert_demographics"))
                 ),
                 br(),
                 layout_columns(
                   col_widths = c(6, 6),
                   card(card_header("Age distribution"),
                        plotOutput("expert_age_plot", height = "300px")),
                   card(card_header("BMI distribution"),
                        plotOutput("expert_bmi_plot", height = "300px"))
                 ),
                 br(),
                 card(card_header("Demographic profile by treatment"),
                      plotOutput("expert_demographic_plot", height = "360px"))
        ),
        tabPanel("2. Data quality",
                 br(),
                 layout_columns(
                   col_widths = c(6, 6),
                   card(card_header("Complete records"),
                        h2(textOutput("expert_complete_records"))),
                   card(card_header("Missing data overview"),
                        plotOutput("expert_missing_plot", height = "300px"))
                 ),
                 br(),
                 card(card_header("Missing values by variable"),
                      tableOutput("expert_missing_table"))
        ),
        tabPanel("3. Treatment comparison",
                 br(),
                 p("Outcome percentages use participants with non-missing outcome data."),
                 card(card_header("Outcome rates by treatment"),
                      plotOutput("expert_outcome_plot", height = "380px")),
                 br(),
                 layout_columns(
                   col_widths = c(6, 6),
                   card(card_header("Outcome counts and percentages"),
                        tableOutput("expert_treatment_table")),
                   card(card_header("Relative risk and absolute risk difference"),
                        tableOutput("expert_effect_table"))
                 )
        ),
        tabPanel("4. Statistical tests",
                 br(),
                 card(card_header("P-values from statistical tests"),
                      plotOutput("expert_pvalue_plot", height = "320px")),
                 br(),
                 card(card_header("Chi-square tests and independent-samples t-tests"),
                      tableOutput("expert_tests_table")),
                 br(),
                 p("These are unadjusted tests. Interpret p-values alongside effect sizes and confidence intervals; they do not establish causality.",
                   style = "color: #6b7280;")
        ),
        tabPanel("5. Survival analysis",
                 br(), h4("Survival analysis readiness"),
                 p("Kaplan–Meier curves, numbers at risk, log-rank tests and Cox models require a verified follow-up time and censoring definition for every participant."),
                 p("The dataset contains DEATH, DEATHDAY, DWHF and DWHFDAYS. Confirm from the DIG data dictionary whether DEATHDAY and DWHFDAYS contain follow-up times for participants who did not experience the event. Event-only dates cannot be used as complete survival follow-up times.",
                   style = "color: #6b7280;"),
                 tableOutput("expert_survival_check")
        )
      )
    ),
    conditionalPanel(
      condition = "input.expert_mode != true",
      h4("Please switch on Expert mode to view detailed analysis.")
    )
  ),
  
  
  # ----------------------------------------------------------
  # NAVIGATION BAR SETTINGS
  # ----------------------------------------------------------
  # NAVIGATION BAR SETTINGS
  # ----------------------------------------------------------
  
  title = "DIG Trial explorer",
  
  theme = bs_theme(
    bg = "#eef5fc",
    fg = "#1a1a1a"
  ),
  
  navbar_options = navbar_options(
    collapsible = FALSE
  ),
  
  header = tags$style("
    .navbar-brand {
      font-weight: 800;
      letter-spacing: 0.5px;
    }

    .navbar-collapse {
      justify-content: center;
    }

    .navbar-nav {
      flex-grow: 0;
      margin: 0 auto;
      background: white;
      border-radius: 999px;
      padding: 6px;
      gap: 16px;
    }

    .navbar-nav .nav-link {
      border-radius: 999px;
      padding: 10px 28px !important;
      color: #0b5cbf !important;
    }

    .navbar-nav .nav-link.active {
      background: #0b5cbf;
      color: white !important;
    }

    .overview-wrap {
      max-width: 960px;
      margin: 0 auto !important;
      padding: 0 24px 24px;
    }
  "),
  
  # ----------------------------------------------------------
  # SIDEBAR FILTERS
  # ----------------------------------------------------------
  
  sidebar = sidebar(
    open = "closed",
    
    checkboxGroupInput(
      inputId = "sex",
      label = "Sex",
      choices = c("Male" = 1, "Female" = 2),
      selected = c(1, 2)
    ),
    
    checkboxGroupInput(
      inputId = "race",
      label = "Race",
      choices = c("White" = 1, "Non-white" = 2),
      selected = c(1, 2)
    ),
    
    sliderInput(
      inputId = "age",
      label = "Age",
      min = 21,
      max = 90,
      value = c(21, 90)
    )
  )
)


# ============================================================
# 4. SERVER
# ============================================================

server <- function(input, output) {
  
  
  # ----------------------------------------------------------
  # COMMON FILTERED DATA
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    dig %>%
      filter(
        SEX %in% as.numeric(input$sex),
        RACE %in% as.numeric(input$race),
        AGE >= input$age[1],
        AGE <= input$age[2]
      )
    
  })
  
  
  # ----------------------------------------------------------
  # PATIENTS: AGE, BMI, HEART RATE, BLOOD PRESSURE,
  # RACE AND MEDICAL HISTORY
  # ----------------------------------------------------------
  
  output$age_plot <- renderPlot({
    
    filtered <- filtered_data()
    
    if (input$patient_chart == "Age") {
      
      Age <- ggplot(filtered, aes(x = AGE, fill = group)) +
        geom_histogram(
          color = "#e9ecef",
          alpha = 0.6,
          position = "identity",
          binwidth = 5
        ) +
        scale_fill_manual(
          values = c("#6e03ab", "#0ac219")
        ) +
        theme_ipsum() +
        coord_cartesian(xlim = c(20, 95)) +
        labs(
          title = "How old were the patients?",
          x = "Age (years)",
          y = "Number of patients",
          fill = ""
        )
      
      Age
      
    } else if (input$patient_chart == "BMI") {
      
      Bmi <- ggplot(filtered, aes(x = BMI, fill = group)) +
        geom_histogram(
          color = "#e9ecef",
          alpha = 0.6,
          position = "identity",
          binwidth = 2,
          na.rm = TRUE
        ) +
        scale_fill_manual(
          values = c("#6e03ab", "#0ac219")
        ) +
        theme_ipsum() +
        coord_cartesian(xlim = c(10, 60)) +
        labs(
          title = "Body weight of the patients",
          x = "BMI (kg/m²)",
          y = "Number of patients",
          fill = ""
        )
      
      Bmi
      
    } else if (input$patient_chart == "Heart rate") {
      
      HR <- ggplot(filtered, aes(x = HEARTRTE, fill = group)) +
        geom_histogram(
          color = "#e9ecef",
          alpha = 0.6,
          position = "identity",
          binwidth = 5,
          na.rm = TRUE
        ) +
        scale_fill_manual(
          values = c("#6e03ab", "#0ac219")
        ) +
        theme_ipsum() +
        coord_cartesian(xlim = c(30, 160)) +
        labs(
          title = "What were the patients' heart rates?",
          x = "Heart rate (beats/min)",
          y = "Number of patients",
          fill = ""
        )
      
      HR
      
    } else if (input$patient_chart == "Blood pressure") {
      
      BP <- ggplot(filtered) +
        geom_histogram(
          aes(
            x = SYSBP,
            y = after_stat(count),
            fill = group
          ),
          color = "#e9ecef",
          alpha = 0.6,
          position = "identity",
          binwidth = 10,
          na.rm = TRUE
        ) +
        geom_histogram(
          aes(
            x = DIABP,
            y = -after_stat(count),
            fill = group
          ),
          color = "#e9ecef",
          alpha = 0.6,
          position = "identity",
          binwidth = 10,
          na.rm = TRUE
        ) +
        theme_ipsum() +
        labs(
          title = "Blood pressure was almost identical in both groups",
          x = "Blood pressure (mmHg)",
          y = "Number of patients",
          fill = ""
        ) +
        scale_fill_manual(
          values = c("#6e03ab", "#0ac219")
        ) +
        annotate(
          "label",
          x = 190,
          y = 300,
          label = "Systolic"
        ) +
        annotate(
          "label",
          x = 40,
          y = -300,
          label = "Diastolic"
        ) +
        scale_y_continuous(labels = abs)
      
      BP
      
    } else if (input$patient_chart == "Race") {
      
      filtered$race <- factor(
        filtered$RACE,
        levels = c(1, 2),
        labels = c("White", "Non-white")
      )
      
      Race <- ggplot(
        filtered,
        aes(x = race, fill = group)
      ) +
        geom_bar(
          position = "dodge",
          color = "#e9ecef",
          width = 0.4
        ) +
        coord_flip() +
        scale_fill_manual(
          values = c("#6e03ab", "#0ac219")
        ) +
        theme_ipsum() +
        theme(
          legend.position = "bottom"
        ) +
        guides(
          fill = guide_legend(reverse = TRUE)
        ) +
        labs(
          title = "Ethnic background of the patients",
          x = "",
          y = "Number of patients",
          fill = ""
        )
      
      Race
      
    } else if (input$patient_chart == "Medical history") {
      
      history <- filtered %>%
        group_by(group) %>%
        summarise(
          `Prior heart attack` = mean(PREVMI, na.rm = TRUE) * 100,
          `Angina` = mean(ANGINA, na.rm = TRUE) * 100,
          `Diabetes` = mean(DIABETES, na.rm = TRUE) * 100,
          `High blood pressure` = mean(HYPERTEN, na.rm = TRUE) * 100,
          .groups = "drop"
        )
      
      history_long <- history %>%
        pivot_longer(
          cols = -group,
          names_to = "condition",
          values_to = "percent"
        )
      
      History <- ggplot(
        history_long,
        aes(
          x = reorder(condition, percent),
          y = percent,
          fill = group
        )
      ) +
        geom_col(
          position = "dodge",
          color = "#e9ecef",
          width = 0.4
        ) +
        coord_flip() +
        scale_fill_manual(
          values = c("#6e03ab", "#0ac219")
        ) +
        theme_ipsum() +
        theme(
          legend.position = "bottom"
        ) +
        guides(
          fill = guide_legend(reverse = TRUE)
        ) +
        labs(
          title = "Medical history of the patients",
          x = "",
          y = "Percentage of patients (%)",
          fill = ""
        )
      
      History
    }
    
  })
  
  
  # ----------------------------------------------------------
  # DID IT HELP?
  # ----------------------------------------------------------
  
  output$effect_plot <- renderPlot({
    
    filtered <- filtered_data()
    
    if (input$effect_chart == "Overall result") {
      
      effect <- filtered %>%
        group_by(group) %>%
        summarise(
          `Died` = mean(DEATH, na.rm = TRUE) * 100,
          `Died or hospitalised for heart failure` =
            mean(DWHF, na.rm = TRUE) * 100,
          .groups = "drop"
        )
      
      effect_long <- effect %>%
        pivot_longer(
          cols = -group,
          names_to = "outcome",
          values_to = "percent"
        )
      
      Outcome <- ggplot(
        effect_long,
        aes(x = outcome, y = percent, fill = group)
      ) +
        geom_text(
          aes(label = paste0(round(percent, 1), "%")),
          position = position_dodge(width = 0.6),
          vjust = -0.5
        ) +
        geom_bar(
          position = position_dodge(width = 0.6),
          stat = "identity",
          color = "#e9ecef",
          width = 0.4
        ) +
        scale_fill_manual(
          values = c("#6e03ab", "#0ac219")
        ) +
        theme_ipsum() +
        theme(
          legend.position = "bottom"
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.1))
        ) +
        labs(
          title = "Same survival, fewer hospital stays",
          x = "",
          y = "Percentage of patients (%)",
          fill = ""
        )
      
      Outcome
      
    } else if (input$effect_chart == "Over time") {
      
      by_year <- filtered %>%
        group_by(group) %>%
        summarise(
          `After 1 year` =
            mean(DWHF == 1 & DWHFDAYS <= 365, na.rm = TRUE) * 100,
          
          `After 2 years` =
            mean(DWHF == 1 & DWHFDAYS <= 730, na.rm = TRUE) * 100,
          
          .groups = "drop"
        )
      
      by_year_long <- by_year %>%
        pivot_longer(
          cols = -group,
          names_to = "time",
          values_to = "percent"
        )
      
      Time <- ggplot(
        by_year_long,
        aes(x = time, y = percent, fill = group)
      ) +
        geom_bar(
          position = position_dodge(width = 0.6),
          stat = "identity",
          color = "#e9ecef",
          width = 0.6
        ) +
        geom_text(
          aes(label = paste0(round(percent, 1), "%")),
          position = position_dodge(width = 0.6),
          vjust = -0.5
        ) +
        scale_fill_manual(
          values = c("#6e03ab", "#0ac219")
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.1))
        ) +
        theme_ipsum() +
        theme(
          legend.position = "bottom"
        ) +
        labs(
          title = "Did digoxin help?",
          subtitle = "Died or hospitalised for heart failure, over time",
          x = "",
          y = "Percentage of patients (%)",
          fill = ""
        )
      
      Time
    }
    
  })
  
  
  # ----------------------------------------------------------
  # WHO BENEFITED?
  # ----------------------------------------------------------
  
  output$benefit_plot <- renderPlot({
    
    filtered <- filtered_data()
    
    if (input$benefit_chart == "By age") {
      
      benefit <- filtered %>%
        group_by(age_group, group, .drop = FALSE) %>%
        summarise(
          percent = mean(DWHF, na.rm = TRUE) * 100,
          .groups = "drop"
        )
      
      Benefit <- ggplot(
        benefit,
        aes(x = age_group, y = percent, fill = group)
      ) +
        geom_bar(
          position = position_dodge(width = 0.6),
          stat = "identity",
          color = "#e9ecef",
          width = 0.6
        ) +
        geom_text(
          aes(label = paste0(round(percent, 1), "%")),
          position = position_dodge(width = 0.6),
          vjust = -0.5
        ) +
        scale_fill_manual(
          values = c("#6e03ab", "#0ac219")
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.1))
        ) +
        scale_x_discrete(
          drop = FALSE
        ) +
        theme_ipsum() +
        theme(
          legend.position = "bottom"
        ) +
        labs(
          title = "Who benefited most?",
          subtitle = "Died or hospitalised for heart failure, by age group",
          x = "",
          y = "Percentage of patients (%)",
          fill = ""
        )
      
      Benefit
      
    } else if (input$benefit_chart == "By diabetes") {
      
      benefit_diab <- filtered %>%
        group_by(diabetes, group) %>%
        summarise(
          percent = mean(DWHF, na.rm = TRUE) * 100,
          .groups = "drop"
        )
      
      Benefit_diab <- ggplot(
        benefit_diab,
        aes(x = diabetes, y = percent, fill = group)
      ) +
        geom_bar(
          position = position_dodge(width = 0.6),
          stat = "identity",
          color = "#e9ecef",
          width = 0.6
        ) +
        geom_text(
          aes(label = paste0(round(percent, 1), "%")),
          position = position_dodge(width = 0.6),
          vjust = -0.5
        ) +
        scale_fill_manual(
          values = c("#6e03ab", "#0ac219")
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.1))
        ) +
        theme_ipsum() +
        theme(
          legend.position = "bottom"
        ) +
        labs(
          title = "Who benefited most?",
          subtitle = "Died or hospitalised for heart failure, by diabetes",
          x = "",
          y = "Percentage of patients (%)",
          fill = ""
        )
      
      Benefit_diab
    }
    
  })
  
  
  
  # ==========================================================
  # EXPERT ANALYSIS
  # ==========================================================
  
  expert_data <- reactive({ dig })
  
  # 1. Patient characteristics
  output$expert_characteristics <- renderTable({
    expert_data() %>%
      group_by(group) %>%
      summarise(
        Patients = n(),
        `Mean age` = round(mean(AGE, na.rm = TRUE), 2),
        `Age SD` = round(sd(AGE, na.rm = TRUE), 2),
        `Mean BMI` = round(mean(BMI, na.rm = TRUE), 2),
        `BMI SD` = round(sd(BMI, na.rm = TRUE), 2),
        .groups = "drop"
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$expert_demographics <- renderTable({
    dat <- expert_data()
    make_demo <- function(variable, label, value_labels) {
      value <- factor(dat[[variable]], levels = as.numeric(names(value_labels)),
                      labels = unname(value_labels))
      out <- as.data.frame(table(dat$group, value, useNA = "ifany"))
      names(out) <- c("Treatment", "Category", "Patients")
      out$Variable <- label
      out[, c("Variable", "Treatment", "Category", "Patients")]
    }
    bind_rows(
      make_demo("SEX", "Sex", c("1" = "Male", "2" = "Female")),
      make_demo("RACE", "Race", c("1" = "White", "2" = "Non-white")),
      make_demo("DIABETES", "Diabetes", c("0" = "No", "1" = "Yes")),
      make_demo("HYPERTEN", "Hypertension", c("0" = "No", "1" = "Yes"))
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$expert_demographic_plot <- renderPlot({
    dat <- expert_data()
    demo_long <- bind_rows(
      dat %>% filter(!is.na(SEX)) %>% count(group, Category = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")), name = "Patients") %>% mutate(Characteristic = "Sex"),
      dat %>% filter(!is.na(RACE)) %>% count(group, Category = factor(RACE, levels = c(1, 2), labels = c("White", "Non-white")), name = "Patients") %>% mutate(Characteristic = "Race"),
      dat %>% filter(!is.na(DIABETES)) %>% count(group, Category = factor(DIABETES, levels = c(0, 1), labels = c("No diabetes", "Diabetes")), name = "Patients") %>% mutate(Characteristic = "Diabetes"),
      dat %>% filter(!is.na(HYPERTEN)) %>% count(group, Category = factor(HYPERTEN, levels = c(0, 1), labels = c("No hypertension", "Hypertension")), name = "Patients") %>% mutate(Characteristic = "Hypertension")
    ) %>%
      group_by(group, Characteristic) %>%
      mutate(Percent = Patients / sum(Patients) * 100) %>%
      ungroup()
    
    ggplot(demo_long, aes(x = Characteristic, y = Percent, fill = Category)) +
      geom_col(position = "stack", width = 0.68) +
      facet_wrap(~ group) +
      scale_fill_brewer(palette = "Set2") +
      theme_ipsum() +
      theme(legend.position = "bottom") +
      labs(title = "Composition of each treatment group",
           x = NULL, y = "Within-group percentage", fill = "Category")
  })
  
  output$expert_age_plot <- renderPlot({
    ggplot(expert_data(), aes(x = AGE, fill = group)) +
      geom_histogram(bins = 25, alpha = 0.65, position = "identity", na.rm = TRUE) +
      scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
      theme_ipsum() +
      labs(title = "Age distribution by treatment group", x = "Age (years)",
           y = "Number of patients", fill = "Treatment")
  })
  
  output$expert_bmi_plot <- renderPlot({
    ggplot(expert_data(), aes(x = group, y = BMI, fill = group)) +
      geom_boxplot(na.rm = TRUE) +
      scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
      theme_ipsum() + theme(legend.position = "none") +
      labs(title = "BMI distribution by treatment group", x = "Treatment group",
           y = "BMI (kg/m²)")
  })
  
  # 2. Data quality
  output$expert_complete_records <- renderText({
    dat <- expert_data()
    n_complete <- sum(complete.cases(dat))
    paste0(format(n_complete, big.mark = ","), " of ",
           format(nrow(dat), big.mark = ","), " records are complete across all variables (",
           round(100 * n_complete / nrow(dat), 1), "%).")
  })
  
  output$expert_missing_table <- renderTable({
    dat <- expert_data()
    data.frame(
      Variable = names(dat),
      `Missing values` = colSums(is.na(dat)),
      `Missing (%)` = round(colMeans(is.na(dat)) * 100, 2),
      check.names = FALSE
    ) %>% arrange(desc(`Missing values`))
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$expert_missing_plot <- renderPlot({
    dat <- expert_data()
    missing_plot_data <- data.frame(
      Variable = names(dat),
      Missing_Percentage = colMeans(is.na(dat)) * 100
    ) %>%
      filter(Missing_Percentage > 0) %>%
      arrange(Missing_Percentage)
    
    if (nrow(missing_plot_data) == 0) {
      return(ggplot() +
               annotate("text", x = 1, y = 1, label = "No missing values detected") +
               theme_void())
    }
    
    ggplot(missing_plot_data,
           aes(x = reorder(Variable, Missing_Percentage), y = Missing_Percentage)) +
      geom_col(fill = "#6e03ab", width = 0.7) +
      coord_flip() +
      theme_ipsum() +
      labs(title = "Percentage missing by variable", x = NULL,
           y = "Missing values (%)")
  })
  
  # 3. Treatment comparison
  binary_summary <- function(dat, variable, outcome_label) {
    valid <- !is.na(dat[[variable]]) & !is.na(dat$group)
    d <- dat[valid, , drop = FALSE]
    bind_rows(lapply(levels(droplevels(d$group)), function(g) {
      idx <- d$group == g
      n <- sum(idx)
      events <- sum(d[[variable]][idx] == 1)
      ci <- if (n > 0) binom.test(events, n)$conf.int else c(NA_real_, NA_real_)
      data.frame(
        Outcome = outcome_label, Treatment = g, Patients = n, Events = events,
        `Risk (%)` = round(100 * events / n, 2),
        `95% binomial CI` = paste0(round(100 * ci[1], 2), "% to ",
                                   round(100 * ci[2], 2), "%"),
        check.names = FALSE
      )
    }))
  }
  
  output$expert_treatment_table <- renderTable({
    dat <- expert_data()
    bind_rows(
      binary_summary(dat, "DEATH", "Mortality"),
      binary_summary(dat, "HOSP", "Hospitalisation"),
      binary_summary(dat, "DWHF", "Death or worsening heart failure")
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$expert_effect_table <- renderTable({
    dat <- expert_data()
    outcome_vars <- c("Mortality" = "DEATH", "Hospitalisation" = "HOSP",
                      "Death or worsening heart failure" = "DWHF")
    bind_rows(lapply(names(outcome_vars), function(label) {
      variable <- outcome_vars[[label]]
      d <- dat[!is.na(dat[[variable]]) & !is.na(dat$group), , drop = FALSE]
      placebo <- d[d$group == "Placebo", , drop = FALSE]
      digoxin <- d[d$group == "Digoxin", , drop = FALSE]
      n0 <- nrow(placebo)
      n1 <- nrow(digoxin)
      e0 <- sum(placebo[[variable]] == 1)
      e1 <- sum(digoxin[[variable]] == 1)
      p0 <- if (n0 > 0) e0 / n0 else NA_real_
      p1 <- if (n1 > 0) e1 / n1 else NA_real_
      rd_ci <- tryCatch(
        prop.test(c(e1, e0), c(n1, n0), correct = FALSE)$conf.int,
        error = function(e) c(NA_real_, NA_real_)
      )
      rr <- if (!is.na(p0) && p0 > 0) p1 / p0 else NA_real_
      rr_ci <- if (e0 > 0 && e1 > 0 && n0 > e0 && n1 > e1) {
        se <- sqrt(1 / e1 - 1 / n1 + 1 / e0 - 1 / n0)
        exp(log(rr) + c(-1, 1) * 1.96 * se)
      } else c(NA_real_, NA_real_)
      data.frame(
        Outcome = label,
        `Risk difference: Digoxin - Placebo (percentage points)` =
          round(100 * (p1 - p0), 2),
        `95% CI for risk difference` = paste0(round(100 * rd_ci[1], 2), " to ",
                                              round(100 * rd_ci[2], 2)),
        `Relative risk` = round(rr, 3),
        `95% CI for relative risk` = if (all(is.finite(rr_ci))) {
          paste0(round(rr_ci[1], 3), " to ", round(rr_ci[2], 3))
        } else "Not estimable with zero cells",
        check.names = FALSE
      )
    }))
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$expert_outcome_plot <- renderPlot({
    dat <- expert_data()
    outcome_vars <- c("Mortality" = "DEATH", "Hospitalisation" = "HOSP",
                      "Death or worsening HF" = "DWHF")
    plot_data <- bind_rows(lapply(names(outcome_vars), function(label) {
      variable <- outcome_vars[[label]]
      d <- dat[!is.na(dat[[variable]]) & !is.na(dat$group), , drop = FALSE]
      d %>% group_by(group) %>%
        summarise(Percent = mean(.data[[variable]] == 1) * 100, .groups = "drop") %>%
        mutate(Outcome = label)
    }))
    ggplot(plot_data, aes(x = Outcome, y = Percent, fill = group)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_text(aes(label = paste0(round(Percent, 1), "%")),
                position = position_dodge(width = 0.7), vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
      theme_ipsum() + theme(legend.position = "bottom") +
      labs(title = "Selected outcomes by treatment group", x = "",
           y = "Patients with outcome (%)", fill = "Treatment")
  })
  
  # 4. Statistical tests
  output$expert_tests_table <- renderTable({
    dat <- expert_data()
    chi_row <- function(variable, label) {
      d <- dat[!is.na(dat[[variable]]) & !is.na(dat$group), , drop = FALSE]
      tab <- table(d$group, d[[variable]])
      if (nrow(tab) < 2 || ncol(tab) < 2) {
        return(data.frame(Test = label, Statistic = NA, df = NA, `p-value` = NA,
                          Interpretation = "Insufficient outcome categories", check.names = FALSE))
      }
      test <- suppressWarnings(chisq.test(tab))
      data.frame(Test = label, Statistic = round(unname(test$statistic), 3),
                 df = unname(test$parameter), `p-value` = signif(test$p.value, 4),
                 Interpretation = ifelse(test$p.value < 0.05,
                                         "Evidence of an association (p < 0.05)",
                                         "No statistically significant association (p ≥ 0.05)"),
                 check.names = FALSE)
    }
    t_row <- function(variable, label) {
      d <- dat[!is.na(dat[[variable]]) & !is.na(dat$group), , drop = FALSE]
      test <- tryCatch(t.test(d[[variable]] ~ d$group), error = function(e) NULL)
      if (is.null(test)) {
        return(data.frame(Test = label, Statistic = NA, df = NA, `p-value` = NA,
                          Interpretation = "Test could not be calculated", check.names = FALSE))
      }
      data.frame(Test = label, Statistic = round(unname(test$statistic), 3),
                 df = round(unname(test$parameter), 2), `p-value` = signif(test$p.value, 4),
                 Interpretation = ifelse(test$p.value < 0.05,
                                         "Evidence of a difference (p < 0.05)",
                                         "No statistically significant difference (p ≥ 0.05)"),
                 check.names = FALSE)
    }
    bind_rows(
      chi_row("HOSP", "Chi-square: treatment vs hospitalisation"),
      chi_row("DEATH", "Chi-square: treatment vs mortality"),
      t_row("AGE", "Welch t-test: age"),
      t_row("BMI", "Welch t-test: BMI")
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$expert_pvalue_plot <- renderPlot({
    dat <- expert_data()
    p_values <- c(
      "Treatment vs hospitalisation" = tryCatch(
        chisq.test(table(dat$group, dat$HOSP))$p.value, error = function(e) NA_real_),
      "Treatment vs mortality" = tryCatch(
        chisq.test(table(dat$group, dat$DEATH))$p.value, error = function(e) NA_real_),
      "Age difference" = tryCatch(
        t.test(AGE ~ group, data = dat)$p.value, error = function(e) NA_real_),
      "BMI difference" = tryCatch(
        t.test(BMI ~ group, data = dat)$p.value, error = function(e) NA_real_)
    )
    p_data <- data.frame(Test = names(p_values), P_value = as.numeric(p_values)) %>%
      filter(is.finite(P_value))
    
    ggplot(p_data, aes(x = reorder(Test, P_value), y = P_value)) +
      geom_col(fill = "#0b5cbf", width = 0.65) +
      geom_hline(yintercept = 0.05, linetype = "dashed", colour = "#d62728") +
      coord_flip() +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      theme_ipsum() +
      labs(title = "P-values (dashed line: 0.05)", x = NULL, y = "P-value")
  })
  
  # 5. Survival analysis readiness check. Do not fit a model until censoring is verified.
  output$expert_survival_check <- renderTable({
    dat <- expert_data()
    vars <- c("DEATH", "DEATHDAY", "DWHF", "DWHFDAYS")
    meanings <- c(
      "Confirm 0/1 event coding",
      "Confirm follow-up time is recorded for censored participants",
      "Confirm composite event definition",
      "Confirm follow-up time is recorded for all participants"
    )
    data.frame(
      Variable = vars,
      Available = vars %in% names(dat),
      Non_missing = vapply(vars, function(v) if (v %in% names(dat)) sum(!is.na(dat[[v]])) else NA_integer_, integer(1)),
      Meaning_to_confirm = meanings,
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
}


# ============================================================
# 5. RUN THE APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)
