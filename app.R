
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
  
  # ----------------------------------------------------------
  # EXPERT ANALYSIS TAB
  # ----------------------------------------------------------
  
  nav_panel(
    title = "Expert analysis",
    
    conditionalPanel(
      condition = "input.expert_mode == true",
      
      div(
        class = "expert-hero",
        div(
          class = "expert-hero-content",
          span("DIG TRIAL | ADVANCED ANALYSIS", class = "expert-eyebrow"),
          h2("Explore the trial in detail", class = "expert-heading"),
          p(
            "A closer look at patient characteristics, treatment outcomes, data quality and statistical evidence.",
            class = "expert-subheading"
          ),
          div(
            class = "expert-legend",
            span(class = "legend-placebo"),
            span("Placebo"),
            span(class = "legend-digoxin"),
            span("Digoxin")
          )
        )
      ),
      
      div(
        class = "section-intro",
        strong("How to use this section: "),
        "Choose a tab below. Each tab contains charts, tables and explanations. Percentages use patients with available data for each outcome."
      ),
      
      tabsetPanel(
        id = "expert_tabs",
        
        tabPanel(
          "Patient profile",
          br(),
          h4("Patient characteristics"),
          p("Compare the age, BMI and demographic characteristics of the two treatment groups."),
          tableOutput("expert_characteristics"),
          tableOutput("expert_demographics"),
          plotOutput("expert_demographic_plot", height = "450px"),
          plotOutput("expert_age_plot", height = "350px"),
          plotOutput("expert_bmi_plot", height = "350px")
        ),
        
        tabPanel(
          "Data quality",
          br(),
          h4("Completeness of the dataset"),
          textOutput("expert_complete_records"),
          plotOutput("expert_missing_plot", height = "400px"),
          tableOutput("expert_missing_table")
        ),
        
        tabPanel(
          "Treatment comparison",
          br(),
          h4("Treatment outcomes"),
          p("Compare mortality, hospitalisation and the combined outcome between the treatment groups."),
          tableOutput("expert_treatment_table"),
          tableOutput("expert_effect_table"),
          plotOutput("expert_outcome_plot", height = "550px")
        ),
        
        tabPanel(
          "Statistical tests",
          br(),
          h4("Statistical evidence"),
          p("The following tests compare outcomes and patient characteristics between treatment groups. A p-value below 0.05 is commonly used as a significance threshold."),
          tableOutput("expert_tests_table"),
          plotOutput("expert_pvalue_plot", height = "400px")
        ),
        
        tabPanel(
          "Survival analysis",
          br(),
          h4("Survival data readiness"),
          p("Check the availability and completeness of the event and follow-up variables before fitting a survival model."),
          tableOutput("expert_survival_check")
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.expert_mode != true",
      div(
        style = "padding: 40px; text-align: center;",
        h4("Expert analysis is currently switched off"),
        p("Turn on the Expert switch in the navigation bar to view the advanced analysis.")
      )
    )
  ),
  
  
  # ----------------------------------------------------------
  # NAVIGATION BAR SETTINGS
  # ----------------------------------------------------------
  # NAVIGATION BAR SETTINGS
  # ----------------------------------------------------------
  
  title = "DIG Trial explorer",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#315c9b",
    bg = "#f4f7fb",
    fg = "#253247"
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
      max-width: 1100px;
      margin: 0 auto !important;
      padding: 0 24px 24px;
    }

    body { background: #f4f7fb; color: #253247; }
    .navbar { box-shadow: 0 2px 12px rgba(30, 55, 90, .08); }
    .tab-content { padding-top: 18px; }
    .card {
      border: 1px solid #e5eaf2;
      border-radius: 14px;
      box-shadow: 0 4px 16px rgba(35, 55, 85, .05);
      margin-bottom: 12px;
    }
    .card-header {
      background: #ffffff;
      color: #263b5a;
      font-weight: 700;
      border-bottom: 1px solid #edf0f5;
      padding: 14px 18px;
    }
    .card-body { padding: 18px; }
    .expert-heading { font-weight: 800; color: #203a60; margin-bottom: 6px; }
    .expert-subheading { color: #65748b; font-size: 15px; margin-bottom: 22px; }
    .section-intro {
      background: #eaf1fb; border-left: 4px solid #315c9b;
      border-radius: 8px; padding: 12px 16px; color: #34445c;
      margin: 8px 0 18px 0;
    }
    .plain-language { color: #5d6b80; font-size: 13px; margin-top: 8px; }
    .nav-tabs .nav-link { color: #53647d; font-weight: 650; }
    .nav-tabs .nav-link.active { color: #244d83; border-top: 3px solid #315c9b; }
    table { font-size: 13px; }
    
    /* ==========================================
   EXPERT ANALYSIS - HERO DESIGN
========================================== */

.expert-hero {
  background: linear-gradient(120deg, #f0eafa, #edf4fc);
  border: 1px solid #e1d8f1;
  border-radius: 18px;
  padding: 32px 36px;
  margin: 10px 0 24px 0;
  box-shadow: 0 5px 18px rgba(50, 45, 90, 0.06);
}

.expert-eyebrow {
  display: inline-block;
  color: #6e03ab;
  font-size: 11px;
  font-weight: 800;
  letter-spacing: 2px;
  margin-bottom: 12px;
}

.expert-heading {
  color: #263653;
  font-weight: 800;
  margin-bottom: 10px;
}

.expert-subheading {
  color: #66738a;
  font-size: 15px;
  max-width: 720px;
  line-height: 1.7;
  margin-bottom: 20px;
}

.expert-legend {
  display: flex;
  align-items: center;
  gap: 9px;
  color: #536078;
  font-size: 13px;
  font-weight: 600;
}

.legend-placebo,
.legend-digoxin {
  display: inline-block;
  width: 10px;
  height: 10px;
  border-radius: 50%;
}

.legend-placebo {
  background: #6e03ab;
}

.legend-digoxin {
  background: #0b5cbf;
  margin-left: 12px;
}

/* ==========================================
   EXPERT ANALYSIS - TAB NAVIGATION
========================================== */

#expert_tabs .nav-tabs {
  border-bottom: 1px solid #e5e8f0;
  gap: 8px;
  margin-bottom: 22px;
}

#expert_tabs .nav-tabs .nav-link {
  border: 1px solid transparent;
  border-radius: 10px;
  color: #59677e;
  font-weight: 650;
  padding: 11px 16px;
  transition: all 0.2s ease;
}

#expert_tabs .nav-tabs .nav-link:hover {
  background: #f3eff9;
  color: #6e03ab;
}

#expert_tabs .nav-tabs .nav-link.active {
  background: #6e03ab;
  color: #ffffff;
  border-color: #6e03ab;
}

/* ==========================================
   EXPERT ANALYSIS - CARDS
========================================== */

.card {
  border: 1px solid #e5e8f0;
  border-radius: 15px;
  box-shadow: 0 4px 16px rgba(35, 55, 85, 0.05);
  overflow: hidden;
  margin-bottom: 16px;
}

.card-header {
  background: #ffffff;
  color: #293b5a;
  font-weight: 750;
  border-bottom: 1px solid #edf0f5;
  padding: 16px 18px;
}

.card-body {
  padding: 20px;
}

/* ==========================================
   EXPERT ANALYSIS - TABLES
========================================== */

#expert_tabs .table {
  font-size: 13px;
  border-collapse: separate;
  border-spacing: 0;
}

#expert_tabs .table thead th {
  background: #f0eafa;
  color: #4d2675;
  font-weight: 750;
  border-bottom: 2px solid #ded0ef;
  padding: 12px;
}

#expert_tabs .table tbody td {
  padding: 11px 12px;
  color: #39475d;
  vertical-align: middle;
}

#expert_tabs .table tbody tr:nth-child(even) {
  background: #f5f8fc;
}

#expert_tabs .table tbody tr:hover {
  background: #f0eafa;
}

/* ==========================================
   EXPERT ANALYSIS - EXPLANATIONS
========================================== */

#expert_tabs .section-intro {
  background: #f0f4fb;
  border-left: 4px solid #6e03ab;
  border-radius: 9px;
  padding: 15px 18px;
  color: #40516b;
  line-height: 1.65;
  margin: 10px 0 22px 0;
}

#expert_tabs .plain-language {
  color: #65748b;
  font-size: 13px;
  line-height: 1.6;
  margin-top: 12px;
}

#expert_tabs .chart-note {
  background: #f7f8fc;
  border-radius: 8px;
  padding: 12px 14px;
  color: #59677e;
  font-size: 13px;
  line-height: 1.6;
  margin-top: 12px;
}

/* ==========================================
   MOBILE RESPONSIVE DESIGN
========================================== */

@media (max-width: 768px) {
  .expert-hero {
    padding: 24px 20px;
  }

  #expert_tabs .nav-tabs {
    gap: 5px;
  }

  #expert_tabs .nav-tabs .nav-link {
    padding: 9px 11px;
    font-size: 12px;
  }
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
    
    ggplot(demo_long, aes(x = Percent, y = Category, colour = group)) +
      geom_line(aes(group = interaction(Characteristic, Category)),
                colour = "#c8d1df", linewidth = 0.7,
                position = position_dodge(width = 0.45)) +
      geom_point(size = 3.2, position = position_dodge(width = 0.45)) +
      facet_wrap(~ Characteristic, scales = "free_y", ncol = 2) +
      scale_colour_manual(values = c("Placebo" = "#6e03ab", "Digoxin" = "#0b5cbf")) +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                         labels = function(x) paste0(x, "%")) +
      theme_ipsum() +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"),
            panel.grid.minor = element_blank()) +
      labs(title = "Patient characteristics within each treatment group",
           subtitle = "Dots show the percentage in each category",
           x = "Share of treatment group", y = NULL, colour = "Treatment")
  })
  
  output$expert_age_plot <- renderPlot({
    ggplot(expert_data(), aes(x = AGE, colour = group, fill = group)) +
      geom_density(alpha = 0.18, linewidth = 1.1, na.rm = TRUE) +
      scale_colour_manual(values = c("Placebo" = "#6e03ab", "Digoxin" = "#0b5cbf")) +
      scale_fill_manual(values = c("Placebo" = "#6e03ab", "Digoxin" = "#0b5cbf")) +
      theme_ipsum() +
      theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
      labs(title = "Age profile of the two groups", x = "Age (years)",
           y = "Relative concentration", fill = "Treatment", colour = "Treatment")
  })
  
  output$expert_bmi_plot <- renderPlot({
    ggplot(expert_data(), aes(x = group, y = BMI, fill = group)) +
      geom_violin(trim = TRUE, alpha = 0.35, colour = NA, na.rm = TRUE) +
      geom_boxplot(width = 0.16, alpha = 0.85, outlier.alpha = 0.25,
                   na.rm = TRUE, colour = "#34445c") +
      scale_fill_manual(values = c("Placebo" = "#6e03ab", "Digoxin" = "#0b5cbf")) +
      theme_ipsum() + theme(legend.position = "none",
                            panel.grid.minor = element_blank()) +
      labs(title = "BMI spread in each treatment group", x = NULL,
           y = "Body mass index (kg/m²)")
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
               annotate("text", x = 1, y = 1, label = "No missing values detected",
                        size = 5, colour = "#315c9b") +
               theme_void())
    }
    
    ggplot(missing_plot_data,
           aes(x = Missing_Percentage, y = reorder(Variable, Missing_Percentage))) +
      geom_segment(aes(x = 0, xend = Missing_Percentage,
                       yend = reorder(Variable, Missing_Percentage)),
                   colour = "#c8d1df", linewidth = 1) +
      geom_point(size = 3.2, colour = "#315c9b") +
      geom_text(aes(label = paste0(round(Missing_Percentage, 1), "%")),
                hjust = -0.35, size = 3.2, colour = "#34445c") +
      scale_x_continuous(limits = c(0, max(missing_plot_data$Missing_Percentage) * 1.2),
                         expand = expansion(mult = c(0, 0.02))) +
      theme_ipsum() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Where information is missing", x = "Records missing (%)",
           y = NULL)
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
    
    ggplot(plot_data, aes(x = Percent, y = group, colour = group)) +
      geom_line(aes(group = Outcome), colour = "#c8d1df", linewidth = 0.9) +
      geom_point(size = 3.5) +
      geom_text(aes(label = paste0(round(Percent, 1), "%")),
                nudge_x = 1.3, size = 3.3, show.legend = FALSE) +
      facet_wrap(~ Outcome, ncol = 1) +
      scale_colour_manual(values = c("Placebo" = "#6e03ab", "Digoxin" = "#0b5cbf")) +
      scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
      theme_ipsum() +
      theme(legend.position = "none", strip.text = element_text(face = "bold"),
            panel.grid.minor = element_blank()) +
      labs(title = "Observed outcome percentages", x = "Patients with outcome (%)",
           y = NULL)
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
      filter(is.finite(P_value)) %>%
      mutate(Result = ifelse(P_value < 0.05,
                             "Below 0.05 threshold", "At or above 0.05"))
    
    ggplot(p_data, aes(x = P_value, y = reorder(Test, P_value), colour = Result)) +
      geom_segment(aes(x = 0, xend = P_value,
                       yend = reorder(Test, P_value)),
                   colour = "#c8d1df", linewidth = 1) +
      geom_point(size = 3.5) +
      geom_vline(xintercept = 0.05, linetype = "dashed",
                 colour = "#b34b4b", linewidth = 0.8) +
      geom_text(aes(label = format.pval(P_value, digits = 2, eps = 0.001)),
                nudge_x = 0.025, size = 3.2, colour = "#34445c",
                show.legend = FALSE) +
      scale_colour_manual(values = c("Below 0.05 threshold" = "#247a66",
                                     "At or above 0.05" = "#315c9b")) +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      theme_ipsum() +
      theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
      labs(title = "Evidence from the statistical tests",
           subtitle = "Dashed line marks the commonly used 0.05 threshold",
           x = "P-value (0 to 1)", y = NULL, colour = NULL)
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
