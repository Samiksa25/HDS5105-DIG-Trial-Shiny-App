# ============================================================
# DIG TRIAL EXPLORER
# ============================================================

# 1. LOAD PACKAGES

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(bsicons)
library(plotly)


# ============================================================
# 2. LOAD AND PREPARE DATA
# ============================================================

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
  # OVERVIEW
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
  # PATIENTS
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
  # DID IT HELP?
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
  # WHO BENEFITED?
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
  # EXPERT ANALYSIS - SINGLE DASHBOARD PAGE
  # ----------------------------------------------------------
  
  
  nav_panel(
    title = "Expert analysis",
    
    conditionalPanel(
      condition = "input.expert_mode == true",
      
      div(
        class = "expert-dashboard",
        
        div(
          class = "dashboard-header",
          h2("DIG Trial Dashboard"),
          p("Explore patient characteristics, treatment outcomes and statistical findings.")
        ),
        
        # KPI CARDS
        
        fluidRow(
          column(
            3,
            div(
              class = "dashboard-kpi",
              div(class = "kpi-icon", bs_icon("people")),
              h6("Total patients"),
              h3(textOutput("kpi_patients"))
            )
          ),
          column(
            3,
            div(
              class = "dashboard-kpi",
              div(class = "kpi-icon", bs_icon("hospital")),
              h6("Hospitalisations"),
              h3(textOutput("kpi_hospitalisations"))
            )
          ),
          column(
            3,
            div(
              class = "dashboard-kpi",
              div(class = "kpi-icon", bs_icon("heart-pulse")),
              h6("Overall mortality"),
              h3(textOutput("kpi_mortality"))
            )
          ),
          column(
            3,
            div(
              class = "dashboard-kpi",
              div(class = "kpi-icon", bs_icon("activity")),
              h6("Complete records"),
              h3(textOutput("kpi_complete"))
            )
          )
        ),
        
        # FIRST ROW: TREATMENT OUTCOMES
        
        fluidRow(
          column(
            12,
            div(
              class = "dashboard-panel",
              h4("Treatment outcomes"),
              p("Comparison of observed outcomes in the Placebo and Digoxin groups."),
              plotlyOutput("expert_outcome_plot", height = "380px"),
              tableOutput("debug_outcome"),
              div(
                class = "chart-explanation",
                "How to read this: The chart compares the percentage of patients experiencing each outcome. Lower percentages indicate fewer recorded events."
              )
            )
          )
        ),
        
        # SECOND ROW: AGE AND BMI
        
        fluidRow(
          column(
            6,
            div(
              class = "dashboard-panel",
              h4("Age distribution"),
              plotlyOutput("expert_age_plot", height = "420px"),
              div(
                class = "chart-explanation",
                "How to read this: The curves show where patients' ages are concentrated. Similar curves suggest similar age distributions."
              )
            )
          ),
          
          column(
            6,
            div(
              class = "dashboard-panel",
              h4("BMI distribution"),
              plotlyOutput("expert_bmi_plot", height = "420px"),
              div(
                class = "chart-explanation",
                "How to read this: The violin width shows where values are concentrated. The box shows the middle half of BMI values."
              )
            )
          )
        ),
        
        # THIRD ROW: DEMOGRAPHICS AND DATA QUALITY
        
        fluidRow(
          column(
            6,
            div(
              class = "dashboard-panel",
              h4("Patient characteristics"),
              p("Distribution of patients across demographic categories."),
              plotlyOutput("expert_demographic_plot", height = "420px"),
              div(
                class = "chart-explanation",
                "How to read this: The percentages show the composition of each treatment group by sex, race, diabetes and hypertension."
              )
            )
          ),
          
          column(
            6,
            div(
              class = "dashboard-panel",
              h4("Data quality"),
              h5(textOutput("expert_complete_records")),
              plotlyOutput("expert_missing_plot", height = "420px"),
              div(
                class = "chart-explanation",
                "How to read this: Longer bars indicate variables with more missing information. Variables with no missing values are excluded from this chart."
              )
            )
          )
        ),
        
        # FOURTH ROW: TREATMENT TABLES
        
        fluidRow(
          column(
            6,
            div(
              class = "dashboard-panel",
              h4("Outcome counts and percentages"),
              tableOutput("expert_treatment_table")
            )
          ),
          
          column(
            6,
            div(
              class = "dashboard-panel",
              h4("Treatment effect estimates"),
              tableOutput("expert_effect_table"),
              div(
                class = "chart-explanation",
                "Risk difference compares the two groups in percentage points. Relative risk compares their event risks."
              )
            )
          )
        ),
        
        # FIFTH ROW: STATISTICAL ANALYSIS
        
        fluidRow(
          column(
            6,
            div(
              class = "dashboard-panel",
              h4("Statistical evidence"),
              plotOutput("expert_pvalue_plot", height = "280px"),
              div(
                class = "chart-explanation",
                "How to read this: The dashed line marks p = 0.05. Points to the left are below this commonly used threshold."
              )
            )
          ),
          
          column(
            6,
            div(
              class = "dashboard-panel",
              h4("Statistical test results"),
              tableOutput("expert_tests_table"),
              div(
                class = "chart-explanation",
                "A p-value below 0.05 is commonly considered statistically significant. It does not measure the size or importance of an effect."
              )
            )
          )
        ),
        
        # SIXTH ROW: SURVIVAL ANALYSIS
        
        fluidRow(
          column(
            12,
            div(
              class = "dashboard-panel",
              h4("Survival analysis data checks"),
              p("Follow-up time and censoring information must be verified before survival curves or a Cox model can be calculated."),
              tableOutput("expert_survival_check")
            )
          )
        )
      )
    ),
    
    # MESSAGE WHEN EXPERT MODE IS OFF
    
    conditionalPanel(
      condition = "input.expert_mode != true",
      
      div(
        class = "expert-off",
        h3("Expert analysis"),
        p("Switch on Expert mode to view the detailed trial dashboard.")
      )
    )
  ),
  
  
  
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

    body {
      background: #f4f7fb;
      color: #253247;
    }

    .navbar {
      box-shadow: 0 2px 12px rgba(30, 55, 90, .08);
    }

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

    .card-body {
      padding: 18px;
    }

    /* EXPERT DASHBOARD */

    .expert-dashboard {
      max-width: 1500px;
      margin: 0 auto;
      padding: 24px;
      background: #edf3fc;
      border-radius: 20px;
    }

    .dashboard-header {
      padding: 12px 4px 22px;
    }

    .dashboard-header h2 {
      font-size: 27px;
      font-weight: 800;
      color: #203a60;
      margin-bottom: 6px;
    }

    .dashboard-header p {
      color: #718096;
      font-size: 14px;
    }

    .dashboard-kpi {
      background: linear-gradient(145deg, #ffffff, #f3f6fd);
      border: 1px solid #e3eaf5;
      border-radius: 18px;
      padding: 20px;
      min-height: 135px;
      margin-bottom: 18px;
      box-shadow: 0 5px 18px rgba(40, 65, 110, 0.06);
    }

    .dashboard-kpi h6 {
      color: #65748b;
      font-size: 13px;
      margin-top: 12px;
    }

    .dashboard-kpi h3 {
      color: #203a60;
      font-weight: 800;
      font-size: 27px;
      margin-top: 5px;
    }

    .kpi-icon {
      color: #315c9b;
      font-size: 22px;
    }

    .dashboard-panel {
      background: #ffffff;
      border: 1px solid #e1e9f4;
      border-radius: 18px;
      padding: 20px;
      margin-bottom: 20px;
      min-height: 100px;
      box-shadow: 0 5px 18px rgba(40, 65, 110, 0.05);
      overflow-x: auto;
    }

    .dashboard-panel h4 {
      color: #263b5a;
      font-size: 16px;
      font-weight: 750;
      margin-bottom: 8px;
    }

    .dashboard-panel > p {
      color: #8995a8;
      font-size: 12px;
      margin-bottom: 15px;
    }

    .chart-explanation {
      background: #f1f5fc;
      border-radius: 9px;
      padding: 11px 13px;
      margin-top: 12px;
      color: #61718a;
      font-size: 12px;
      line-height: 1.6;
    }

    .dashboard-panel table {
      width: 100%;
      font-size: 12px;
    }

    .dashboard-panel .table {
      margin-bottom: 0;
    }

    .expert-off {
      background: #edf3fc;
      padding: 70px 20px;
      text-align: center;
      border-radius: 18px;
      color: #315c9b;
    }

    @media (max-width: 768px) {
      .expert-dashboard {
        padding: 12px;
      }

      .dashboard-kpi {
        min-height: 115px;
      }

      .dashboard-panel {
        padding: 14px;
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
  # PATIENTS
  # ----------------------------------------------------------
  
  
  output$age_plot <- renderPlot({
    filtered <- filtered_data()
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
  
  output$expert_age_plot <- renderPlotly({
    
    dat <- expert_data() %>%
      filter(!is.na(AGE), !is.na(group)) %>%
      mutate(
        Age_band = cut(
          AGE,
          breaks = seq(20, 100, by = 5),
          right = FALSE,
          include.lowest = TRUE
        )
      ) %>%
      filter(!is.na(Age_band))
    
    age_summary <- dat %>%
      count(Age_band, group, name = "Patients") %>%
      tidyr::complete(
        Age_band,
        group,
        fill = list(Patients = 0)
      ) %>%
      mutate(
        Age_band = factor(Age_band, levels = levels(dat$Age_band)),
        Plot_count = ifelse(group == "Placebo", -Patients, Patients),
        Age_order = as.integer(Age_band)
      ) %>%
      arrange(Age_order)
    
    age_frames <- sort(unique(age_summary$Age_order))
    
    plot_data <- bind_rows(
      lapply(age_frames, function(frame_number) {
        age_summary %>%
          filter(Age_order <= frame_number) %>%
          mutate(Frame = paste0("Up to age band ", frame_number))
      })
    )
    
    plot_ly(
      data = plot_data,
      x = ~Plot_count,
      y = ~Age_band,
      color = ~group,
      frame = ~Frame,
      type = "bar",
      orientation = "h",
      colors = c(
        "Placebo" = "#6e03ab",
        "Digoxin" = "#0b5cbf"
      ),
      text = ~Patients,
      hovertemplate = paste(
        "Age group: %{y}",
        "<br>Patients: %{text}",
        "<extra>%{fullData.name}</extra>"
      )
    ) %>%
      layout(
        title = "Age distribution by treatment group",
        xaxis = list(
          title = "Number of patients",
          tickformat = "d",
          zeroline = TRUE
        ),
        yaxis = list(
          title = "Age group",
          autorange = "reversed"
        ),
        barmode = "overlay",
        legend = list(
          orientation = "h",
          y = -0.2
        ),
        margin = list(l = 100, b = 80)
      ) %>%
      animation_opts(
        frame = 700,
        transition = 300,
        redraw = TRUE
      ) %>%
      animation_button() %>%
      animation_slider()
    
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
      
      ggplot(
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
      
      ggplot(
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
      
      ggplot(
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
      
    } else if (input$benefit_chart == "By diabetes") {
      
      benefit_diab <- filtered %>%
        group_by(diabetes, group) %>%
        summarise(
          percent = mean(DWHF, na.rm = TRUE) * 100,
          .groups = "drop"
        )
      
      ggplot(
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
    }
    
  })
  
  
  # ==========================================================
  # EXPERT ANALYSIS
  # ==========================================================
  
  expert_data <- reactive({ dig })
  
  output$debug_outcome <- renderTable({
    
    dat <- expert_data()
    
    data.frame(
      Variable = c("DEATH", "HOSP", "DWHF"),
      Data_type = c(
        class(dat$DEATH)[1],
        class(dat$HOSP)[1],
        class(dat$DWHF)[1]
      ),
      Non_missing = c(
        sum(!is.na(dat$DEATH)),
        sum(!is.na(dat$HOSP)),
        sum(!is.na(dat$DWHF))
      ),
      Event_count = c(
        sum(dat$DEATH == 1, na.rm = TRUE),
        sum(dat$HOSP == 1, na.rm = TRUE),
        sum(dat$DWHF == 1, na.rm = TRUE)
      )
    )
    
  })
  
  
  
  # ----------------------------------------------------------
  # DASHBOARD KPI VALUES
  # ----------------------------------------------------------
  
  output$kpi_patients <- renderText({
    format(nrow(dig), big.mark = ",")
  })
  
  output$kpi_hospitalisations <- renderText({
    n <- sum(dig$HOSP == 1, na.rm = TRUE)
    
    paste0(
      format(n, big.mark = ","),
      " (",
      round(mean(dig$HOSP == 1, na.rm = TRUE) * 100, 1),
      "%)"
    )
  })
  
  output$kpi_mortality <- renderText({
    paste0(
      round(mean(dig$DEATH == 1, na.rm = TRUE) * 100, 1),
      "%"
    )
  })
  
  output$kpi_complete <- renderText({
    paste0(
      round(mean(complete.cases(dig)) * 100, 1),
      "%"
    )
  })
  
  
  # ----------------------------------------------------------
  # 1. PATIENT CHARACTERISTICS
  # ----------------------------------------------------------
  
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
      
      value <- factor(
        dat[[variable]],
        levels = as.numeric(names(value_labels)),
        labels = unname(value_labels)
      )
      
      out <- as.data.frame(
        table(dat$group, value, useNA = "ifany")
      )
      
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
  
  
  output$expert_demographic_plot <- renderPlotly({
    
    dat <- expert_data()
    
    radar_data <- dat %>%
      group_by(group) %>%
      summarise(
        Male = mean(SEX == 1, na.rm = TRUE) * 100,
        White = mean(RACE == 1, na.rm = TRUE) * 100,
        Diabetes = mean(DIABETES == 1, na.rm = TRUE) * 100,
        Hypertension = mean(HYPERTEN == 1, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = -group,
        names_to = "Characteristic",
        values_to = "Percentage"
      )
    
    radar_data$Characteristic <- factor(
      radar_data$Characteristic,
      levels = c("Male", "White", "Diabetes", "Hypertension")
    )
    
    radar_data <- radar_data %>%
      arrange(group, Characteristic)
    
    plot_ly(
      data = radar_data,
      type = "scatterpolar",
      mode = "lines+markers",
      r = ~Percentage,
      theta = ~Characteristic,
      color = ~group,
      line = list(width = 3),
      marker = list(size = 8),
      fill = "toself",
      colors = c("#6e03ab", "#0b5cbf"),
      hovertemplate = paste(
        "Characteristic: %{theta}",
        "<br>Percentage: %{r:.1f}%",
        "<extra>%{fullData.name}</extra>"
      )
    ) %>%
      layout(
        title = "Patient characteristics by treatment group",
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100),
            ticksuffix = "%"
          )
        ),
        legend = list(
          orientation = "h",
          y = -0.15
        )
      )
    
  })
  
  
  
  output$expert_bmi_plot <- renderPlotly({
    
    dat <- expert_data() %>%
      filter(!is.na(BMI), !is.na(AGE), !is.na(group)) %>%
      mutate(
        Age_band = cut(
          AGE,
          breaks = seq(20, 100, by = 10),
          right = FALSE,
          include.lowest = TRUE
        )
      ) %>%
      filter(!is.na(Age_band))
    
    bmi_data <- dat %>%
      group_by(Age_band, group) %>%
      summarise(
        Mean_BMI = mean(BMI, na.rm = TRUE),
        .groups = "drop"
      )
    
    age_levels <- levels(dat$Age_band)
    
    bmi_frames <- bind_rows(
      lapply(seq_along(age_levels), function(i) {
        bmi_data %>%
          filter(Age_band %in% age_levels[1:i]) %>%
          mutate(Frame = paste("Age groups:", i))
      })
    )
    
    plot_ly(
      data = bmi_frames,
      x = ~Age_band,
      y = ~Mean_BMI,
      color = ~group,
      frame = ~Frame,
      type = "scatter",
      mode = "lines+markers",
      colors = c(
        "Placebo" = "#6e03ab",
        "Digoxin" = "#0b5cbf"
      ),
      line = list(width = 3),
      marker = list(size = 9),
      hovertemplate = paste(
        "Age group: %{x}",
        "<br>Mean BMI: %{y:.2f}",
        "<extra>%{fullData.name}</extra>"
      )
    ) %>%
      layout(
        title = "Mean BMI across age groups",
        xaxis = list(title = "Age group"),
        yaxis = list(title = "Mean BMI (kg/m²)"),
        legend = list(orientation = "h", y = -0.2)
      ) %>%
      animation_opts(
        frame = 1000,
        transition = 500,
        redraw = TRUE
      ) %>%
      animation_slider(
        currentvalue = list(prefix = "Showing ")
      ) %>%
      animation_button(
        x = 1,
        xanchor = "right",
        y = 0,
        yanchor = "top"
      )
    
  })
  
  # ----------------------------------------------------------
  # 2. DATA QUALITY
  # ----------------------------------------------------------
  
  output$expert_complete_records <- renderText({
    
    dat <- expert_data()
    n_complete <- sum(complete.cases(dat))
    
    paste0(
      format(n_complete, big.mark = ","),
      " of ",
      format(nrow(dat), big.mark = ","),
      " records are complete across all variables (",
      round(100 * n_complete / nrow(dat), 1),
      "%)."
    )
    
  })
  
  
  output$expert_missing_table <- renderTable({
    
    dat <- expert_data()
    
    data.frame(
      Variable = names(dat),
      `Missing values` = colSums(is.na(dat)),
      `Missing (%)` = round(colMeans(is.na(dat)) * 100, 2),
      check.names = FALSE
    ) %>%
      arrange(desc(`Missing values`))
    
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  
  
  output$expert_missing_plot <- renderPlotly({
    
    dat <- expert_data()
    
    missing_plot_data <- data.frame(
      Variable = names(dat),
      Missing = colSums(is.na(dat)),
      Missing_Percentage = colMeans(is.na(dat)) * 100
    ) %>%
      filter(Missing > 0) %>%
      arrange(Missing_Percentage)
    
    if (nrow(missing_plot_data) == 0) {
      
      return(
        plot_ly() %>%
          layout(
            title = "No missing values found",
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
      
    }
    
    missing_plot_data$Variable <- factor(
      missing_plot_data$Variable,
      levels = missing_plot_data$Variable
    )
    
    missing_frames <- bind_rows(
      lapply(seq_len(nrow(missing_plot_data)), function(i) {
        
        missing_plot_data[1:i, ] %>%
          mutate(Frame = paste("Variables:", i))
        
      })
    )
    
    plot_ly(
      data = missing_frames,
      x = ~Missing_Percentage,
      y = ~Variable,
      frame = ~Frame,
      type = "bar",
      orientation = "h",
      marker = list(color = "#6e03ab"),
      text = ~paste0(round(Missing_Percentage, 1), "%"),
      textposition = "outside",
      hovertemplate = paste(
        "Variable: %{y}",
        "<br>Missing: %{x:.2f}%",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "Missing values by variable",
        xaxis = list(title = "Missing values (%)"),
        yaxis = list(title = "Variable"),
        margin = list(l = 110, r = 50),
        showlegend = FALSE
      ) %>%
      animation_opts(
        frame = 800,
        transition = 400,
        redraw = TRUE
      ) %>%
      animation_slider(
        currentvalue = list(prefix = "Showing ")
      ) %>%
      animation_button(
        x = 1,
        xanchor = "right",
        y = 0,
        yanchor = "top"
      )
    
  })
  
  
  output$expert_effect_table <- renderTable({
    
    dat <- expert_data()
    
    outcome_vars <- c(
      "Mortality" = "DEATH",
      "Hospitalisation" = "HOSP",
      "Death or worsening heart failure" = "DWHF"
    )
    
    bind_rows(
      lapply(names(outcome_vars), function(label) {
        
        variable <- outcome_vars[[label]]
        
        d <- dat[
          !is.na(dat[[variable]]) & !is.na(dat$group),
          ,
          drop = FALSE
        ]
        
        placebo <- d[d$group == "Placebo", , drop = FALSE]
        digoxin <- d[d$group == "Digoxin", , drop = FALSE]
        
        n0 <- nrow(placebo)
        n1 <- nrow(digoxin)
        
        e0 <- sum(placebo[[variable]] == 1)
        e1 <- sum(digoxin[[variable]] == 1)
        
        p0 <- if (n0 > 0) e0 / n0 else NA_real_
        p1 <- if (n1 > 0) e1 / n1 else NA_real_
        
        rd_ci <- tryCatch(
          prop.test(
            c(e1, e0),
            c(n1, n0),
            correct = FALSE
          )$conf.int,
          error = function(e) c(NA_real_, NA_real_)
        )
        
        rr <- if (!is.na(p0) && p0 > 0) {
          p1 / p0
        } else {
          NA_real_
        }
        
        rr_ci <- if (
          e0 > 0 &&
          e1 > 0 &&
          n0 > e0 &&
          n1 > e1
        ) {
          
          se <- sqrt(
            1 / e1 - 1 / n1 +
              1 / e0 - 1 / n0
          )
          
          exp(log(rr) + c(-1, 1) * 1.96 * se)
          
        } else {
          c(NA_real_, NA_real_)
        }
        
        data.frame(
          Outcome = label,
          
          `Risk difference: Digoxin - Placebo (percentage points)` =
            round(100 * (p1 - p0), 2),
          
          `95% CI for risk difference` = paste0(
            round(100 * rd_ci[1], 2),
            " to ",
            round(100 * rd_ci[2], 2)
          ),
          
          `Relative risk` = round(rr, 3),
          
          `95% CI for relative risk` = if (all(is.finite(rr_ci))) {
            paste0(
              round(rr_ci[1], 3),
              " to ",
              round(rr_ci[2], 3)
            )
          } else {
            "Not estimable with zero cells"
          },
          
          check.names = FALSE
        )
      })
    )
    
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  
  # ----------------------------------------------------------
  # ANIMATED TREATMENT OUTCOME GRAPH
  # ----------------------------------------------------------
  
  
  output$expert_outcome_plot <- renderPlotly({
    
    dat <- expert_data()
    
    outcome_vars <- c(
      "Mortality" = "DEATH",
      "Hospitalisation" = "HOSP",
      "Death or worsening HF" = "DWHF"
    )
    
    plot_data <- bind_rows(
      lapply(names(outcome_vars), function(label) {
        
        variable <- outcome_vars[[label]]
        
        d <- dat %>%
          filter(!is.na(group), !is.na(.data[[variable]]))
        
        d %>%
          group_by(group) %>%
          summarise(
            Total = n(),
            Events = sum(.data[[variable]] == 1),
            Percentage = mean(.data[[variable]] == 1) * 100,
            .groups = "drop"
          ) %>%
          mutate(Outcome = label)
        
      })
    )
    
    plot_data$group <- factor(
      plot_data$group,
      levels = c("Placebo", "Digoxin")
    )
    
    plot_data$Outcome <- factor(
      plot_data$Outcome,
      levels = names(outcome_vars)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Outcome,
      y = ~Percentage,
      color = ~group,
      type = "bar",
      colors = c(
        "Placebo" = "#6e03ab",
        "Digoxin" = "#0b5cbf"
      ),
      text = ~paste0(
        round(Percentage, 1), "%",
        "<br>",
        Events, " / ", Total, " patients"
      ),
      textposition = "outside"
    ) %>%
      layout(
        title = "Treatment outcomes by group",
        xaxis = list(title = ""),
        yaxis = list(
          title = "Patients with outcome (%)",
          rangemode = "tozero"
        ),
        barmode = "group"
      )
    
  })
  
  
  
  
  # ----------------------------------------------------------
  # 4. STATISTICAL TESTS
  # ----------------------------------------------------------
  
  output$expert_tests_table <- renderTable({
    
    dat <- expert_data()
    
    chi_row <- function(variable, label) {
      
      d <- dat[
        !is.na(dat[[variable]]) & !is.na(dat$group),
        ,
        drop = FALSE
      ]
      
      tab <- table(d$group, d[[variable]])
      
      if (nrow(tab) < 2 || ncol(tab) < 2) {
        
        return(
          data.frame(
            Test = label,
            Statistic = NA,
            df = NA,
            `p-value` = NA,
            Interpretation = "Insufficient outcome categories",
            check.names = FALSE
          )
        )
      }
      
      test <- suppressWarnings(chisq.test(tab))
      
      data.frame(
        Test = label,
        Statistic = round(unname(test$statistic), 3),
        df = unname(test$parameter),
        `p-value` = signif(test$p.value, 4),
        Interpretation = ifelse(
          test$p.value < 0.05,
          "Evidence of an association (p < 0.05)",
          "No statistically significant association (p ≥ 0.05)"
        ),
        check.names = FALSE
      )
    }
    
    
    t_row <- function(variable, label) {
      
      d <- dat[
        !is.na(dat[[variable]]) & !is.na(dat$group),
        ,
        drop = FALSE
      ]
      
      test <- tryCatch(
        t.test(d[[variable]] ~ d$group),
        error = function(e) NULL
      )
      
      if (is.null(test)) {
        
        return(
          data.frame(
            Test = label,
            Statistic = NA,
            df = NA,
            `p-value` = NA,
            Interpretation = "Test could not be calculated",
            check.names = FALSE
          )
        )
      }
      
      data.frame(
        Test = label,
        Statistic = round(unname(test$statistic), 3),
        df = round(unname(test$parameter), 2),
        `p-value` = signif(test$p.value, 4),
        Interpretation = ifelse(
          test$p.value < 0.05,
          "Evidence of a difference (p < 0.05)",
          "No statistically significant difference (p ≥ 0.05)"
        ),
        check.names = FALSE
      )
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
    
    # Calculate p-values automatically
    test_results <- data.frame(
      Test = character(),
      P_value = numeric()
    )
    
    # 1. Mortality: Chi-square test
    mortality_data <- dat %>%
      filter(!is.na(group), !is.na(DEATH))
    
    mortality_test <- chisq.test(
      table(mortality_data$group, mortality_data$DEATH)
    )
    
    # 2. Hospitalisation: Chi-square test
    hospital_data <- dat %>%
      filter(!is.na(group), !is.na(HOSP))
    
    hospital_test <- chisq.test(
      table(hospital_data$group, hospital_data$HOSP)
    )
    
    # 3. Age: Independent t-test
    age_data <- dat %>%
      filter(!is.na(group), !is.na(AGE))
    
    age_test <- t.test(
      AGE ~ group,
      data = age_data
    )
    
    # 4. BMI: Independent t-test
    bmi_data <- dat %>%
      filter(!is.na(group), !is.na(BMI))
    
    bmi_test <- t.test(
      BMI ~ group,
      data = bmi_data
    )
    
    # Combine results
    test_results <- data.frame(
      Test = c(
        "Mortality",
        "Hospitalisation",
        "Age difference",
        "BMI difference"
      ),
      P_value = c(
        mortality_test$p.value,
        hospital_test$p.value,
        age_test$p.value,
        bmi_test$p.value
      )
    )
    
    # Classify statistical significance
    test_results <- test_results %>%
      mutate(
        Significance = ifelse(
          P_value < 0.05,
          "Significant",
          "Not significant"
        )
      )
    
    # Create p-value dot plot
    ggplot(
      test_results,
      aes(
        x = P_value,
        y = reorder(Test, P_value),
        colour = Significance
      )
    ) +
      
      geom_vline(
        xintercept = 0.05,
        linetype = "dashed",
        colour = "#b34b4b",
        linewidth = 1
      ) +
      
      geom_segment(
        aes(
          x = 0,
          xend = P_value,
          yend = reorder(Test, P_value)
        ),
        colour = "#c8d1df",
        linewidth = 1
      ) +
      
      geom_point(size = 4) +
      
      geom_text(
        aes(
          label = paste0(
            "p = ",
            format.pval(P_value, digits = 3, eps = 0.001)
          )
        ),
        nudge_x = 0.025,
        size = 3
      ) +
      
      scale_colour_manual(
        values = c(
          "Significant" = "#247a66",
          "Not significant" = "#315c9b"
        )
      ) +
      
      scale_x_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, 0.2)
      ) +
      
      labs(
        title = "Statistical Evidence",
        subtitle = "Dashed line indicates the significance threshold (p = 0.05)",
        x = "P-value",
        y = NULL,
        colour = NULL
      ) +
      
      theme_ipsum() +
      
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 5. SURVIVAL ANALYSIS READINESS CHECK
  # ----------------------------------------------------------
  
  output$expert_survival_check <- renderTable({
    
    dat <- expert_data()
    
    vars <- c(
      "DEATH",
      "DEATHDAY",
      "DWHF",
      "DWHFDAYS"
    )
    
    meanings <- c(
      "Confirm 0/1 event coding",
      "Confirm follow-up time is recorded for censored participants",
      "Confirm composite event definition",
      "Confirm follow-up time is recorded for all participants"
    )
    
    data.frame(
      Variable = vars,
      Available = vars %in% names(dat),
      
      Non_missing = vapply(
        vars,
        function(v) {
          if (v %in% names(dat)) {
            sum(!is.na(dat[[v]]))
          } else {
            NA_integer_
          }
        },
        integer(1)
      ),
      
      Meaning_to_confirm = meanings,
      check.names = FALSE
    )
    
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
}


# ============================================================
# 5. RUN THE APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)