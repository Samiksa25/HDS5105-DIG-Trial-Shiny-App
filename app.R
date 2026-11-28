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
library(shinyWidgets)
library(shinyjs)


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

ui <- page_fillable(
  title = "DIG Trial Explorer",
  theme = bs_theme(
    version = 5,
    bg = "#0b1224",
    fg = "#e5eaf5",
    primary = "#35bdf2",
    secondary = "#bd82ef"
  ),
  
  navset_pill_list(
    id = "main_nav",
    
    nav_panel(
      "Overview",
      div(
        class = "dashboard-content",
        h2("Trial at a glance"),
        uiOutput("overview_kpis"),
        
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Baseline"),
            tableOutput("overview_baseline"),
            p(class = "small-note",
              "A comparison of patient characteristics at baseline.")
          ),
          card(
            card_header("Outcomes"),
            plotOutput("overview_outcomes_plot", height = "190px"),
            p(class = "small-note",
              "Risk differences compare Digoxin with Placebo.")
          )
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Survival"),
            p(
              "The survival data-readiness checks are available on the Survival page.",
              class = "small-note"
            )
          ),
          card(
            card_header("Subgroups"),
            p(
              "Compare treatment outcomes by age or diabetes on the Subgroups page.",
              class = "small-note"
            )
          )
        ),
        
        card(
          card_header("Safety"),
          p(
            "Safety event comparisons are available on the Safety page.",
            class = "small-note"
          )
        ),
        
        div(
          class = "dashboard-legend",
          span(class = "legend-blue"), " Digoxin",
          span(class = "legend-purple"), " Placebo"
        )
      )
    ),
    
    nav_panel(
      "Baseline",
      div(
        class = "dashboard-content",
        h2("Baseline characteristics"),
        card(
          card_header("Patient characteristics"),
          plotlyOutput("expert_demographic_plot", height = "380px")
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Age distribution"),
            plotlyOutput("expert_age_plot", height = "300px")
          ),
          card(
            card_header("BMI distribution"),
            plotlyOutput("expert_bmi_plot", height = "300px")
          )
        )
      )
    ),
    
    nav_panel(
      "Outcomes",
      div(
        class = "dashboard-content",
        h2("Treatment outcomes"),
        card(
          card_header("Treatment outcomes by group"),
          plotlyOutput("expert_outcome_plot", height = "380px")
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Relative risk and risk difference"),
            tableOutput("expert_effect_table")
          ),
          card(
            card_header("Statistical test results"),
            tableOutput("expert_tests_table")
          )
        ),
        card(
          card_header("P-value comparison"),
          plotOutput("expert_pvalue_plot", height = "280px")
        )
      )
    ),
    
    nav_panel(
      "Survival",
      div(
        class = "dashboard-content",
        h2("Survival analysis"),
        card(
          card_header("Survival data readiness"),
          tableOutput("expert_survival_check")
        ),
        p(
          class = "small-note",
          "Kaplan-Meier and Cox regression analyses should only be added ",
          "after the event and censoring definitions have been verified."
        )
      )
    ),
    
    nav_panel(
      "Subgroups",
      div(
        class = "dashboard-content",
        h2("Subgroup analysis"),
        selectInput(
          "benefit_chart",
          "Select subgroup:",
          choices = c("By age", "By diabetes")
        ),
        card(
          card_header("Treatment outcomes across subgroups"),
          plotlyOutput("benefit_plot", height = "380px")
        )
      )
    ),
    
    nav_panel(
      "Safety",
      div(
        class = "dashboard-content",
        h2("Safety outcomes"),
        card(
          card_header("Adverse events by treatment group"),
          plotOutput("overview_safety_plot", height = "300px")
        ),
        card(
          card_header("Data quality"),
          plotlyOutput("expert_missing_plot", height = "300px")
        )
      )
    )
  ),
  
  header = tags$style(HTML("
    body {
      background: #0b1224;
      color: #e5eaf5;
      font-family: Arial, sans-serif;
    }

    .bslib-page-fill {
      padding: 14px;
    }

    .navset-pill-list {
      display: flex;
      gap: 20px;
      min-height: calc(100vh - 28px);
      background: #0b1224;
    }

    .navset-pill-list > .nav {
      flex: 0 0 245px;
      background: #070e1d;
      border-radius: 14px 0 0 14px;
      padding: 105px 10px 20px;
      position: relative;
    }

    .navset-pill-list > .nav::before {
      content: 'DIG Trial Explorer';
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      min-height: 85px;
      padding: 28px 18px;
      color: white;
      font-size: 20px;
      font-weight: 700;
      border-bottom: 1px solid #25334e;
    }

    .navset-pill-list > .nav .nav-link {
      color: #9aa8c2;
      border-radius: 8px;
      padding: 15px 20px;
      margin-bottom: 5px;
      font-size: 16px;
    }

    .navset-pill-list > .nav .nav-link.active {
      background: #172746;
      color: #ffffff;
      border-left: 4px solid #35bdf2;
    }

    .navset-pill-list > .tab-content {
      flex: 1;
      min-width: 0;
      padding: 12px 20px;
      background: #0b1224;
    }

    .dashboard-content {
      max-width: 1500px;
      margin: 0 auto;
      padding: 12px;
    }

    .dashboard-content h2 {
      font-size: 25px;
      font-weight: 750;
      color: #f1f5ff;
      margin: 5px 0 18px;
    }

    .card {
      background: #121e38;
      border: 1px solid #263757;
      border-radius: 18px;
      color: #e5eaf5;
      margin-bottom: 16px;
      box-shadow: none;
    }

    .card-header {
      background: transparent;
      color: #f1f5ff;
      border-bottom: none;
      font-size: 17px;
      font-weight: 700;
      padding: 16px 18px 5px;
    }

    .card-body {
      padding: 14px 18px 18px;
    }

    .small-note {
      color: #94a3bd;
      font-size: 12px;
      margin-top: 8px;
    }

    .form-label {
      color: #aab8d0;
    }

    .form-select {
      background-color: #172746;
      color: white;
      border-color: #344767;
    }

    table {
      color: #dce5f5 !important;
    }

    .table {
      --bs-table-bg: transparent;
      --bs-table-color: #dce5f5;
      --bs-table-border-color: #263757;
    }

    .dashboard-legend {
      color: #9aa8c2;
      padding: 5px 0 20px;
    }

    .legend-blue, .legend-purple {
      display: inline-block;
      width: 12px;
      height: 12px;
      border-radius: 50%;
      margin-left: 8px;
    }

    .legend-blue {
      background: #35bdf2;
    }

    .legend-purple {
      background: #bd82ef;
    }

    .overview-kpi-row {
      display: grid;
      grid-template-columns: repeat(3, minmax(0, 1fr));
      gap: 16px;
      margin-bottom: 18px;
    }

    .overview-kpi {
      background: #121e38;
      border: 1px solid #263757;
      border-radius: 18px;
      padding: 20px;
      min-height: 120px;
    }

    .overview-kpi-title {
      color: #aab8d0;
      font-size: 15px;
    }

    .overview-kpi-value {
      color: #35bdf2;
      font-size: 34px;
      font-weight: 750;
      margin-top: 3px;
    }

    .overview-kpi-placebo {
      color: #bd82ef;
      font-size: 15px;
    }

    @media(max-width: 850px) {
      .navset-pill-list {
        display: block;
      }

      .navset-pill-list > .nav {
        width: 100%;
        padding: 85px 10px 10px;
        border-radius: 14px;
      }

      .navset-pill-list > .tab-content {
        padding: 10px 0;
      }

      .overview-kpi-row {
        grid-template-columns: 1fr;
      }
    }
  "))
)


# 4. SERVER
# ============================================================

server <- function(input, output, session) {
  
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
  
  observeEvent(input$mode, {
    if (input$mode == "Expert") {
      nav_hide("main_nav", "Overview")
      nav_hide("main_nav", "Patients")
      nav_hide("main_nav", "Did it help?")
      nav_hide("main_nav", "Who benefited?")
      nav_show("main_nav", "Expert analysis")
      nav_select("main_nav", "Expert analysis")
      toggle_sidebar("filters", open = FALSE)
      shinyjs::hide(selector = ".collapse-toggle")
    } else {
      nav_show("main_nav", "Overview")
      nav_show("main_nav", "Patients")
      nav_show("main_nav", "Did it help?")
      nav_show("main_nav", "Who benefited?")
      nav_hide("main_nav", "Expert analysis")
      nav_select("main_nav", "Overview")
      shinyjs::show(selector = ".collapse-toggle")
    }
  })
  
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "sex", selected = c(1, 2))
    updateCheckboxGroupInput(session, "race", selected = c(1, 2))
    updateSliderInput(session, "age", value = c(21, 90))
  })
  observeEvent(input$go_page, {
    nav_select("main_nav", input$go_page)
  })
  
  observeEvent(input$go_expert, {
    updateRadioGroupButtons(session, "mode", selected = "Expert")
  })
  
  # ----------------------------------------------------------
  # PATIENTS
  # ----------------------------------------------------------
  
  
  output$age_plot <- renderPlotly({
    filtered <- filtered_data()
    if (input$patient_chart == "Age") {
      Age <- filtered %>%
        ggplot( aes(x=AGE, fill=group)) +
        geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 5) +
        scale_fill_manual(values=c("#8ec5ff", "#1e3a8a")) +
        theme_ipsum() +
        coord_cartesian(xlim = c(20, 95)) +
        labs(title = "How old were the patients?",
             x = "Age (years)",
             y = "Number of patients",
             fill = "")
      ggplotly(Age) %>% style(hovertemplate = "Age: %{x}<br>Patients: %{y}<extra>%{fullData.name}</extra>")
    } else if (input$patient_chart == "BMI"){
      Bmi <- filtered %>%
        ggplot( aes(x=BMI, fill=group)) +
        geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 2, na.rm = TRUE) +
        scale_fill_manual(values=c("#8ec5ff", "#1e3a8a")) +
        theme_ipsum() +
        coord_cartesian(xlim = c(10, 60)) +
        labs(title = "Body weight of the patients",
             x = "BMI (kg/m²)",
             y = "Number of patients",
             fill = "")
      ggplotly(Bmi) %>% style(hovertemplate = "BMI: %{x}<br>Patients: %{y}<extra>%{fullData.name}</extra>")
    } else if (input$patient_chart == "Heart rate"){ 
      HR <- filtered %>%
        ggplot( aes(x=HEARTRTE, fill=group)) +
        geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 5, na.rm = TRUE) +
        scale_fill_manual(values=c("#8ec5ff", "#1e3a8a")) +
        theme_ipsum() +
        coord_cartesian(xlim = c(30, 160)) +
        labs(title = "What were the patients' heart rates?",
             x = "Heart rate (beats/min)",
             y = "Number of patients",
             fill = "")
      ggplotly(HR) %>% style(hovertemplate = "Heart rate: %{x} bpm<br>Patients: %{y}<extra>%{fullData.name}</extra>")
    }else if (input$patient_chart == "Blood pressure"){
      BP <- ggplot(filtered) +
        geom_histogram( aes(x = SYSBP, y = after_stat(count), fill= group), color="#e9ecef",alpha = 0.6, position = 'identity', binwidth = 10, na.rm = TRUE) +
        geom_histogram( aes(x = DIABP, y = -after_stat(count), fill= group), color="#e9ecef", alpha = 0.6, position = 'identity', binwidth = 10, na.rm = TRUE) +
        theme_ipsum() +
        labs(title = "Blood pressure was almost identical in both groups",
             x = "Blood pressure(mmHg)",
             y = "Number of patients",
             fill = "") +
        scale_fill_manual(values = c("#8ec5ff", "#1e3a8a")) +
        annotate("text", x = 190, y = 300, label = "Systolic") +
        annotate("text", x = 40, y = -300, label = "Diastolic") +
        scale_y_continuous(labels = abs)
      BP
    }else if (input$patient_chart == "Race"){
      filtered$race <- factor(filtered$RACE, levels = c(1, 2), labels = c("White", "Non-white"))
      Race <- ggplot(filtered, aes(x = race, fill = group)) +
        geom_bar(position = "dodge", color = "#e9ecef", width = 0.4) +
        coord_flip() +
        scale_fill_manual(values = c("#8ec5ff", "#1e3a8a")) +
        theme_ipsum() + theme(legend.position = "bottom") +
        guides(fill = guide_legend(reverse = TRUE)) +
        labs(title = "Ethnic background of the patients",
             x = "",
             y = "Number of patients",
             fill = "")
      ggplotly(Race) %>% style(hovertemplate = "%{y}<br>Patients: %{x}<extra>%{fullData.name}</extra>")
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
        scale_fill_manual(values = c("#8ec5ff", "#1e3a8a")) +
        theme_ipsum() +
        theme(legend.position = "bottom") +
        guides(fill = guide_legend(reverse = TRUE)) +
        labs(title = "Medical history of the patients",
             x = "",
             y = "Percentage of patients (%)",
             fill = "")
      ggplotly(History) %>% style(hovertemplate = "%{y}<br>%{x:.1f}% of patients<extra>%{fullData.name}</extra>")  }  
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
  
  output$effect_plot <- renderPlotly({
    
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
      
      ggplotly(ggplot(
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
          values = c("#8ec5ff", "#1e3a8a")
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
        )) %>% style(hovertemplate = "%{x}<br>%{y:.1f}% of patients<extra>%{fullData.name}</extra>")
      
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
      
      ggplotly(ggplot(
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
          values = c("#8ec5ff", "#1e3a8a")
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
        )) %>% style(hovertemplate = "%{x}<br>%{y:.1f}% of patients<extra>%{fullData.name}</extra>")
    }
    
  })
  
  
  # ----------------------------------------------------------
  # WHO BENEFITED?
  # ----------------------------------------------------------
  
  output$benefit_plot <- renderPlotly({
    
    filtered <- filtered_data()
    
    if (input$benefit_chart == "By age") {
      
      benefit <- filtered %>%
        group_by(age_group, group, .drop = FALSE) %>%
        summarise(
          percent = mean(DWHF, na.rm = TRUE) * 100,
          .groups = "drop"
        )
      
      ggplotly(ggplot(
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
          values = c("#8ec5ff", "#1e3a8a")
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
        )) %>% style(hovertemplate = "%{x}<br>%{y:.1f}% of patients<extra>%{fullData.name}</extra>")
      
    } else if (input$benefit_chart == "By diabetes") {
      
      benefit_diab <- filtered %>%
        group_by(diabetes, group) %>%
        summarise(
          percent = mean(DWHF, na.rm = TRUE) * 100,
          .groups = "drop"
        )
      
      ggplotly(ggplot(
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
          values = c("#8ec5ff", "#1e3a8a")
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
        )) %>% style(hovertemplate = "%{x}<br>%{y:.1f}% of patients<extra>%{fullData.name}</extra>")
    }
    
  })
  
  
  # ==========================================================
  # EXPERT ANALYSIS
  # ==========================================================
  
  expert_data <- reactive({ dig })
  
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
        d <- dat %>% filter(!is.na(group), !is.na(.data[[variable]]))
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
    
    plot_data$group <- factor(plot_data$group, levels = c("Placebo", "Digoxin"))
    plot_data$Outcome <- factor(plot_data$Outcome, levels = names(outcome_vars))
    
    plot_ly(
      data = plot_data,
      x = ~group,
      y = ~Percentage,
      frame = ~Outcome,
      color = ~group,
      type = "bar",
      colors = c("Placebo" = "#6e03ab", "Digoxin" = "#0b5cbf"),
      text = ~paste0(round(Percentage, 1), "%<br>", Events, " / ", Total, " patients"),
      textposition = "outside",
      hovertemplate = paste(
        "Treatment: %{x}",
        "<br>Patients with outcome: %{y:.1f}%",
        "<br>%{text}",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "Treatment outcomes by group",
        xaxis = list(title = "Treatment group"),
        yaxis = list(title = "Patients with outcome (%)", rangemode = "tozero"),
        barmode = "group",
        showlegend = FALSE
      ) %>%
      animation_opts(frame = 900, transition = 350, redraw = TRUE) %>%
      animation_slider(currentvalue = list(prefix = "Outcome: ")) %>%
      animation_button(x = 1, xanchor = "right", y = 0, yanchor = "top")
    
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
    
    dat
    
    # ==========================================================
    # OVERVIEW PAGE
    # ==========================================================
    
    output$overview_kpis <- renderUI({
      dat <- expert_data()
      
      get_rate <- function(variable, group_name) {
        if (!variable %in% names(dat)) return(NA_real_)
        
        d <- dat[
          !is.na(dat[[variable]]) &
            !is.na(dat$group) &
            as.character(dat$group) == group_name,
          ,
          drop = FALSE
        ]
        
        if (nrow(d) == 0) return(NA_real_)
        mean(d[[variable]] == 1, na.rm = TRUE) * 100
      }
      
      make_card <- function(title, digoxin, placebo) {
        div(
          class = "overview-kpi",
          div(class = "overview-kpi-title", title),
          div(
            class = "overview-kpi-value",
            if (is.na(digoxin)) "N/A" else paste0(round(digoxin, 1), "%")
          ),
          div(
            class = "overview-kpi-placebo",
            if (is.na(placebo)) {
              "Placebo: N/A"
            } else {
              paste0("Placebo: ", round(placebo, 1), "%")
            }
          )
        )
      }
      
      div(
        class = "overview-kpi-row",
        make_card(
          "Death or worsening heart failure",
          get_rate("DWHF", "Digoxin"),
          get_rate("DWHF", "Placebo")
        ),
        make_card(
          "All-cause death",
          get_rate("DEATH", "Digoxin"),
          get_rate("DEATH", "Placebo")
        ),
        make_card(
          "Digoxin toxicity",
          get_rate("DIG", "Digoxin"),
          get_rate("DIG", "Placebo")
        )
      )
    })
    
    
    output$overview_baseline <- renderTable({
      dat <- expert_data()
      
      dat %>%
        group_by(group) %>%
        summarise(
          Patients = n(),
          `Mean age` = round(mean(AGE, na.rm = TRUE), 1),
          `Female (%)` = round(mean(SEX == 2, na.rm = TRUE) * 100, 1),
          `Mean BMI` = round(mean(BMI, na.rm = TRUE), 1),
          .groups = "drop"
        )
    }, striped = FALSE, bordered = FALSE)
    
    
    output$overview_outcomes_plot <- renderPlot({
      dat <- expert_data()
      
      outcome_vars <- c(
        "Death" = "DEATH",
        "Any admission" = "HOSP",
        "Death or worsening HF" = "DWHF"
      )
      
      outcome_data <- bind_rows(lapply(names(outcome_vars), function(label) {
        variable <- unname(outcome_vars[[label]])
        if (!variable %in% names(dat)) return(NULL)
        
        d <- dat[
          !is.na(dat[[variable]]) & !is.na(dat$group),
          ,
          drop = FALSE
        ]
        
        placebo <- d[as.character(d$group) == "Placebo", , drop = FALSE]
        digoxin <- d[as.character(d$group) == "Digoxin", , drop = FALSE]
        
        if (nrow(placebo) == 0 || nrow(digoxin) == 0) return(NULL)
        
        p0 <- mean(placebo[[variable]] == 1, na.rm = TRUE)
        p1 <- mean(digoxin[[variable]] == 1, na.rm = TRUE)
        
        data.frame(Outcome = label, Difference = (p1 - p0) * 100)
      }))
      
      validate(need(nrow(outcome_data) > 0, "Outcome variables are not available."))
      
      ggplot(outcome_data, aes(x = Difference, y = Outcome)) +
        geom_vline(
          xintercept = 0,
          linetype = "dashed",
          colour = "#687895"
        ) +
        geom_segment(
          aes(x = 0, xend = Difference, yend = Outcome),
          colour = "#526783",
          linewidth = 1.5
        ) +
        geom_point(colour = "#35bdf2", size = 4) +
        theme_minimal() +
        theme(
          text = element_text(colour = "#dce5f5"),
          axis.title = element_blank(),
          axis.text = element_text(colour = "#aab8d0"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "#121e38", colour = NA),
          panel.background = element_rect(fill = "#121e38", colour = NA)
        ) +
        labs(x = NULL, y = NULL)
    })
    
    
    output$overview_safety_plot <- renderPlot({
      dat <- expert_data()
      
      safety_vars <- c(
        "Digoxin toxicity" = "DIG",
        "Ventricular arrhythmia" = "VENA",
        "Heart attack" = "MI"
      )
      
      safety_data <- bind_rows(lapply(names(safety_vars), function(label) {
        variable <- unname(safety_vars[[label]])
        if (!variable %in% names(dat)) return(NULL)
        
        dat %>%
          filter(!is.na(.data[[variable]]), !is.na(group)) %>%
          group_by(group) %>%
          summarise(
            Percentage = mean(.data[[variable]] == 1, na.rm = TRUE) * 100,
            .groups = "drop"
          ) %>%
          mutate(Event = label)
      }))
      
      validate(need(
        nrow(safety_data) > 0,
        "No matching safety variables were found. Check DIG, VENA and MI in the dataset."
      ))
      
      ggplot(
        safety_data,
        aes(x = Percentage, y = Event, fill = group)
      ) +
        geom_col(position = position_dodge(width = 0.7), width = 0.55) +
        scale_fill_manual(
          values = c("Digoxin" = "#35bdf2", "Placebo" = "#bd82ef")
        ) +
        theme_minimal() +
        theme(
          text = element_text(colour = "#dce5f5"),
          axis.text = element_text(colour = "#aab8d0"),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "#121e38", colour = NA),
          panel.background = element_rect(fill = "#121e38", colour = NA),
          legend.position = "bottom"
        ) +
        labs(fill = NULL, x = NULL, y = NULL)
    })
    
    <- expert_data()
    
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
