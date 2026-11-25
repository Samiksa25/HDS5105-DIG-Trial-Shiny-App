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
  nav_panel(title = "Patients", plotOutput(outputId = "age_plot")),
  nav_panel("Did it help?", p("Third page content.")),
  nav_panel("Who benefited?", p("Fourth page content.")),
  title = "DIG Trial explorer",
  sidebar = sidebar(
    checkboxGroupInput(inputId = "sex" , label = "Sex", choices = c("Male", "Female"), selected = c("Male", "Female")),
    checkboxGroupInput(inputId = "race", label = "Race", choices = c("White", "Non-white"), selected = c("White", "Non-white")),
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
    Age <- dig %>%
      ggplot( aes(x=AGE, fill=group)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 5) +
      scale_fill_manual(values=c("#6e03ab", "#0ac219")) +
      theme_ipsum() +
      labs(title = "How old were the patients?",
           x = "Age (years)",
           y = "Number of patients",
           fill = "")
    Age
  })
  
}

shinyApp(ui = ui, server = server)