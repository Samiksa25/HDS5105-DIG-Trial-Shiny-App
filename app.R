library(shiny)
library(bslib)

# Define UI for app that draws a histogram ----
ui <- page_navbar(
  nav_panel(title = "Overview", p("First page content.")),
  nav_panel(title = "Patients", p("Second page content.")),
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
 
  
}

shinyApp(ui = ui, server = server)