library(shiny)

ui <- fluidPage(
  titlePanel("Productivity vs Wages Atlas"),
  p("BIOL 185 project scaffold. App modules will be added next."),
  tags$hr(),
  h4("Next build targets"),
  tags$ul(
    tags$li("Map view for state-level gap metrics"),
    tags$li("State line chart for indexed productivity and wages"),
    tags$li("Gap rankings and methods tab")
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
