library(shiny)

ui <- fluidPage(
  actionButton("do", "Click Me")
)

server <- function(input, output, session) {
  observeEvent(input$do, {
   print('hello')
  })
}

shinyApp(ui, server)

