library(tidyverse)
library(shiny)
library(shinythemes)


# load about text ---------------------------------------------------------
about <- read_file("about.html")

# yearspan of historical data
yearspan <- 25

# ui ----------------------------------------------------------------------
ui <- fluidPage(
  # theme = shinytheme("slate"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tags$div(
    class = "flex-column",
    titlePanel("RLEM hatch tool"),
    tags$em("Predict if mites are hatched, unhatched, or soon-to-hatch using current climate data."),
    HTML("<br>"),
  ),
  tabsetPanel(
    # selected = "About",
    tabPanel(
      "Estimate",
      tags$div(
        class = "flex-center",
        HTML("<br><br>"),
        tags$div(
          selectizeInput(
            inputId = "location",
            label = "",
            choices = NULL,
            # multiple = TRUE,
            selected = NULL,
          ),
          checkboxInput("irrigation_applied", "Irrigation applied this year", FALSE),
          conditionalPanel(
            condition = "input.irrigation_applied == true",
            numericInput("irrigation_amount", "Irrigation amount (mm)", value = 0),
            dateInput("irrigation_date", "Date of irrigation", value = Sys.Date())
          ),
          actionButton("submit", "Estimate hatch"),
        )
      ),
      tags$div(
        class = "flex-center",
        htmlOutput("hatchpred"),
        plotOutput("hatchplot")
      ),
      column(
        6,
      ),
    ),
    tabPanel(
      "About",
      column(
        offset = 2,
        8,
        HTML(
          '<div class="logos">
                  <img src="grdc.png" width="100" height="40"/>
                  <img src="cesar.png" width="120" height="40"/>
                  <img src="csiro.png" width="50" height="50"/>
                  <img src="unimelb.png" width="50" height="50"/>
                  <img src="dpird.png" width="120" height="40"/>

                </div>'
        ),
        HTML(about),
        tags$div(
          class = "flex-column",
          htmlOutput("hatchtabletitle"),
          tableOutput("hatchtable"),
          plotOutput("climateplot")
        )
      )
    )
  )
)
