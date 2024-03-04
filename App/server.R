library(tidyverse)
library(shiny)
library(shinybusy)
library(httr)

source("calc_hatch_function.R")
source("get_silo_data.R")

# data preparation ---------------------------------------------------------
# global function

postcode <- read_csv("australian_postcodes.csv") %>%
  # head %>%
  filter(state != "NT") %>%
  filter(lon != 0) %>%
  mutate(loc = paste(locality, state, postcode)) %>%
  distinct(loc, lon, lat)


# yearspan of historical data
yearspan <- 25
yearfinish <- as.numeric(format(Sys.time(), "%Y"))

# Server ------------------------------------------------------------------
server <- function(session, input, output) {
  VALUES <- reactiveValues(
    hatch_est = tibble(x = 1),
    hatchdate = NULL,
    climdata = tibble(),
    valid_submission = FALSE
  )

  # run simulation and update the click and marker without changing zoom and reloading
  observeEvent(input$submit, {
    message("validating location submission")
    if (input$location %in% postcode$loc) {
      VALUES$valid_submission <- TRUE
    } else {
      VALUES$valid_submission <- FALSE
    }

    if (VALUES$valid_submission) {
      message("getting climate data and estimating  hatch")
      # browser()
      xy <- postcode %>%
        filter(loc == input$location) %>%
        select(lon, lat)

      show_modal_spinner()

      # run simulation
      # inputs for get_silo_data
      yearstart <- yearfinish - yearspan - 1
      longitude <- xy$lon
      latitude <- xy$lat

      # get silo data
      out <- get_silo_data(yearstart, yearfinish, longitude, latitude)
      RAIN <- out[["RAIN"]]
      TMIN <- out[["TMIN"]]
      TMAX <- out[["TMAX"]]
      silodata <- out[["silodata"]]

      # Include irrigation if applied
      if (input$irrigation_applied && !is.null(input$irrigation_date)) {
        irrigation_date <- as.Date(input$irrigation_date)
        irrigation_amount <- input$irrigation_amount
        message(sprintf("adding %1.2f mm irrigation to %s", irrigation_amount, irrigation_date))

        # Assuming 'silodata' has a column 'YYYY-MM-DD' for dates
        # Add irrigation amount to 'daily_rain' for the specified date
        silodata <- silodata %>%
          mutate(daily_rain = if_else(`YYYY-MM-DD` == irrigation_date, daily_rain + irrigation_amount, daily_rain))

        RAIN[[yday(irrigation_date)]] <- RAIN[[yday(irrigation_date)]] + irrigation_amount
      }

      # run hatch model
      hatch_est <- calc_hatch(TMIN, TMAX, RAIN, longitude)
      print(hatch_est)
      years <- silodata$`YYYY-MM-DD` %>%
        format("%Y") %>%
        unique() %>%
        as.numeric()
      if (length(hatch_est) < length(years)) {
        hatch_est <- c(hatch_est, NA)
      }

      VALUES$hatch_est <- tibble(years, hatch_est) %>%
        mutate(hatchdate = as.Date(sprintf("%d-01-01", years)) + hatch_est -
          1)

      VALUES$climdata <- silodata


      remove_modal_spinner() # remove it when done
      # browser()
    }
  })

  observeEvent(input$submit, {
    if (!is.null(VALUES$hatch_est$hatchdate)) {
      message("updating hatch estimates")
      hatchdate <- VALUES$hatch_est %>%
        filter(format(hatchdate, "%Y") == yearfinish) %>%
        pull(hatchdate)
      VALUES$hatchdate <- hatchdate
    }

    if (length(VALUES$hatchdate) == 0) {
      VALUES$hatchdate <- NULL
    }
  })

  output$hatchpred <- renderText({
    input$submit
    if (VALUES$valid_submission) {
      message("render hatch estimate text")
      isolate({
        loc <- input$location
        if (is.null(VALUES$hatchdate)) {
          hatchstatus <- "UNHATCHED"
          hatchdate <- "Rainfall and temperature conditions not yet met"
        } else if (VALUES$hatchdate <= Sys.Date()) {
          hatchstatus <- "HATCHED"
          hatchdate <- format(VALUES$hatchdate, "%d %B %Y")
        } else if (VALUES$hatchdate > Sys.Date()) {
          hatchstatus <- "SOON TO HATCH"
          hatchdate <- format(VALUES$hatchdate, "%d %B %Y")
        } else {
          return("ERROR")
        }


        template <- "
          <hr>
          <div id='prediction'>
            <div id='hatchtext'>%s\n</div>
            <div id='predictiontext'>
              <div>Predicted hatch date:</div>
              <div>%s</div>
            <div>%s</div>
            </div>
          </div>
          <hr>"

        sprintf(template, hatchstatus, hatchdate, loc)
      })
    }
  })

  output$climateplot <- renderPlot({
    input$submit
    if (VALUES$valid_submission) {
      message("plot climatic data for current year")
      plottitle <- ifelse(
        !is.null(VALUES$hatchdate),
        sprintf(
          "For %s hatch predicted on %s",
          format(VALUES$hatchdate, "%Y"), format(VALUES$hatchdate, "%d %B")
        ),
        "Rainfall and temperature conditions required for hatching have not been met for this year to date"
      )
      rain_df <- VALUES$climdata %>%
        filter(format(`YYYY-MM-DD`, "%Y") == yearfinish) %>%
        select(`YYYY-MM-DD`, daily_rain)

      p <- VALUES$climdata %>%
        filter(format(`YYYY-MM-DD`, "%Y") == yearfinish) %>%
        select(`YYYY-MM-DD`, min_temp, max_temp) %>%
        pivot_longer(-c(`YYYY-MM-DD`)) %>%
        ggplot(aes(`YYYY-MM-DD`, value)) +
        geom_line(aes(color = name)) +
        geom_bar(
          data = rain_df,
          stat = "identity",
          aes(x = `YYYY-MM-DD`, y = daily_rain, fill = "rain"),
          alpha = 0.2
        ) +
        scale_fill_manual(values = "darkblue") +
        theme_classic() +
        xlab("") +
        ylab(paste0("Temperature (°C) or Rainfall", ifelse(input$irrigation_applied, " and irrigation", ""), " (mm)")) +
        ggtitle(plottitle) +
        scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
        scale_y_continuous(expand = c(0, 0)) +
        guides(color = guide_legend(""), fill = guide_legend("")) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          title = element_text(face = "bold", size = 11)
        )

      # only mark hatch if it has occurred
      if (length(VALUES$hatchdate) == 1) {
        p + geom_text(aes(VALUES$hatchdate, 20),
          label = "egg\nhatch\n↓",
          alpha = 0.7
        )
      } else {
        p
      }
    }
  })

  output$hatchtabletitle <- renderText({
    if (VALUES$valid_submission) {
      message("render table title")
      isolate({
        loc <- input$location
      })
      template <- "
          <p>
            <b>Detailed hatch information for %s\n</b>
          </p>
      <br><br>"

      sprintf(template, loc)
    } else {
      "<p><b>Select and submit location in 'Estimate' tab to show details here</b></p>"
    }
  })


  output$hatchtable <- renderTable({
    input$submit
    if (VALUES$valid_submission) {
      message("make historical hatch table")
      VALUES$hatch_est %>%
        mutate(years = as.integer(years)) %>%
        arrange(desc(years)) %>%
        filter(hatch_est != 0) %>%
        mutate(date = format(hatchdate, "%d %B")) %>%
        select(Year = years, `Estimated hatch` = date)
    } else {
      NULL
    }
  })

  output$hatchplot <- renderPlot(height = 240, {
    input$submit
    if (VALUES$valid_submission) {
      message("make hatch plot")
      d <- VALUES$hatch_est
      cyear <- format(Sys.Date(), "%Y")
      date0 <- as.Date(paste0(cyear, "-01-01")) - 1

      MU <- mean(d$hatch_est)
      SD <- sd(d$hatch_est)

      drange <- round(qnorm(c(0.025, 0.975), mean = MU, sd = SD))
      days <- seq(min(drange) - 20, max(drange) + 20, by = 1)
      df <- tibble(
        prob = dnorm(days, mean = MU, sd = SD),
        date = date0 + days
      )

      y50 <- 1.25
      q50 <- tibble(
        date = date0 + qnorm(c(0.25, 0.75), mean = MU, sd = SD)
      ) %>%
        expand(date, y = c(0, y50))

      y95 <- 1.5
      q95 <- tibble(
        date = date0 + qnorm(c(0.025, 0.975), mean = MU, sd = SD)
      ) %>%
        expand(date, y = c(0, y95))


      months <- seq.Date(date0 + 50, length.out = 6, by = "1 month")

      # conditionally show mean on plot
      if (is.null(VALUES$hatchdate)) {
        mean_label <- geom_text(aes(date0 + MU, 0.5),
          color = "white",
          label = format(date0 + MU, "historical average:\n%d %B"),
          family = "serif"
        )
      } else {
        mean_label <- NULL
      }

      ggplot(df) +
        geom_tile(aes(date, 0.5, fill = prob)) +
        mean_label +
        geom_line(data = q50, aes(x = date, y = y50)) +
        geom_line(data = q50, aes(x = date, y = y, group = date)) +
        geom_label(
          data = q50, aes(x = mean(date), y = y50),
          family = "serif", label.size = NA,
          label = "50%"
        ) +
        geom_point(data = q50, aes(x = date, y = y, group = date)) +
        geom_line(data = q95, aes(x = date, y = y95), linetype = 2) +
        geom_line(data = q95, aes(x = date, y = y, group = date), linetype = 2) +
        geom_label(
          data = q95, aes(x = mean(date), y = y95),
          family = "serif", label.size = NA,
          label = "95%"
        ) +
        scale_y_continuous(limits = c(-0, 1.75), expand = c(0, 0)) +
        scale_x_date(
          date_labels = "%d %b",
          date_breaks = "2 weeks",
          # breaks = months,
          # limits = c(min(months), max(months)),
          expand = c(0, 0)
        ) +
        scale_fill_gradient(low = "white", high = "darkgrey") +
        ggtitle("\nHistorical Hatch Probability") +
        # theme_void() +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.border = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10, angle = 45, hjust = 0.5, vjust = 0.5),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          # axis.title.x = element_blank(),
          axis.title.y = element_blank()
        ) +
        guides(fill = "none")
    }
  })

  updateSelectizeInput(
    session,
    "location",
    choices = postcode$loc,
    selected = "",
    server = TRUE,
    options = list(placeholder = "Enter suburb/postcode")
  )
}
