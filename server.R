library(tidyverse)
library(shiny)
library(shinybusy)
library(httr)

source("calc_hatch_function.R")
source("get_silo_data.R")

# data prepration ---------------------------------------------------------
# global function

postcode = read_csv("australian_postcodes.csv") %>%
  # head %>%
  filter(state!="NT") %>%
  mutate(loc = paste(locality, state, postcode)) %>%
  distinct(loc, lon, lat)


# yearspan of historical data
yearspan = 25

# Server ------------------------------------------------------------------
server <- function(session, input, output) {
  VALUES <- reactiveValues(
    hatch_est = tibble(x = 1),
    hatchdate = NULL,
    climdata = tibble(),
    valid_submission = FALSE
  )
  
  updateSelectizeInput(
    session,
    "location",
    choices = postcode$loc,
    selected = '',
    server = TRUE
  )
  observeEvent(input$submit,{
    message("validating location submission")
    if(input$location %in% postcode$loc){
      VALUES$valid_submission = TRUE
    } else {
      VALUES$valid_submission = FALSE
    }
  })
  
  # run simulation and update the click and marker without changing zoom and reloading
  observeEvent(input$submit, {
    if(VALUES$valid_submission) {
      message("getting climate data and estimating  hatch")
      # browser()
      xy = postcode %>%
        filter(loc == input$location) %>%
        select(lon, lat)
      
      show_modal_spinner()
      
      # run simulation
      # inputs for get_silo_data
      yearfinish = as.numeric(format(Sys.time(), "%Y"))
      yearstart = yearfinish - yearspan - 1
      longitude <- xy$lon
      latitude  <- xy$lat
      
      # get silo data
      out = get_silo_data(yearstart, yearfinish, longitude, latitude)
      RAIN = out[['RAIN']]
      TMIN = out[['TMIN']]
      TMAX = out[['TMAX']]
      silodata = out[['silodata']]
      
      # run hatch model
      hatch_est = calc_hatch(TMIN, TMAX, RAIN, longitude)
      print(hatch_est)
      years = silodata$`YYYY-MM-DD` %>%
        format("%Y") %>%
        unique() %>%
        as.numeric()
      if (length(hatch_est) < length(years))
        hatch_est = c(hatch_est, NA)
      
      VALUES$hatch_est = tibble(years, hatch_est) %>%
        mutate(hatchdate = as.Date(sprintf("%d-01-01", years)) + hatch_est -
                 1)
      
      VALUES$climdata = silodata
      
      
      remove_modal_spinner() # remove it when done
      # browser()
    }
  })

  observeEvent(input$submit,{
    
    if (!is.null(VALUES$hatch_est$hatchdate)) {
      message("updating hatch estimates")
      hatchdate = VALUES$hatch_est %>%
        filter(format(hatchdate, "%Y") == format(Sys.time(), "%Y")) %>%
        pull(hatchdate)
      VALUES$hatchdate = hatchdate
      
    }
    
    if (length(VALUES$hatchdate) == 0) {
      VALUES$hatchdate = NULL
    }

    
  })  
  
  output$hatchpred = renderText({
    if(VALUES$valid_submission){
    message("render hatch estimate text")

      ifelse(
        !is.null(VALUES$hatchdate),
        sprintf("<div id='hatchtext'>Hatch predicted on %s</div>",
                format(VALUES$hatchdate, "%d %B")),
        "Rainfall and temperature conditions required for hatching have not been met for this year to date"
      )
    }
  })


  output$climateplot <- renderPlot( {
    input$submit
    if (VALUES$valid_submission) {
      message("plot climatic data for current year")
      plottitle = ifelse(
        !is.null(VALUES$hatchdate),
        sprintf("Hatch predicted on %s",
                format(VALUES$hatchdate, "%d %B")),
        "Rainfall and temperature conditions required for hatching have not been met for this year to date"
      )

      p = VALUES$climdata %>%
        filter(format(`YYYY-MM-DD`, "%Y") == format(Sys.time(), "%Y")) %>%
        select(`YYYY-MM-DD`, min_temp, max_temp, daily_rain) %>%
        pivot_longer(-c(`YYYY-MM-DD`, daily_rain)) %>%
        ggplot(aes(`YYYY-MM-DD`, value)) +
        geom_line(aes(color = name)) +
        geom_bar(stat = "identity",
                 aes(y = daily_rain, fill = "rain"),
                 alpha = 0.2) +
        scale_fill_manual(values = "darkblue") +
        theme_classic() +
        xlab("") +
        ylab("Temperature (°C) or Rainfall (mm)") +
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
                      alpha = 0.7)
      } else{
        p
      }

    }
  })

  output$hatchtable <- renderTable({
    input$submit
    if (VALUES$valid_submission) {
    message("make historical hatch table")
      VALUES$hatch_est %>% 
        mutate(years = as.integer(years)) %>%
        arrange(desc(years)) %>% 
        mutate(date = format(hatchdate, "%d %B")) %>%
        select(Year = years, `Estimated hatch`=date)
    } else {
      NULL
    }
  })

  output$hatchplot <- renderPlot(height=200, {
    message("make hatch plot")
    input$submit
    if (VALUES$valid_submission) {
      d = VALUES$hatch_est
      cyear = format(Sys.Date(), "%Y")
      date0 = as.Date(paste0(cyear, "-01-01")) - 1

      MU = mean(d$hatch_est)
      SD = sd(d$hatch_est)

      drange = round(qnorm(c(0.025, 0.975), mean=MU, sd=SD))
      days = seq(min(drange)-20, max(drange)+20,by=1)
      df = tibble(
        prob = dnorm(days, mean=MU, sd=SD),
        date = date0  + days)

      y50 = 1.25
      q50 = tibble(
        date = date0 + qnorm(c(0.25, 0.75), mean=MU, sd=SD)) %>%
        expand(date, y = c(0, y50))

      y95 = 1.5
      q95 = tibble(
        date = date0 + qnorm(c(0.025, 0.975), mean=MU, sd=SD)) %>%
        expand(date, y = c(0, y95))


      months = seq.Date(date0+50, length.out = 6, by = "1 month")

      # conditionally show mean on plot
      if(is.null(VALUES$hatchdate)){
        mean_label = geom_text(aes(date0+MU, 0.5), color="white",
                  label=format(date0+MU,"historical average:\n%d %B"),
                  family='serif')
      } else {
        mean_label = NULL
      }
        
      ggplot(df) +
        geom_tile(aes(date, 0.5, fill = prob)) +
        mean_label +
        geom_line(data=q50, aes(x=date, y=y50)) +
        geom_line(data=q50, aes(x=date, y=y, group=date)) +
        geom_label(data=q50, aes(x = mean(date), y=y50),
                   family="serif", label.size = NA,
                   label="50%") +
        geom_point(data=q50, aes(x=date, y=y, group=date)) +
        geom_line(data=q95, aes(x=date, y=y95), linetype=2) +
        geom_line(data=q95, aes(x=date, y=y, group=date), linetype=2) +
        geom_label(data=q95, aes(x = mean(date),y=y95),
                   family="serif", label.size = NA,
                   label="95%") +

        scale_y_continuous(limits = c(-0, 2.5), expand=c(0,0)) +
        scale_x_date(date_labels = "%d %b",
                     date_breaks = "2 weeks",
                     # breaks = months,
                     # limits = c(min(months), max(months)),
                     expand=c(0,0)) +
        scale_fill_gradient(low="white", high="darkgrey") +
        # ggtitle() +
        # theme_void() +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.border = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=10, angle=45, hjust=0.5, vjust=0.5),
          axis.title.x = element_text(size=14, color="#333333"),
          axis.ticks.y = element_blank(),
          # axis.title.x = element_blank(),
          axis.title.y = element_blank()
        ) +
        guides(fill="none") +
        xlab("\nHistorical Hatch Probability")

    }
  })
  
}
