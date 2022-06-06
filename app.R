  library(tidyverse)
library(shiny)
library(shinythemes)
library(leaflet)
library(shinybusy)
library(maptools)
library(rgeos)
library(sf)
library(httr)

source("calc_hatch_function.R")
source("get_silo_data.R")

# data prepration ---------------------------------------------------------
# global function

data("wrld_simpl", package = "maptools")
auss_bound <- subset(wrld_simpl, NAME == "Australia") %>% 
  sf::st_as_sf() %>% 
  sf::st_set_crs(4326) # (CRS("+init=epsg:4326"))
# return true or false
xy_in_aus <- function(long, lat){
  data.frame(x = long, y = lat) %>% 
    sf::st_as_sf(coords = 1:2, crs = 4326) %>% 
    sf::st_intersection(sf::st_geometry(auss_bound)) %>% 
    nrow() != 0
}

# yearspan of historical data 
yearspan = 25
file.path("C:.Users")
# ui ----------------------------------------------------------------------
ui <- shinyUI(
  navbarPage("", 
             selected = "RLEM hatch date estimate", 
             theme = shinytheme("cosmo"), # slate
             
             
             tabPanel("RLEM hatch date estimate",
                      HTML("<i>Draft version</i>"),
    
                      # add a leaflet map
                      h3("Zoom and select your location on the map"),
                      leafletOutput("smap", height = 300, width=400),
                      textOutput("checklatlong"),
                      h3(sprintf("Hatch prediction for current year")),
                      plotOutput("climateplot"),
                      h3(sprintf("Hatching probablity across past %d years", 
                                 yearspan)),
                      tableOutput("hatchplot")
                      
             ),
             tabPanel("About",column(4, HTML(
"This web tool was developed by James Maino through a GRDC investment (CES2010-001RXT) with contributions from Cesar Australia, the University of Melbourne, and the Department of Primary Industries and Regional Development. The tool is based on past research by McDonald et al. (2015) on rainfall and temperature requirements for hatching of over-summering eggs of the redlegged earth mite. See McDonald et al. (2015) for details on the underlying algorithm. Climate data is provided through the Queensland government's SILO database which makes gridded Australian climate data available from 1889 to yesterday. <br><br>

Possible future developments may include: <br>
-map based predictions for the hatched, recently hatched, and unhatched status of RLEM for the current date. <br>
-addition of irrigation
-comparison of current climate against historical climate <br>
-further validation through field collections <br>
-display rainfall temperature and rainfall triggers on plot <br><br>

<b>References</b> <br>
Grains Research and Development investment CES2010-001RXT. Future options for the control of the Redlegged earth mite in Australian grain crops. <br><br>

McDonald, G., Umina, P.A., Macfadyen, S., Mangano, P. & Hoffmann, A. Predicting the timing of first generation egg hatch for the pest redlegged earth mite Halotydeus destructor (Acari: Penthaleidae). Exp Appl Acarol 65, 259–276 (2015). https://doi.org/10.1007/s10493-014-9876-x"
             )))

  )
)


# Server ------------------------------------------------------------------
server <- function(session, input, output){
  
  values <- reactiveValues(hatch_est = tibble(), climdata=tibble())
  
  # add the small map
  output$smap <- renderLeaflet({
    isolate({
      leaflet(options = leafletOptions(zoomControlPosition = "topright")) %>%
        setView(lng = 135.51, lat = -25.98, zoom = 3) %>%
        addTiles()
    })
  })
  # run simulation and update the click and marker without changing zoom and reloading
  observeEvent(input$smap_click, {
    if(xy_in_aus(input$smap_click$lng, input$smap_click$lat)){
      
      show_modal_spinner() 
      
      # run simulation
      # inputs for get_silo_data
      yearfinish = as.numeric(format(Sys.time(), "%Y"))
      yearstart = yearfinish - yearspan - 1
      longitude <- as.numeric(input$smap_click$lng)	
      latitude  <- as.numeric(input$smap_click$lat)
      
      # get silo data
      out = get_silo_data(yearstart, yearfinish,longitude, latitude)
      RAIN = out[['RAIN']]
      TMIN = out[['TMIN']]
      TMAX = out[['TMAX']]
      silodata = out[['silodata']]
      
      # run hatch model
      hatch_est = calc_hatch(TMIN, TMAX, RAIN, longitude)
      years = silodata$`YYYY-MM-DD` %>% 
        format("%Y") %>% 
        unique() %>% 
        as.numeric()
      
      if(length(hatch_est) < length(years)) hatch_est = c(hatch_est, NA)
      
      values$hatch_est = tibble(years, hatch_est) %>% 
        mutate(hatch_date = as.Date(sprintf("%d-01-01", years)) + hatch_est-1)
      
      values$climdata = silodata
      
      # update map with marker
      leafletProxy("smap") %>%
        clearMarkers() %>% 
        addMarkers(lng = input$smap_click$lng, lat = input$smap_click$lat, 
                   label = "Climate data extracted for selected location")
      
      remove_modal_spinner() # remove it when done
      
    }else{
      leafletProxy("smap") %>%
        clearMarkers()
    }
  })
  # show coordinates with click
  output$checklatlong <- renderText({ 
    if(!is.null(input$smap_click)){
      if(!xy_in_aus(input$smap_click$lng, input$smap_click$lat)){
        "Selected location is not in Australia"
      } else{
        NULL
      }
    }
  })
  
  output$climateplot <- renderPlot({
    if(!is.null(input$smap_click) && 
       xy_in_aus(input$smap_click$lng, input$smap_click$lat)){
      # browser()
      hatchdate = values$hatch_est %>% 
        filter(format(hatch_date, "%Y") == format(Sys.time(), "%Y")) %>% 
        pull(hatch_date)
      
      plottitle = ifelse(length(hatchdate)==1, 
                         sprintf("Hatch predicted on %s", 
                                format(hatchdate, "%d %B")), 
                         "Rainfall and temperature conditions required for hatching have not been met for this year to date")
      
      p = values$climdata %>%
        filter(format(`YYYY-MM-DD`, "%Y") == format(Sys.time(), "%Y")) %>% 
        select(`YYYY-MM-DD`, min_temp, max_temp, daily_rain) %>%
        pivot_longer(-c(`YYYY-MM-DD`, daily_rain)) %>%
        ggplot(aes(`YYYY-MM-DD`, value)) + 
        geom_line(aes(color=name)) + 
        geom_bar(stat = "identity", aes(y=daily_rain, fill = "rain"), 
                 alpha = 0.2) +
        scale_fill_manual(values = "darkblue") + 
        theme_classic() +
        xlab("") +
        ylab("Temperature (°C) or Rainfall (mm)") +
        ggtitle(plottitle) +
        scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
        scale_y_continuous(expand=c(0,0)) + 
        guides(color = guide_legend(""), fill = guide_legend(""))+
        theme(axis.text.x = element_text(angle=45, hjust = 1), 
              legend.position = "bottom",
              title = element_text(face = "bold", size = 11))
      
      # only mark hatch if it has occurred
      if(length(hatchdate)==1){
        p + geom_text(aes(hatchdate, 20),
                      label = "egg\nhatch\n↓", 
                      alpha = 0.7)
      }else{p}
      
    }else{
      ggplot() + 
        ggtitle("Click on map to select location") + 
        theme_void() + 
        theme(title = element_text(face = "bold", size = 11))
    }
  })
  
  output$hatchplot <- renderTable({
    if(!is.null(input$smap_click) && 
       xy_in_aus(input$smap_click$lng, input$smap_click$lat)){
      # browser()
      quants = values$hatch_est %>%
        filter(hatch_est != 0) %>%
        pull(hatch_est) %>%
        quantile(1:10 / 10, na.rm=T)
      tibble(`Chance of hatch by given date` = names(quants), 
             Date = as.Date("2021-01-01") + quants - 1) %>% 
        mutate(Date = format(Date, "%d %B"))
    }else{
      tibble(`Click on map to select location` = numeric())
    }
  })
  
  
  
}


# run the app -------------------------------------------------------------
shinyApp(ui = ui, server = server)
