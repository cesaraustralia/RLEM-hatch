library(tidyverse)

# default to present year, but have check box to use average data

input = list()
input$yearstart = 1922
input$yearfinish = 2021

input_coords = list()
input_coords$long <- 142.780727 	
input_coords$lat <- -37.215526



# get silo temperature and rainfall data
startDay<-as.numeric(format(input$startDate,'%j'))
# get temp from silo
params = list(
  lat=paste(input_coords$lat),
  lon=paste(input_coords$long),
  start=sprintf("%s0101", input$yearstart), 
  finish=sprintf("%s1231",input$yearfinish),
  format="csv",
  comment="RXN",
  username="john.doe@xyz.com.au",
  password="silo"
)
res <- httr::GET("https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php", query=params)
# browser()
silodata <- readr::read_csv(httr::content(res, as="text")) 
silodata$jday = format(silodata$`YYYY-MM-DD`, "%j") 
silodata = silodata[silodata$jday != "366", ]

sep_silo_by_year = function(y, dates) {
  if(any(dates[order(dates)] != dates)) stop("dates and data must be in accending order")
  julian = as.integer(format(dates, "%j"))
  out = lapply(1:365, function(x) y[julian == x])
  names(out) = 1:365
  
  # overwrite past years with current year data
  if(format(Sys.time(), "%Y") == format(max(dates), "%Y")){
    warning("Overwriting historical data with current year to date")
    currentyear  = as.numeric(format(max(dates), "%Y"))
    current =  y[format(dates, "%Y") == currentyear]
    maxjul = length(current)
    for(i in 1:maxjul){
      out[[i]] = rep(current[i], length(out[[i]])-1)
    }
  }
  return(out)
}


TMIN = sep_silo_by_year(silodata$min_temp,   silodata$`YYYY-MM-DD`)
TMAX = sep_silo_by_year(silodata$max_temp,   silodata$`YYYY-MM-DD`)
RAIN = sep_silo_by_year(silodata$daily_rain, silodata$`YYYY-MM-DD`)

source("calc_hatch_function.R")
hatch_est = calc_hatch(TMIN, TMAX, RAIN, input_coords$long)

bind_rows(
  data.frame(TMIN, var = "TMIN"),
  data.frame(TMAX, var = "TMAX"),
  data.frame(RAIN, var = "RAIN")
) %>% 
  pivot_longer(c(-var), names_to = "doy") %>% 
  mutate(doy = as.numeric(gsub("X","",doy))) %>%
  group_by(var, doy) %>%
  summarise(val = mean(value), sd = sd(value)) %>% 
  ggplot(aes(doy, val, color=var, fill=var)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=val-sd, ymax=val+sd), color = NA, alpha = 0.1) + 
  geom_vline(xintercept = hatch_est)

quants = quantile(hatch_est, 1:10 / 10)
tibble(percent = names(quants), date = as.Date("2021-01-01") + quants - 1) %>% 
  

tibble(x =as.Date("2021-01-01") + hatch_est - 1) %>%
  ggplot(aes(x=x, y = stat(density))) +
  geom_histogram(bins=10, alpha=0.5) + 
  geom_density(size = 1) + 
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") + 
  theme_bw() + 
  xlab("") + 
  ylab("Liklihood density")

as.Date("2021-01-01") + hatch_est - 1

