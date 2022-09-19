library(tidyverse)
library(lubridate)


d1 = read_csv("data/McDonald2015_observations.csv") %>% 
  mutate(garry_doy = `Garry final j`) %>% 
  mutate(HatchDate = 
    as.Date(`Published or estimated date of egg hatch`, format = "%d/%m/%Y")) %>%
  mutate(Year = format(HatchDate, "%Y"))
  
d2 = read_csv("data/McDonald2015_climate.csv")%>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  mutate(Year = format(Date, "%Y")) 

################### SINGLE TREAD TEST ###################

source('calc_hatch_function_single.R')
pred = d1 %>% 
  mutate(Y = year(HatchDate)) %>% 
  # filter(grepl("syngenta", Location)) %>%
  select(Loc = Location, Y = Year, Lon = Longitude, HatchDate, garry_doy) %>% 
  rowwise %>% 
  mutate(
    hatchday = 
      d2 %>%
      filter(Location == Loc) %>% 
      filter(year(Date) == Y) %>% 
      mutate(Longitude = Lon) %>%
      mutate(max_temp = Tmax,
             min_temp = Tmin,
             daily_rain = Precip) %>%
      calc_hatch()
  )

pred %>%
  mutate(LocYear = paste(Loc, Y)) %>% 
  ggplot() +
  geom_point(aes(yday(HatchDate)-hatchday, 
                 LocYear)) + 
  # DONT KNOW WHY THIS IS OFFSET BY ONE?
  geom_point(aes(yday(HatchDate)-1- garry_doy, LocYear), color = "red", shape = 21) + 
  xlab("error (days)") + 
  geom_vline(xintercept = 0)



##################### PARALLEL TEST ###############################
source("calc_hatch_function_parallel.R")

d = left_join(d2, d1, by=c("Location", "Year")) 

sep_by_site_year = function(y, dates, loc) {
  site_year = paste(loc, format(dates, "%Y"))
  u_site_year = unique(site_year)
  julian = as.integer(format(dates, "%j"))
  out = lapply(1:365, function(x) y[julian == x])
  names(out) = 1:365
  
  for(i in u_site_year){
    idates = dates[site_year == i] 
    if(any(idates[order(idates)] != idates)) stop("dates and data must be in accending order")
  }
  check = unlist(lapply(out, length)) 
  if(!all(check == check[1])) stop("missing data detected")
  return(out)
}

for (iyear in unique(d$Year)){
  print(sum(d$Year == iyear))
}

# d  = d %>%
#   filter(Location == "Ballarat (cesar syngenta)") %>%
#   filter(Year == "2008")

TMIN = sep_by_site_year(d$Tmin,   d$Date, d$Location)
TMAX = sep_by_site_year(d$Tmax,   d$Date, d$Location)
RAIN = sep_by_site_year(d$Precip, d$Date, d$Location)
longitude = d %>% 
  distinct(Location, Year, Longitude) %>%
  pull(Longitude)

hatch_est = calc_hatch(TMIN, TMAX, RAIN, longitude)


d1$preddate = as.Date("2000-01-01") + hatch_est 
d1$predict = hatch_est
format(d1$HatchDate, "%j")
d1$`Published or estimated date of egg hatch j`
d1 %>%
  mutate(LocYear = paste(Location, Year)) %>% 
  ggplot() +
  geom_point(aes(`Published or estimated date of egg hatch j`-hatch_est, 
                 LocYear)) + 
  geom_point(aes(`Published or estimated date of egg hatch j`- 1 -`Garry final j`, 
                 LocYear), color = "red", shape = 21) + 
  xlab("error (days)") + 
  geom_vline(xintercept = 0)

d1 %>%
  mutate(LocYear = paste(Location, Year)) %>% 
  mutate(`Garry final` = as.Date(`Garry final`, format = "%d/%m/%Y")) %>%
  select(Location, Longitude, Latitude, observed_hatchdate = HatchDate, 
         predicted_hatchdate = preddate) %>% 
  write_csv("data/RLEM hatch timing validation McDonald 2015.csv")

names(d1)
 
# plot(format(d1$HatchDate, "%j"), hatch_est)
# abline(0, 1)
# 
# plot(format(d1$HatchDate, "%j"), hatch_est)
# abline(0, 1)


