library(tidyverse)

source('get_silo_data.R')
source("calc_hatch_function_parallel.R")

# from McDonald 2015, Tucker 1935
stage_offset = c( # days
  2, # larva
  6, # protonymph 
  11,# deutonymph    
  16,# tritonymph   
  21 # adult 
)

# https://docs.google.com/spreadsheets/d/1lENevcc-XhyXT1cxBLfgQIOrZY0dTMCQHIxBTZrdRNo/edit#gid=497076656

filename = "data/RLEM hatch timing validation.xlsx"
d = readxl::read_excel(filename, sheet=1) %>% 
  pivot_longer(cols = Mite_1:Mite_50) 

# get hatch predictions
hatch_file = "data/RLEM hatch timing validation.csv"
if(!file.exists(hatch_file)){
  # load site details
  hatch = readxl::read_excel(filename, sheet=2) %>% 
    distinct(Site, Longitude, Latitude) %>% 
    mutate(hatch = NA)
  # get climate data
  for(i in 1:nrow(hatch)){
    out = get_silo_data(yearstart=2022, yearfinish=2022, 
                  longitude=hatch$Longitude[i], 
                  latitude =hatch$Latitude[i])
    hatch$hatch[i] = 
      calc_hatch(out[['TMIN']], out[['TMAX']], out[['RAIN']], 
                 longitude=hatch$Longitude[i])
  }
  write_csv(hatch, hatch_file)
} else {
  hatch = read_csv(hatch_file)
}

# get median from age structured data and offset date by stage duration
dsum = d %>% 
  left_join(hatch) %>%  
  group_by(Date_collected, Site, Latitude, Longitude, hatch) %>%
  summarise(medianstage = median(value), .groups='drop') %>% 
  mutate(stage_offset = stage_offset[medianstage]) %>%
  # date collected = 17/5/22 = julian 137
  mutate(observed_hatch = 137 - stage_offset) %>% 
  mutate(obs_pred_diff = observed_hatch - hatch) %>%
  mutate(year = '2022') %>% 
  select(year, Longitude, Latitude, obs_pred_diff)
dsum
# plot obs vs pred
dsum %>% 
  mutate(predicted_hatch = hatch) %>%
  ggplot() + 
  geom_point(aes(Longitude, Latitude, size=abs(obs_pred_diff)))


################### plot ##########
library(tmap)
library(sf)

d2 = "data/RLEM hatch timing validation McDonald 2015.csv" %>% 
  read_csv() %>% 
  mutate(obs_pred_diff = 
           as.numeric(format(observed_hatchdate,  "%j")) - 
           as.numeric(format(predicted_hatchdate, "%j"))) %>% 
  mutate(year = format(observed_hatchdate, "%Y")) %>% 
  select(year, Longitude, Latitude, obs_pred_diff) %>% 
  bind_rows(dsum) %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs=4326)

sum(d2$year==2022)
sum(d2$year!=2022)
min(d2$obs_pred_diff, na.rm=TRUE)
max(d2$obs_pred_diff, na.rm=TRUE)
mean(d2$obs_pred_diff, na.rm=TRUE)
sd(d2$obs_pred_diff, na.rm=TRUE)/sqrt(length(d2))


aus = "C:/Users/james/Dropbox (Personal)/Maps/Australia by state ABS/STE11aAust.shp" %>% 
  read_sf() %>% 
  st_simplify(dTolerance = 1000)

mybbox = st_bbox(c(
  xmin = 141, xmax = 150, ymax = -35, ymin = -40), crs = st_crs(4326))
# mybbox = st_bbox(c(
#   xmin = 110, xmax = 120, ymax = -30, ymin = -36), crs = st_crs(4326))

tm1 = aus %>%
  tm_shape(bbox= mybbox) +
  tm_fill(col = "white") + 
  tm_shape(aus) +
  tm_borders(lwd = 1, col = "black") +
  tm_shape(d2) +
  tm_dots(title = "Prediction\nerror (d)", col='obs_pred_diff', size = 0.5, alpha =0.6, 
          palette = 'RdYlGn') +
  tm_layout(bg.color = "lightblue", 
            legend.position = c("left", "top")) 
tm1
tmap_save(tm1, "plots/hatch_validation_east.png", width=7, height=6)  


# conclusions 
# works well enough: prediction within two weeks of observed hatch
