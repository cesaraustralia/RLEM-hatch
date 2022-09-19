library(tidyverse)
library(raster)
library(tmap)
library(sf)
source("app/calc_hatch_function_slow.R")

# 0.5 degree resolution mean awap 1996-2015
RAIN = brick("data/awap/ag10/mu_rain_for_DOY_ag10.tif")
TMIN = brick("data/awap/ag10/mu_Tmin_for_DOY_ag10.tif")
TMAX = brick("data/awap/ag10/mu_Tmax_for_DOY_ag10.tif")
plot(RAIN[[1]])

hatch = calc_hatch(TMIN, TMAX, RAIN, ndays = 200)

aus = "C:/Users/james/Dropbox (Personal)/Maps/Australia by state ABS/STE11aAust.shp" %>% 
  read_sf() %>% 
  st_simplify(dTolerance = 1000)

h = RAIN[[1]]
h2 = h
hdate = as.Date("2022-01-01") + hatch - 1
hdate[hdate == "2021-12-31"] = NA
dates = c(
          seq.Date(as.Date("2022-01-01"), by = "1 month", length=10)) - 1
for (i in 2:length(dates)){
  d1 = dates[i-1]
  d2 = dates[i]
  hdate[hdate>d1 & hdate<=d2] = d2  
}
unique(hdate)
h[] = as.factor(hdate)
labs = levels(as.factor(hdate)) %>% 
  as.Date() %>% 
  format("%B")
pal = viridis::magma(length(labs))
tm1 = tm_shape(aus) +
  tm_fill(col = "white") + 
  tm_shape(mask(h, aus),  is.master = T) + 
  tm_raster(title = "Average hatch", palette = pal,labels = labs) +
  tm_shape(aus, is.master = T) +
  tm_borders(lwd = 1, col = "black") +
  tm_layout(bg.color = "lightblue", legend.position = c("left", "bottom")) 
tm1

h2[] = hatch
writeRaster(h2, "plots/average_hatch_day.tif")

tmap_save(tm1, "plots/average_hatch_month.png", width=7, height=6)  

   
# # 0.05 degree resolution awap (too slow)
# RAIN = brick("C://awap/mu_rain_for_DOY_0.05m.tif")
# TMIN = brick("C://awap/mu_Tmin_for_DOY_0.05m.tif")
# TMAX = brick("C://awap/mu_Tmax_for_DOY_0.05m.tif")

