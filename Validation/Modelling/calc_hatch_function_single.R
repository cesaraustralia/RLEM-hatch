calc_hatch <- function(clim){
  
  # Description: 
  #a function that returns the predicted RLEM hatch date based on McDonald 2015
  
  # Inputs:
  # the input is a data.frame holding silo data: 
  # min_temp, max_temp, daily_rain, longitude
  
  # Outputs:  
  # hatch is a list of locations holding the predicted hatch julian day
  
  if(nrow(clim)==0) return(NA)
  
  TMIN = clim$min_temp
  TMAX = clim$max_temp
  RAIN = clim$daily_rain
  
  
  muT = (TMAX + TMIN)/2
  # mean daily tempearture, C
  MDT<-TMAX - (TMAX - TMIN)/4

  # Threshold in Celsius
  MDTthresh = ifelse(clim$Longitude[1] > 130, 16, 20.5)
  
  hatch = NA
  countdown = 0
  suitable = FALSE
  for(day in 15:365){
    # array of previous 10 day indices
    prev10 = (day-9):day
    # array of previous 10 day mean temp
    MDT10 = mean(MDT[prev10]) 
    # development penalty see p. 264 mcdonald 2016
    penalty = sum(8*pmax(0, MDT[prev10]-19)/10) 
    if(penalty < 0)stop("invalid penalty")
    # total rainfall over the 5 days prior to the prev10
    rain5 = sum( RAIN[(day-14):(day-10)] )
    # conditions suitable for hatching 
    suitable = rain5>5 & MDT10<MDTthresh
    # reset hatch count down if conditions not suitable
    if(countdown > 0 & !suitable) countdown = 0 
    # decrement count down by one day
    if(countdown > 0 & suitable) countdown = countdown - 1
    # if hatch detected, return day of year
    if(countdown < 0) return(day)
    # start hatch count down
    if(suitable & is.na(hatch) & countdown == 0) countdown = penalty + 0.00001
    
  }
  # no hatch
  return(hatch)
}
  