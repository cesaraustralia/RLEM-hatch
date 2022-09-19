calc_hatch <- function(TMIN, TMAX, RAIN, longitude=NULL){
  # Description: 
  #a function that returns the predicted RLEM hatch date based on McDonald 2015
  
  # Inputs:
  # TMIN, TMAX, RAIN are lists of locations holding daily minimum temperature, maximum temperature, and rainfall respectively ordered by julian day. 
  
  # Outputs:  
  # hatch is a list of locations holding the predicted hatch julian day
  
  if(length(longitude)==1) longitude = rep(longitude, length(TMIN[[1]]))
  
  
  muT<-TMIN
  muT[]<-0
  for(i in 1:365){
    # cat(paste('creating muT for doy: ',i))
    muT[[i]]<-(TMAX[[i]] + TMIN[[i]])/2
  }
  
  # load MDT for diapause break calcuation (McDonald 2015)
  MDT<-TMIN
  MDT[]<-0
  for(i in 1:365){
    # cat(paste('creating MDT for doy: ',i))
    MDT[[i]]<-TMAX[[i]] - (TMAX[[i]] - TMIN[[i]])/4
  }
  
  # Threshold in Celsius
  MDTthresh = rep(20.5, length(MDT[[1]])) # WA 
  MDTthresh[longitude > 130] = 16 # not WA
  
  # initialise vars
  MDT10 <- rain5 <-hatching <- hatch <- penalty <- rep(0, length(MDT[[1]]))
  
  for(day in 15:365){
    prev10 = (day-9):day
    MDT10 = Reduce(`+`, MDT[prev10])/length(prev10) # mean
    
    # create penalty of number of day degrees over threshold
    calc_penalty = function(x) { 
      x = x - 19
      x[x<0] = 0
      x
    }
    penalty = lapply(MDT[prev10], calc_penalty) # mean
    penalty = Reduce(`+`, penalty)
    penalty = penalty/length(prev10)
      
    # rainfall over past 5 days
    prev5 = (day-10):(day-14)
    rain5 = Reduce(`+`, RAIN[prev5])
    
    # locations with suitable conditions for hatching commencement
    suitable = rain5[]>5 & MDT10[]<MDTthresh
    
    # if hatch conditions are not suitable, reset any hatching counters
    index<-hatching[]>0 & !suitable
    hatching[index] = 0
    
    # decrement any remainingg hatching counters
    index<-hatching[]>0 
    hatching[index] = hatching[index] - 1
    
    # start counter anywhere conditions are suitable and hatching has not commenced
    index<- suitable & hatching[]==0
    if(any(index)){
      hatching[index] <-  8*penalty[index] + # see p. 264 mcdonald 2016
        0.000001  # small number so hatching is never 0 (instantaneous)
    }
    
    # Anywhere hatch counters have reached zero, save hatch date
    index<-hatching[]<0 & hatch[] == 0 
    if(any(index)) hatch[index] = day 
    
  }
  return(hatch)
}