get_silo_data = function(
  yearstart, yearfinish, longitude, latitude){
  
  # get temp from silo
  params = list(
    lat=sprintf("%1.6f", latitude),
    lon=sprintf("%1.6f", longitude),
    start=sprintf("%s0101", yearstart), 
    finish=sprintf("%s1231",yearfinish),
    format="csv",
    comment="RXN",
    username="john.doe@xyz.com.au",
    password="silo"
  )
  res <- GET("https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php", query=params)
  # browser()
  silodata <- readr::read_csv(httr::content(res, as="text")) 
  silodata$jday = format(silodata$`YYYY-MM-DD`, "%j") 
  silodata = silodata[silodata$jday != "366", ]
  
  sep_silo_by_year = function(y, dates) {
    if(any(dates[order(dates)] != dates)) stop("dates and data must be in accending order")
    julian = as.integer(format(dates, "%j"))
    out = lapply(1:365, function(x) y[julian == x])
    names(out) = 1:365
    # overwrite missing data in current year with 999999999 so that hatching is not triggered
    if(format(Sys.time(), "%Y") == format(max(dates), "%Y")){
      warning("overwrite missing data in current year with zeros")
      currentyear  = as.numeric(format(max(dates), "%Y"))
      current =  y[format(dates, "%Y") == currentyear]
      maxjul = length(current)
      for(i in (maxjul+1):365){
        out[[i]] = c(out[[i]], 999999999) 
      }
    }        
    return(out)
  }
  # browser()
  TMIN = sep_silo_by_year(silodata$min_temp,   silodata$`YYYY-MM-DD`)
  TMAX = sep_silo_by_year(silodata$max_temp,   silodata$`YYYY-MM-DD`)
  RAIN = sep_silo_by_year(silodata$daily_rain, silodata$`YYYY-MM-DD`)
  return(list(TMIN=TMIN, TMAX=TMAX, RAIN=RAIN, silodata=silodata))
}


     