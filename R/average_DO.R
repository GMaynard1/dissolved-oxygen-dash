## ---------------------------
## Script name: average_DO.R
##
## Purpose of script: Takes dissolved oxygen concentration and temperature data 
##  from download_DO.R and 
##
## Date Created: 2024-07-30
##
## Software code created by U.S. Government employees is 
## not subject to copyright in the United States 
## (17 U.S.C. §105).
##
## Email: george.maynard@noaa.gov
##
## ---------------------------
## Notes:
##   
##
## ---------------------------
#' average_DO
#' 
#' This function takes a dataframe from download_DO and user inputs and uses 
#' them to bin a file of raw dissolved oxygen and temperature observations by 
#' time. The function returns a new dataframe. 
#' 
#' @param raw_data a dataframe generated using the download_DO function. It 
#' should contain at least the following columns: tow_id, timestamp, latitude..degrees_north.,
#' longitude..degrees_east.,temperature..degree_c.,DO..ml.l..1. There is no 
#' default value.
#' @param time_bin the size of each bin in hours. The default value is 6 hours. 

average_DO=function(raw_data,time_bin=6){
  ## Set up an empty dataframe to store time averaged data
  new_data=data.frame(
    tow_id=as.numeric(),
    timestamp=as.character(),
    lat=as.numeric(),
    lon=as.numeric(),
    temperature=as.numeric(),
    DO_mgl=as.numeric()
  )
  ## Convert timestamps to datetime objects
  raw_data$timestamp=lubridate::ymd_hms(raw_data$time..UTC.)
  for(tow in unique(raw_data$tow_id)){
    x=subset(raw_data,raw_data$tow_id==tow)
    start_time=lubridate::round_date(min(x$timestamp),unit="hour")
    end_time=lubridate::round_date(max(x$timestamp),unit="hour")
    ## If the start and end of the haul are less than the specified time_bin
    ## apart, average the haul
    if(difftime(end_time,start_time,units="hours")<=time_bin){
      temp_data=data.frame(
        tow_id=tow,
        timestamp=mean(x$timestamp),
        lat=round(mean(x$latitude..degrees_north.),2),
        lon=round(mean(x$longitude..degrees_east.),2),
        temperature=mean(x$temperature..degree_C.),
        DO_mgl=mean(x$DO..ml.l..1.)
      )
      ## Append haul averages to the new_data dataframe
      new_data=rbind(new_data,temp_data)
    } else {
      ## Otherwise, set a time bin end for six hours after the start_time
      bin_end=start_time+lubridate::hours(time_bin)
      ## As long as the bin end is before the haul end, average all observations 
      ## within the bin and append them to the new_data frame
      while(bin_end<end_time){
        y=subset(x,x$timestamp>start_time&x$timestamp<bin_end)
        temp_data=data.frame(
          tow_id=tow,
          timestamp=mean(y$timestamp),
          lat=round(mean(y$latitude..degrees_north.),2),
          lon=round(mean(y$longitude..degrees_east.),2),
          temperature=mean(y$temperature..degree_C.),
          DO_mgl=mean(y$DO..ml.l..1.)
        )
        new_data=rbind(new_data,temp_data)
        start_time=bin_end
        bin_end=bin_end+lubridate::hours(time_bin)
      }
    }
  }
  return(new_data)
}