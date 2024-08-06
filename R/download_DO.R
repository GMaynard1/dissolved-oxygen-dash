## ---------------------------
## Script name: download_DO.R
##
## Purpose of script: Downloads dissolved oxygen concentration data from an 
##  ERDDAP server and returns the raw data specified by the user.
##
## Date Created: 2024-07-16
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
#' download_DO
#' 
#' This function takes user inputs and uses them to create a URL to check if 
#' any dissolved oxygen / temperature data are available that meet the criteria 
#' from the specified  ERDDAP server. The function returns a dataframe if data 
#' are available or a warning if no data are available. 
#' 
#' @param erddap the base URL of the targeted ERDDAP server (e.g. 
#' https://erddap.emolt.net/erddap/tabledap/eMOLT_RT_LOWELL). Defaults to the
#' example address. 
#' @param startDate the beginning date for data collection. Should be provided
#' in yyyy-mm-dd format. There is no default value. The default value is one week ago.
#' @param endDate the end date for data collection. Should be provided in 
#' yyyy-mm-dd format. The default value is today's date. 
#' @param minWaterDetect the minimum acceptable water dection value as a percent.
#' The default value is 50.
#' @param maxTemp the maximum acceptable water temperature in degrees C. The 
#' default value is 20. 
#' @param minTemp the minimum acceptable water temperature in degrees C. The 
#' default value is 0
#' @param minLat the minimum latitude (southern extent) of the data request. The
#' default value is 38
#' @param maxLat the maximum latitude (northern extent) of the data request. The 
#' default value is 45
#' @param minLon the minimum longitude (western extent) of the data request. The
#' default value is -76
#' @param maxLon the maximum longitude (eastern extent) of the data request. The
#' default value is -66
#' @returns Returns a dataframe from the ERDDAP server or a warning message if
#' no data are available. 

download_DO=function(
    erddap="https://erddap.emolt.net/erddap/tabledap/eMOLT_RT_LOWELL",
    endDate=Sys.Date(),startDate=endDate-lubridate::days(7),minWaterDetect=50,maxTemp=20,minTemp=0,minLat=38,maxLat=45,minLon=-76,maxLon=-66){
  ## Set up the url based on user inputs
  url=paste0(
    erddap,
    ".csvp?tow_id%2Ctime%2Clatitude%2Clongitude%2Ctemperature%2CDO%2CDO_percentage%2Cwater_detect_perc%2Csensor_type&time%3E=",
    startDate,
    "T00%3A00%3A00Z&time%3C=",
    endDate,
    "T23%3A59%3A59&temperature%3E=",
    minTemp,
    "&temperature%3C=",
    maxTemp,
    "&water_detect_perc%3E=",
    minWaterDetect,
    "&latitude%3E=",
    minLat,
    "&latitude%3C=",
    maxLat,
    "&longitude%3E=",
    minLon,
    "&longitude%3C=",
    maxLon
  )
  ## If the data exist, download them. If not, return a warning
  if(httr::GET(url)$status==200){
    data=read.csv(url)
    data$latitude..degrees_north.=round(data$latitude..degrees_north.,2)
    data$longitude..degrees_east.=round(data$longitude..degrees_east.,2)
  } else {
    data=NA
    warning("No data available within the specified parameters")
  }
  return(data)
}
