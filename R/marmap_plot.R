## ---------------------------
## Script name: marmap_plot.R
##
## Purpose of script: A function to download bathymetric data and create a 
##  basic plot.
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
## Notes: The default bounding box will display the continental shelf of the
##   northeastern United States
##
## ---------------------------
#' @param bathy a bathymetric dataset. There is no default value. Providing a 
#' dataset overrides the resolution parameter
#' @param north the maximum latitude of the bounding box to plot in decimal 
#' degrees. The default value is 47.
#' @param south the minimum latitude of the bounding box to plot in decimal 
#' degrees. The default value is 35.
#' @param east the maximum longitude of the bounding box to plot in decimal
#' degrees.  The default value is -56.
#' @param west the minimum longitude of the bounding box to plot in decimal 
#' degrees. The default value is -81. 
#' @param resolution the resolution of data to download and plot. Smaller 
#' numbers provide finer resolution but take longer. The default is 10.
#' @param deepest.isobath the deepest isobath to plot. Defaults to -1000 m.
#' @param shallowest.isobath the shallowest isobath to plot. Defaults to 0 m.
#' @param isobath.step the isobath step to plot. Defaults to 100 m.

marmap_plot=function(bathy=NA,north=47,south=35,east=-56,west=-81,resolution=10,deepest.isobath=-1000,shallowest.isobath=0,isobath.step=100){
  
  if(length(bathy)==1){
    ## Download bathymetric data; use resolution = 10 for debugging
    bathy=marmap::getNOAA.bathy(
      lon1=west,
      lon2=east,
      lat1=south,
      lat2=north,
      resolution=resolution
    )
  }
  ## Create color ramp
  blues=c(
    "lightsteelblue4", 
    "lightsteelblue3",
    "lightsteelblue2", 
    "lightsteelblue1"
  )
  ## Plot the bathymetry
  marmap::plot.bathy(
    bathy,
    step=isobath.step,
    deepest.isobath=deepest.isobath,
    shallowest.isobath=shallowest.isobath,
    col="darkgray",
    image = TRUE, 
    land = TRUE, 
    lwd = 0.1,
    bpal = list(
      c(0, max(bathy), "gray"),
      c(min(bathy),0,blues)
    ),
    main="",
    ylim=c(south,north),
    xlim=c(west,east)
  )
}