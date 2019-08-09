###############################################################################
# UAGRA R Scripts - camera_trap                                     analisys.R
###############################################################################
# Analysis functions for cameratrap package
# Note: <notes here>
#
# version 0.1
# created prea 20190507
# updated
###############################################################################



#### set filtering ############################################################
#' @export
filterCatalog <- function(aFilter) {
  .setOption("catalogHasFilter", aFilter)

}

#### access catalog data ######################################################
#' @export
catalog <- function(filter=TRUE) {
  ctl <- .getOption('catalog')
  return(ctl[,filter])
}



#### calculates effort matrix (events per hour) ###############################
#' @export
#' @note TODO implement catalog filtering
cam.hours <- function(catalog, filter) {
  # get hour from photo timestamp
  catalog$Photo.Hour <- hour(catalog$Photo.Timestamp)
  if(nrow(catalog[catalog$Photo.Hour==0,])>0) {
    catalog[catalog$Photo.Hour==0,]$Photo.Hour <- 24
  }
  # cross-tabulate
  tbl <- data.frame(xtabs(~Photo.Hour, catalog))
  tbl$Percent <- tbl$Freq / sum(tbl$Freq)
  return(tbl)
}


#### calculates effort matrix (camera days per year) ##########################
# the function returns the matrix sampling effort by sampling unit (camera-days) per year
cam.days <- function(catalog, year) {
  yr <- dtaframe[dtaframe$Sampling.Event == year, ]
  yr$ndays <- as.numeric(difftime(yr$End.Date, yr$Start.Date))
  selvar <- subset(yr, select = c(Sampling.Unit.Name, Start.Date, End.Date, ndays))
  cam.days <- unique(selvar)
}
