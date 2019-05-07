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



#### calculates effort matrix (events per hour) ###############################

cam.hours <- function(catalog=, )

# the function returns the matrix sampling effort by sampling unit (camera-days) per year
cam.days <- function(dtaframe, year) {
  yr <- dtaframe[dtaframe$Sampling.Event == year, ]
  yr$ndays <- as.numeric(difftime(yr$End.Date, yr$Start.Date))
  selvar <- subset(yr, select = c(Sampling.Unit.Name, Start.Date, End.Date, ndays))
  cam.days <- unique(selvar)
}
