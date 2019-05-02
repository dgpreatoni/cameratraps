###############################################################################
# UAGRA R Scripts - camera_trap                                           zzz.R
###############################################################################
# startup functions for cameratrap package
# Note: <notes here>
#
# version 0.1
# created prea 20190502 moved startup functions here
# updated
###############################################################################

#### create a package environment to store some globals
.pkgOptions <- new.env(FALSE, globalenv())

assign("EXIFTOOL", NULL, envir=.pkgOptions)
assign("metadataFileName", 'metadata.txt', envir=.pkgOptions)
assign("repositoryPath", NULL, envir=.pkgOptions)

#### package initialization
.onLoad <- function(libname, pkgname) {
  cat("This is package cameratraps\n")
  ## check for exiftool existence
  # exiftool path can be accessed using get(EXIFTOOL, envir=.pkgOptions)
  exiftool <- Sys.which('exiftool')
  if(exiftool!="") {
    assign("EXIFTOOL", exiftool, envir=.pkgOptions)
    cat("\tEXIFtool found in", exiftool, '\n')
  } else {
    msg <- "Error: exiftool not found. Please check and install it."
    msg <- ifelse(.Platform$OS.type=='windows', c(msg, "\nOn Windows remember to place exiftool.exe in C:\\WINDOWS."), msg)
    stop(msg)
  }
}
