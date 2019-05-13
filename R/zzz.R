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

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



#### create a package environment to store some globals
.pkgOptions <- new.env(hash=FALSE, parent=emptyenv())

#### define some package-level globals. Globals are ugly.
assign("catalogFileName", 'catalog', envir=.pkgOptions)
assign("EXIFTOOL", NULL, envir=.pkgOptions)
assign('metadata', list(), envir=.pkgOptions) #' @note list is by Site, should be refined better: if we store the whole catalog, storing metadata is useless
assign("metadataFileName", 'metadata.txt', envir=.pkgOptions)
assign("repositoryPath", NULL, envir=.pkgOptions)
assign("catalog", NULL, envir=.pkgOptions) # this holds the _whole_ catalog

#### package initialization
.onLoad <- function(libname, pkgname) {
  ## packageStartupMessage calls have been silenced, see ?.onAttach, "Good practice" section
  #packageStartupMessage("This is package cameratraps\n")
  ## check for exiftool existence
  # exiftool path can be accessed using get(EXIFTOOL, envir=.pkgOptions)
  exiftool <- Sys.which('exiftool')
  if(exiftool!="") {
    assign("EXIFTOOL", exiftool, envir=.pkgOptions)
    #packageStartupMessage("\tEXIFtool found in", exiftool, '\n')
  } else {
    msg <- "Error: exiftool not found. Please check and install it."
    msg <- ifelse(.Platform$OS.type=='windows', c(msg, "\nOn Windows remember to place exiftool(-k).exe in C:\\WINDOWS, and to rename it as exiftool.exe."), msg)
    stop(msg)
  }
}

#### End of File ####
