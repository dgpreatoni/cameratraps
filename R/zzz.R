###############################################################################
# UAGRA R Scripts - Rcameratraps                                         zzz.R
###############################################################################
# startup functions for Rcameratraps package
# Note: <notes here>
#
# version 0.1
# created prea 20190502 moved startup functions here.
# updated prea 20190808 polished up code
#         prea 20190513 added some more globals, which are evil but useful.
#
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

###############################################################################


#### create a package environment to store some globals
.pkgOptions <- new.env(hash=FALSE, parent=emptyenv())


#### get/set options
.getOption <- function(optname) {
  get0(optname, envir=.pkgOptions)
}

.setOption <- function(optname, optvalue) {
  assign(optname, optvalue, envir=.pkgOptions)
}


#### package initialization (attach)
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\nThis is cameratraps ", packageVersion('cameratraps'), ".\n")
}


#### package initialization (load)
.onLoad <- function(libname, pkgname) {
  # packageStartupMessage calls have been silenced, see ?.onAttach, "Good practice" section
  # startup messages must go in .onAttach
  # packageStartupMessage("This is package cameratraps\n")
  #### Define some package-level globals.
  #### Globals are ugly.
  #### Globals can be accessed with get(<option name>, envir=.pkgOptions)
  .setOption('catalog', NULL) # this is the _whole_ in-memory catalog
  .setOption('catalogFileName', '.catalog.rds')
  .setOption('catalogHasChanged', NULL) # "is dirty" flag, TRUE when the in-memory catalog has changed and should be written on disk
  .setOption('catalogHasFilter', NULL) # if some filtering on the catalog data is present it will be stored here as a valid "[" (row-wise) expression
  .setOption('EXIFTOOL', NULL)
  # all 'known' file extensions must go here.
  .setOption('known.extensions',  c('AVI', 'avi', 'JPG', 'jpg', 'M4V', 'm4v', 'MOV', 'mov', 'MOD', 'mod', 'MP4', 'mp4'))
  .setOption('metadata', list()) #' @note list is by Site, should be refined better: if we store the whole catalog, storing metadata is useless
  .setOption('metadataFileName', 'metadata.txt')
  .setOption('repositoryPath', NULL)
  #### place other initialization stuff here

  # check for exiftool existence
  # once done, exiftool path can be accessed using .getOption('EXIFTOOL')
  exiftool <- Sys.which('exiftool')
  if(exiftool!="") {
    .setOption("EXIFTOOL", exiftool)
    packageStartupMessage("  EXIFtool found in", exiftool, '\n')
  } else {
    msg <- "Error: exiftool not found. Please check and install it."
    msg <- ifelse(.Platform$OS.type=='windows', c(msg, "  On Windows remember to place exiftool(-k).exe in C:\\WINDOWS, and to rename it as exiftool.exe."), msg)
    stop(msg)
  }
}


#### End of File ####
