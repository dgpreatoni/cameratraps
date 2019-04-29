################################################################################
# processing functions

#### package initialization
.onLoad <- function(libname, pkgname) {
  cat("This is package cameratrap\n")
  #### check for exiftool existence
  # exiftool path can be accessed using get(EXIFTOOL, envir=.pkgOptions)
  #@TODO this uses UNIX 'which', and should be ported to Win and Mac
  exiftool <- system2('which', args='exiftool', stdout=TRUE)
  if('status' %in% names(attributes(exiftool))) { # if system2 returns an error code different than 0 (normal completion), then EXIFTOOL has a 'status' attribute.
    stop("Error: exiftool not found. Please check and install it.")
  } else {
    assign("EXIFTOOL", exiftool, envir=.pkgOptions)
    cat("\tEXIFtool found in", exiftool, '\n')
  }
}
