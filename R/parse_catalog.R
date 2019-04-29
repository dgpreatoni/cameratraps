###############################################################################
# UAGRA R Scripts - camera_trap                                parse_catalog.R
###############################################################################
# Functions to parse camera trap data stored on a live filesystem.
#
# Notes
#  The package keeps a copy of the 'catalog' in memory. To minimize filesystem
# traversals, if a cached version is found (as a .rda file in a project root
# directory), the package just _updates_ the catalog (how?).
#
#
#
# version 0.1
# created fra 20160826
# updated prea 20160829
# updated prea 20161106
# updated prea 20180119
###############################################################################

# #### list all directories in a directory
# getDirectoryContent <- function(path=getRepository()) {
#   dirs <- list.dirs(path, full.names=FALSE, recursive=FALSE)
#   # exclude directories whose name begins with "@"
#   dirs <- dirs[-grep("^@", dirs)]
#   return(dirs)
# }
#
# #### list all site directories in a repository
# listSiteDir <- function() {
#   return(getDirectoryContent())
# }
#
# #### list all camera directories in a site directory
# listCameraDir <- function(cameraName) {
#   rep <- getRepository()
#   path <- paste(rep, cameraName, sep='/')
#   cat("rep->", rep, "path-> ", path, "\n")
#   return(getDirectoryContent(path))
# }

#### traverse all the directories in a repository
updateCatalog <- function() {
  theRepo <- get("repositoryPath", envir=.pkgOptions)
  siteDirs <- list.dirs(path=theRepo, full.names=FALSE, recursive=FALSE)
  # exclude directories whose name begins with "@"
  siteDirs <- siteDirs[-grep("^@", siteDirs)]
  #cat("\tgot site dirs:", siteDirs, "\n")
  for(siteDir in siteDirs) {
    theSite <- paste(theRepo, siteDir, sep='/')
    cat("\tprocessing site ", theSite, "\n")

  }
}

#### traverse the repository, update only metadata
updateMetadata <- function() {
  metadata <- list() # data container
  theRepo <- get("repositoryPath", envir=.pkgOptions)
  siteDirs <- list.dirs(path=theRepo, full.names=FALSE, recursive=FALSE)
  # exclude directories whose name begins with "@"
  siteDirs <- siteDirs[-grep("^@", siteDirs)]
  cat("Updating metadata for project", theRepo, "\n")
  cat("\tsites found:", length(siteDirs), "\n")
  #cat("\tgot site dirs:", siteDirs, "\n")
  if(length(siteDirs)>0) {
    metadata[['sites']] <- data.frame()
  }
  for(siteDir in siteDirs) {
    theSite <- paste(theRepo, siteDir, sep='/')
    cat("\tprocessing site ", theSite, "\n")
    theMetadataFile <- paste(theSite, get("metadataFileName", envir=.pkgOptions), sep="/")
    if(file.exists(theMetadataFile)) {
      siteMetadata <- .parseMetadata(theMetadataFile)
      cat("\t\tmetadata found\n")
      for(n in names(siteMetadata)) {
        metadata[['sites']][,n] <- md[n]
      }

    }
  }
}

#### parse a camera directory
parseCameraDir <- function(cameraDirName) {
  cat('\tparsing camera', cameraDirName, '\n') # silence that whan done
  oldwd <- getwd()
  setwd(cameraDirName)
  ## when a Samba (MAC) writes something on a NAS Synology create @eaDir files
  ## that create problem therefore I have to remove it
  # check for metadata
  if(!file.exists(metadataFileName)) {
    stop('metadata file is missing!')
  }
  cameraMetadata <- parseMetadata()
  cardDir <- file.info(list.dirs(full.names=FALSE, recursive=FALSE))
  if ("@eaDir" %in% row.names(cardDir)){
    cardDir <- cardDir[!row.names(cardDir)=="@eaDir",]
  }
  cardDir$name <- row.names(cardDir)
  # throw away undesired directories (say, Synology NAS create '@eadata' or '@-folders' everywhere), grep out the date pattern we use
  cardDir <- cardDir[grepl("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}", cardDir$name),]
  #@TODO cardDir$name holds a list of date-like strings, that should be useful to convert trapping event dates into trapping events number or whatever. need be, do it here.
  #@TODO make it parallel
  cardData <- list()
  for(ca in cardDir$name) {
    cData <- getEXIFData(ca)
    if(nrow(cData)>0) {
      cardData[[ca]] <- cData
    }
  }
  #flatten list
  cardData <- do.call('rbind', cardData)
  #if(is.null(cardData) {

  #}
  # add camera metadata
  cardData$Raw.Path <- cameraDirName
  cardData$Camera.Serial.Number <- cameraMetadata['serial']
  cardData$Camera.Start.Date.and.Time <- cameraMetadata['start']
  cardData$Camera.End.Date.and.Time <- cameraMetadata['end']
  cardData$Camera.Manufacturer <- cameraMetadata['make']
  cardData$Camera.Model <- cameraMetadata['model']
  cardData$Camera.Name <- cameraMetadata['name']
  cardData$Latitude <- as.numeric(cameraMetadata['lat'])
  cardData$Longitude <- as.numeric(cameraMetadata['lon'])
  cardData$Sampling.Unit.Name <- cameraDirName
  setwd(oldwd)
  invisible(cardData)
}

#### parse a site directory
parseSiteDir <- function(siteDirName) {
  if(getOption("verbose")) {cat('cameratraps::parseSiteDir parsing site', siteDirName, '\n')}
  # get metadata
  mdFileName <- paste(getRepository(), siteDirName, get("metadataFileName", envir=.pkgOptions), sep='/')
  if(file.exists(mdFileName)) {
    siteMetadata <- .parseMetadata(mdFileName)
  }

  # cameraDir <- file.info(list.dirs(full.names=FALSE, recursive=FALSE))
  # if ("@eaDir" %in% row.names(cameraDir)){
  #   cameraDir <- cameraDir[!row.names(cameraDir)=="@eaDir",]
  # }
  # cameraDir$name <- row.names(cameraDir)
  # #@TODO make it parallel
  # cameraData <- list()
  # for(cd in cameraDir$name) {
  #   cData <- parseCameraDir(cd)
  #   if(nrow(cData)>0) {
  #     cameraData[[cd]] <- cData
  #   }
  # }
  # #flatten list
  # cameraData <- do.call('rbind', cameraData)
  # # add site metadata
  # cameraData$Raw.Path <- paste(siteDirName, cameraData$Raw.Path, sep='/')
  # #cameraData$Sampling.Unit.Name <- siteMetadata['name']
  # setwd(oldwd)
  # invisible(cameraData)
  return(siteMetadata)
}
