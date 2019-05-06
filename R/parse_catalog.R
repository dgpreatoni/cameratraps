###############################################################################
# UAGRA R Scripts - camera_trap                                parse_catalog.R
###############################################################################
# Functions to parse camera trap data stored on a live filesystem.
#
# Notes
# The package keeps a copy of the 'catalog' in memory. To minimize filesystem
# traversals, if a cached version is found (as a .rda file in a project root
# directory), the package just _updates_ the catalog (how?).
#
#
#
# version 0.2
# created fra  20160826
# updated prea 20160829
# updated prea 20161106
# updated prea 20180119
# updated prea 20180216
# updated prea 20190506 cleaned up stuff, rewrote from scratch updateCatalog()
###############################################################################


#### list all "site directories" in a repository ##############################
#' @export
listSiteDir <- function() {
  siteList <- .getDirectoryContent()
  return(siteList)
}


#### list all camera directories in a site directory #########################
#' @export
listCameraDir <- function(siteDirName) {
  rep <- getRepository()
  path <- paste(rep, siteDirName, sep='/')
  camList <- .getDirectoryContent(path)
  #cat("\trep-> ", rep, "\n\tsite-> ", siteDirName, "\n\tcam-> ", camList)
  return(camList)
}


#### list all data directories in a camera directory ##########################
#' @export
listDataDir <- function(siteDirName, cameraDirName) {
  rep <- getRepository()
  path <- paste(rep, siteDirName, cameraDirName, sep='/')
  dataList <- .getDirectoryContent(path)
  #cat("\trep-> ", rep, "\n\tsite-> ", siteDirName, "\n\tcam-> ", camList)
  # if need be, we can filter out here the directory names patterned as YYYY=MM-DD
  # cardDir <- cardDir[grepl("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}", cardDir$name),]
  return(dataList)
}


#### traverse all the directories in a repository
#' @export
updateCatalog <- function(verbose=FALSE) {
  theRepo <- getRepository()
  if(verbose) cat('Repo is ', theRepo, '\n')
  siteNames <- listSiteDir()
  if(verbose) cat("Sites: ", siteNames, "\n")
  catalogData <- list()
  for(site in siteNames) { # process a site directory
    sitePath <- paste(theRepo, site, sep='/')
    if(verbose) cat("\tprocessing site ", site, "\n")
    # process, if any, medatata.txt here
    if(file.exists(paste(sitePath, 'metadata.txt', sep='/'))) {
      .pkgOptions$metadata[[site]] <- .parseMetadata(path=sitePath)
    } else {
      warning("\tno medatata found for site ", site, "\n")
    }
    cameraNames <- listCameraDir(site)
    .pkgOptions$metadata[[site]]$cameras <- list()
    siteData <- list()
    for(camera in cameraNames) { # process a camera directory
      cameraPath <- paste(sitePath, camera, sep='/')
      if(verbose) cat("\t\tprocessing camera ", camera, "\n")
      # process metadata (must be there!)
      .pkgOptions$metadata[[site]][[camera]] <- .parseMetadata(path=cameraPath)
      sdcardDirs <- listDataDir(site, camera)
      cameraData <- list()
      for(sdcard in sdcardDirs) { # process data dump directories
        dataPath <- paste(cameraPath, sdcard, sep='/')
        if(verbose) cat("\t\t\tprocessing sdcard ", sdcard, "\n")
        sdcData <- getEXIFData(dataPath, tz=.pkgOptions$metadata[[site]][[camera]][['timezone']])
        if(nrow(sdcData) > 0) {
          cameraData[[sdcard]] <- sdcData
        }
      } # sdcard (dataDirs) loop
      # flatten sd card data
      cameraData <- do.call('rbind', cameraData)
      # fix some fields content, add camera metadata
      cameraData$Raw.Path <- dataPath
      cameraData$Raw.Names <- basename(as.character(cameraData$Raw.Names))
      cameraData$Camera.Serial.Number <- .pkgOptions$metadata[[site]][[camera]][['serial']]
      cameraData$Camera.Start.Date.and.Time <- .pkgOptions$metadata[[site]][[camera]][['start']]
      cameraData$Camera.End.Date.and.Time <- .pkgOptions$metadata[[site]][[camera]][['end']]
      cameraData$Camera.Manufacturer <- .pkgOptions$metadata[[site]][[camera]][['make']]
      cameraData$Camera.Model <- .pkgOptions$metadata[[site]][[camera]][['model']]
      cameraData$Latitude <- as.numeric(.pkgOptions$metadata[[site]][[camera]][['lat']])
      cameraData$Longitude <- as.numeric(.pkgOptions$metadata[[site]][[camera]][['lon']])
      cameraData$Sampling.Unit.Name <- camera
      # stash
      siteData[[camera]] <- cameraData
    } # cameraDirs loop
    # flatten camera data
    siteData <- do.call('rbind', siteData)
    catalogData[[site]] <- siteData
  } # siteDirs loop
  # flatten catalog data
  catalogData <- do.call('rbind', catalogData)
  row.names(catalogData) <- NULL
  invisible(catalogData)
}
