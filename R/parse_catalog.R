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


#### traverse all the directories in a repository #############################
#' @export
updateCatalog <- function(verbose=FALSE) {
  theRepo <- getRepository()
  if(verbose) cat('Repository is ', theRepo, '\n')
  siteNames <- listSiteDir()
  if(verbose) cat("Sites: ", siteNames, "\n")
  catalogData <- list()
  for(site in siteNames) { # process a site directory
    sitePath <- paste(theRepo, site, sep='/')
    if(verbose) cat("  processing site ", site, "\n")
    # process, if any, medatata.txt here
    if(file.exists(paste(sitePath, .pkgOptions$metadataFileName, sep='/'))) {
      .pkgOptions$metadata[[site]] <- .parseMetadata(path=sitePath)
    } else {
      warning("  no medatata found for site ", site, "\n")
    }
    cameraNames <- listCameraDir(site)
    #'@note do we have to store metadata as globals?
    .pkgOptions$metadata[[site]]$cameras <- list()
    siteData <- list()
    for(camera in cameraNames) { # process a camera directory
      cameraPath <- paste(sitePath, camera, sep='/')
      if(verbose) cat("    processing camera ", camera, "\n")
      # process metadata (must be there!)
      .pkgOptions$metadata[[site]][[camera]] <- .parseMetadata(path=cameraPath)
      sdcardDirs <- listDataDir(site, camera)
      cameraData <- list()
      for(sdcard in sdcardDirs) { # process data dump directories
        dataPath <- paste(cameraPath, sdcard, sep='/')
        if(verbose) cat("      processing sdcard ", sdcard, "\n")
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
  # shape up to a standard catalog
  emptyCatalog <- .createCatalog()
  # align the catalog to the basic catalog structure, doing a 'fast merge' as per http://stackoverflow.com/a/32162311/3215235
  # get columns in catalogData, but not in siteData
  diffCols <- setdiff(names(emptyCatalog), names(catalogData)) # order is mandatory
  # add blank columns to siteData
  for(i in 1:length(diffCols)) {
    catalogData[diffCols[i]] <- NA
  }
  # make sure field types are all OK
  # note that passing a 'tz' to as.POSIX* means that tz is a scalar (ugly but true): https://stackoverflow.com/questions/32084042/converting-to-local-time-in-r-vector-of-timezones
  if(length(unique(catalogData$Timezone))>1) { # we have more than a single timezone, split up, set, reassemble
    tmpCatalogByTz <- split(catalogData, catalogData$Timezone)
    for(tz in names(tmpCatalogByTz)) {
      tmpCatalogByTz[[tz]]$Photo.Timestamp <- as.POSIXct(paste(tmpCatalogByTz[[tz]]$Photo.Date, tmpCatalogByTz[[tz]]$Photo.Time), tz=tz)
    }
    catalogData <- do.call('rbind', tmpCatalogByTz)
    rm(tmpCatalogByTz)
  } else { # all data share a single timezone
    tz <- catalogData[1,]$Timezone
    catalogData$Photo.Timestamp <- as.POSIXct(paste(catalogData$Photo.Date, catalogData$Photo.Time), tz=tz)
  }
  invisible(catalogData)
}


#### parallel traverse all the directories in a repository ####################
updateCatalog2 <- function(verbose=FALSE) {
  theRepo <- getRepository()
  # just list directories
  theDirs <- list.dirs(path=theRepo, full.names=FALSE, recursive=TRUE)
  # remove @-names
  theDirs <- theDirs[grep("^@", theDirs, invert=TRUE)]
  # 1st element is current directory, drop it
  theDirs <- theDirs[-1]
  # assign depth levels
  theDirs <- data.frame(path=theDirs, level=lengths(regmatches(theDirs, gregexpr("/", theDirs))))
  theDirs$path <- as.character(theDirs$path)
  # level 0 -> site; level 1 -> camera; level 2 -> sd card
  theDirs$level <- factor(theDirs$level, levels=c(0,1,2), labels=c('site', 'camera', 'sdcard'))
  # size up jobs
  theCards <- theDirs[theDirs$level=='sdcard',]
  # go parallel 1: get EXIF data from sdcard directories
  EXIFData <- parallel::mclapply(paste(theRepo, theCards$path, sep='/'), function(x) getEXIFData(x, tz=Sys.timezone(), offset=0)) # timezone data will be fixed after
  EXIFData <- do.call('rbind', EXIFData)
  # shape up to a standard catalog
  emptyCatalog <- .createCatalog()
  # align the catalog to the basic catalog structure, doing a 'fast merge' as per http://stackoverflow.com/a/32162311/3215235
  # get columns in catalogData, but not in siteData
  diffCols <- setdiff(names(emptyCatalog), names(EXIFData)) # order is mandatory
  # add blank columns to siteData
  for(i in 1:length(diffCols)) {
    EXIFData[diffCols[i]] <- NA
  }
  # fix some fields content, LATER: add camera metadata
  EXIFData$Raw.Path <- dirname(EXIFData$Raw.Names)
  EXIFData$Raw.Names <- basename(as.character(EXIFData$Raw.Names))
  # pull out camera directory name
  camNames <- do.call('rbind',strsplit(EXIFData[,'Raw.Path'], '/'))
  EXIFData$Sampling.Unit.Name <- camNames[,ncol(camNames)-1]
  rm(camNames)
  # get and attach camera metadata
  theCameras <- theDirs[theDirs$level=='camera',]
  # go parallel 2: get metadata from camera directories
  cameraData <- parallel::mclapply(paste(theRepo, theCameras$path, sep='/'), function(x) .parseMetadata(x))
  names(cameraData) <- basename(theCameras$path)
  EXIFData <- split(EXIFData, EXIFData$Sampling.Unit.Name)
  for(cam in names(cameraData)) { # attach camera metadata
    if(nrow(EXIFData[[cam]]) > 0) {
      EXIFData[[cam]]$Camera.Serial.Number <- cameraData[[cam]]$serial
      EXIFData[[cam]]$Camera.Start.Date.and.Time <- cameraData[[cam]]$start
      EXIFData[[cam]]$Camera.End.Date.and.Time <- cameraData[[cam]]$end
      EXIFData[[cam]]$Camera.Manufacturer <- cameraData[[cam]]$make
      EXIFData[[cam]]$Camera.Model <- cameraData[[cam]]$model
      EXIFData[[cam]]$Latitude <- as.numeric(cameraData[[cam]]$lat)
      EXIFData[[cam]]$Longitude <- as.numeric(cameraData[[cam]]$lon)
    }
  }
  #'@note correct here for timezone?
  EXIFData <- do.call('rbind', EXIFData)
  #'@note get and attach site metadata



    # make sure field types are all OK
  # note that passing a 'tz' to as.POSIX* means that tz is a scalar (ugly but true): https://stackoverflow.com/questions/32084042/converting-to-local-time-in-r-vector-of-timezones
  if(length(unique(catalogData$Timezone))>1) { # we have more than a single timezone, split up, set, reassemble
    tmpCatalogByTz <- split(catalogData, catalogData$Timezone)
    for(tz in names(tmpCatalogByTz)) {
      tmpCatalogByTz[[tz]]$Photo.Timestamp <- as.POSIXct(paste(tmpCatalogByTz[[tz]]$Photo.Date, tmpCatalogByTz[[tz]]$Photo.Time), tz=tz)
    }
    catalogData <- do.call('rbind', tmpCatalogByTz)
    rm(tmpCatalogByTz)
  } else { # all data share a single timezone
    tz <- catalogData[1,]$Timezone
    catalogData$Photo.Timestamp <- as.POSIXct(paste(catalogData$Photo.Date, catalogData$Photo.Time), tz=tz)
  }
  invisible(catalogData)
}
