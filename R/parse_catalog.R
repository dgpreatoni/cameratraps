###############################################################################
# UAGRA R Scripts - Rcameratraps                              parse_catalog.R
###############################################################################
# functions to parse camera trap data stored on a live filesystem
# Note: <notes here>
#
# version 0.2
# created fra  20160826
# updated prea 20190808 cleaned up code, fixwd roxygen tags
#         prea 20190506 cleaned up stuff, rewrote from scratch updateCatalog()
#                       parallel experimental implementation of updateCatalog2
# updated prea 20180216
# updated prea 20180119
# updated prea 20161106
# updated prea 20160829

###############################################################################

## tasks to be performed at package load must go in zzz.R


#### package reserved dot-functions ###########################################
# dot-functions aren't @export-ed
# dot-functions aren't doxygenized


#### get all file names in a repository #######################################
.getAllFiles <- function() {
  fileNames <- data.frame(Raw.Names=character(), Raw.Path=character()) # use field names from .createEmptyCatalog()
  theRepo <- getRepository()
  siteDirs <- listSiteDir()
  for(s in siteDirs) {
    cameraDirs <- listCameraDir(s)
    for(c in cameraDirs) {
      dataDirs <- listDataDir(s, c)
      for(d in dataDirs) {
        fileNames <- rbind(fileNames, data.frame(Raw.Names=list.files(path=paste(theRepo, s, c, d, sep=.Platform$file.sep), pattern=paste0(paste0(.getOption("known.extensions"),"$"), collapse="|")), Raw.Path=paste(theRepo, s, c, d, sep=.Platform$file.sep), stringsAsFactors=FALSE))
      }
    }
  }
  invisible(fileNames)
}


#### create (without storing) a catalog of a repository #######################
.createCatalog <- function(verbose=FALSE) {
  theRepo <- getRepository()
  if(verbose) cat('Repository is ', theRepo, '\n')
  siteNames <- listSiteDir()
  if(verbose) cat("Sites: ", siteNames, "\n")
  catalogData <- list()
  for(site in siteNames) { # process a site directory
    sitePath <- paste(theRepo, site, sep=.Platform$file.sep)
    if(verbose) cat("  processing site ", site, "\n")
    # process, if any, medatata.txt here
    if(file.exists(paste(sitePath, .pkgOptions$metadataFileName, sep=.Platform$file.sep))) {
      .pkgOptions$metadata[[site]] <- .parseMetadata(path=sitePath)
    } else {
      warning("  no medatata found for site ", site, "\n")
    }
    cameraNames <- listCameraDir(site)
    #'@note do we have to store metadata as globals?
    .pkgOptions$metadata[[site]]$cameras <- list()
    siteData <- list()
    for(camera in cameraNames) { # process a camera directory
      cameraPath <- paste(sitePath, camera, sep=.Platform$file.sep)
      if(verbose) cat("    processing camera ", camera, "\n")
      # process metadata (must be there!)
      .pkgOptions$metadata[[site]][[camera]] <- .parseMetadata(path=cameraPath)
      sdcardDirs <- listDataDir(site, camera)
      cameraData <- list()
      for(sdcard in sdcardDirs) { # process data dump directories
        dataPath <- paste(cameraPath, sdcard, sep=.Platform$file.sep)
        if(verbose) cat("      processing sdcard ", sdcard, "\n")
        sdcData <- getEXIFData(dataPath, tz=.pkgOptions$metadata[[site]][[camera]][['timezone']])
        if(nrow(sdcData) > 0) {
          cameraData[[sdcard]] <- sdcData
        }
      } # sdcard (dataDirs) loop
      # flatten sd card data
      cameraData <- do.call('rbind', cameraData)
      # fix some fields content, add camera metadata, this also happens in handle_catalog.R::updateCatalog()
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
      cameraData$Camera.Name <- .pkgOptions$metadata[[site]][[camera]][['name']]
      cameraData$Site.Name <- .pkgOptions$metadata[[site]][['name']]
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
  emptyCatalog <- .createEmptyCatalog()
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


#### parallel version of createCatalog ########################################
#' @note this is just experimental and should not be used
#' @note looks like parallelisation doesn't speed up things
.createCatalog2 <- function(verbose=FALSE) {
  theRepo <- getRepository()
  # just list directories
  theDirs <- list.dirs(path=theRepo, full.names=FALSE, recursive=TRUE)
  # remove @-names
  theDirs <- theDirs[grep("^@", theDirs, invert=TRUE)]
  # 1st element is current directory, drop it
  theDirs <- theDirs[-1]
  # assign depth levels
  theDirs <- data.frame(path=theDirs, level=lengths(regmatches(theDirs, gregexpr(.Platform$file.sep, theDirs))))
  theDirs$path <- as.character(theDirs$path)
  # level 0 -> site; level 1 -> camera; level 2 -> sd card
  theDirs$level <- factor(theDirs$level, levels=c(0,1,2), labels=c('site', 'camera', 'sdcard'))
  # size up jobs
  theCards <- theDirs[theDirs$level=='sdcard',]
  # go parallel 1: get EXIF data from sdcard directories
  catalogData <- parallel::mclapply(paste(theRepo, theCards$path, sep=.Platform$file.sep), function(x) getEXIFData(x, tz=Sys.timezone(), offset=0)) # timezone data will be fixed after
  catalogData <- do.call('rbind', catalogData)
  # shape up to a standard catalog
  emptyCatalog <- .createEmptyCatalog()
  # align the catalog to the basic catalog structure, doing a 'fast merge' as per http://stackoverflow.com/a/32162311/3215235
  # get columns in catalogData, but not in siteData
  diffCols <- setdiff(names(emptyCatalog), names(catalogData)) # order is mandatory
  # add blank columns to siteData
  for(i in 1:length(diffCols)) {
    catalogData[diffCols[i]] <- NA
  }
  # fix some fields content, LATER: add camera metadata
  catalogData$Raw.Path <- dirname(catalogData$Raw.Names)
  catalogData$Raw.Names <- basename(as.character(catalogData$Raw.Names))
  # pull out camera directory name
  camNames <- do.call('rbind',strsplit(catalogData[,'Raw.Path'], .Platform$file.sep))
  catalogData$Sampling.Unit.Name <- camNames[,ncol(camNames)-1]
  rm(camNames)
  # get and attach camera metadata
  theCameras <- theDirs[theDirs$level=='camera',]
  # go parallel 2: get metadata from camera directories
  cameraData <- parallel::mclapply(paste(theRepo, theCameras$path, sep=.Platform$file.sep), function(x) .parseMetadata(x))
  names(cameraData) <- basename(theCameras$path)
  catalogData <- split(catalogData, catalogData$Sampling.Unit.Name)
  for(cam in names(cameraData)) { # attach camera metadata
    if(nrow(catalogData[[cam]]) > 0) {
      catalogData[[cam]]$Camera.Serial.Number <- cameraData[[cam]]$serial
      catalogData[[cam]]$Camera.Start.Date.and.Time <- cameraData[[cam]]$start
      catalogData[[cam]]$Camera.End.Date.and.Time <- cameraData[[cam]]$end
      catalogData[[cam]]$Camera.Manufacturer <- cameraData[[cam]]$make
      catalogData[[cam]]$Camera.Model <- cameraData[[cam]]$model
      catalogData[[cam]]$Latitude <- as.numeric(cameraData[[cam]]$lat)
      catalogData[[cam]]$Longitude <- as.numeric(cameraData[[cam]]$lon)
      catalogData[[cam]]$Sampling.Unit.Name <- cam
      catalogData[[cam]]$Camera.Name <-cameraData[[cam]]$name
    }
  }
  #'@note correct here for timezone?
  catalogData <- do.call('rbind', catalogData)
  #'@note get and attach site metadata
  theSites <- theDirs[theDirs$level=='site',]
  # go parallel 3: get metadata from site directories (if any)
  siteData <- parallel::mclapply(paste(theRepo, theSites$path, sep=.Platform$file.sep), function(x) .parseMetadata(x, check=FALSE))
  names(siteData) <- basename(theSites$path)
  #'@note todo: site data are read but not used, actually we use  'name'...
  catalogData <- split(catalogData, substr(catalogData$Sampling.Unit.Name, 1, 8))
  for(site in names(siteData)) { # attach, if any, site metadata
    catalogData[[site]]$Site.Name <- siteData[[site]]$name
  }
  catalogData <- do.call('rbind', catalogData)
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


#### public functions #########################################################


#### list all "site directories" in a repository ##############################
#' @export
#' @title list repository structures
#' @description list site directories in a repository
#' @family repository functions
listSiteDir <- function() {
  siteList <- .getDirectoryContent()
  return(siteList)
}


#### list all "camera directories" in a site directory ########################
#' @export
#' @title list repository structures
#' @description list camera directories in a repository
#' @family repository functions
listCameraDir <- function(siteDirName) {
  rep <- getRepository()
  path <- paste(rep, siteDirName, sep=.Platform$file.sep)
  camList <- .getDirectoryContent(path)
  return(camList)
}


#### list all "data directories" in a camera directory ########################
#' @export
#' @title list repository structures
#' @description list sd card data directories in a repository
#' @family repository functions
listDataDir <- function(siteDirName, cameraDirName) {
  rep <- getRepository()
  path <- paste(rep, siteDirName, cameraDirName, sep=.Platform$file.sep)
  dataList <- .getDirectoryContent(path)
    # if need be, we can filter out here the directory names patterned as YYYY=MM-DD
  # cardDir <- cardDir[grepl("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}", cardDir$name),]
  return(dataList)
}


#### End Of File ####
