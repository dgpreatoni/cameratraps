###############################################################################
# UAGRA R Scripts - camera_trap                               base_functions.R
###############################################################################
# Convenience functions for cameratrap [ackage]
# Note: <notes here>
#
# version 0.3
# created fra 20160826
# updated prea 20160829
# updated prea 20161106
# updated prea 20180119
# updated prea 20190502 moved startup functions to zzz.R
#                       getwd() and setwd() aren't used anymore
#                       timezone identification now uses lutz::tz_lookup_coords()
# updated prea 20190509 added roxygen tags
###############################################################################

#### tasks to be performed at package load must go in zzz.R

#### package reserved dot-functions ###########################################

#### list all directories in a directory ######################################
#' @title List directories within a repository directory
#' @description List all 'valid' directories inside a repository directtory
#' @param path=getRepository() an existing path, used as a repository root.\br Normally, no parameter is needed, since is is assumed the repository path set with ĺink{setRepository}.
#' @return A character vector containing the names of the directories in the specified directory, sorted alphabetiaclly. This command is non-recursive.
#' @seealso \link{list.dirs}
.getDirectoryContent <- function(path=getRepository()) {
  dirs <- list.dirs(path, full.names=FALSE, recursive=FALSE)
  # exclude directories whose name begins with "@"
  dirs <- dirs[grep("^@", dirs, invert=TRUE)]
  invisible(dirs)
}

#### create an empty dataframe for catalog data, assigns as global (column names as per Rovero and Zimmermann 2016)
#' @title Create an (empty) catalog
#' @description Create an empty dataframe with all the columns needed to store camera trap files data according to Rovero and Zimmermann.
#' @return a data frame object.
.createEmptyCatalog <- function() {
  catalogData <- data.frame(Organization.Name=character(),
                            Project.Name=character(),
                            Sampling.Unit.Name=character(),
                            Latitude=numeric(),
                            Longitude=numeric(),
                            Sampling.Event=character(),
                            Photo.Type=character(),
                            Photo.Date=character(),
                            Photo.Time=character(),
                            Timezone=character(),
                            Photo.Timestamp=character(),
                            Raw.Names=character(),
                            Raw.Path=character(),
                            Genus=character(),
                            Species=character(),
                            Number.of.Animals=numeric(),
                            Person.Identifying.the.Photo=character(),
                            Camera.Serial.Number=numeric(),
                            Camera.Start.Date.and.Time=character(),
                            Camera.End.Date.and.Time=character(),
                            Person.setting.up.the.Camera=character(),
                            Person.picking.up.the.Camera=character(),
                            Camera.Manufacturer=character(),
                            Camera.Model=character(),
                            Camera.Name=character(),
                            Site.Name=character(),
                            Sequence.Info=numeric())
  #assign("catalogData", catalogData, envir=.pkgOptions)
}


#### check whether a catalog file is present ##################################
#.catalogExists <- function() {
#  return(file.exists())
#}

## check if a catalog spreadsheet already exists: if yes, the existing catalog could contain user-entered information that must not be overwritten, such as species identification and repeated rows caused by multiple species in the same photo: all this has to be preserved
#if(file.exists(paste(catalogFileName, 'xls', sep='.'))) { # re-align old catalog to new one, the csv version is not checked for existence
#  oldCatalogData <- read.xlsx(file=paste(catalogFileName, 'xls', sep='.'), sheetName=projName)
#  oldCatalogData <- readWorksheetFromFile(paste(catalogFileName, 'xls', sep='.'), sheet=projName)
#  # get indexes for rows in catalogData that match thise present in oldCatalogData
#  matches <- match(paste(catalogData$Sampling.Event,catalogData$Raw.Names, sep=.Platform$file.sep), paste(oldCatalogData$Sampling.Event, oldCatalogData$Raw.Names, sep=.Platform$file.sep))
#  # eliminate mateching rows
#  newCatalogData <- catalogData[is.na(matches),]
#  if(nrow(newCatalogData)>0) {
#    catalogData <- rbind(oldCatalogData, newCatalogData)
#  }
#}



#### parse a metadata file, yielding a named array #############################
#' @export
#' @title Parse a \code{metadata.txt} file
#' @description Reads a site or camera medatata file and stores data present therein as a list of key/value pairs.
#' @param path=getwd() a valid path that acts either as a camera or site directory in a \code{cameratraps} repository.
#' @param metadataFile a character, by default is \code{'metadata.txt'}. This can be also changed as package option.
#' @param check boolean, default is \code{TRUE}. If the \code{'metadata.txt'} file is not present in \code{path} an error is returned. If \code{FALSE} the functions returns \code{NULL} if the metadata file is not found. Use with care.
#' @return a list of values, each list element is named as per the 'key' found in the metadata file/
#' @seealso \link{metadataFileFormat}
.parseMetadata <- function(path=getwd(), metadataFile=.pkgOptions$metadataFileName, check=TRUE) {
  metadataFilePath <- paste(path, metadataFile, sep=.Platform$file.sep)
  if(file.exists(metadataFilePath)==TRUE) { # process file
    # open silently, as connection
    conn <- file(metadataFilePath, open='r')
    lines <- readLines(conn, warn=FALSE) # final lines with no CRLF could raise warnings
    close(conn)
    # clean up comments: a comment is anything between a pond # and a <cr>
    lines <- gsub('#.*', '', lines)
    lines <- lines[lines!=""]
    # replace the first colon with a marker
    lines <- sub(':', "^", lines)
    lines <- strsplit(lines, "^", fixed=TRUE)
    # parse and collapse
    lines <- do.call('rbind', lapply(lines, function(x) data.frame(key=x[1], value=x[2])))
    lines$key <- trimws(lines$key)
    lines$value <- trimws(lines$value)
    metadata <- as.list(as.character(lines$value))
    names(metadata) <- tolower(lines$key) # dirty fix, in case someone mistyped keys...
    # finish up parsing any element that needs a specific type
    # as of August 2017 the "legal" tags are:
    # *make:    | Maker of the camera trap         | Please use consistent naming and spelling
    # *model:   | Model of the camera trap         | Please use consistent naming and spelling
    # *serial:  | Serial number of the camera trap | Please transcribe the exact serial number
    # *lat:     | Camera position, latitude        | Use raw latitude in decimal degrees, WGS84 (i.e. EPSG:4326)
    if(exists('lat', where=metadata)) { metadata$lat <- as.numeric(metadata$lat) }
    # *lon:     | Camera position, longitude       | Use raw longitude in decimal degrees, WGS84 (i.e. EPSG:4326)
    if(exists('lon', where=metadata)) { metadata$lon <- as.numeric(metadata$lon) }
    # timezone:| Time zone of camera position     | Use time Olson/IANA zone names as from R OlsonNames() function
    if(exists('timezone', where=metadata)) { # check for a valid timezone
      if(!any(OlsonNames()==metadata$timezone)) { # exists, but no match with Olson names
        metadata[['timezone']] <- as.character(lutz::tz_lookup_coords(metadata$lat, metadata$lon, method="accurate", warn=FALSE))
        #metadata[['timezone']] <- as.character(GNtimezone(metadata$lat, metadata$lon)$timezoneId)
        warning("Time zone ", metadata$timezone , " is not a valid time zone identifier in ", metadataFilePath, ".\nAttempting to derive time zone from latitude and longitude: ", metadata[['timezone']], ".\nSee ?OlsonNames() for valid time zone codes.")
      }
    } else { # timezone not present in metadata file, use lat/lon, if they exist, if not, no timezone info
      if(exists('lat', where=metadata) & exists('lon', where=metadata)) {
        metadata[['timezone']] <- as.character(lutz::tz_lookup_coords(metadata$lat, metadata$lon, method="accurate", warn=FALSE))
        #metadata[['timezone']] <- as.character(GNtimezone(metadata$lat, metadata$lon)$timezoneId)
        warning("Time zone information not found in ", metadataFilePath, ".\nAttempting to derive time zone from latitude and longitude: ", metadata[['timezone']], ".\n")
      }
    }
    # *start:   | Camera start timestamp           | Use date and time expressed in ISO 8601 format  yyyy-MM-ddTHH:mm:ssK
    if(exists('start', where=metadata)) {
      tzInfo <- ifelse(exists('timezone', where=metadata), metadata$timezone, Sys.timezone())  # use timezone, if doesn't exist default to local timezone
      metadata$start <- as.POSIXct(metadata$start, tz=tzInfo, format="%Y-%m-%dT%H:%M:%S", usetz=TRUE)
    }
    # *end:     | Camera end timestamp             | Use date and time expressed in ISO 8601 format  yyyy-MM-ddTHH:mm:ssK
    if(exists('end', where=metadata)) {
      tzInfo <- ifelse(exists('timezone', where=metadata), metadata$timezone, Sys.timezone())  # use timezone, if doesn't exist default to local timezone
      metadata$end <- as.POSIXct(metadata$end, tz=tzInfo, format="%Y-%m-%dT%H:%M:%S", usetz=TRUE)
    }
    # height:  | Camera ground height             | Height from ground at which the camera treap has been placed
    if(exists('height', where=metadata)) { metadata$height <- as.numeric(metadata$height) }
    # aspect:  | Camera lens aspect               | Aspect (in degrees, 0 to 360) at which the camera lens was facing
    if(exists('aspect', where=metadata)) { metadata$aspect <- as.numeric(metadata$aspect) }
    # placed:  | Who placed the camera            | First name, last name
    # removed: | Who removed the camera           | First name, last name
    # return data
    invisible(metadata)
  } else { # metadata file not found
    if(check==TRUE) { # complain if etadata file does not exist, this is standard behaviour
      stop("File ", .pkgOptions$metadataFileName, " not found in ", path, ". Aborting.\n")
    } else { # skip checks, be silent
      invisible(NULL)
    }
  }
}


#### public functions #########################################################

#### return path to image/video filesystem repository #########################
#' @export
#' @title Get the repository path
#' @description Return the current \code{cameratraps} repository root path
#' @return A character string or \code{NULL} and an error message, if a repository path has not been set.
#' @seealso \link{setRepository}
getRepository <- function() {
  rep <- get("repositoryPath", envir=.pkgOptions)
  if(is.null(rep)) {
    stop("Repository not defined. Please use setRepository()\n")
  } else {
    return(rep)
  }
}

#### set path to image/video filesystem repository ############################
#' @export
#' @note TODO add existing catalog ingestion
#' @title Set the repository path
#' @description Set the root directory for the current \code{cameratraps} repository
#' @param path character, a valid path.
#' @param create boolean: create that directory if it does not exist.
#' @param attach boolean (not yet implemented), check whether a catalog file has already been made and use it.
#' @return the catalog path as a charaxctes string, or \code{NULL} in case of an error.
setRepository <- function(path=getwd(), create=FALSE) {
  path <- normalizePath(path, mustWork=FALSE)
  pathExists <- dir.exists(path)
  if(pathExists==FALSE) {
    if(create) {
      dir.create(path)
      pathExists <- dir.exists(path) # should be TRUE now
    } else {
      stop("Repository ", path, " does not exist.")
      invisible(NULL)
    }
  }
  # assume path exists
  if(pathExists) {
    #@TODO if path exists, check for an existing catalog
    cat("Repository set to:", path, "\n")
    assign("repositoryPath", normalizePath(path), envir=.pkgOptions)
    invisible(path)
  }
}


#### pull out EXIF data for all AVI and JPEG files inside a directory #########
#' @export
#' @title extract EXIF data
#' @description get EXIF data from image and video files in a given directory
#' @param EXIFDir character, a \code{cameratraps} "sd card directory", i.e. a directory where camera trap files are stored.
#' @param tz character, see \link{OlsonNames}: a valit time zone designator, will be used ot fix EXIF timestamps accordin ti the time zone where the camera trap operated.
#' @param offset numeric, a time offset in hours (decimal hours) that will be algebtically added to EXIF timestamps. Use with care.
#' @return a dataframe containing file names (\code{Raw.Names}), a timestamp (\code{Photo.Time}), the type of file (\code{Photo.Type}, and the "camera directory" where each file is stored (\code{Sampling.Event}).
getEXIFData <- function(EXIFDir=getwd(), tz=Sys.timezone(), offset=0, verbose=FALSE) {
  # for maintenance purpose: all known file extensions are now declarde as package global .pkgOptions$known.extensions
  #known_extensions <- c('AVI', 'avi', 'JPG', 'jpg', 'M4V', 'm4v', 'MOV', 'mov', 'MOD', 'mod', 'MP4', 'mp4')
  # Some notes on time and time zone information:
  # as it seems, exiftool extracts timestamps assuming they are in the timezone of the machine where exiftool itself is running.
  # This can be undesirable, as sometimes we need the timestamps in the timezone the camera trap operated in, which may be different from the timezone we're working.
  # If a time zone name (i.e. a valid Olson time zone designator) or a time zone offset has been supplied, time zone information will be corrected accordingly.
  # check for time zone
  if(tz %in% OlsonNames() == FALSE) stop("Time zone name \"", tz, "\" is not compliant to Olson/IANA format.\n  Please see ?OlsonNames.")
  tmpCsvFile <- tempfile(pattern=paste("EXIF", gsub(.Platform$file.sep, '-', EXIFDir), sep=''), fileext=".csv")
  # fix csv file names containing spaces
  tmpCsvFile <- gsub(' ', '_', tmpCsvFile)
  # ask exiftool to pull out just the tags we need, some files can hide _binary_ tags that are difflcult to process...
  #@FIXME EXIFTOOL MUST BE PASSED the directory...
  res <- system2(.pkgOptions$EXIFTOOL, args=paste('-FileModifyDate -Filetype -CHARSET UTF8 ', paste('-ext', .pkgOptions$known.extensions, collapse=' '), ' -csv "',  normalizePath(EXIFDir), '" > ', tmpCsvFile, sep=''), stderr=verbose)
  if(res==0){ # EXIFTOOL exited nicely, we have a csv to parse
    EXIFData <- utils::read.csv(tmpCsvFile, stringsAsFactors=FALSE)
    unlink(tmpCsvFile)
    names(EXIFData) <- c('Raw.Names','Photo.Time','Photo.Type')
    # process Photo.Time to be compliant with Rovero and Zimmermann
    # the standard EXIF timestamp format is "YYYY:mm:dd HH:MM:SS", plus, if any, timezone info as "+HH:MM" or "-HH:MM" or "Z"
    EXIFData$Photo.Time <- as.character(EXIFData$Photo.Time)
    # separate timestamp from EXIF time zone
    EXIFData$XXtime <- substr(EXIFData$Photo.Time, 1, 19)
    # convert into POSIX datetime, use timezone info if available, if not, use Sys.timezone
    EXIFData$XXtime <- as.POSIXct(EXIFData$XXtime, format="%Y:%m:%d %H:%M:%S") # in Sys.timezone (which could be wrong)
    # add an offset (in seconds) if needed
    EXIFData$XXtime <- EXIFData$XXtime + offset # e.g. 19800 if is 5.30h
    # correct for timezone
    if(tz!=Sys.timezone()) { # a timezone has been specified, correct timestamps to that timezone
      EXIFData$XXtime <- format(EXIFData$XXtime, tz=tz, usetz=TRUE)
    } else { # keep Sys.timezone @TODO perhaps this else clause is unnecessary...
      EXIFData$XXtime <- format(EXIFData$XXtime)
    }
    # format as per Rovero and Zimmermann
    EXIFData$Photo.Date <- substr(EXIFData$Photo.Time, 1, 10)
    EXIFData$Photo.Date <- gsub(':', '-', EXIFData$Photo.Date)
    EXIFData$Photo.Time <- substr(EXIFData$Photo.Time, 12, 19)
    EXIFData$XXtime <- NULL
    EXIFData$Timezone <- tz
    EXIFData$Sampling.Event <- basename(EXIFDir) # use basename assuming EXIFDir is a full path
    #EXIFData$Photo.Type <- as.character(EXIFData$Photo.Type)
  } else { # EXIFTOOL call returned an error, create an empty dataframe
    EXIFData <- data.frame(Raw.Names=character(), Photo.Time=character(),Photo.Type=character(), Sampling.Event=character())
  }
  invisible(EXIFData)
}

#### End of File ####
