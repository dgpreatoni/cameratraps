###############################################################################
# UAGRA R Scripts - camera_trap                               handle_catalog.R
###############################################################################
# Functions to handle a camera trap data catalog.
#
# Notes
# The package keeps a copy of the 'catalog' in memory. To minimize filesystem
# traversals, if a cached version is found (as a spreadsheet and/or a .rds file
# (i.e. written by saveRDS()) in a project root directory), the package
# first updates the catalog using the spreadsheet version , then rewrites the .rds.
#
#
#
# version 0.1
# created prea 20190513
###############################################################################

#'@note todo: implement a new updateCatalog*(), that actually _updates_, i.e. pulls out of the repository just a list of filenames in each and every sdcard directory, checks against existing catalog and adds just the new (if any) files.


#### check if an in-memory catalog exists #####################################
.catalogExists <- function() {
  return(is.null(.getOption('catalog'))==FALSE)
}


#### check if a catalog file exists ###########################################
#' @export
.catalogFileExists <- function() {
  res <- logical()
  res['xlsx'] <- file.exists(paste(getRepository(), 'catalog.xlsx', sep='/'))
  res['RDS'] <- file.exists(paste(getRepository(), .getOption('catalogFileName'), sep='/'))
  return(res)
}

#### returns the in-memory catalog ###########################################
#' @export
getCatalog <- function() {
  return(.getOption('catalog'))
}


#### read in a catalog file ###################################################
.readCatalogFile <- function() {
  whichCatalog <- .catalogFileExists()
  if(any(whichCatalog)==FALSE) stop('No catalog file(s) found in ', getRepository(), ". Generate a catalog with createCatalog().")
  if(whichCatalog['xlsx']==TRUE) { # prefer xlsx version
    .setOption('catalog', readxl::read_excel(path=paste(getRepository(), 'catalog.xlsx', sep='/')))
    message('Catalog file for ', getRepository(), ' read.')
    .setOption('catalogHasChanged', FALSE)
  } else {
    if(whichCatalog['RDS']==TRUE) { # use the RDF version as a backup
      .setOption('catalog', readRDS(file=paste(getRepository(), .getOption('catalogFileName'))))
      message('RDS catalog file for ', getRepository(), ' read.\n  RDS version is a backup copy and may be out of sync.')
      .setOption('catalogHasChanged', FALSE)
    } else { # no catalog file present?
      stop("No catalog file present in repository ", getRepository())
    }
  }
}


#### write out a catalog file ################################################
.writeCatalogFile <- function() {
  whichCatalog <- .catalogFileExists()
  if(any(whichCatalog)==TRUE) { # we have at least a file catalog, warn and overwrite
    warning("a catalog file(s) exists in ", getRepository(), " and will be overwritten.")
  }
  # suppress this if too verbose
  message("saving RDS catalog to ", paste(getRepository(), .getOption('catalogFileName'), sep='/'), ".")
  saveRDS(.getOption('catalog'), file=paste(getRepository(), .getOption('catalogFileName'), sep='/'))
  message("saving xlsx catalog to ", paste(getRepository(), 'catalog.xlsx', sep='/'), ".")
  writexl::write_xlsx(.getOption('catalog'), path=paste(getRepository(), 'catalog.xlsx', sep='/')) #, sheetName='Catalog', row.names=FALSE)
  message("catalog files written to ", getRepository(), ".")
  .setOption('catalogHasChanged', FALSE)
}


#### create a catalog ########################################################
#' @title Create a catalog
#' @description Check whether a catalog file exists, if not create and write to repository a new catalog.
#' @return nothing. The catalog is stored as an in-package  object.
#' @export
createCatalog <- function(verbose=TRUE) {
  if(.catalogExists()==TRUE) {
    stop("a catalog exists, must be just updated. Use ?updateCatalog() instead.")
  } else {
    .setOption('catalog', .createCatalog(verbose=verbose))
    .writeCatalogFile()
  }
}


#### update catalog ###########################################################
#' @title Update an existing catalog
#' @description Check against the existing catalog and add just the new (if any) camera trap files and metadata.
#' @return nothing. The catalog is stored as an in-package  object.
#' @export
updateCatalog <- function() {
  theRepo <- getRepository()
  if(.catalogExists()==TRUE) {
    # catalog exists, update it
    message("catalog exists, updating.")
    # get all the filenames in the physical repository
    allFiles <- .getAllFiles()
    # get all the filenames in the current catalog
    catFiles <- .getOption("catalog")[,c('Raw.Path', 'Raw.Names')]
    # check for matching files (i.e. filenames already present in the catalog)
    matches <- match(paste(allFiles$Raw.Path, allFiles$Raw.Names, sep='/'), paste(catFiles$Raw.Path, catFiles$Raw.Names, sep='/'))
    # eliminate matching files (i.e. already in the catalog)
    newFiles <- allFiles[is.na(matches),]
    if(nrow(newFiles)>0) { # we have new files!
      #' @note @todo pull out unique paths
      #' @note @todo scan and get EXIF just for the new files, we have Raw.Path + Raw.Names
      #' @note @todo append to the existing catalog
      #' @note @todo write out xlsx and RDS files
    }
  } else {
    warning("a repository catalog does not exists for ", theRepo, ".\n  create a catalog with ?createCatalog().")
  }
}
