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


#### check if an XLSX catalog exists ##########################################
.catalogFileExists <- function() {
  res <- logical()
  res['xlsx'] <- file.exists(paste(getRepository(), 'catalog.xlsx', sep='/'))
  res['RDS'] <- file.exists(paste(getRepository(), .getOption('catalogFileName'), sep='/'))
  return(res)
}


#### read in a catalog file ##################################################
.readCatalogFile <- function() {
  whichCatalog <- .catalogFileExists()
  if(all(whichCatalog)==FALSE) stop('No catalog file(s) found in ', getRepository(), ". Generate a catalog with createCatalog().")
  if(whichCatalog['xlsx']==TRUE) { # prefer xlsx version
    .setOption('catalog', readxl::read_excel(path=paste(getRepository(), 'catalog.xlsx', sep='/')))
    message('Catalog file for ', getRepository(), ' read.')
  } else {
    if(whichCatalog['RDS']==TRUE) { # use the RDF version as a backup
      .setOption('catalog', readRDS(file=paste(getRepository(), .getOption('catalogFileName'))))
      message('RDS catalog file for ', getRepository(), ' read.\n  RDS version is a backup copy and may be out of sync.')
    } else { # no catalog file present?
      stop("No catalog file present in repository ", getRepository())
    }
  }
}


#### create a catalog ########################################################
#' @export
createCatalog <- function(verbose=TRUE) {
  if(.catalogExists()==TRUE) {
    stop("a catalog exists, must be just updated")
  } else {
    .setOption('catalog', .createCatalog(verbose=verbose))
    #'@note todo implement writing the catalog as a spreadsheet and as a .RData file.
    cat("about to save to ", paste(getRepository(), .getOption('catalogFileName'), sep='/'), "\n")
    saveRDS(.getOption('catalog'), file=paste(getRepository(), .getOption('catalogFileName'), sep='/'))
    cat("RDS saved, now saving ",paste(getRepository(), 'catalog.xlsx', sep='/'), "\n")
    writexl::write_xlsx(.getOption('catalog'), path=paste(getRepository(), 'catalog.xlsx', sep='/')) #, sheetName='Catalog', row.names=FALSE)
    message("catalog file written to ", getRepository())
  }
}


#### update catalog ###########################################################
#' @title Update an existing catalog
#' @description Check against the existing catalog and add just the new (if any) camera trap files and metadata.
#' @return a data frame object.
#' @export
updateCatalog <- function() {
  theRepo <- getRepository()
  if(.catalogExists()) {
    # catalog exists, update it
    message("catalog exists, updating.")
    # get all the filenames in the physical catalog
    allFiles <- .getAllFiles()
    catFiles <- .getOption("catalog")[,c('Raw.Path', 'Raw.Names')]
    # check for matching files (i.e. filenames already present in the catalog)
    matches <- match(paste(allFiles$Raw.Path, allFiles$Raw.Names, sep='/'), paste(catFiles$Raw.Path, catFiles$Raw.Names, sep='/'))
    # eliminate matching (i.e. already in the catalog) rows
    newFiles <- allFiles[is.na(matches),]
    if(nrow(newFiles)>0) { # we have new files!
      #' @note @todo scan and get EXIF just for the new files, we have Raw.Path + Raw.Names
      #' @note @todo append to the existing catalog
      #' @note @todo write out xlsx and RDS files
    }
  } else {
    warning("a repository catalog does not exists for ", theRepo, ".\n  create a catalog with ?createCatalog().")
  }
}
