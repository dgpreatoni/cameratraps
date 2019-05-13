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

#### check if a catalog exists ###############################################
.catalogExists <- function() {
  return(is.null(.pkgOptions$catalog)==FALSE)
}

#### create a catalog ########################################################
#' @export
createCatalog <- function(verbose=TRUE) {
  if(.catalogExists()==TRUE) {
    stop("a catalog exists, must be just updated")
  } else {
    .pkgOptions$catalog <- .createCatalog(verbose=verbose)
    #'@note todo implement writing the catalog as a spreadsheet and as a .RData file.
    saveRDS(.pkgOptions$catalog, file=paste(getRepository(), .pkgOptions$catalogFilename))
    xlsx::write.xlsx2(.pkgOptions$catalog, file='catalog.xlsx', sheetName='Catalog', row.names=FALSE)
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
    cat("catalog exists, updating.")
    allFiles <- .getAllFiles()
    # check for matching (i.e. already present in the catalog) files
    matches <- match(paste(catalogData$Sampling.Event,catalogData$Raw.Names, sep='/'), paste(oldCatalogData$Sampling.Event, oldCatalogData$Raw.Names, sep='/'))
    # eliminate mateching rows
    newCatalogData <- catalogData[is.na(matches),]
    if(nrow(newCatalogData)>0) {
      catalogData <- rbind(oldCatalogData, newCatalogData)
    }



  } else {
    warning("a repository catalog does not exists for ", theRepo, ".\n  Create a catalog with ?createCatalog().")
  }
}
