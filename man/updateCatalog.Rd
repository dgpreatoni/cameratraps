\name{updateCatalog}
\alias{updateCatalog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create/update a camera trap data catalog
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
updateCatalog(verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (verbose = FALSE)
{
    theRepo <- getRepository()
    if (verbose)
        cat("Repo is ", theRepo, "\n")
    siteNames <- listSiteDir()
    if (verbose)
        cat("Sites: ", siteNames, "\n")
    catalogData <- list()
    for (site in siteNames) {
        sitePath <- paste(theRepo, site, sep = "/")
        if (verbose)
            cat("\tprocessing site ", site, "\n")
        if (file.exists(paste(sitePath, "metadata.txt", sep = "/"))) {
            .pkgOptions$metadata[[site]] <- .parseMetadata(path = sitePath)
        }
        else {
            warning("\tno medatata found for site ", site, "\n")
        }
        cameraNames <- listCameraDir(site)
        .pkgOptions$metadata[[site]]$cameras <- list()
        siteData <- list()
        for (camera in cameraNames) {
            cameraPath <- paste(sitePath, camera, sep = "/")
            if (verbose)
                cat("\t\tprocessing camera ", camera, "\n")
            .pkgOptions$metadata[[site]][[camera]] <- .parseMetadata(path = cameraPath)
            sdcardDirs <- listDataDir(site, camera)
            cameraData <- list()
            for (sdcard in sdcardDirs) {
                dataPath <- paste(cameraPath, sdcard, sep = "/")
                if (verbose)
                  cat("\t\t\tprocessing sdcard ", sdcard, "\n")
                sdcData <- getEXIFData(dataPath, tz = .pkgOptions$metadata[[site]][[camera]][["timezone"]])
                if (nrow(sdcData) > 0) {
                  cameraData[[sdcard]] <- sdcData
                }
            }
            cameraData <- do.call("rbind", cameraData)
            cameraData$Raw.Path <- dataPath
            cameraData$Raw.Names <- basename(as.character(cameraData$Raw.Names))
            cameraData$Camera.Serial.Number <- .pkgOptions$metadata[[site]][[camera]][["serial"]]
            cameraData$Camera.Start.Date.and.Time <- .pkgOptions$metadata[[site]][[camera]][["start"]]
            cameraData$Camera.End.Date.and.Time <- .pkgOptions$metadata[[site]][[camera]][["end"]]
            cameraData$Camera.Manufacturer <- .pkgOptions$metadata[[site]][[camera]][["make"]]
            cameraData$Camera.Model <- .pkgOptions$metadata[[site]][[camera]][["model"]]
            cameraData$Latitude <- as.numeric(.pkgOptions$metadata[[site]][[camera]][["lat"]])
            cameraData$Longitude <- as.numeric(.pkgOptions$metadata[[site]][[camera]][["lon"]])
            cameraData$Sampling.Unit.Name <- camera
            siteData[[camera]] <- cameraData
        }
        siteData <- do.call("rbind", siteData)
        catalogData[[site]] <- siteData
    }
    catalogData <- do.call("rbind", catalogData)
    row.names(catalogData) <- NULL
    invisible(catalogData)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
