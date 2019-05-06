\name{repository structure}
\alias{repositoryStructure}
\docType{data}
\title{
Camera Trap repository structure
}
\description{
Files and directories organization in a \code{cameratraps} repository.
}
\usage{}
\format{}
\details{
In the \code{cameratraps} package, camera trap data (i.e. images and videos) are stored "raw" on a file system: no database is involved and physical storage is kept as simple as possible: for example, a shared directory on a slow but terabyte-spacious Network Attached Storage could be conveniently used.

That is to say, some where exists a "\emph{data warehouse}", i.e. a physical host (a NAS, for instance, serving a shared directory of some sort), where all the camera trap material is stored.

\section{\bold{Project directory}}

Inside the data warehouse directory, a "\emph{project directory}" must be created for each project producing camera trap data. A project directory can be named at one's choice. A good advice is ti define (if it hasn't already been made) a consistent acronym for the project itself, and use it as a \emph{prefix} in the project directory name (e.g. \code{camera_traps/LAM-Lampi}, where \code{camera_traps} is the shared directory data warehouse, and \code{LAM-Lampi} is the directory containing all the material for a project called "Lampi".

Inside the main data warehouse root directory, it can also exist a \code{scripts} directory, that contains all the programs needed for maintenance and handling of the whole repository. Do not touch its contents if you're not conscious of what you're doing. Thou hast beeh warned.

A project subdirectory can contain a "\emph{catalog file}": actually, if \code{cameratraps} is in use, a catalog file should exist there, automatically made by the \link{updateCatalog} function. The catalog file can be opened and edited by hand, but every time the \link{updateCatalog} function is run, any information preesnt in the catalog will be updated, if something in the files and directories structure has changed. The only exception is for the columns named \code{Genus} and \code{Species}, that are intended as user-editable columns: in this case, if \link{update|Catalog} finds some non-\code{NULL} value, it will preserve the entire record (i.e. row).

Here and there in the directories constituting the project directory, a "metedata file" can be present. See \link{metadataFileFormat} for further details.

Any file or directory whose name begins with \code{@} will be ignored. This in part is a legacy from old Synology NAS, but can come handy.

\section{Site directory}

Inside the project subdirectory a "\emph{site directory}" must be created for each camera trap array. This means that if you have just a single camera, you \bold{must} create a site directory for just that single camera.

A site directory \bold{must} be named as follows:\cr
  \code{<PRJ>-S<sss>}\cr
where \code{<PRJ>} is the project acronym, all uppercase; and \code{<sss>} is a three-digit number (nota thet this allows for only 1000 "sites" for a single "project").

Inside a site directory, if needed, a "\emph{metadata file}" \bold{can} exist. See  \link{metadataFileFormat} for details on metadata file format and structure.

\section{Camera trap directory}

A site directory contains one or more "\emph{camera trap directories}", one for each camera trap constituting the camera trap array deployed in that site. Note that a "\emph{camera trap}" in this context is not simply a camera trap, but represents a given camera trap deployed in that site for a given time span. This means that if a camera trap placed in a site has been active for some time and then has been moved to a different location in the same camera trap array (i.e. the camera stays in the same "site" but in a different location), you must open a new camera trap directory.

A camera trap directory must be named as follows:\cr
  \code{<PRJ>-S<sss>-C<ccc>}\cr
where \code{<PRJ>} is the project acronym, all uppercase, \code{<sss>} is the site identifier three-digit number, and \code{<ccc>} is a three-digit number identifying uniquely that camera trap in that site.

A camera trap directory \bold{must} contain a metadata file relative to that camera trap metadata (see further for metadata file format and structure).

\section{Memory card directory}

Other than the mandatory metadata file, a camera trap directory contains at least a single "\emph{memory card subdirectory}", that is, one "data dump directory" for each time a memory card used on that camera has been removed and its data downloaded in the data warehouse.

A memory card subdirectory must be named as follows:\cr
  \code{<YYYY>-<MM>-<DD>}\cr
where \code{<YYYY>-<MM>-<DD>} is the date (year as four-digit number, month as two-digit and day as two-digit) when the memory card has been removed from the camera trap.

A memory card directory must contain files "as they were" on the original memory card, when removed fomr the operating camera trap. Any modifications made to the files \bold{must} be made in a subdirectory of the memory card directory named \code{@data} (such a name will be ignored by \link{updateCatalog}), so that original pictures and/or videos will not be mixed with derived products. Files copied from a memory card \bold{must not} be renamed or modified in any way.

\section{Example structure}

The following fine example of ASCII art demonstrate the rather boring structure of an hypothetical project directory.

\verb{
<data warehouse>
  |
  +- PRJ Project One
  |   +- catalog.xls
  |   +- PRJ-S001
  |   |   + metadata.txt
  |   |   +- PRJ-S001-C001
  |   |   |   + metadata.txt
  |   |   |   + IMG0001.JPG
  |   |   |   + IMG0002.JPG
  |   |   |   + IMG0003.JPG
  |   |   |   + ...
  |   |   +- PRJ-S001-C002
  |   |       + metadata.txt
  |   |       + IMG0001.JPG
  |   |       + IMG0002.JPG
  |   |       + IMG0003.JPG
  |   |       + ...
  |   +- @Ignore_me
  |   |
  |   +- PRJ-S002
  |       +- PRJ-S002-C001
  |       |   + @Skipme
  |       |   + metadata.txt
  |       |   + IMG0001.JPG
  |       |   + IMG0002.JPG
  |       |   + IMG0003.JPG
  |       |   + ...
  |       +- PRJ-S002-C001
  |       |   + metadata.txt
  |       |   + IMG0001.JPG
  |       |   + IMG0002.JPG
  |       |   + IMG0003.JPG
  |       |   + ...
  +- ZZZ Project Two
      +- catalog.xls
      +- ZZZ-S001
          + metadata.txt
          +- ZZZ-S001-C001
          |   + metadata.txt
          |   + IMG0001.JPG
          |   + IMG0002.JPG
          |   + IMG0003.JPG
          |   + ...
          +- ZZZ-S001-C002
              + metadata.txt
              + IMG0001.JPG
              + IMG0002.JPG
              + IMG0003.JPG
              + ...
  }
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
%\examples{
%data(repositoryStructure.Rd)
%## maybe str(repositoryStructure.Rd) ; plot(repositoryStructure.Rd) ...
%}
%\keyword{datasets}
