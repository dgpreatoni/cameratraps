\name{listSiteDir}
\alias{listSiteDir}
\alias{listCameraDir}
\alias{listDataDir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve and list repository directory contents
}
\description{
Traverse "Site", "Camera" or "data dump" (i.e. SD card dump) directories in a \code{cameratraps} repository and list objects threin.
}
\usage{
listSiteDir()
listCameraDir(siteDirName)
listDataDir(siteDirName, cameraDirName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{siteDirName}{a valid character string for a "site" directory. See \link{repositoryStructure} for details.}
  \item{cameraDirName}{a valid character string for a "camera" directory. See \link{repositoryStructure} for details.}
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
