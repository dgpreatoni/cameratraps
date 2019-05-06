\name{setRepository}
\alias{getRepository}
\alias{setRepository}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{getRepository} %- this manpage also goes for the 'get'
\title{
set/get a camera trap data repository
}
\description{
attach to R a camera trap repository, creating it if necessary
}
\usage{
setRepository(path = getwd(), create = FALSE)
getRepository()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
a valid path where the files and direstories structure of a cameratraps repository is located. See \link{repositoryStructure} for a detailed description of a repository structure.
}
  \item{create}{
a logical value. If \code{TRUE}, R will try creating the directory in \code{path}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A character representation of \code{path}, if command completed successfully.
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
Damiano G. Preatoni \email{prea@prea.net}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Set /tmp/cameratraps as Repository path, create it if doesn't exist
setRepository('/tmp/cameratraps', create=TRUE)

## check that
getRepository()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{repository}% use one of  RShowDoc("KEYWORDS")

