\name{internal functions}
\alias{internal_functions}
\alias{.createCatalog}
\alias{.parseMetadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal reserved functions
}
\description{
Internal dot-functions in \code{cameratraps} package.

These  functions are not indended to be available to an end user. This page is for documentation only.
}
\usage{
.createCatalog()
.parseMetadata(metadataFile='metadata.txt')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    %%     ~~  \item{x}{
  %%     ~~Describe \code{x} here~~
}
\details{
%%  ~~ If necessary, more details than the description abov(e ~~
\code{.createCatalog} creates a catalog dataframe, named \code{catalogData} and stored in the package namespace.
\code{.parseMetadata} parses a \code{metadata.txt} file.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
