\name{metadataFileFormat.Rd}
\alias{metadataFileFormat}
\docType{data}
\title{
\code{metadata.txt} file format definition
}
\description{
List of valid key:value pairs that can be found in a \code{cameratraps} \code{metadata.txt} file.
}
%%  ~~ \usage{data("metadataFileFormat.Rd")}
Inside a camera trap directory (and optionally inside a site directory) a "metadata file" can be present.

A metadata file is a \emph{text file} and must be named \code{metadata.txt} (all lowercase).

Inside the metadata file, one can use the tag/value pairs described below.

A tag is made by a reserved word (see below), followed by a colon, (optionally) a space, and any possible values, as described below.

Note that some of the tags (the ones with an asterisk) are mandatory for a metadata file placed inside a camera trap directory.

If need be, further tags can be defined.

\format{
  A \code{metadata.txt} file can contain the following key/value pairs, one per row, separating key from value with a colon.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}



    Metadata file
-------------

 Tag        Description                         Values
----------+-------------------------------------+-------------------------------------------------------------------
*make:    |[C] Make of the camera trap          | Please use consistent naming and spelling
*model:   |[C] Model of the camera trap         | Please use consistent naming and spelling
*serial:  |[C] Serial number of the camera trap | Please transcribe the exact serial number
*lat:     |[C] Camera position, latitude        | Use raw latitude in decimal degrees, WGS84 (i.e. EPSG:4326)
*lon:     |[C] Camera position, longitude       | Use raw longitude in decimal degrees, WGS84 (i.e. EPSG:4326)
 timezone:|[C] Time zone of camera position     | Use time Olson/IANA zone name [1] as from R OlsonNames() function
*start:   |[C] Camera start timestamp           | Use date and time expressed in ISO 8601 format [2]
*end:     |[C] Camera end timestamp             | Use date and time expressed in ISO 8601 format [2]
 height:  |[C] Camera ground height             | Height from ground at which the camera treap has been placed
 aspect:  |[C] Camera lens aspect               | Aspect (in degrees, 0 to 360) at which the camera lens was facing
 placed:  |[C] Who placed the camera            | First name, last name
 removed: |[C] Who removed the camera           | First name, last name
 name:    |[CS] Site or Camera name             | Please use consistent naming and spelling
--------
 *: the tag is mandatory
 S: tag is recognized for “sites”, ignored otherwise.
 C: tag is recognized for “cameras”, ignored otherwise.

[1] Olson/IANA time zones and time zone names can be found at https://en.wikipedia.org/wiki/List_of_tz_database_time_zones

[2] Dates and time must be expressed according to the ISO 8601 standard (UNI EN 28601, 1993), as follows:
  yyyy-MM-ddTHH:mm:ssK
where:
  yyyy year as four-digits number
    MM month as two-digits number
    dd day of month as two-digits number
    HH hour, 24-hour format, as two-digits number
    mm minutes as two-digits number
    ss seconds (or '00' if exact seconds unknown) as two-digits number
     K time zone offset, expressed as =hh:mm where "=" is either "+" or "-", "hh" is the hour offset and "mm" is tne minutes offset (usually '00'). Use the 'Z' (Zulu time) designator if and only if timestamps are _actually_ in Zulu (UTC) time. IF no offset is specified, the timezone key will be used, if the timezone key is not present, a lookup on GeoNames web services will be attempted to identify the timezone from latitude and longitude.





  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(metadataFileFormat.Rd)
## maybe str(metadataFileFormat.Rd) ; plot(metadataFileFormat.Rd) ...
}
\keyword{datasets}
