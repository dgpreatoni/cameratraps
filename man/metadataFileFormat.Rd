\name{metadata file format}
\alias{metadataFileFormat}
\docType{data}
\title{\code{metadata.txt} file format definition}
\description{
List of valid key:value pairs that can be found in a \code{cameratraps} \code{metadata.txt} file.
}
%%  ~~ \usage{data("metadataFileFormat.Rd")}
\format{
Inside any camera trap directory, and optionally inside a site directory, a "metadata file" can be present.

A metadata file is a \emph{text file} and must be named \code{metadata.txt} (all lowercase).

Inside a metadata file, one can use the tag/value pairs described below.

Inside a metadata file, anything following the hash character (\code{#}) will be treated as a comment, i.e. ignored.

A tag is made by a reserved word (see below), followed by a colon, (optionally) a space, and any possible values, as described below.

Note that some of the tags (the ones with an asterisk) are mandatory for a metadata file placed inside a camera trap directory.

If need be, further tags can be defined.


\tabular{rcll}{
\bold{Tag} \tab \bold{Context} \tab \bold{Description} \tab \bold{Values} \cr
*\code{make}:     \tab [C]   \tab Make of the camera trap   \tab Please use consistent naming and spelling \cr
*\code{model}:    \tab [C]   \tab Model of the camera trap         \tab Please use consistent naming and spelling \cr
*\code{serial}:   \tab [C]   \tab Serial number of the camera trap \tab Please transcribe the exact serial number \cr
*\code{lat}:      \tab [C]   \tab Camera position, latitude        \tab Use raw latitude in decimal degrees, WGS84 (i.e. EPSG:4326) \cr
*\code{lon}:      \tab [C]   \tab Camera position, longitude       \tab Use raw longitude in decimal degrees, WGS84 (i.e. EPSG:4326) \cr
 \code{timezone}: \tab [C]   \tab Time zone of camera position     \tab Use time Olson/IANA zone name [1] as from R OlsonNames() function \cr
*\code{start}:    \tab [C]   \tab Camera start timestamp           \tab Use date and time expressed in ISO 8601 format [2] \cr
*\code{end}:      \tab [C]   \tab Camera end timestamp             \tab Use date and time expressed in ISO 8601 format [2] \cr
 \code{height}:   \tab [C]   \tab Camera ground height             \tab Height from ground at which the camera treap has been placed \cr
 \code{aspect}:   \tab [C]   \tab Camera lens aspect               \tab Aspect (in degrees, 0 to 360) at which the camera lens was facing \cr
 \code{placed}:   \tab [C]   \tab Who placed the camera            \tab First name, last name \cr
 \code{removed}:  \tab [C]   \tab Who removed the camera           \tab First name, last name \cr
 \code{name}:     \tab [CS]  \tab Site or Camera name              \tab Please use consistent naming and spelling \cr
}
 *: the tag is mandatory.

 \code{S}: tag is recognized for “sites”, ignored otherwise.

 \code{C}: tag is recognized for “cameras”, ignored otherwise.

[1] Olson/IANA time zones and time zone names can be found at \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones}

[2] Dates and time must be expressed according to the ISO 8601 standard (UNI EN 28601, 1993), as follows:
  \code{yyyy-MM-ddTHH:mm:ssK}\cr
where:
\tabular{ll}{
\code{yyyy} \tab year as four-digits number.\cr
\code{MM} \tab month as two-digits number.\cr
\code{dd} \tab day of month as two-digits number.\cr
\code{HH} \tab hour, 24-hour format, as two-digits number.\cr
\code{mm} \tab minutes as two-digits number.\cr
\code{ss} \tab seconds (or '\code{00}' if exact seconds unknown) as two-digits number.\cr
\code{K} \tab time zone offset, expressed as \code{=hh:mm} where "\code{=}" is either "\code{+}" or "\code{-}", "\code{hh}" is the hour offset and "\code{mm}" is the minutes offset (usually '\code{00}'). Use the '\code{Z}' (Zulu time) designator if and only if timestamps are \emph{actually} in Zulu (UTC) time. If no offset is specified, the \code{timezone} key will be used, if the \code{timezone} key is not present, a lookup on web services will be attempted to identify the timezone from latitude and longitude.\cr
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
%% ~~ \examples{
%% ~~ data(metadataFileFormat.Rd)
%% ~~ ## maybe str(metadataFileFormat.Rd) ; plot(metadataFileFormat.Rd) ...
%% ~~ }
\keyword{metadata}
