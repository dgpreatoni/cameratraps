UAGRA camera trap data archival protocol
========================================
An updated version of this document is on Google Drive at https://docs.google.com/a/prea.net/document/d/1SiDopetdmSJdVQBybVQ6tMCxa7S75sZks58xd6B0qt0/edit?usp=sharing
Version 1
Created by D.G. Preatoni, F. Bisi, 2016 08 30
Modified by D.G. Preatoni, 2017 08 24
Version History
---------------
v0 2016 08 30 - initial protocol
v1 2017 08 24 - added time zone specification tag 
Data Warehouse
==============
All data (photo, video) coming from camera traps are archived on vormela server.
The data warehouse is located at:
  [UNC] \\VORMELA\ARCHIVIO\immagini\camera_traps
  [NFS] vormela.uagra.net:/volume2/ARCHIVIO/immagini\camera_traps
For Windows clients, the UNC resource listed above ("\\VORMELA\ARCHIVIO\immagini") is  normally mounted as drive "I:", so that the data warehouse can be accessed as "I:\camera_traps". Alternatively, "\\VORMELA\ARCHIVIO" is mapped as drive "V:", so the data warehouse can be also accessed as "V:\immagini\camera_traps".

Warehouse structure
===================

Project directory
-----------------
Inside the data warehouse directory, a "project directory" must be created for each project producing camera trap data. A project directory can be named at one's choice. Define (if it hasn't already been made) a consistent acronym, and use it as a _prefix_ in the project directory name.
Inside the main data warehouse root directory there also exists a scripts directory, that contains all the programs needed for maintenance and handling of the whole repository. Do not touch its contents if you're not conscious of what you're doing. Thou hast beeh warned.
A project subdirectory can contain a "catalog file", automatically made by one of the maintenance scripts. The file can be opened and edited, but once the appropriate maintenance script is run, any existing information will be updated, if different. The only exception is for the columns named XXXXX XXXXXXXX (please complete), that are intended as user-editable only: in this case, if the maintenance program finds some value, it will preserve it.
There is no metadata file (see further) inside a project directory (at least, for now).
@TODO: write script to create the catalog, document the catalog file structure.

Site directory
--------------
Inside the project subdirectory a "site directory" must be created for each camera trap array. This means that if you have just a single camera, you _must_ create a site directory for just that single camera.
A site directory must be named as follows:
  <PRJ>-S<sss>
where "<PRJ>" is the project acronym, all uppercase; and "<sss>" is a three-digit number.
Inside a site directory, if needed, a "metadata file" _can_ be made. See further for metadata file format and structure.

Camera trap directory
---------------------
A site directory contains one or more "camera trap directories", one for each camera trap constituting the camera trap array deployed in that site. Note that a "camera trap" in this context is not simply a camera trap, but represents a given camera trap deployed in that site for a given time span. This means that if a camera trap placed in a site has been active for some time and then has been moved to a different location in the same camera trap array (i.e. the camera stays in the same "site" but in a different location), you must open a new camera trap directory.
A camera trap directory must be named as follows:
  <PRJ>-S<sss>-C<ccc>
where "<PRJ>" is the project acronym, all uppercase; and "<sss>" is the site identifier three-digit number and "<ccc>" is a three-digit number identifying uniquely that camera trap in that site.
A camera trap directory _must_ contain a metadata file relative to that camera trap metadata (see further for metadata file format and structure).

Memory card directory
---------------------
Other than the mandatory metadata file, a camera trap directory contains a "memory card subdirectory" for each time a memory card used on that camera has been removed and its data downloaded in the data warehouse.
A memory card subdirectory must be named as follows:
  <YYYY>-<MM>-<DD>
where "<YYYY>-<MM>-<DD>" is the date (year as four-digit number, month as two-digit and day as two-digit) when the memory card has been removed from the camera trap.
A memory card directory must contain files "as they were" on the original memory card when removed fomr the operating camera trap. Any modifications made to the files _must_ be made in a subdirectory named "data", so that original pictures and/or videos will not be mixed with derived products. Files copied from a memory card _must_ not be renamed or modified in any way.

Example structure
-----------------
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
  |   +- PRJ-S002
  |       +- PRJ-S002-C001
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

Metadata file
-------------
Inside a camera trap directory (and optionally inside a site directory) a "metadata file" can be present.
A metadata file is a _text file_ and must be named "metadata.txt" (all lowercase).
Inside the metadata file, one can use the tag/value pairs described below.
A tag is made with a reserved tag word (see table below), followed by a colon, a space and any possible value as described in the table below.
Note that some of the tags (the ones with an asterisk) are mandatory for a metadata file placed inside a camera trap directory.
If need be, further tags can be defined.
 Tag        Description                         Values
----------+----------------------------------+-------------------------------------------------------------------
*make:    | Maker of the camera trap         | Please use consistent naming and spelling
*model:   | Model of the camera trap         | Please use consistent naming and spelling
*serial:  | Serial number of the camera trap | Please transcribe the exact serial number
*lat:     | Camera position, latitude        | Use raw latitude in decimal degrees, WGS84 (i.e. EPSG:4326)
*lon:     | Camera position, longitude       | Use raw longitude in decimal degrees, WGS84 (i.e. EPSG:4326)
 timezone:| Time zone of camera position     | Use time Olson/IANA zone name [1] as from R OlsonNames() function
*start:   | Camera start timestamp           | Use date and time expressed in ISO 8601 format [2]
*end:     | Camera end timestamp             | Use date and time expressed in ISO 8601 format [2]
 height:  | Camera ground height             | Height from ground at which the camera treap has been placed
 aspect:  | Camera lens aspect               | Aspect (in degrees, 0 to 360) at which the camera lens was facing
 placed:  | Who placed the camera            | First name, last name
 removed: | Who removed the camera           | First name, last name

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
     

