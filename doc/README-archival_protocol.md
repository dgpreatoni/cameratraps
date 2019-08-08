UAGRA camera trap data archival protocol
========================================
The current version of this document is on Google Drive at https://docs.google.com/a/prea.net/document/d/1SiDopetdmSJdVQBybVQ6tMCxa7S75sZks58xd6B0qt0/edit?usp=sharing

Version 2.0
Created by D.G. Preatoni, F. Bisi, 2016 08 30
Modified by D.G. Preatoni, 2019 08 08
Modified by D.G. Preatoni, 2019 05 02
Modified by D.G. Preatoni, 2018 01 22
Modified by D.G. Preatoni, 2017 08 24

Version History
---------------
V2.0 2019 08 08 - polished up for github “public” version, “Data Warehouse” section rewritten to be less specific.
V1.2 2019 05 02 - corrected reference to UAGRA NAS (it was vormela, now is meles), minor clean-ups
V1.1 2018 01 22 - changed repository paths to meles, deleted reference to the scripts directory
v1.0 2017 08 24 - added time zone specification tag 
v0.0 2016 08 30 - initial protocol

Data Warehouse
==============
All data (photo, video) coming from camera traps are archived on a NAS  (Network Attached Storage) server. Actually, NAS hardware has become cost-effective to handle storage capacities in the terabyte range. As of 2019, we’re using a NAS with 44 TB storage space, configured in RAID-6. This covers for hard disk failures, but not for changes in data.

A terabyte-range backup solution is outside the scope of this document, as is how to set up a NAS as well as RAID configuration: this section serves the sole purpose of giving the reader an example of how a NAS can be used to set up a camera trap repository.

As an example, the NAS file system section hosting the Data Warehouse can be made locally (i.e. on a Local Area Network) available using standard NAS resources: actually, most of the commercially available NAS can “export” directories using CIFS (“Windows shared folders”) and/or NFS (UNIX Network File System). We can this have a shared repository accessible in more than a fashion:

  [CIFS] \\MELES\ARCHIVIO\immagini\camera_traps
  [NFS]  meles.uagra.net:/volume2/ARCHIVIO/immagini\camera_traps
For Windows clients, the CIFS resource listed above ("\\MELES\ARCHIVIO\immagini...") can be conveniently mounted as a drive (e.g. drive "I:"), so that the data warehouse can be accessed as "I:\camera_traps...". Similarly, on UNIX systems, the NAS resource can be conveniently mounted at some directory in the local workstation filesystem.  


Warehouse structure
===================
The cameratraps package expects that a “Repository” follows a precise directory and file structure: differently from demanding all the storage burden to a database (with all the resulting overheads), using a file system is perhaps the simplest approach, but a strictly enforced structure on how files and directories should be organized is mandatory.

Project directory
-----------------
Inside the data warehouse “root” directory, a "project directory" (hereinafter a “Repository”) must be created for each project producing camera trap data. A Repository can be named at one's choice. Define (if it hasn't already been made) a consistent acronym, and use it as a _prefix_ in the Repository directory name.

A Repository can contain a "catalog file", as an Excel spreadsheet: if such a file exists, mind that it was automatically made by one of the Repository maintenance scripts, and it could be updated without notice. The catalog file can be opened and edited -- after all is “just a spreadsheet” -- but once the appropriate maintenance script is run, any existing information will be updated. This means that any user intervention can change the catalog file, making it out of synchronization with the actual content of the files and directories constituting a Repository. The caperatraps package supplies a series of catalog-handling functions to update the catalog or to take into account changes made externally (i.e. by using a spreadsheet program). In particular, it is expected that the columns named “Genus” and “ Species” will be changed by external edits: if the catalog maintenance functions find some values in those fields, they will preserve the entire row.

It is not expected the presence of a “metadata file” (see further) in the root directory of a Repository (at least, for now).


Site directory
--------------
Inside the Project Directory of a Repository a series of "site directories" must be created for each camera trap array. This means that if you have just a single camera, you _must_ create a site directory for just that single camera.

A site directory must be named as follows:
  <PRJ>-S<sss>
where "<PRJ>" is the project acronym, all uppercase; and "<sss>" is a three-digit number, different for each site.
Inside a site directory, if needed, a "metadata file" _can_ be made. See further for metadata file format and structure.


Camera trap directory
---------------------
A site directory contains at least one (or more) "camera trap directory", one for each camera trap constituting the camera trap array deployed in that site. Note that a "camera trap" in this context is not simply a camera trap, but represents a given camera trap deployed in that site for a given time span. This means that if a camera trap placed in a site has been active for some time and then has been moved to a different location in the same camera trap array (i.e. the camera stays in the same "site" but in a different “location” within the “site”), you must create a new camera trap directory.

A camera trap directory must be named as follows:
  <PRJ>-S<sss>-C<ccc>
where "<PRJ>" is the project acronym, all uppercase; and "<sss>" is the site identifier three-digit number and "<ccc>" is a three-digit number identifying uniquely that camera trap in that site.
A camera trap directory _must_ contain a metadata file relative to that camera trap metadata (see further for metadata file format and structure).


Memory card directory
---------------------
Other than the mandatory metadata file, a camera trap directory contains at least one "memory card subdirectory". Actually, a memory card subdirectory _must_ be created and populated each time a memory card used on that camera has been removed and its data downloaded in the Repository.

A memory card subdirectory must be named as follows:
  <YYYY>-<MM>-<DD>
where "<YYYY>-<MM>-<DD>" is the date (year as four-digit number, month as two-digit and day as two-digit) when the memory card has been removed from the camera trap.

A memory card directory must contain files "as they were" on the original memory card when removed from the operating camera trap. Any modifications made to the files _must_ be made in a subdirectory named "@data" (thus, “@data” directory is optional), so that original pictures and/or videos will not be mixed with derived products. Files copied from a memory card _must_ not be renamed or modified in any way.


Example structure
-----------------

    <data warehouse>
      |
      +- PRJ Project One
      |   +- catalog.xlsx
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
A metadata file is a _text file_ and _must_ be named "metadata.txt" (all lowercase).

Inside the metadata file, one can use the tag/value pairs described below.
A tag is made by a reserved word (see table below), followed by a colon, (optionally) a space, and any possible values, as described in the table below.
Note that some of the tags (the ones with an asterisk) are mandatory for a metadata file placed inside a camera trap directory.
If need be, further tags can be defined.

| Tag      | Description                         | Values                                                            |
|----------|-------------------------------------|-------------------------------------------------------------------|
| [1]make:   |[C] Make of the camera trap          | Please use consistent naming and spelling |
| [1]model:  |[C] Model of the camera trap         | Please use consistent naming and spelling |
| [1[]serial:|[C] Serial number of the camera trap | Transcribe the exact serial number |
| [1]lat:    |[C] Camera position, latitude        | Use raw latitude in decimal degrees, WGS84 (i.e. EPSG:4326) |
| [1]lon:    |[C] Camera position, longitude       | Use raw longitude in decimal degrees, WGS84 (i.e. EPSG:4326) |
| timezone:  |[C] Time zone of camera position     | Use time Olson/IANA zone name [2] as from R OlsonNames() function |
| [1]start:  |[C] Camera start timestamp           | Use date and time expressed in ISO 8601 format [3] |
| [1]end:    |[C] Camera end timestamp             | Use date and time expressed in ISO 8601 format [3] |
| height:    |[C] Camera ground height             | Height from ground at which the camera treap has been placed |
| aspect:    |[C] Camera lens aspect               | Aspect (in degrees, 0 to 360) at which the camera lens was facing |
| placed:    |[C] Who placed the camera            | First name, last name |
| removed:   |[C] Who removed the camera           | First name, last name |
| name:      |[C][S] Site or Camera name           | Please use consistent naming and spelling |

 [1]: the tag is mandatory
 [S]: tag is recognized for “sites”, ignored otherwise.
 [C]: tag is recognized for “cameras”, ignored otherwise.

[2] Olson/IANA time zones and time zone names can be found at https://en.wikipedia.org/wiki/List_of_tz_database_time_zones

[3] Dates and time must be expressed according to the ISO 8601 standard (UNI EN 28601, 1993), as follows:
  yyyy-MM-ddTHH:mm:ssK
where:
  yyyy year as four-digits number
    MM month as two-digits number
    dd day of month as two-digits number
    HH hour, 24-hour format, as two-digits number
    mm minutes as two-digits number
    ss seconds (or '00' if exact seconds unknown) as two-digits number
     K time zone offset, expressed as =hh:mm where "=" is either "+" or "-", "hh" is the hour offset and "mm" is tne minutes offset (usually '00'). Use the 'Z' (Zulu time) designator if and only if timestamps are _actually_ in Zulu (UTC) time. IF no offset is specified, the timezone key will be used, if the timezone key is not present, a lookup on GeoNames web services will be attempted to identify the timezone from latitude and longitude.
