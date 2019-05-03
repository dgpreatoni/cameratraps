Final dataframe structure
- columns are in alphabetical order by name
- eventually they must be reordered e.g. accodring to Rovero and Zimmermann 2016
- if need be, further columns can be added, be consistent with column naming
- each row contains: the field name, the R field mode,[the name of R function in camera_trap_parser.R thet fills in], a concise description of the semantics, the source of data

Camera.End.Date.and.Time=character(); [parseCameraDir]; when a camera finished operating (timestamp); from metadata file in camera directory
Camera.Manufacturer=character(); [parseCameraDir]; camera manufacturer;  from metadata file in camera directory
Camera.Model=character(); [parseCameraDir]; camera model;  from metadata file in camera directory
Camera.Name=character(); [parseCameraDir]; camera name; camera directory name plus metadata file in camera directory, if any
Camera.Serial.Number=numeric(); [parseCameraDir]; camera serial number, from metadata file in camera directory
Camera.Start.Date.and.Time=character(); [parseCameraDir]; when a camera started operating (timestamp); from metadata file in camera directory
Genus=character(); [-]; species identification, genus; to be filled in manually
Latitude=numeric(); [parseCameraDir]; camera position, latitude EPSG:4326; from metadata file in camera directory
Longitude=numeric(); [parseCameraDir]; camera position, longitude EPSG:4326; from metadata file in camera directory
Number.of.Animals=numeric(); [-]; number of animals; to be filled in manually
Organization.Name=character(); [-]; organization responsible for the project; from project directory metadata file, if any, or manually filled in
Person.Identifying.the.Photo=character(); [-]; last name, first name of who compiled Genus, Species and Number.of.Animals; to be filled in manually
Person.picking.up.the.Camera=character(); []; last name, first name of who picked up the camera; either from camera metadata file or to be filled in manually
Person.setting.up.the.Camera=character(); []; last name, first name of who deployed the camera; either from camera metadata file or to be filled in manually
Photo.Date=character(); [getEXIFData]; photo event timestamp: date; from photo EXIF data
Photo.Time=character(); [getEXIFData]; photo event timestamp: time; from photo EXIF data
Photo.Type=character(); [getEXIFData]; photo type (image format): date; from photo EXIF data
Project.Name=character(); [main script, from projName]; name of the trapping project; from project directory name, or project directory metadata file, if any
Raw.Names=character(); [getEXIFData]; the raw image file name; from photo EXIF data
Raw.Path=character(); [parseCameraDir, parseSiteDir]; the raw image file path; added during cataog creation, is used as unique identifier along with Raw.Names
Sampling.Event=character(); [getEXIFData]; sampling event ID; from memory card directory name
Sampling.Unit.Name=character(); [parseSiteDir]; name of the camera trap array; from metadata file in site directory (directory name plus metadata if any)
Sequence.Info=numeric(); []; ????? please check on Rovero and Zimmermann
Species=character(); [-]; species identification, species; to be filled in manually
