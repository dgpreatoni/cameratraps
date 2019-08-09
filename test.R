## this tests cameratraps package
library(cameratraps)

#theRepo <- '/data/testrepository'
theRepo <- '/lan/archivio/Camera traps/SBR-Sun Bear Project/'
#theRepo <- '/lan/archivio/Video/IBS-IBIS_SACRO/alimentazione'
setRepository(theRepo)

## Not Run:
# getRepository()

createCatalog()

#ctl <- updateCatalog()
