## this tests cameratraps package
library(cameratraps)


theRepo <- '/data/testrepository'

setRepository(theRepo)

## Not Run:
# getRepository()

ctl <- updateCatalog(verbose=TRUE)
