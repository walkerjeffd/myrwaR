## ----db-connect----------------------------------------------------------
library(myrwaR)
base_dir <- 'D:/Dropbox/Work/mystic/db'
db_path <- file.path(base_dir, "MysticDB_20160208.accdb")
ch <- db_connect(db_path)

## ----load-projects-------------------------------------------------------
projects <- wqx_projects(ch, projects='BASE')
str(projects)

## ----csv-projects, eval=FALSE--------------------------------------------
#  write.csv(projects, file='projects.csv', row.names=FALSE)

## ----load-results--------------------------------------------------------
results <- wqx_results(ch, projects='BASE')
str(results)

## ----validate-results----------------------------------------------------
wqx_validate_results(results)

## ----validate-results-invalid--------------------------------------------
results_invalid <- results
results_invalid[1, "CharacteristicName"] <- "Moose"
wqx_validate_results(results_invalid)

## ----csv-results, eval=FALSE---------------------------------------------
#  write.csv(results, file='results.csv', na="", row.names=FALSE)

## ----load-locations------------------------------------------------------
locations <- wqx_locations(ch, locations=unique(results$MonitoringLocationID))
str(locations)

## ----validate-locations--------------------------------------------------
wqx_validate_locations(locations)

## ----csv-locations, eval=FALSE-------------------------------------------
#  write.csv(locations, file='locations.csv', na="", row.names=FALSE)

