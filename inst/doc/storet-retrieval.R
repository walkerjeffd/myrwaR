## ----setup, message = FALSE, warning = FALSE-----------------------------
library(myrwaR)
library(lubridate)
library(dplyr)
library(tidyr)

## ----loc-----------------------------------------------------------------
st_locations <- storet_locations(verbose = TRUE)
str(st_locations)

## ----res-----------------------------------------------------------------
st_results <- storet_results()
str(st_results)

## ----db-results----------------------------------------------------------
ch <- db_connect("D:/Dropbox/Work/mystic/db/MysticDB_20160208.accdb")
db_results <- wqx_results(ch, projects='BASE')
close(ch)

## ----db-filter-dates-----------------------------------------------------
db_results <- mutate(db_results,
                     ActivityStartDate    = as.Date(ActivityStartDate),
                     MonitoringLocationID = paste("MYRWA", MonitoringLocationID, sep = "-")) %>%
  filter(ActivityStartDate > min(st_results$ActivityStartDate),
         ActivityStartDate < max(st_results$ActivityStartDate))

## ----cnt-loc-------------------------------------------------------------
storet_compare_count(storet = st_results$MonitoringLocationIdentifier,
                     db     = db_results$MonitoringLocationID,
                     name   = "LocationID")

## ----cnt-char------------------------------------------------------------
storet_compare_count(storet = st_results$CharacteristicName,
                     db     = db_results$CharacteristicName,
                     name   = "CharacteristicName")

## ----cnt-yr--------------------------------------------------------------
storet_compare_count(storet = year(st_results$ActivityStartDate),
                     db     = year(db_results$ActivityStartDate),
                     name   = "Year")

## ----cnt-date------------------------------------------------------------
storet_compare_count(storet = st_results$ActivityStartDate,
                     db     = db_results$ActivityStartDate,
                     name   = "Date")

