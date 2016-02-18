## ----setup, echo=FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = FALSE, comment = "#>")

## ----libraries, message = FALSE, warning = FALSE-------------------------
library(myrwaR)
library(lubridate)
library(dplyr)
library(tidyr)

## ----loc-----------------------------------------------------------------
storet_locations <- get_storet_locations(verbose = TRUE)
str(storet_locations)

## ----res-----------------------------------------------------------------
storet_results <- get_storet_results()
str(storet_results)

## ----db-results----------------------------------------------------------
ch <- db_connect("D:/Dropbox/Work/mystic/db/MysticDB_20160208.accdb")
db_results <- wqx_results(ch, projects='BASE')
close(ch)

## ----db-filter-dates-----------------------------------------------------
db_results <- mutate(db_results,
                     ActivityStartDate    = as.Date(ActivityStartDate),
                     MonitoringLocationID = paste("MYRWA", MonitoringLocationID, sep = "-"),
                     ActivityID           = paste("MYRWA", ActivityID, sep = "-"))

## ----cnt-loc-------------------------------------------------------------
storet_compare_count(storet = list(LocationID = storet_results$MonitoringLocationIdentifier),
                     db     = list(LocationID = db_results$MonitoringLocationID))

## ----cnt-char------------------------------------------------------------
storet_compare_count(storet = list(CharacteristicName = storet_results$CharacteristicName),
                     db     = list(CharacteristicName = db_results$CharacteristicName))

## ----cnt-yr--------------------------------------------------------------
storet_compare_count(storet = list(Year = year(storet_results$ActivityStartDate)),
                     db     = list(Year = year(db_results$ActivityStartDate)))

## ----cnt-date------------------------------------------------------------
storet_compare_count(storet = list(Date = storet_results$ActivityStartDate),
                     db     = list(Date = db_results$ActivityStartDate))

## ----filter-db-----------------------------------------------------------
db_results <- filter(db_results, ActivityStartDate %in% unique(storet_results$ActivityStartDate))

storet_compare_count(storet = list(LocationID = storet_results$MonitoringLocationIdentifier),
                     db     = list(LocationID = db_results$MonitoringLocationID))
storet_compare_count(storet = list(CharacteristicName = storet_results$CharacteristicName),
                     db     = list(CharacteristicName = db_results$CharacteristicName))
storet_compare_count(storet = list(Year = year(storet_results$ActivityStartDate)),
                     db     = list(Year = year(db_results$ActivityStartDate)))
storet_compare_count(storet = list(Date = storet_results$ActivityStartDate),
                     db     = list(Date = db_results$ActivityStartDate))

## ----cnt-date-loc--------------------------------------------------------
storet_compare_count(storet = list(Date       = storet_results$ActivityStartDate,
                                   LocationID = storet_results$MonitoringLocationIdentifier),
                     db     = list(Date       = db_results$ActivityStartDate,
                                   LocationID = db_results$MonitoringLocationID))

## ----cnt-activity-id-----------------------------------------------------
storet_compare_count(storet = list(ActivityID = storet_results$ActivityIdentifier),
                     db     = list(ActivityID = db_results$ActivityID))

## ----filter-differences--------------------------------------------------
filter(storet_results,
       MonitoringLocationIdentifier == "MYRWA-BEI093",
       ActivityStartDate == as.Date("2015-01-23")) %>%
  select(ActivityIdentifier, ActivityStartDate, ActivityStartTime.Time,
         MonitoringLocationIdentifier, CharacteristicName, ResultMeasureValue,
         ResultMeasure.MeasureUnitCode)

filter(db_results,
       MonitoringLocationID == "MYRWA-BEI001",
       ActivityStartDate == as.Date("2015-01-23")) %>%
  select(ActivityID, ActivityStartDate, ActivityStartTime, MonitoringLocationID,
         CharacteristicName, ResultValue, ResultUnit)

