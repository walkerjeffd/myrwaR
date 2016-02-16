## ----setup, echo=FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = FALSE, comment = "#>")
base_dir <- 'D:/Dropbox/Work/mystic/db'
db_path <- file.path(base_dir, "MysticDB_20160208.accdb")

## ----load-package--------------------------------------------------------
library(myrwaR)

## ----connect-------------------------------------------------------------
ch <- db_connect(path = db_path)

## ----list-tables---------------------------------------------------------
myrwaR::db_list_tables(ch)

## ----locations-----------------------------------------------------------
locations <- db_table(ch, table_name = "Location")
str(locations)

## ----results-------------------------------------------------------------
results <- db_results(ch)

## ----load-wq-------------------------------------------------------------
wq <- load_wq(path = db_path, sample_types = c("S"), exclude_flags = TRUE)
str(wq)

## ----close---------------------------------------------------------------
close(ch)

## ----compare-------------------------------------------------------------
compare_database(old_path = file.path(base_dir, "MysticDB_20160120.accdb"),
                 new_path = file.path(base_dir, "MysticDB_20160208.accdb"))

## ----compare-log, eval=FALSE---------------------------------------------
#  compare_database(old_path = file.path(base_dir, "MysticDB_20150227.accdb"),
#                   new_path = file.path(base_dir, "MysticDB_20150529.accdb"),
#                   log_file = file.path(base_dir, 'compare_20150227_20150529.txt'))

## ----qaqc-suite----------------------------------------------------------
ch <- db_connect(db_path)
db_qaqc_suite(ch, print.rows=FALSE)
close(ch)

