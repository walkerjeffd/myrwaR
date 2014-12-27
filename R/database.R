#' Connect to Access database
#' 
#' Opens a connection to an access database
#' 
#' @param path Path to database
#' @export
#' @return Connection handle
#' @examples
#' ch <- db_connect('C://Users//Jeff//Dropbox//Work//mystic//data//MysticDB_20140510.accdb')
db_connect <- function(path) {
  ch <- RODBC::odbcConnectAccess2007(path)
  ch
}

#' Load water quality data
#' 
#' Loads the water quality data from the Result table merged with the Visit table
#' 
#' @param ch Connection handle to database
#' @export
#' @examples
#' ch <- db_connect('C://Users//Jeff//Dropbox//Work//mystic//data//MysticDB_20140510.accdb')
#' wq <- db_results(ch)
db_results <- function(ch) {  
  tblResult <- db_table(ch, 'Result')
  tblVisit <- db_table(ch, 'Visit')
  
  df <- merge(tblResult, tblVisit, by.x='VisitID', by.y='ID', all.x=T)
  
  df[, 'Datetime'] <- lubridate::with_tz(df[, 'Datetime'], tz='EST')
  df[, 'CharacteristicID'] <- factor(df[, 'CharacteristicID'])
  df[, 'LocationID'] <- factor(df[, 'LocationID'])
  df[, 'Units'] <- factor(df[, 'Units'])
  df[, 'ProjectID'] <- factor(df[, 'ProjectID'])
  
  df <- droplevels(df)
  
  df
}


#' Load table from database
#' 
#' Loads the data from a table in the database
#' 
#' @param ch Connection handle to database
#' @export
#' @examples
#' ch <- db_connect('C://Users//Jeff//Dropbox//Work//mystic//data//MysticDB_20140510.accdb')
#' tbl.Results <- db_table(ch, 'Results')
db_table <- function(ch, table_name) {
  df <- RODBC::sqlFetch(ch, table_name)  
  df
}

#' Retrieve table names in database
#' 
#' Retrieves a character vector of table names
#' 
#' @param ch Connection handle to database
#' @export
#' @examples
#' ch <- db_connect('C://Users//Jeff//Dropbox//Work//mystic//data//MysticDB_20140510.accdb')
#' table_names <- db_list_tables(ch)
db_list_tables <- function(ch) {
  tables <- RODBC::sqlTables(ch, tableType='TABLE')$TABLE_NAME
  tables
}

#' Retrieve table schema from database
#' 
#' Retrieves the field names and types from a given database table
#' 
#' @param ch Connection handle to database
#' @export
#' @examples
#' ch <- db_connect('C://Users//Jeff//Dropbox//Work//mystic//data//MysticDB_20140510.accdb')
#' schema_results <- db_table_fields(ch, 'Results')
db_table_fields <- function(ch, table_name) {
  fields <- RODBC::sqlColumns(ch, table_name)[, c("COLUMN_NAME", "TYPE_NAME")]
  fields
}

#' Retrieve locations from database
#' 
#' Retrieves the station locations from the database by merging Location, WaterBody, and LocationType tables
#' 
#' @param ch Connection handle to database
#' @export
#' @examples
#' ch <- db_connect('C://Users//Jeff//Dropbox//Work//mystic//data//MysticDB_20140510.accdb')
#' locations <- db_locations(ch)
db_locations <- function(ch) {
  tbl_location <- db_table(ch, 'Location')
  tbl_waterbody <- db_table(ch, 'WaterBody')
  tbl_location_type <- db_table(ch, 'LocationType')
  
  df <- merge(tbl_location, tbl_waterbody, by.x='WaterBodyID', by.y='ID', all.x=TRUE)
  df <- merge(df, tbl_location_type, by.x='LocationTypeID', by.y='ID', all.x=TRUE)
  
  df
}

#' Load water quality data
#' 
#' Loads water quality data with associated tables
#' 
#' @param path Path to MyRWA Access Database
#' @param projects ProjectID(s) to keep in dataset (default=NULL for all projects)
#' @param sample_types SampleTypeID(s) to keep in dataset (default='S' to exclude blanks and duplicates, if NULL retains all sample types)
#' @param exclude_flags If TRUE, excludes all samples with any flag, otherwise returns all samples
#' @export
load_wq <- function(path, projects=NULL, sample_types='S', exclude_flags=FALSE) {
  ch <- db_connect(path)
  
  wq <- db_results(ch)
  locations <- db_table(ch, 'Location')
  wq <- merge(wq, locations, by.x='LocationID', by.y='ID')
  
  if (!is.null(projects)) {
    wq <- subset(wq, ProjectID %in% projects)
  }
  
  if (!is.null(sample_types)) {
    wq <- subset(wq, SampleTypeID %in% sample_types)
  }
  
  if (exclude_flags) {
    wq <- subset(wq, is.na(FlagID) | FlagID %in% c("", " "))
  }
  
  wq
} 
