#' Connect to Access database
#' 
#' Opens a connection to an access database
#' 
#' @param db_path Path and filename to database
#' @export
#' @return Connection handle
#' @examples
#' ch <- db_connect('C://Users//Jeff//Dropbox//Work//mystic//data//MysticDB_20140510.accdb')
db_connect <- function(db_path) {
  ch <- RODBC::odbcConnectAccess2007(db_path)
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
  
  df$Datetime <- lubridate::with_tz(df$Datetime, tz='EST')
  
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
