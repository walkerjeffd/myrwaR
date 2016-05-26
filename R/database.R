#' Connect to MyRWA WQ database
#'
#' Opens a connection to the MyRWA WQ database
#'
#' @param path Path to database
#' @export
#' @return RODBC connection handle
#' @examples
#' ch <- db_connect("D:/Dropbox/Work/mystic/db/MysticDB_20160208.accdb")
db_connect <- function(path) {
  if (odbcConnectAccess2007_exists()) {
    ch <- RODBC::odbcConnectAccess2007(path)
  }
  ch
}

#' Load Results table merged with Visit table
#'
#' Loads the water quality data from the Result table merged with Visit table
#'
#' @param ch Open connection handle to database
#' @param ... Additional arguments passed to db_table()
#' @importFrom lubridate with_tz
#' @return Data frame of water quality data containing visit information
#' @export
#' @examples
#' ch <- db_connect("D:/Dropbox/Work/mystic/db/MysticDB_20160208.accdb")
#' wq <- db_results(ch)
db_results <- function(ch, ...) {
  tbl_result <- db_table(ch, "Result", ...)
  tbl_visit <- db_table(ch, "Visit", ...)

  df <- merge(tbl_result, tbl_visit, by.x="VisitID", by.y="ID", all.x=TRUE)

  df[["Datetime"]] <- with_tz(df[["Datetime"]], tz="EST")
  df[["CharacteristicID"]] <- factor(df[["CharacteristicID"]])
  df[["LocationID"]] <- factor(df[["LocationID"]])
  df[["Units"]] <- factor(df[["Units"]])
  df[["ProjectID"]] <- factor(df[["ProjectID"]])

  df <- droplevels(df)

  df
}


#' Load single table from database
#'
#' Loads the data from a table in the database
#'
#' @param ch Connection handle to database
#' @param table_name Table name
#' @param ... Additional arguments passed to sqlFetch()
#' @return A dataframe containing the database table
#' @export
#' @examples
#' ch <- db_connect("D:/Dropbox/Work/mystic/db/MysticDB_20160208.accdb")
#' tbl.Results <- db_table(ch, "Results")
db_table <- function(ch, table_name, ...) {
  if (odbcConnectAccess2007_exists()) {
    df <- sqlFetch(ch, table_name, ...)
  }
  df
}

#' List of table names in database
#'
#' Retrieves a character vector of table names
#'
#' @param ch Connection handle to database
#' @return A character vector of table names
#' @export
#' @examples
#' ch <- db_connect("D:/Dropbox/Work/mystic/db/MysticDB_20160208.accdb")
#' table_names <- db_list_tables(ch)
db_list_tables <- function(ch) {
  if (odbcConnectAccess2007_exists()) {
    tables <- sqlTables(ch, tableType="TABLE")$TABLE_NAME
  }
  tables
}

#' Retrieve table schema from database
#'
#' Retrieves the field names and types from a given database table
#'
#' @param ch Connection handle to database
#' @return A dataframe containing the column names and types for the specified
#'   table
#' @export
#' @examples
#' ch <- db_connect("D:/Dropbox/Work/mystic/db/MysticDB_20160208.accdb")
#' schema_results <- db_table_fields(ch, table_name = "Results")
db_table_fields <- function(ch, table_name) {
  if (odbcConnectAccess2007_exists()) {
    fields <- sqlColumns(ch, table_name)[, c("COLUMN_NAME", "TYPE_NAME")]
  }
  fields
}

#' Retrieve locations from database
#'
#' Retrieves the station locations from the database by merging Location,
#' WaterBody, and LocationType tables
#'
#' @param ch Connection handle to database
#' @return A dataframe containing the locations in the database
#' @export
#' @examples
#' ch <- db_connect("D:/Dropbox/Work/mystic/db/MysticDB_20160208.accdb")
#' locations <- db_locations(ch)
db_locations <- function(ch) {
  tbl_location <- db_table(ch, "Location")
  tbl_waterbody <- db_table(ch, "WaterBody")
  tbl_location_type <- db_table(ch, "LocationType")

  df <- merge(tbl_location, tbl_waterbody, by.x="WaterBodyID",
              by.y="ID", all.x=TRUE)
  df <- merge(df, tbl_location_type, by.x="LocationTypeID",
              by.y="ID", all.x=TRUE)

  df
}

#' Load water quality data
#'
#' Loads water quality Results table merged with Visit and Location tables
#'
#' @param path Full path to MyRWA Access Database
#' @param projects vector of ProjectIDs to keep (default=NULL to return all projects)
#' @param sample_types SampleTypeID(s) to keep in dataset (default="S" to
#'        exclude blanks and duplicates, if NULL returns all sample types)
#' @param exclude_flags If TRUE, excludes all samples with any flag,
#'        otherwise returns all samples
#' @return A dataframe containing water quality data
#' @export
#' @examples
#' df <- load_wq(path = "D:/Dropbox/Work/mystic/db/MysticDB_20160208.accdb")
#' # all baseline samples including flagged and qaqc samples
#' baseline <- load_wq(path = "D:/Dropbox/Work/mystic/db/MysticDB_20160208.accdb", projects="BASE", sample_types=NULL, exclude_flags=FALSE)
load_wq <- function(path, projects=NULL, sample_types="S",
                    exclude_flags=FALSE) {
  ch <- db_connect(path)

  wq <- db_results(ch)
  locations <- db_table(ch, "Location")
  wq <- merge(wq, locations, by.x="LocationID", by.y="ID")

  if (!is.null(projects)) {
    wq <- subset(wq, ProjectID %in% projects)
  }

  if (!is.null(sample_types)) {
    wq <- subset(wq, SampleTypeID %in% sample_types)
  }

  if (exclude_flags) {
    wq <- subset(wq, is.na(FlagID) | FlagID %in% c("", " "))
  }

  wq <- droplevels(wq)

  close(ch)

  wq
}

odbcConnectAccess2007_exists <- function () {
  if (!require("RODBC")) {
    stop("RODBC is not installed")
  }
  if (!exists('odbcConnectAccess2007', where='package:RODBC', mode='function')) {
    stop("RODBC::odbcConnectAccess2007() is not available. This feature is only available on Windows-based systems.")
  }
  TRUE
}
