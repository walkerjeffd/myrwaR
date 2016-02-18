#' Fetch MyRWA Locations Table from STORET
#'
#' Fetches MyRWA locations from STORET using the http://waterqualitydata.us REST API.
#' Locations are downloaded to a temporary zip file, unzipped, and loaded into an R
#' data frame.
#'
#' @param verbose If TRUE (default), prints information about the download
#' @export
#' @return data frame of locations
#' @importFrom httr GET write_disk
#' @examples
#' storet_locations <- get_storet_locations()
#'
get_storet_locations <- function(verbose=TRUE) {
  td <- tempdir()
  fname <- tempfile(pattern="storet_stn_", tmpdir = td, fileext = ".zip")
  if (verbose) cat("Fetching locations table from STORET...")
  response <- GET("http://waterqualitydata.us/Station/search?organization=MYRWA&mimeType=csv&zip=yes&sorted=no",
                 write_disk(fname))
  if (response$status_code != 200) {
    if (verbose) cat("FAILED\n")
    stop("Error downloading locations table from STORET")
  }
  if (verbose) cat("OK\n")
  csvfile <- unzip(fname, exdir = td)
  if (verbose) cat("STORET locations zip file saved to:", fname, "\n")
  locations <- read.csv(csvfile, stringsAsFactors=FALSE)
  if (verbose) cat("STORET locations csv file saved to:", csvfile, "\n")
  locations
}

#' Fetch MyRWA Results Table from STORET
#'
#' Fetches MyRWA locations from STORET using the http://waterqualitydata.us REST API.
#' Locations are downloaded to a temporary zip file, unzipped, and loaded into an R
#' data frame.
#'
#' @param verbose If TRUE (default), prints information about the download
#' @export
#' @return data frame of results
#' @importFrom httr GET write_disk
#' @importFrom lubridate ymd
#' @examples
#' storet_results <- get_storet_results()
#'
get_storet_results <- function(verbose=TRUE) {
  td <- tempdir()
  fname <- tempfile(pattern="storet_results_", tmpdir = td, fileext = ".zip")
  if (verbose) cat("Fetching results table from STORET...")
  response <- GET("http://waterqualitydata.us/Result/search?organization=MYRWA&mimeType=csv&zip=yes&sorted=no",
                 write_disk(fname))
  if (response$status_code != 200) {
    if (verbose) cat("FAILED\n")
    stop("Error downloading results table from STORET")
  }
  if (verbose) cat("OK\n")
  if (verbose) cat("STORET results zip file saved to:", fname, "\n")
  csvfile <- unzip(fname, exdir = td)
  if (verbose) cat("STORET results csv file saved to:", csvfile, "\n")
  results <- read.csv(csvfile, stringsAsFactors=FALSE)
  results$ActivityStartDate <- as.Date(ymd(results$ActivityStartDate))
  results
}

#' Compare sample counts between STORET and Database
#'
#' Compares the number of samples for each unique set of values in one ore more
#' columns between STORET and the database. If there are any differences, they will be
#' printed to the console.
#'
#' Note that the names of the two list arguments must be identical. Any names can be used
#' to rename the original columns, with the exception of "Source" and "n" which are added
#' by this function.
#'
#' @param storet A named list containing one or more columns from STORET results table
#' @param db A named list containing one or more columns from Database results table
#' @export
#' @return nothing
#' @importFrom dplyr group_by_ arrange_ tally ungroup
#' @importFrom tidyr spread
#' @examples
#' storet_compare_count(storet = list(Date = storet_results$ActivityStartDate,
#'                                    LocationID = storet_results$MonitoringLocationIdentifier),
#'                      db     = list(Date = db_results$ActivityStartDate,
#'                                    LocationID = db_results$MonitoringLocationID))
storet_compare_count <- function(storet, db) {
  storet <- as.data.frame(storet)
  db <- as.data.frame(db)

  if (!identical(sort(colnames(storet)), sort(colnames(db)))) {
    stop("Names do not match between ",
         "storet (", paste(sort(colnames(storet)), collapse = ", "), ") and ",
         "db (", paste(sort(colnames(db)), collapse = ", "), ")")
  }

  if (nrow(storet) == 0) {
    stop("storet has no rows")
  }

  if (nrow(db) == 0) {
    stop("db has no rows")
  }

  names <- colnames(storet)

  if (any(c("Source", "n") %in% names)) {
    stop('List names cannot include "Source" or "n", please rename the columns')
  }

  cnt_storet <- group_by_(storet, .dots = lapply(names, as.symbol))
  cnt_storet <- tally(cnt_storet)
  cnt_storet <- ungroup(cnt_storet)
  cnt_storet$Source <- "STORET"

  cnt_db <- group_by_(db, .dots = lapply(names, as.symbol))
  cnt_db <- tally(cnt_db)
  cnt_db <- ungroup(cnt_db)
  cnt_db$Source <- "DB"

  cnt <- rbind(cnt_storet, cnt_db)

  cnt <- spread(cnt, Source, n, fill=0)

  cnt <- arrange_(cnt, .dots = lapply(names, as.symbol))

  cnt <- as.data.frame(cnt)

  cnt <- cnt[(cnt$DB != cnt$STORET), ]

  if (nrow(cnt) > 0) {
    cat("Sample counts by (", paste(names, collapse = ", "), ") are different:\n\n")
    cnt[["STORET-DB"]] <- cnt$STORET - cnt$DB
    if (nrow(cnt) > 50) {
      print(cnt[1:50, ], row.names = FALSE)
      cat("and", (nrow(cnt) - 50), "more rows...")
    } else {
      print(cnt, row.names = FALSE)
    }
  } else {
    cat("Sample counts by (", paste(names, collapse = ", "), ") are the same\n")
  }
}
