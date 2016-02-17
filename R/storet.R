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
#'
storet_locations <- function(verbose=TRUE) {
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
#'
storet_results <- function(verbose=TRUE) {
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
#' Compares the number of samples per unique value in a single column between STORET and the database.
#'
#' @param storet Vector from STORET
#' @param db Vector from Database
#' @param name Name of the variable
#' @export
#' @return nothing
#' @importFrom tidyr spread
#'
storet_compare_count <- function(storet, db, name="VAR") {
  storet <- as.character(storet)
  db <- as.character(db)

  cnt_storet <- table(storet)
  cnt_storet <- as.data.frame(cnt_storet, stringsAsFactors = FALSE)
  cnt_storet$Source <- "STORET"
  names(cnt_storet) <- c(name, "Count", "Source")

  cnt_db <- table(db)
  cnt_db <- as.data.frame(cnt_db, stringsAsFactors = FALSE)
  cnt_db$Source <- "DB"
  names(cnt_db) <- c(name, "Count", "Source")

  cnt <- rbind(cnt_storet, cnt_db)

  cnt <- spread(cnt, Source, Count, fill=0)

  cnt <- cnt[(cnt$DB != cnt$STORET), ]

  if (nrow(cnt) > 0) {
    cnt <- cnt[order(cnt[[name]]), ]
    cat("Sample counts by", name, "are different!\n\n")
    cnt[["STORET-DB"]]=cnt$STORET - cnt$DB
    print(cnt, row.names = FALSE)
  } else {
    cat("Sample counts are the same!\n")
  }
}
