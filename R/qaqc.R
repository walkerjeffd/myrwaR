#' Run QAQC test suite on database
#'
#' Runs a suite of tests to check for invalid values, relationships, and other issues in the database.
#'
#' @param ch Connection handle to database
#' @param print.rows If TRUE, prints individual rows that did not pass any given test (default=FALSE)
#' @return nothing
#' @export
#' @examples
#' db_qaqc_suite(ch)
db_qaqc_suite <- function(ch, print.rows) {
  cat("Loading tables...")
  tblResult <- db_table(ch, "Result", stringsAsFactors=FALSE)
  tblVisit <- db_table(ch, "Visit", stringsAsFactors=FALSE)
  tblProject <- db_table(ch, "Project", stringsAsFactors=FALSE)
  tblResultFlag <- db_table(ch, "ResultFlag", stringsAsFactors=FALSE)
  tblUnits <- db_table(ch, "Units", stringsAsFactors=FALSE)
  tblSampleType <- db_table(ch, "SampleType", stringsAsFactors=FALSE)
  tblMethod <- db_table(ch, "Method", stringsAsFactors=FALSE)
  cat("done\n\n")

  failed <- FALSE

  cat("Checking Result table...\n")

  failed <- failed | check_db(tblResult, c("CharacteristicID", "Units", "Qualifier", "FlagID"), "empty strings", check_empty_strings, print.rows=print.rows)
  failed <- failed | check_db(tblResult, c("CharacteristicID", "FlagID"), "lowercase strings", check_lowercase_strings, print.rows=print.rows)
  failed <- failed | check_db(tblResult, c("CharacteristicID", "Units", "Qualifier", "FlagID"), "untrimmed strings", check_untrimmed_strings, print.rows=print.rows)
  failed <- failed | check_db(tblResult, c("Units"), "invalid units", check_allowed_values, tblUnits$ID, print.rows=print.rows)
  failed <- failed | check_db(tblResult, c("FlagID"), "invalid flags", check_allowed_values, tblResultFlag$ID, print.rows=print.rows)

  cat("Checking Visit table...\n")

  failed <- failed | check_db(tblVisit, c("UniqueID", "LocationID", "ProjectID", "SampleTypeID", "SampleDepthType"), "empty strings", check_empty_strings, print.rows=print.rows)
  failed <- failed | check_db(tblVisit, c("ProjectID", "SampleTypeID", "SampleDepthType"), "lowercase strings", check_lowercase_strings, print.rows=print.rows)
  failed <- failed | check_db(tblVisit, c("UniqueID", "LocationID", "ProjectID", "SampleTypeID", "SampleDepthType"), "untrimmed strings", check_untrimmed_strings, print.rows=print.rows)
  failed <- failed | check_db(tblVisit, c("UniqueID", "LocationID", "ProjectID", "SampleTypeID", "SampleDepthType"), "NA values", check_na_values, print.rows=print.rows)
  failed <- failed | check_db(tblVisit, c("Datetime"), "datetimes out-of-range", check_range_dates, print.rows=print.rows)
  failed <- failed | check_db(tblVisit, c("SampleTypeID"), "invalid sample type IDs", check_allowed_values, tblSampleType$ID, print.rows=print.rows)
  failed <- failed | check_db(tblVisit, c("ProjectID"), "invalid project IDs", check_allowed_values, tblProject$ID, print.rows=print.rows)

  if (failed) {
    warning("Database did not pass validation checks, see details above")
  } else {
    cat("Done! Everything looks OK.")
  }
}


check_db <- function (df, names, description, fun, ..., print.rows=TRUE) {
  cat(".. Checking for", description, "\n")
  failed <- FALSE
  for (name in names) {
    cat("....", name, "...")
    idx <- fun(df[[name]], ...)
    if (length(idx) > 0) {
      failed <- TRUE
      cat("FAILED\n\n")
      cat("ERROR: There are", length(idx), "row(s) with", description, "in column", paste0('"', name, '"'), "\n\n")
      if (print.rows) {
        if (length(idx) > 50) {
          print(df[idx[1:50], c("ID", name)], row.names=FALSE, quote = TRUE)
          cat("and", (length(idx)-50), "more rows...\n")
        } else {
          print(df[idx, c("ID", name)], row.names=FALSE, quote = TRUE)
        }
        cat("\n\n")
      }
    } else {
      cat("OK\n")
    }
  }
  failed
}

check_untrimmed_strings <- function(x) {
  x <- as.character(x)
  which(!is.na(x) & x != stringr::str_trim(x))
}

check_empty_strings <- function(x) {
  x <- as.character(x)
  which(!is.na(x) & stringr::str_trim(x) == "")
}

check_lowercase_strings <- function(x) {
  x <- as.character(x)
  which(!is.na(x) & stringr::str_to_upper(x) != x)
}

check_na_values <- function(x) {
  which(is.na(x))
}

check_range_dates <- function(x) {
  min_date <- as.Date("1900-01-01")
  max_date <- as.Date(Sys.Date())

  x <- as.Date(x)

  which(!is.na(x) & ((x < min_date) | (x > max_date)))
}

check_allowed_values <- function(x, allowed) {
  which(!is.na(x) & !(x %in% allowed))
}

#' Compare two database versions and summarize differences
#'
#' Loads data from two separate databases and prints a summary of which records
#' have been added or removed. Note this does not check for changes to
#' individual values, it only looks at which IDs have been added or removed.
#'
#' @param old_path Path to older database version
#' @param new_path Path to newest database version
#' @param log_file Path to output log file (optional)
#' @export
compare_database <- function(old_path, new_path, log_file=NULL) {
  if (!is.null(log_file)) {
    # turn on file logging
    sink(log_file)
  }

  hr_line <- paste0(paste0(rep("=", 80), collapse=""), "\n")

  ch_old <- db_connect(old_path)
  ch_new <- db_connect(new_path)

  cat("MyRWA WQ Database Comparison\n")
  cat("  Old DB: ", basename(old_path), "\n")
  cat("  New DB: ", basename(new_path), "\n")

  # compare Result table
  cat(hr_line)
  cat("Result Table\n\n")
  res_new <- db_table(ch_new, "Result")
  res_old <- db_table(ch_old, "Result")

  res_add <- res_new[which(res_new$ID %in% setdiff(res_new$ID, res_old$ID)),]
  res_del <- res_old[which(res_old$ID %in% setdiff(res_old$ID, res_new$ID)),]

  cat("# Rows Added:", nrow(res_add), "\n")
  if (nrow(res_add) > 0) {
    cat("Summary of Added Results:\n")
    print(summary(res_add[, c("ID", "CharacteristicID", "Units")]))
  }
  cat("\n# Rows Removed:", nrow(res_del), "\n")
  if (nrow(res_del) > 0) {
    cat("Summary of Removed Results:\n")
    print(summary(res_del[, c("ID", "CharacteristicID", "Units")]))
  }

  # compare Visit table
  cat(hr_line)
  cat("Visit Table\n\n")
  vis_new <- db_table(ch_new, "Visit")
  vis_old <- db_table(ch_old, "Visit")

  vis_add <- vis_new[which(vis_new$ID %in% setdiff(vis_new$ID, vis_old$ID)),]
  vis_add <- droplevels(vis_add)
  vis_del <- vis_old[which(vis_old$ID %in% setdiff(vis_old$ID, vis_new$ID)),]
  vis_del <- droplevels(vis_del)

  cat("# Rows Added:", nrow(vis_add), "\n")
  if (nrow(vis_add) > 0) {
    cat("ProjectIDs with Added Visits:",
        sort(unique(as.character(vis_add$ProjectID))), "\n")
    cat("LocationIDs with Added Visits:",
        sort(unique(as.character(vis_add$LocationID))), "\n")
    cat("Summary of Added Visits:\n")
    print(summary(vis_add[, c("ID", "Datetime", "LocationID", "ProjectID")]))
  }
  cat("\n# Rows Removed:", nrow(vis_del), "\n")
  if (nrow(vis_del) > 0) {
    cat("ProjectIDs with Removed Visits:",
        sort(unique(as.character(vis_del$ProjectID))), "\n")
    cat("LocationIDs with Removed Visits:",
        sort(unique(as.character(vis_del$LocationID))), "\n")
    cat("Summary of Removed Visits:\n")
    print(summary(vis_del[, c("ID", "Datetime", "LocationID", "ProjectID")]))
  }

  # compare Location table
  cat(hr_line)
  cat("Location Table\n\n")
  loc_new <- db_table(ch_new, "Location")
  loc_old <- db_table(ch_old, "Location")

  loc_add <- loc_new[which(loc_new$ID %in% setdiff(loc_new$ID, loc_old$ID)),]
  loc_add <- droplevels(loc_add)
  loc_del <- loc_old[which(loc_old$ID %in% setdiff(loc_old$ID, loc_new$ID)),]
  loc_del <- droplevels(loc_del)

  cat("# Rows Added:", nrow(loc_add), "\n")
  if (nrow(loc_add) > 0) {
    cat("Added LocationIDs:", paste(loc_add$ID, collapse=", "), "\n")
    cat("Summary of Added Locations:\n")
    print(summary(loc_add[, c("ID", "WaterBodyID", "MunicipalityID", "LocationTypeID")]))
  }
  cat("\n# Rows Removed:", nrow(loc_del), "\n")
  if (nrow(loc_del) > 0) {
    cat("Removed LocationIDs:", paste(loc_del$ID, collapse=", "), "\n")
    cat("Summary of Removed Locations:\n")
    print(summary(loc_del[, c("ID", "WaterBodyID", "MunicipalityID", "LocationTypeID")]))
  }

  close(ch_old)
  close(ch_new)

  if (!is.null(log_file)) {
    # turn off file logging
    sink(NULL)
  }

  invisible()
}
