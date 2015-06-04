#' Checks for untrimmed strings
#'
#' Given character vector, checks if any of the unique values have extra spaces
#' at the start or end (e.g. "a " or " a").
#' Prints message listing any untrimmed values.
#'
#' @param x character vector
#' @export
#' @return boolean
#' @examples
#' check_string_trim(c("a", "b", "c ")) # false
check_string_untrimmed <- function(x) {
  x <- subset(x, !is.na(x))
  trimmed <- sapply(as.character(unique(x)), stringr::str_trim)
  unmatched <- names(trimmed[names(trimmed) != unname(trimmed)])
  if (length(unmatched) > 0) {
    print(paste0("Found ", length(unmatched), " untrimmed unique values: ",
                 paste(paste0("'", unmatched, "'"), collapse=", ")))
    return(FALSE)
  }
  return(TRUE)
}

#' Checks for empty strings
#'
#' Given character vector, checks if any of the unique values are empty strings.
#' Prints message listing any empty string values.
#'
#' @param x character vector
#' @export
#' @return boolean
#' @examples
#' check_string_trim(c("a", "b", " "))  # false
#' check_string_trim(c("a", "b", "  ")) # false
check_no_empty_strings <- function(x) {
  x.unique <- as.character(unique(x))
  x.trim <- sapply(x.unique, stringr::str_trim)
  x.empty <- names(x.trim[which(x.trim == "")])

  if (length(x.empty) > 0) {
    print(paste0("Found ", sum(x %in% x.empty),
                 " empty values with factor levels: ",
                 paste(paste0("'", x.empty, "'"),
                       collapse=", ")))
    return(FALSE)
  }
  return(TRUE)
}

#' Runs batch of QAQC tests on Results table
#'
#' Checks for empty and untrimmed strings
#'
#' @param ch connection handle to database
#' @export
#' @return nothing
#' @examples
#' qaqc_table_results(ch)
qaqc_table_results <- function(ch) {
  res <- db_table(ch, "Results")
  testthat::expect_true(check_no_empty_strings(res$Qualifier))
  testthat::expect_true(check_no_empty_strings(res$CharacteristicID))
  testthat::expect_true(check_no_empty_strings(res$SampleType))
  testthat::expect_true(check_no_empty_strings(res$LocationID))
  testthat::expect_true(check_no_empty_strings(res$ProjectID))
  testthat::expect_true(check_no_empty_strings(res$Datetime))
  testthat::expect_true(check_no_empty_strings(res$SampleTypeID))
  testthat::expect_true(check_no_empty_strings(res$Units))
  testthat::expect_true(check_no_empty_strings(res$SampleTypeID))
}

#' Compare two database versions and summarize differences
#'
#' Loads data from two separate databases and prints a summary of which records
#' have been added or removed. Note this does not check for changes to
#' individual values, it only looks at which IDs have been added or removed.
#'
#' @param old_path Path to older database version
#' @param new_path Path to newest database version
#' @export
compare_database <- function(old_path, new_path) {
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

  invisible()
}
