#' Checks for untrimmed strings
#' 
#' Given character vector, checks if any of the unique values have extra spaces at the start or end (e.g. "a " or " a"). Prints message listing any untrimmed values.
#' 
#' @param x character vector
#' @export
#' @return boolean
#' @examples
#' check_string_trim(c("a", "b", "c ")) # false
check_string_untrimmed <- function(x) {
  require(stringr)
  x <- subset(x, !is.na(x))
  trimmed <- sapply(as.character(unique(x)), stringr::str_trim)
  unmatched <- names(trimmed[names(trimmed) != unname(trimmed)])
  if (length(unmatched) > 0) {
    print(paste0('Found ', length(unmatched), ' untrimmed unique values: ', paste(paste0("'", unmatched, "'"), collapse=', ')))
    return(FALSE)
  }
  return(TRUE)
}

#' Checks for empty strings
#' 
#' Given character vector, checks if any of the unique values are empty strings. Prints message listing any empty string values.
#' 
#' @param x character vector
#' @export
#' @return boolean
#' @examples
#' check_string_trim(c("a", "b", " "))  # false
#' check_string_trim(c("a", "b", "  ")) # false
check_no_empty_strings <- function(x) {
  require(stringr)
  x.unique <- as.character(unique(x))
  x.trim <- sapply(x.unique, stringr::str_trim)
  x.empty <- names(x.trim[which(x.trim=='')])
  
  if (length(x.empty) > 0) {
    print(paste0('Found ', sum(x %in% x.empty), ' empty values with factor levels: ', paste(paste0("'", x.empty, "'"), collapse=', ')))
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
  res <- db_table(ch, 'Results')
  expect_true(check_no_empty_strings(res$Qualifier))
  expect_true(check_no_empty_strings(res$CharacteristicID))
  expect_true(check_no_empty_strings(res$SampleType))
  expect_true(check_no_empty_strings(res$LocationID))
  expect_true(check_no_empty_strings(res$ProjectID))
  expect_true(check_no_empty_strings(res$Datetime))
  expect_true(check_no_empty_strings(res$SampleTypeID))
  expect_true(check_no_empty_strings(res$Units))
  expect_true(check_no_empty_strings(res$SampleTypeID))  
}
