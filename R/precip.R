#' Load precipitation from LoganPrecip.xlsx
#'
#' Loads the hourly precipitation data from LoganPrecip.xlsx.
#' Returns zoo object or data frame with columns (Datetime, Precip)
#'
#' @param path path and filename to precipitation spreadsheet (LoganPrecip.xlsx)
#' @param as.type return type as 'dataframe' (default) or 'zoo'
#' @export
#' @return dataframe or zoo object of hourly precipitation values
#' @examples
#' filepath <- file.path("..","..","_share_db","Processed",
#'                       "Precip","LoganPrecip.xlsx")
#' pcp.df <- load_precip_from_xls(filepath, as.type='dataframe')
#' pcp.zoo <- load_precip_from_xls(filepath, as.type='zoo')
load_precip_from_xls <- function(path, as.type=c("dataframe","zoo")) {
  as.type <- match.arg(as.type)

  message(paste0("Loading precip file: ", normalizePath(path)))

  # open precip file
  ch <- RODBC::odbcConnectExcel2007(path)

  # fetch data
  df <- RODBC::sqlFetch(ch, sqtable = "Processed precipitation",
                        na.strings = "NA", as.is = TRUE)
  RODBC::close(ch)

  # drop first two columns ("mm/dd/yyyy","hour")
  df <- df[, c("Datetime", "Precip")]

  df$Datetime <- lubridate::ymd_hms(df$Datetime, tz="EST")

  if (as.type == "zoo") {
    z <- zoo::zoo(df$Precip, df$Datetime)
    return(z)
  }

  df
}

#' Computes antecedent precipitation
#'
#' Computes antecedent precipitation as a rolling sum. Ensures the time series
#' is regular and continuous
#'
#' @param x data frame of hourly precipitation values
#' @param period antecedent period in number of hours
#' @param precip.name name of precipitation values column
#' @param datetime.name name of date/time column
#' @export
#' @return numeric vector of antecedent precipitation
#' @examples
#' pcp$Precip.48 <- antecedent_precip(pcp, period=48, precip.name="Precip",
#'                                    datetime.name="Datetime")
antecedent_precip <- function(x, period=48, precip.name="Precip",
                              datetime.name="Datetime") {
  check_hourly(x[[datetime.name]])
  apcp <- stats::filter(x[, precip.name], rep(1, period), side = 1)
  apcp
}


#' Adds antecedent precipitation columns
#'
#' Appends antecedent precipitation columns for multiple periods to an
#' existing wq data frame
#'
#' @param wq water quality data frame
#' @param precip precipitation date frame (hourly timestep)
#' @param period antecedent period in number of hours
#' @param precip.threshold threshold for assigning wet/dry to Weather column
#' @param precip.name name of precipitation column
#' @export
#' @return numeric vector of antecedent precipitation
append_weather <- function(wq, precip, period=48, precip.threshold=0.25,
                           precip.name="Precip") {
  anteprecip.name <- paste("Precip", period, sep=".")

  if (!(anteprecip.name %in% names(precip))) {
    cat("Computing antecedent precip...")
    precip[, anteprecip.name] <- antecedent_precip(precip, period=period,
                                                   precip.name=precip.name)
    cat("done\n")
  }

  if (!("DateHour" %in% names(wq))) {
    cat("Computing DateHour column in wq dataframe...")
    wq$DateHour <- lubridate::floor_date(wq$Datetime,"hour")
    cat("done\n")
  }

  if (anteprecip.name %in% names(wq)) {
    cat(paste0("Removing existing ", anteprecip.name, " from wq dataframe..."))
    wq <- wq[, -which(names(wq) == anteprecip.name)]
    cat("done\n")
  }

  # merge wq and precip
  cat("Merging wq and precip...")
  x <- merge(wq, precip[, c("Datetime", anteprecip.name)],
             by.x="DateHour", by.y="Datetime", all.x=T)
  cat("done\n")

  # compute weather
  x$Weather <- factor(ifelse(x[, anteprecip.name] >= precip.threshold,
                             "Wet", "Dry"))

  x
}
