#' Load precipitation from Excel File
#'
#' Loads the hourly precipitation data from an Excel spreadsheet such as
#' LoganPrecip.xlsx.
#'
#' @param path Path and filename to precipitation spreadsheet (LoganPrecip.xlsx)
#' @param sheet.name Name of workbook sheet to load data from
#' @param tz Timezone of datetimes (default="EST")
#' @param datatime.name Name of datetime column
#' @param value.name Name of precipitation value column
#' @param as.type Return type as 'dataframe' (default) or 'zoo'
#' @export
#' @return dataframe or zoo object of hourly precipitation values
#' @examples
#' filepath <- file.path("..","..","_share_db","Processed",
#'                       "Precip","LoganPrecip.xlsx")
#' pcp.df <- load_precip_from_xls(filepath, as.type='dataframe')
#' pcp.zoo <- load_precip_from_xls(filepath, as.type='zoo')
load_precip_from_xls <- function(path, sheet.name="Processed precipitation",
                                 tz="EST", datetime.name="Datetime",
                                 value.name="Precip", as.type=c("dataframe","zoo")) {
  as.type <- match.arg(as.type)

  # read precip file
  ch <- RODBC::odbcConnectExcel2007(path)
  x <- RODBC::sqlFetch(ch, sqtable = sheet.name,
                       na.strings = "NA", as.is = TRUE)
  close(ch)

  # extract datetime and value columns
  x <- x[, c(datetime.name, value.name)]

  # parse datetimes
  x[, datetime.name] <- lubridate::ymd_hms(x[, datetime.name], tz = tz)

  if (as.type == "zoo") {
    x <- zoo::zoo(x = x[, value.name], order.by = x[, datetime.name])
  }

  x
}

#' Load precipitation from USGS Web Services
#'
#' Fetches and returns precipitation timeseries from USGS web services.
#' Timeseries is automatically aggregated to hourly timesteps.
#'
#' @param startDate Start date of timeseries as string
#' @param endDate End date of timeseries as string
#' @param siteNumber USGS station ID (default=01104683 for Muddy River)
#' @param tz Timezone assigned to resulting dataframe (default="EST")
#' @param as.type Return type as 'dataframe' (default) or 'zoo'
#' @export
#' @return dataframe or zoo object of hourly precipitation timeseries in
#'   inches/hour
#' @examples
#' pcp_usgs <- load_precip_from_usgs(startDate="2015-05-01",
#'                                   endDate="2015-05-10")
load_precip_from_usgs <- function(startDate, endDate, siteNumber="01104683",
                                  tz="EST", as.type=c("dataframe","zoo")) {
  as.type <- match.arg(as.type)
  parameterCd <- "00045" # precipitation
  x <- dataRetrieval::readNWISuv(siteNumber=siteNumber,
                                 parameterCd=parameterCd,
                                 startDate=startDate,
                                 endDate=endDate,
                                 tz="America/New_York")

  if (nrow(x) == 0) {
    stop("USGS returned no results for precipitation query")
  }

  x <- dplyr::select(x, Datetime=dateTime, Precip=X_00045_00011)
  x <- dplyr::mutate(x,
                     Datetime=lubridate::with_tz(Datetime, tzone=tz),
                     Datetime=lubridate::floor_date(Datetime, unit="hour"))
  x <- dplyr::group_by(x, Datetime)
  x <- dplyr::summarise(x, Precip=sum(Precip))
  x <- as.data.frame(x)

  if (as.type == "zoo") {
    x <- zoo::zoo(x = x[, "Precip"], order.by = x[, "Datetime"])
  }

  x
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
  apcp <- as.numeric(apcp)
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
