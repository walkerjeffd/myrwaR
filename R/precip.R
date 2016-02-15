#' Load precipitation dataset from Excel File
#'
#' Loads the hourly precipitation data from an Excel spreadsheet
#' (specifically: LoganPrecip.xlsx).
#'
#' @param path Full path and filename to precipitation spreadsheet (LoganPrecip.xlsx)
#' @param sheet.name Name of workbook sheet to load data from (default="Processed precipitation")
#' @param tz Timezone of date/time stamps (default="EST")
#' @param datetime.name Name of datetime column (default="Datetime")
#' @param value.name Name of precipitation column (default="Precip")
#' @param as.type Return type as 'dataframe' (default) or 'zoo'
#' @importFrom readxl read_excel
#' @importFrom lubridate ymd_hms round_date
#' @importFrom zoo zoo
#' @export
#' @return dataframe or zoo object of hourly precipitation values
#' @examples
#' filepath <- system.file("extdata", "LoganPrecip.xlsx", package = "myrwaR")
#' pcp.df <- load_precip_from_xls(filepath, as.type='dataframe')
#' pcp.zoo <- load_precip_from_xls(filepath, as.type='zoo')
load_precip_from_xls <- function(path, sheet.name="Processed precipitation",
                                 tz="EST", datetime.name="Datetime",
                                 value.name="Precip",
                                 as.type=c("dataframe","zoo")) {
  as.type <- match.arg(as.type)

  # read precip file
  x <- read_excel(path, sheet = sheet.name)

  # extract datetime and value columns
  x <- x[, c(datetime.name, value.name)]

  # remove missing rows
  x <- x[complete.cases(x), ]

  # parse datetimes and round to nearest minute
  x[[datetime.name]] <- ymd_hms(x[[datetime.name]], tz = tz)
  x[[datetime.name]] <- round_date(x[[datetime.name]], unit="minute")

  # check regular
  if (!is.regular_hourly(x[[datetime.name]])) {
    warning("Precipitation timeseries is not continuous and hourly")
  }

  if (as.type == "zoo") {
    x <- zoo(x = x[[value.name]], order.by = x[[datetime.name]])
  }

  x
}

#' Load precipitation from USGS Web Services
#'
#' Fetches and returns precipitation timeseries from USGS web services.
#' Timeseries is automatically aggregated to hourly timesteps.
#'
#' @param start_date Start date of timeseries as string
#' @param end_date End date of timeseries as string (default=NULL meaning use today's date)
#' @param station_id USGS station ID as string (default="01102500" for Aberjona River)
#' @param tz Timezone assigned to resulting dataframe (default="EST")
#' @param as.type Return type as 'dataframe' (default) or 'zoo'
#' @param datetime.name Name of datetime column (default="Datetime")
#' @param value.name Name of precipitation column (default="Precip")
#' @importFrom dataRetrieval readNWISuv
#' @importFrom lubridate with_tz floor_date
#' @importFrom zoo zoo coredata
#' @export
#' @return dataframe or zoo object of hourly precipitation timeseries in
#'   inches/hour
#' @examples
#' pcp_usgs <- load_precip_from_usgs(start_date="2015-05-01",
#'                                   end_date="2015-05-10")
load_precip_from_usgs <- function(start_date, end_date=NULL,
                                  station_id="01102500",
                                  tz="EST", as.type=c("dataframe","zoo"),
                                  datetime.name = "Datetime",
                                  value.name = "Precip") {
  if (is.null(end_date)) {
    end_date <- today()
  }

  as.type <- match.arg(as.type)

  parameterCd <- "00045" # precipitation code

  x <- readNWISuv(siteNumber=station_id,
                  parameterCd=parameterCd,
                  startDate=start_date,
                  endDate=end_date,
                  tz="America/New_York")

  if (nrow(x) == 0) {
    warning("USGS returned no results for precipitation query")
    return(NULL)
  }

  x_datetime <- x[["dateTime"]]
  x_value <- x[["X_00045_00011"]]

  x_datetime <- with_tz(x_datetime, tzone=tz)

  z <- zoo(x_value, order.by = x_datetime)
  z <- aggregate(z, by = floor_date(time(z), unit="hour"), sum)

  if (as.type == "dataframe") {
    df <- data.frame(date = time(z), value = coredata(z))
    names(df) <- c(datetime.name, value.name)
    return(df)
  } else {
    return(z)
  }
}

fill_precip_with_usgs <- function(x, start_date=NULL, end_date=NULL,
                                  station_id="01104683",
                                  datetime.name="Datetime",
                                  value.name="Precip") {
  if (is.null(start_date)) {
    start_date <- max(x[, datetime.name])
  }
  if (is.null(end_date)) {
    end_date <- today()
  }

  start_date <- as.character(start_date)
  end_date <- as.character(end_date)

  usgs <- load_precip_from_usgs(start_date = start_date,
                                end_date = end_date,
                                station_id = station_id,
                                tz = lubridate::tz(x[, datetime.name]),
                                as.type = "dataframe",
                                datetime.name = datetime.name,
                                value.name = value.name)
  if (is.null(usgs)) {
    warning("USGS did not reqturn any data, so nothing was appended")
    return(x)
  }

  usgs <- usgs[which(usgs[[datetime.name]] > max(x[[datetime.name]])), ]

  x_new <- dplyr::rbind_list(x, usgs)

  if (!is.regular_hourly(x_new[[datetime.name]])) {
    warning("Filled timeseries is not hourly and continuous")
  }

  x_new
}

#' Compute antecedent precipitation
#'
#' Computes antecedent precipitation as a rolling sum. Time series
#' must be continuous at hourly intervals
#'
#' @param x Dataframe of hourly precipitation values
#' @param period Duration of antecedent period as hours (default=48)
#' @param delay Shift in the antecedent period as hours (default=0)
#' @param fun Function applied to each period (e.g. sum, max)
#' @param datetime.name Name of date/time column
#' @param value.name Name of precipitation column
#' @importFrom zoo zoo rollapply coredata
#' @export
#' @return numeric vector of antecedent precipitation for given period, delay, and fun
#' @examples
#' filepath <- system.file("extdata", "LoganPrecip.xlsx", package = "myrwaR")
#' pcp <- load_precip_from_xls(filepath, as.type='dataframe')
#' pcp$Precip.48 <- antecedent_precip(pcp, period=48, delay=0,
#'                                    datetime.name="Datetime",
#'                                    value.name="Precip")
antecedent_precip <- function(x, period=48, delay=0, fun=sum,
                              value.name="Precip",
                              datetime.name="Datetime") {
  if (!inherits(x, 'zoo')) {
    x <- zoo(x = x[[value.name]], order.by = x[[datetime.name]])
  }

  if (!is.regular_hourly(x)) {
    stop(paste0("Timeseries is not regular"))
  }

  apcp <- rollapply(x, period, fun, align = 'right', fill = NA)
  apcp <- stats::lag(apcp, k = -1*delay, na.pad = TRUE)
  coredata(apcp)
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
#' @importFrom lubridate floor_date
#' @export
#' @return numeric vector of antecedent precipitation
append_weather <- function(wq, precip, period=48, precip.threshold=0.25,
                           precip.name="Precip") {
  anteprecip.name <- paste("Precip", period, sep=".")

  if (!(anteprecip.name %in% names(precip))) {
    cat("Computing antecedent precip...")
    precip[, anteprecip.name] <- antecedent_precip(precip, period=period,
                                                   value.name=precip.name)
    cat("done\n")
  }

  if (!("DateHour" %in% names(wq))) {
    cat("Computing DateHour column in wq dataframe...")
    wq$DateHour <- floor_date(wq$Datetime,"hour")
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
  x$Weather <- factor(ifelse(x[[anteprecip.name]] >= precip.threshold,
                             "Wet", "Dry"))

  x
}
