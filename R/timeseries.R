#' Constructs regular zoo object
#'
#' Given vector of datetimes and values, constructs regular time series with filling
#'
#' @param dates vector of POSIXct values
#' @param values vector of numeric values
#' @param by target frequency (e.g. "hour", "day", etc), see seq.POSIXt
#' @param fill value to fill missing elements
#' @export
#' @return boolean
#' @examples
#' zoo.regular(df$Datetime, df$Value, by="hour", fill=NA)
zoo.regular <- function(dates, values, by="hour", fill=NA) {
  require()
  z <- zoo::zoo(values, dates)
  z <- merge(z,
             zoo::zoo(, seq(floor_date(start(z), "day"),
                            ceiling_date(end(z), "day"),
                            by="day")),
             fill=fill)
  z <- z[1:(length(z) - 1)]
  return(z)
}

#' Checks if vector of hourly timestamps is regular
#'
#' Given vector of POSIXct values, checks if the series is regular (i.e. consecutive and complete)
#'
#' @param x vector of POSIXct values
#' @export
#' @return boolean
#' @examples
#' check_hourly(df$Datetime)
check_hourly <- function(x) {
  x.difftime <- as.numeric(difftime(x[2:length(x)],
                                    x[1:(length(x) - 1)],
                                    units="secs"))

  if (min(x.difftime) < 3600) {
    x.check <- x[which(x.difftime < 3600)[1]]
    stop(paste0("Time series is not regular, check timeseries at ", x.check))
    return(FALSE)
  } else if (max(x.difftime) > 3600) {
    x.check <- x[which(x.difftime > 3600)[1]]
    stop(paste0("Time series is not regular, check timeseries at ", x.check))
    return(FALSE)
  }

  return(TRUE)
}

#' Get water year from date
#'
#' Given vector of POSIXct values, returns vector of water years
#'
#' @param x vector of POSIXct values
#' @export
#' @return numeric vector of water years
#' @examples
#' water_year(c(ymd("2000-09-30","2000-10-01","2000-10-02"))) # c(2000,2000,2001)
water_year <- function(x) {
  lubridate::year(lubridate::floor_date(x, unit="month") + months(3))
}
