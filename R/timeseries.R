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
  z <- zoo::zoo(values, dates)
  z <- merge(z,
             zoo::zoo(, seq(floor_date(start(z), "day"),
                            ceiling_date(end(z), "day"),
                            by=by)),
             fill=fill)
  z <- z[1:(length(z) - 1)]
  return(z)
}

#' Checks if vector of hourly timestamps is regular
#'
#' Given a timeseries as a zoo object or vector of datetime values, checks if
#' the series is regular (i.e. consecutive and complete) at hourly timesteps
#'
#' @param x zoo object or vector of datetimes
#' @export
#' @return boolean
#' @examples
#' is.regular_hourly(df$Datetime)
is.regular_hourly <- function(x) {
  if (inherits(x, 'zoo')) {
    x <- time(x)
  }
  dt <- as.numeric(difftime(x[2:length(x)],
                            x[1:(length(x) - 1)],
                            units="secs"))

  if (any(dt != 3600)) {
    check_at <- which(dt != 3600)[1]
    message(paste0("Timeseries is not regular, check timesteps around ",
                   x[check_at], " (index ", check_at, ")"))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Get water year from date
#'
#' Returns water year of date vector for given starting month (default=October).
#'
#' @param x vector of dates
#' @param start_month month that begins a water year
#' @export
#' @return numeric vector of water years
#' @examples
#' water_year(c(ymd("2000-09-30","2000-10-01","2000-10-02")))
water_year <- function(x, start_month=10) {
  if (start_month == 1) {
    return(lubridate::year(x))
  } else {
    return(ifelse(lubridate::month(x)>=start_month,
                  lubridate::year(x)+1,
                  lubridate::year(x)))
  }
}
