#' Delineates storm events based on hourly precipitation
#'
#' Uses a timeseries of hourly precipitation to define storm events and create
#' summary table
#'
#' @param x Dataframe containing hourly precipitation timeseries
#' @param datetime.name Name of datetime column
#' @param value.name Name of precipitation value column
#' @param interevent.period Minimum inter-event period to distinguish separate
#'   events
#' @param threshold.total Minimum total precipitation for an individual event
#' @export
#' @return dataframe or zoo object of hourly precipitation values
assign_precip_events <- function(x, datetime.name="Datetime", value.name="Precip",
                                 interevent.period=8, threshold.total=0.1) {

  if (!(datetime.name %in% names(x))) {
    stop(paste0("Could not find datetime column called ", datetime.name))
  }

  if (!(value.name %in% names(x))) {
    stop(paste0("Could not find value column called ", value.name))
  }

  if (!check_hourly(x[, datetime.name])) {
    stop("Timeseries is not hourly regular")
  }

  df <- x

  # extract datetime and value columns
  x <- dplyr::select_(x, "Datetime"=datetime.name, "Value"=value.name)

  # make sure df is sorted by datetime
  x <- x[order(x[, "Datetime"]), ]

  # compute runs of non-zero values
  x_rle <- rle(x[, "Value"] > 0)

  # assign boolean flag to indicate wet events
  x$IsWet <- rep(x_rle$lengths * (x_rle$values | x_rle$lengths < interevent.period),
                 x_rle$lengths)
  x$IsWet <- x$IsWet > 0

  # assign wet event IDs
#   x$WetEventID <- c(as.numeric(x$IsWet[1]),
#                     ifelse(diff(x$IsWet) > 0, diff(x$IsWet), 0))
  x$EventID <- c(as.numeric(x$IsWet[1]), diff(x$IsWet))
  x$EventID <- abs(x$EventID)
  x$EventID <- cumsum(x$EventID)

  # compute total sum of each event
  x_total <- dplyr::group_by(x, EventID)
  x_total <- dplyr::summarise(x_total, Total=sum(Value))

  # take subset of events that meet minimum threshold
  x_total <- subset(x_total, Total >= threshold.total)

  # assign wet events an ID of 0 if they did not meet the threshold
  x[which(!(x$EventID %in% x_total$EventID)), "IsWet"] <- FALSE

  # reassign EventID based on current IsWet
  x$EventID <- c(as.numeric(x$IsWet[1]), diff(x$IsWet))
  x$EventID <- abs(x$EventID)
  x$EventID <- cumsum(x$EventID)

  # get rle of EventID
  x_rle_event <- rle(x$EventID)

  # add counter for each dry/wet event
  x$EventDuration <- unlist(sapply(x_rle_event$lengths, seq))

  # set WetEventID and DryEventID to NA during opposite event type
#   x[which(x$WetEventID == 0), "WetEventID"] <- NA
#   x[which(x$DryEventID == 0), "DryEventID"] <- NA

  # assign counter for dry events (number of hours since end of last wet event)
  # x$DryEventPeriod <- ifelse(!is.na(x$DryEventID), x$Counter, 0)

  # assign counter for wet events (number of hours since end of last dry event)
  # x$WetEventPeriod <- ifelse(!is.na(x$WetEventID), x$Counter, 0)

  # add Wet/Dry EventID, EventPeriod to original df and return
#   df$WetEventID <- x$WetEventID
#   df$WetEventPeriod <- x$WetEventPeriod
#   df$DryEventID <- x$DryEventID
#   df$DryEventPeriod <- x$DryEventPeriod
  df$EventID <- x$EventID
  df$EventType <- ifelse(x$IsWet, "Wet", "Dry")
  df$EventDuration <- x$EventDuration

  df
}

events.summary <- function(df, datetime.name="DATETIME", value.name="VALUE") {
  # summarizes each event event in dataframe df
  # assumes df contains columns EVENT_ID (from assign.events),
  #   DATETIME (POSIXct), and VALUE (numeric)
  if (!("EVENT_ID" %in% names(df))) {
    stop("Unable to find column EVENT_ID in dataframe")
  }
  if (!(datetime.name %in% names(df))) {
    stop("Unable to find datetime column in dataframe")
  }
  if (!(value.name %in% names(df))) {
    stop("Unable to find value column in dataframe")
  }
  df$DATETIME <- df[, datetime.name]
  df$VALUE <- df[, value.name]
  event <- ddply(subset(df, !is.na(EVENT_ID)), c("EVENT_ID"), summarise,
                 DURATION=length(DATETIME),
                 START=min(DATETIME),
                 END=max(DATETIME),
                 TOTAL=sum(VALUE),
                 PEAK=max(VALUE))
  event
}
