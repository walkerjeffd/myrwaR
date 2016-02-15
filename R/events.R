#' Delineate storm events based on hourly precipitation
#'
#' Uses a timeseries of hourly precipitation to define storm events and create
#' summary table of events
#'
#' @param x Dataframe containing hourly precipitation timeseries
#' @param datetime.name Name of datetime column
#' @param value.name Name of precipitation value column
#' @param interevent.period Minimum inter-event period to distinguish separate
#'   events
#' @param threshold.total Minimum total precipitation for an individual event
#' @importFrom dplyr group_by summarise
#' @export
#' @return original dataframe with additional columns EventID, EventType, EventDuration
assign_precip_events <- function(x, datetime.name="Datetime", value.name="Precip",
                                 interevent.period=8, threshold.total=0.1) {

  if (!(datetime.name %in% names(x))) {
    stop(paste0("Could not find datetime column called ", datetime.name))
  }

  if (!(value.name %in% names(x))) {
    stop(paste0("Could not find value column called ", value.name))
  }

  if (!is.regular_hourly(x[[datetime.name]])) {
    stop("Timeseries is not hourly and continuous")
  }

  # save copy of original dataframe
  df <- x

  # extract datetime and value columns
  x <- x[, c(datetime.name, value.name)]
  names(x) <- c("Datetime", "Value")

  # make sure df is sorted by datetime
  x <- x[order(x[["Datetime"]]), ]

  # compute runs of non-zero values
  x_rle <- rle(x[["Value"]] > 0)

  # assign boolean flag to indicate wet events
  x$IsWet <- rep(x_rle$lengths * (x_rle$values | x_rle$lengths < interevent.period),
                 x_rle$lengths)
  x$IsWet <- x$IsWet > 0

  # assign EventID
  x$EventID <- c(as.numeric(x$IsWet[1]), diff(x$IsWet))
  x$EventID <- abs(x$EventID)
  x$EventID <- cumsum(x$EventID)

  # compute total sum of each event
  x_total <- group_by(x, EventID)
  x_total <- summarise(x_total, Total=sum(Value))
  x_total <- as.data.frame(x_total)

  # take subset of events that meet minimum threshold
  x_total <- subset(x_total, Total >= threshold.total)

  # remove wet events if they did not meet the threshold
  x[which(!(x$EventID %in% x_total$EventID)), "IsWet"] <- FALSE

  # reassign EventID based on current IsWet
  x$EventID <- c(as.numeric(x$IsWet[1]), diff(x$IsWet))
  x$EventID <- abs(x$EventID)
  x$EventID <- cumsum(x$EventID)

  # get rle of EventID
  x_rle_event <- rle(x$EventID)

  # add counter for each event
  x$EventDuration <- unlist(sapply(x_rle_event$lengths, seq))

  # add results to original df
  df$EventID <- x$EventID
  df$EventType <- ifelse(x$IsWet, "Wet", "Dry")
  df$EventStep <- x$EventStep

  df
}

#' Summarize storm events
#'
#' Creates a summary table of individual storm events based on hourly precipitation. The
#' input dataframe must contain the EventID and EventType columns generated
#' by assign_precip_events().
#'
#' @param x Dataframe containing hourly precipitation and already processed by assign_precip_events()
#' @param datetime.name Name of datetime column
#' @param value.name Name of precipitation column
#' @importFrom dplyr group_by summarise
#' @export
#' @return dataframe
precip_event_summary <- function(x, datetime.name="Datetime", value.name="Precip") {
  if (!("EventID" %in% names(x))) {
    stop("Unable to find column EventID in dataframe")
  }
  if (!("EventType" %in% names(x))) {
    stop("Unable to find column EventType in dataframe")
  }
  if (!(datetime.name %in% names(x))) {
    stop("Unable to find datetime column in dataframe")
  }
  if (!(value.name %in% names(x))) {
    stop("Unable to find value column in dataframe")
  }

  if (nrow(x) == 0) {
    stop("Precip dataframe has no rows")
  }

  x <- x[, c("EventID", "EventType", datetime.name, value.name)]
  names(x)[3:4] <- c("Datetime", "Precip")

  # x <- x[(x$EventType == "Wet" & !is.na(x$EventID)), ]

#   if (nrow(x) == 0) {
#     stop("Precip dataframe has wet events")
#   }

  x <- group_by(x, EventID, EventType)
  x <- summarise(x,
                 StartDatetime=min(Datetime),
                 EndDatetime=max(Datetime),
                 TotalDuration=n(),
                 PeakIntensity=max(Precip),
                 MeanIntensity=sum(Precip)/TotalDuration,
                 TotalDepth=sum(Precip))
  x <- ungroup(x)
  x <- as.data.frame(x)

  x$AntecedentDryPeriod <- ifelse(x$EventType=="Wet", dplyr::lag(x$TotalDuration), NA_real_)

  x
}
