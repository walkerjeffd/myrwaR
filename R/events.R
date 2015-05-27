assign.events <- function(df, datetime.name="DATETIME", value.name="VALUE",
                          interevent=8, threshold=0.1) {
  # assigns events to data frame such as storm events or discharge events
  require(plyr)
  if (!(datetime.name %in% names(df))) {
    stop(paste0("Could not find datetime column called ", datetime.name))
  }
  if (!(value.name %in% names(df))) {
    stop(paste0("Could not find value column called ", value.name))
  }
  if (!is.regular(df[, datetime.name])) {
    stop("Dataframe is not regular")
  }
  # make sure df is sorted by datetime
  df <- df[order(df[, datetime.name]), ]

  # extract datetime and value
  x <- data.frame(DATETIME=df[, datetime.name],
                  VALUE=df[, value.name])

  # compute runs
  x_rle <- rle(x$VALUE > 0)

  # EVENT: logical column indicating if timestep is part of event
  # does not yet account for events that do not meet minimum threshold
  x$EVENT <- rep(x_rle$lengths * (x_rle$values | x_rle$lengths < interevent),
                 x_rle$lengths)
  x$EVENT <- x$EVENT > 0

  # EVENT_ID: numeric column assigning unique IDs to each event
  x$EVENT_ID <- c(as.numeric(x$EVENT[1]),
                  ifelse(diff(x$EVENT) > 0, diff(x$EVENT), 0))
  x$EVENT_ID <- cumsum(x$EVENT_ID) * x$EVENT

  # compute sum of each event
  x_event <- plyr::ddply(x, .(EVENT_ID), plyr::summarise, SUM=sum(VALUE))

  # take subset of events that meet minimum threshold
  x_event <- subset(x_event, SUM >= threshold)

  # assign events an ID of 0 if they did not meet the threshold
  x$EVENT_ID[which(!(x$EVENT_ID %in% x_event$EVENT_ID))] <- 0

  # reassign EVENT based on current EVENT_ID
  x$EVENT <- x$EVENT_ID > 0

  # reassign EVENT_ID based on new EVENT
  x$EVENT_ID <- c(as.numeric(x$EVENT[1]),
                  ifelse(diff(x$EVENT) > 0,
                         diff(x$EVENT), 0))
  x$EVENT_ID <- cumsum(x$EVENT_ID) * x$EVENT

  # assign DRY_ID for dry periods
  x$DRY_ID <- c(as.numeric(!x$EVENT[1]),
                ifelse(diff(!x$EVENT) > 0,
                       diff(!x$EVENT), 0))
  x$DRY_ID <- cumsum(x$DRY_ID) * (!x$EVENT)

  # get rle of EVENT_ID
  x_rle_event <- rle(x$EVENT_ID)

  # add counter for each dry/wet event
  x$COUNTER <- unlist(sapply(x_rle_event$lengths, seq))

  # set EVENT_ID during dry events and DRY_ID during wet events to NA
  x$EVENT_ID[which(x$EVENT_ID == 0)] <- NA
  x$DRY_ID[which(x$DRY_ID == 0)] <- NA

  # assign counter for dry events (number of hours since end of last wet event)
  x$DRY_HOURS <- ifelse(!is.na(x$DRY_ID), x$COUNTER, 0)

  # assign counter for wet events (number of hours since end of last dry event)
  x$EVENT_HOURS <- ifelse(!is.na(x$EVENT_ID), x$COUNTER, 0)

  # add EVENT_ID, DRY_HOURS, WET_HOURS to df and return
  df$EVENT_ID <- x$EVENT_ID
  df$EVENT_HOURS <- x$EVENT_HOURS
  df$DRY_ID <- x$DRY_ID
  df$DRY_HOURS <- x$DRY_HOURS

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
