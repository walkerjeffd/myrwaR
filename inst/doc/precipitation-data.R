## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = FALSE, comment = "#>")

## ----load-package, message=FALSE, warning=FALSE--------------------------
library(myrwaR)
library(dplyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

## ----load-xls------------------------------------------------------------
# get path to example precip file provided within myrwaR package
# this is a truncated version of the official MyRWA precipitation file
xls_path <- system.file("extdata", "LoganPrecip.xlsx", package = "myrwaR")
prcp <- load_precip_from_xls(path = xls_path, as.type = "dataframe")
str(prcp)

## ----load-xls-zoo--------------------------------------------------------
prcp_zoo <- load_precip_from_xls(path = xls_path, as.type = "zoo")
str(prcp_zoo)

## ----load-prcp-usgs------------------------------------------------------
prcp_usgs <- load_precip_from_usgs(start_date="2015-01-01", end_date="2015-01-10", station_id="01102500")
str(prcp_usgs)

## ----plot-prcp-usgs, fig.width=6, fig.height=4, warning=FALSE------------
mutate(prcp_usgs, PrecipCumsum = cumsum(Precip)) %>%
  ggplot(aes(Datetime)) +
  geom_line(aes(y=Precip, color="Precip")) +
  geom_line(aes(y=PrecipCumsum, color="PrecipCumsum")) +
  scale_color_manual('', values=c(Precip="steelblue", PrecipCumsum="orangered"),
                     labels=c(Precip="Hourly Precip", PrecipCumsum="Cumulative Precip")) +
  theme(legend.position=c(0, 1),
        legend.justification=c(0, 1))

## ----ante-prcp-----------------------------------------------------------
ante_prcp <- antecedent_precip(prcp, period = 48)
summary(ante_prcp)

## ----ante-error, results="hide", warning=FALSE, error=TRUE---------------
antecedent_precip(prcp[-100, ], period = 48)

## ----ante-prcp-col-------------------------------------------------------
prcp$Precip24 <- antecedent_precip(prcp, period = 24)
prcp$Precip48 <- antecedent_precip(prcp, period = 48)

## ----ante-prcp-col-delay-------------------------------------------------
prcp$Precip48.6 <- antecedent_precip(prcp, period = 48, delay = 6)

## ----ante-prcp-col-max---------------------------------------------------
prcp$Precip24.max <- antecedent_precip(prcp, period = 24, fun = max)

## ----plot-ante-prcp, fig.width=6, fig.height=4, warning=FALSE------------
ggplot(prcp[(24*15):(24*30), ], aes(Datetime)) +
  geom_line(aes(y=Precip, color="Precip")) +
  geom_line(aes(y=Precip24, color="Precip24")) +
  geom_line(aes(y=Precip24.max, color="Precip24.max")) +
  geom_line(aes(y=Precip48, color="Precip48")) +
  geom_line(aes(y=Precip48.6, color="Precip48.6")) +
  scale_color_manual(NULL,
                     labels=c(Precip="Hourly Precip",
                              Precip24="24-hr Total Precip", 
                              Precip24.max="24-hr Max Precip",
                              Precip48="48-hr Total Precip", 
                              Precip48.6="48-hour Total Precip (6-hr Delay)"),
                     values=c(Precip="black",
                              Precip24="red", 
                              Precip24.max="orange",
                              Precip48="deepskyblue", 
                              Precip48.6="olivedrab3")) +
  theme(legend.position=c(0, 1),
        legend.justification=c(0, 1),
        legend.background=element_blank())

## ----append-weather------------------------------------------------------
wq <- load_wq("D:/Dropbox/Work/mystic/db/MysticDB_20160208.accdb", projects="BASE", sample_types="S", exclude_flags=FALSE)
wq <- filter(wq, year(Datetime) %in% 2010:2011)
xls_path <- system.file("extdata", "LoganPrecip.xlsx", package = "myrwaR")
prcp <- load_precip_from_xls(path = xls_path, as.type = "dataframe")

wq_prcp <- append_weather(wq, prcp, period = 48, precip.threshold = 0.25, precip.name = "Precip")
str(wq_prcp)

## ----plot-prcp-032010, fig.width=6, fig.height=4, warning=FALSE----------
prcp_032010 <- dplyr::filter(prcp, lubridate::year(Datetime)==2010, month(Datetime)==3)
ggplot(prcp_032010, aes(Datetime, Precip)) +
  geom_line() +
  labs(y="Precip (in/hr)")

## ----prcp-events, fig.width=6, fig.height=4, warning=FALSE---------------
prcp_032010_evt <- assign_precip_events(x = prcp_032010)
str(prcp_032010_evt)

## ----plot-prcp-evt, fig.width=6, fig.height=4, warning=FALSE-------------
ggplot(prcp_032010_evt, aes(Datetime, Precip, group=EventID, color=interaction(EventType, EventID))) +
  geom_line() +
  scale_color_discrete("EventType.EventID") +
  labs(y="Precip (in/hr)")

## ----plot-prcp-wet, fig.width=6, fig.height=8, warning=FALSE-------------
filter(prcp_032010_evt, EventType == "Wet") %>%
  group_by(EventID) %>%
  mutate(PrecipCumsum=cumsum(Precip)) %>%
  ggplot(aes(Datetime)) +
  geom_line(aes(y=Precip, color="Precip")) +
  geom_line(aes(y=PrecipCumsum, color="PrecipCumsum")) +
  scale_color_manual(NULL, labels=c(Precip="Hourly Precip", PrecipCumsum="Cumulative Precip"),
                     values=c(Precip="steelblue", PrecipCumsum="orangered")) +
  scale_x_datetime(labels=scales::date_format("%b %d %H:%M")) +
  ylim(0, NA) +
  labs(x="", y="Precip (in/hr) / Cumul Precip (in)") +
  facet_wrap(~EventID, scales="free", labeller = "label_both", ncol = 2) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

## ----prcp-evt-summary----------------------------------------------------
prcp_032010_evt_summary <- precip_event_summary(prcp_032010_evt)
str(prcp_032010_evt_summary)

## ----prcp-evt-summary-wet------------------------------------------------
filter(prcp_032010_evt_summary, EventType=="Wet") %>%
  summary

## ----prcp-evt-wq-1-------------------------------------------------------
wq <- load_wq("D:/Dropbox/Work/mystic/db/MysticDB_20160208.accdb", projects="BASE", sample_types="S", exclude_flags=FALSE)
wq <- filter(wq, year(Datetime) %in% 2010:2011)

xls_path <- system.file("extdata", "LoganPrecip.xlsx", package = "myrwaR")
prcp <- load_precip_from_xls(path = xls_path, as.type = "dataframe")
prcp <- dplyr::filter(prcp, year(Datetime) %in% 2010:2010)

## ----prcp-evt-wq-2-------------------------------------------------------
prcp_evt <- assign_precip_events(x = prcp)

summary(prcp_evt)

## ----prcp-evt-wq-3-------------------------------------------------------
wq <- mutate(wq, Datehour = floor_date(Datetime, unit = "hour"))
wq_prcp_evt <- left_join(wq, prcp_evt, by=c("Datehour"="Datetime"))
str(wq_prcp_evt)

## ----prcp-evt-wq-4-------------------------------------------------------
prcp_evt_summary <- precip_event_summary(prcp_evt)
wq_prcp_evt <- left_join(wq_prcp_evt, prcp_evt_summary, by=c("EventID", "EventType"))
str(wq_prcp_evt)

