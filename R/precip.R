#' Load precipitation from LoganPrecip.xlsx
#' 
#' Loads the hourly precipitation data from LoganPrecip.xlsx. Returns zoo object or data frame with columns (Datetime, Precip)
#' 
#' @param path path and filename to precipitation spreadsheet (LoganPrecip.xlsx)
#' @param as.type return type as 'dataframe' (default) or 'zoo'
#' @export
#' @return dataframe or zoo object of hourly precipitation values
#' @examples
#' pcp.df <- load_precip_from_xls(file.path("..","..","_share_db","Processed","Precip","LoganPrecip.xlsx"), as.type='dataframe')
#' pcp.zoo <- load_precip_from_xls(file.path("..","..","_share_db","Processed","Precip","LoganPrecip.xlsx"), as.type='zoo')
load_precip_from_xls <- function(path, as.type=c('dataframe','zoo')) {
  require(RODBC)
  require(lubridate)
  require(zoo)
  
  as.type <- match.arg(as.type)
    
  message(paste0('Loading precip file: ', normalizePath(path)))
  
  # open precip file
  ch <- odbcConnectExcel2007(path)
  
  # fetch data
  df <- sqlFetch(ch, sqtable = "Processed precipitation",
                        na.strings = "NA",
                        as.is = TRUE)
  close(ch)

  # drop first two columns ('mm/dd/yyyy','hour')
  df <- df[,c('Datetime','Precip')]
  
  df$Datetime <- ymd_hms(df$Datetime, tz="EST")
  
  if (as.type == 'zoo') {
    z <- zoo(df$Precip, df$Datetime)
    return(z)
  }
  
  df
}

#' Computes antecedent precipitation
#' 
#' Computes antecedent precipitation as a rolling sum. Ensures the time series is regular and continuous
#' 
#' @param x data frame of hourly precipitation values
#' @param period antecedent period in number of hours
#' @param precip.name name of precipitation values column
#' @param datetime.name name of date/time column
#' @export
#' @return numeric vector of antecedent precipitation
#' @examples
#' pcp$Precip.48 <- antecedent_precip(pcp, period=48, precip.name="Precip", datetime.name="Datetime")
antecedent_precip <- function(x, period=48, precip.name="Precip", datetime.name="Datetime") {
  check_hourly(x[[datetime.name]])
  apcp <- filter(x[,precip.name], rep(1, period), side = 1)
  apcp
}
