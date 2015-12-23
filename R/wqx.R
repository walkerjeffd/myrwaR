#' Convert Projects to WQX Format
#'
#' Extracts projects from database and returns data frame in WQX format
#'
#' @param ch Open connection handle to database
#' @param projects Vector of ProjectIDs
#' @export
#' @return data frame of projects in WQX format
#' @importFrom plyr rename
#'
wqx_projects <- function(ch, projects) {
  if (length(projects) == 0) {
    stop('Must specify one or more ProjectIDs in projects argument.')
  }

  name_map <- c(ID="ProjectID",
                ProjectName="ProjectName",
                ProjectDescription="ProjectDescription")

  proj <- db_table(ch, 'Project', stringsAsFactors=FALSE)

  proj <- subset(proj, ID %in% projects)

  if (nrow(proj) == 0) {
    stop('No projects found in database, projects argument should be a vector of project IDs')
  }

  proj <- proj[, names(name_map)]

  proj <- rename(proj, name_map)

  proj
}


#' Convert Locations to WQX Format
#'
#' Extracts locations from database and returns data frame in WQX format
#'
#' @param ch Open connection handle to database
#' @param locations Vector of LocationIDs
#' @export
#' @return data frame of locations in WQX format
#' @importFrom dplyr left_join filter
#' @importFrom plyr revalue rename
#'
wqx_locations <- function(ch, locations) {
  if (length(locations) == 0) {
    stop('Must specify one or more LocationIDs in locations argument.')
  }

  loc <- db_table(ch, 'Location', stringsAsFactors=FALSE)
  loc_type <- db_table(ch, 'LocationType', stringsAsFactors=FALSE)
  loc_type <- rename(loc_type, c(ID="LocationTypeID"))
  loc <- left_join(loc, loc_type, by="LocationTypeID")

  loc <- subset(loc, ID %in% locations)

  if (nrow(loc) == 0) {
    stop('No locations found in database, locations argument should be a vector of location IDs')
  }

  county_codes <- c("Barnstable"="001",
                    "Berkshire"="003",
                    "Bristol"="005",
                    "Dukes"="007",
                    "Essex"="009",
                    "Franklin"="011",
                    "Hampden"="013",
                    "Hampshire"="015",
                    "Middlesex"="017",
                    "Nantucket"="019",
                    "Norfolk"="021",
                    "Plymouth"="023",
                    "Suffolk"="025",
                    "Worcester"="027")
  loc$County <- revalue(loc$County, county_codes, warn_missing=FALSE)

  name_map <- c(ID="MonitoringLocationID",
                LocationDescription="MonitoringLocationName",
                LocationTypeName="MonitoringLocationType",
                Latitude="MonitoringLocationLatitude",
                Longitude="MonitoringLocationLongitude",
                LocationMethod="MonitoringLocationHorizontalCollectionMethod",
                CoordinateSystem="MonitoringLocationHorizontalCoordinateReferenceSystem",
                County="MonitoringLocationCountyCode")

  loc <- loc[, names(name_map)]

  loc <- rename(loc, name_map)

  loc[, 'MonitoringLocationState'] <- 'MA'

  loc
}


#' Convert Results to WQX Format
#'
#' Extracts results from database and returns data frame in WQX format
#'
#' @param ch Open connection handle to database
#' @param projects Vector of ProjectIDs
#' @param locations Vector of LocationIDs (default=NULL to get all locations)
#' @param start_date Start date of results to export
#' @param end_date End date of results to export
#' @export
#' @return data frame of results in WQX format
#'
wqx_results <- function(ch, projects, locations=NULL, start_date='1900-01-01', end_date='2099-01-01') {

  if (length(projects) == 0) {
    stop('Must specify one or more ProjectIDs in projects argument.')
  }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  res <- db_results(ch, stringsAsFactors=FALSE)
  res <- dplyr::mutate(res,
                       CharacteristicID=as.character(CharacteristicID),
                       LocationID=as.character(LocationID),
                       Units=as.character(Units),
                       ProjectID=as.character(ProjectID),
                       MethodID=as.character(MethodID))

  res <- dplyr::filter(res, ProjectID %in% projects)

  if (!is.null(locations)) {
    res <- dplyr::filter(res, LocationID %in% locations)
  }

  if (nrow(res) == 0) {
    stop('No results found for given projects and locations')
  }

  # force all datetimes to be EST
  res <- dplyr::mutate(res, Datetime=lubridate::with_tz(Datetime, tzone="EST"))
  res <- dplyr::filter(res,
                       as.Date(Datetime) >= start_date,
                       as.Date(Datetime) <= end_date)

  if (nrow(res) == 0) {
    stop('Results table has no rows after filtering by projects and locations')
  }

  methods <- db_table(ch, 'Method', stringsAsFactors=FALSE)
  methods <- dplyr::mutate(methods, ID=as.character(ID))
  methods <- dplyr::rename(methods, MethodCharacteristicID=CharacteristicID)
  characteristics <- db_table(ch, 'Characteristic', stringsAsFactors=FALSE)
  sample_fractions <- db_table(ch, 'SampleFraction', stringsAsFactors=FALSE)
  result_flags <- db_table(ch, 'ResultFlag', stringsAsFactors=FALSE)

  res <- dplyr::left_join(res, methods, by=c("MethodID"="ID"))
  res <- dplyr::left_join(res, characteristics, by=c("CharacteristicID"="ID"))
  res <- dplyr::left_join(res, sample_fractions, by=c("SampleFractionID"="ID"))
  res <- dplyr::left_join(res, result_flags, by=c("FlagID"="ID"))

  # replace all empty strings with NA (WQX does not like "")
  for (name in names(res)) {
    if (is.character(res[[name]])) {
      idx <- which(stringr::str_trim(res[[name]]) == "")
      res[idx, name] <- NA
    }
  }

  res <- dplyr::mutate(res,
                       ActivityID=paste(LocationID,
                                        strftime(Datetime, "%Y%m%d%H%M"),
                                        stringr::str_sub(LabOrField, 1, 1),
                                        stringr::str_trim(SampleTypeID),
                                        sep=":"),
                       ActivityMediaName="Water",
                       ActivityStartDate=format(Datetime, "%Y-%m-%d"),
                       ActivityStartTime=format(Datetime, "%H:%M:%S"),
                       ActivityStartTimeZone=lubridate::tz(Datetime),
                       FlagID=stringr::str_to_upper(stringr::str_trim(FlagID)),
                       ResultStatusID="Final",
                       ResultValueType=ifelse(CharacteristicID=="DO_SAT",
                                              "Calculated", "Actual"),
                       SampleTypeID=stringr::str_trim(SampleTypeID))

  # assign activity type
  res <- dplyr::mutate(res, ActivityType=paste(LabOrField, SampleTypeID, sep="_"))
  stopifnot(all(!is.na(res$LabOrField)))
  stopifnot(all(!is.na(res$SampleTypeID)))
  activity_types <- c("Field_FD"="Quality Control Field Replicate Msr/Obs",
                      "Field_S"="Field Msr/Obs",
                      "Lab_FD"="Quality Control Sample-Field Replicate",
                      "Lab_S"="Sample-Routine")
  res <- dplyr::mutate(res,
                       ActivityType=plyr::revalue(ActivityType, activity_types))

  # assign sample collection method
  idx <- which(res$ActivityType %in% c("Quality Control Sample-Field Replicate",
                                       "Sample-Routine"))
  res$SampleCollectionMethod <- NA_character_
  res$SampleCollectionEquipmentName <- NA_character_
  res[idx, "SampleCollectionMethod"] <- "grab"
  res[idx, "SampleCollectionEquipmentName"] <- "Water Bottle"

  # assign detection qualifiers
  detection_conditions <- c("<"="Present Below Quantification Limit",
                            ">"="Present Above Quantification Limit")
  detection_quantitation_types <- c("<"="Lower Quantitation Limit",
                                    ">"="Upper Quantitation Limit")
  res <- dplyr::mutate(res,
     ResultDetectionCondition=plyr::revalue(Qualifier, detection_conditions),
     ResultDetectionQuantitationLimitMeasure=ifelse(Qualifier %in% c("<", ">"),
                                                    ResultValue, NA),
     ResultDetectionQuantitationLimitUnit=ifelse(Qualifier %in% c("<", ">"),
                                                 Units, NA),
     ResultDetectionQuantitationLimitType=plyr::revalue(Qualifier, detection_quantitation_types),
     ResultValue=ifelse(!is.na(ResultDetectionCondition), NA, ResultValue))

  # set detection condition to Not Reported for any remaining missing values
  idx <- which(is.na(res$ResultValue) & is.na(res$ResultDetectionCondition))
  res[idx, 'ResultDetectionCondition'] <- 'Not Reported'

  # convert units to WQX domain
  units <- c('CFU/100ml'='cfu/100ml',
             'mg/l'='mg/l',
             'deg C'='deg C',
             '%'='%',
             'uS/cm'='uS/cm',
             'MPN/100ml'='MPN/100ml',
             'ppt'='ppth',
             'NTU'='NTU')
  res <- dplyr::mutate(res, ResultUnit=plyr::revalue(Units, units))

  # set pH units to None
  idx <- which(res$CharacteristicName=='pH')
  res[idx, 'ResultUnit'] <- 'None'

  res <- dplyr::mutate(res,
                       DbCharacteristicName=CharacteristicName,
                       CharacteristicName=WQX_Name)

  res <- dplyr::rename(res,
                       MonitoringLocationID=LocationID,
                       MethodSpeciation=Speciation,
                       ResultMeasureQualifier=WQX_FlagCode, # map flag
                       ResultSampleFraction=SampleFractionName,
                       ResultAnalyticalMethodID=WQX_MethodID,
                       ResultAnalyticalMethodContext=WQX_ContextCode)

  res <- dplyr::select(res,
                       ProjectID,
                       MonitoringLocationID,
                       ActivityID,
                       ActivityType,
                       ActivityMediaName,
                       ActivityStartDate,
                       ActivityStartTime,
                       ActivityStartTimeZone,
                       CharacteristicName,
                       MethodSpeciation,
                       SampleCollectionMethod,
                       SampleCollectionEquipmentName,
                       ResultDetectionCondition,
                       ResultValue,
                       ResultUnit,
                       ResultMeasureQualifier,
                       ResultSampleFraction,
                       ResultStatusID,
                       ResultValueType,
                       ResultAnalyticalMethodID,
                       ResultAnalyticalMethodContext,
                       ResultDetectionQuantitationLimitType,
                       ResultDetectionQuantitationLimitMeasure,
                       ResultDetectionQuantitationLimitUnit,
                       ResultComment)

  return(res)
}

#' Validate Results Table
#'
#' Checks for missing columns, missing values, and erroneous values not in
#' WQX domain. If an error occurs, a warning message is shown.
#'
#' @param x Dataframe of results table
#' @export
#' @return nothing
#'
wqx_validate_results <- function(x) {
  data(wqx_domain, envir=environment())

  failed <- FALSE

  cat('Checking results for missing columns...')
  required_columns <- c('ProjectID', 'MonitoringLocationID', 'ActivityID',
                        'ActivityType', 'ActivityMediaName', 'ActivityStartDate',
                        'ActivityStartTime', 'ActivityStartTimeZone',
                        'CharacteristicName', 'ResultStatusID', 'ResultValueType')
  missing_columns <- setdiff(required_columns, names(x))
  if (length(missing_columns) > 0) {
    warning(paste0('Missing column(s) in results: ',
                   paste0(paste0('"', missing_columns, '"'), collapse=", ")))
    failed <- TRUE
  }
  cat('OK\n')

  cat('Checking results for columns with missing values...')
  for (name in required_columns) {
    if (any(is.na(x[, name]))) {
      warning(paste0('Missing value in column not allowed: ', name))
      failed <- TRUE
    }
  }
  cat('OK\n')

  cat('Checking results have units when value is not empty...')
  idx <- which(!is.na(x[['ResultValue']]))
  if (any(is.na(x[idx, 'ResultUnit']))) {
    cat('FAIL\n')
    warning('Missing value(s) in ResultUnit when ResultValue is not empty')
    failed <- TRUE
  } else {
    cat('OK\n')
  }

  cat('Checking results have detection condition when value is empty...')
  idx <- which(is.na(x[['ResultValue']]))
  if (any(is.na(x[idx, 'ResultDetectionCondition']))) {
    cat('FAIL\n')
    warning('Missing value(s) in ResultDetectionCondition when ResultValue is empty')
    failed <- TRUE
  } else {
    cat('OK\n')
  }

  cat('Checking results against WQX domain values\n')

  column_mapping <- c("ActivityType"="Activity Type",
                      "ActivityMediaName"="Activity Media Name",
                      "SampleCollectionEquipmentName"="Sample Collection Equipment Name",
                      "CharacteristicName"="Characteristic Name",
                      "MethodSpeciation"="Method Speciation",
                      "ResultMeasureQualifier"="Result Measure Qualifier",
                      "ResultDetectionCondition"="Result Detection Condition",
                      "ResultSampleFraction"="Result Sample Fraction",
                      "ResultStatusID"="Result Status ID",
                      "ResultValueType"="Result Value Type",
                      "ResultDetectionQuantitationLimitType"="Result Detection Quantitation Limit Type",
                      "ResultUnit"="Units",
                      "ActivityStartTimeZone"="Time Zone Code")
  for (name in names(column_mapping)) {
    cat('  ', name, '...')
    x_values <- unique(x[[name]])
    if (is.null(x_values)) stop(name)

    x_values <- x_values[!is.na(x_values)]
    x_values <- x_values[!(x_values == "")]

    wqx_name <- column_mapping[[name]]
    wqx_values <- wqx_domain[[wqx_name]][, 1]

    invalid <- setdiff(x_values, wqx_values)

    if (length(invalid) > 0) {
      cat('FAIL\n')
      warning(paste0('Invalid value(s) in results column ', name,
                     ': ', paste(paste0('"', invalid, '"'), collapse=', ')))
      failed <- TRUE
    } else {
      cat('OK\n')
    }
  }

  cat('Checking results for invalid methods...')
  x_methods <- dplyr::select(x,
                             ResultAnalyticalMethodID,
                             ResultAnalyticalMethodContext)
  x_methods <- unique(x_methods)
  x_methods <- dplyr::mutate(x_methods,
                             MethodContextAndID = paste(ResultAnalyticalMethodContext,
                                                        ResultAnalyticalMethodID,
                                                        sep="~"))
  wqx_methods <- wqx_domain$`Result Analytical Method ID`
  wqx_methods <- dplyr::mutate(wqx_methods,
                               MethodContextAndID = paste(MethodContext,
                                                          MethodID,
                                                          sep="~"))
  invalid_methods <- setdiff(x_methods$MethodContextAndID,
                             wqx_methods$MethodContextAndID)

  if (length(invalid_methods) > 0) {
    cat('FAIL\n')
    warning(paste0('Invalid method Context~ID pairs: ',
                   paste(paste0('"', invalid_methods, '"'), collapse=', ')))
    failed <- TRUE
  } else {
    cat('OK\n')
  }

  if (failed) {
    cat('\nValidation Failed\n')
  } else {
    cat('\nValidation Complete (OK)\n')
  }
}



#' Validate Locations Table
#'
#' Checks for missing columns, missing values, and erroneous values not in
#' WQX domain. If an error occurs, a warning message is shown.
#'
#' @param x Dataframe of locations table
#' @export
#' @return nothing
#'
wqx_validate_locations <- function(x) {
  data(wqx_domain, envir=environment())

  failed <- FALSE

  cat('Checking locations for missing columns...')

  required_columns <- c('MonitoringLocationID',
                        'MonitoringLocationName',
                        'MonitoringLocationType',
                        'MonitoringLocationLatitude',
                        'MonitoringLocationLongitude',
                        'MonitoringLocationHorizontalCollectionMethod',
                        'MonitoringLocationHorizontalCoordinateReferenceSystem',
                        'MonitoringLocationCountyCode',
                        'MonitoringLocationState')
  missing_columns <- setdiff(required_columns, names(x))
  if (length(missing_columns) > 0) {
    warning(paste0('Missing column(s) in locations: ',
                   paste0(paste0('"', missing_columns, '"'), collapse=", ")))
    failed <- TRUE
  }
  cat('OK\n')

  cat('Checking locations for columns with missing values...')
  for (name in required_columns) {
    if (any(is.na(x[, name]))) {
      warning(paste0('Missing value in column not allowed: ', name))
      failed <- TRUE
    }
  }
  cat('OK\n')

  cat('Checking locations against WQX domain values\n')

  column_mapping <- c("MonitoringLocationType"="Monitoring Location Type",
                      "MonitoringLocationHorizontalCollectionMethod"="Monitoring Location Horizontal Collection Method",
                      "MonitoringLocationHorizontalCoordinateReferenceSystem"="Monitoring Location Horizontal Coordinate Reference System")
  for (name in names(column_mapping)) {
    cat('  ', name, '...')
    x_values <- unique(x[[name]])
    if (is.null(x_values)) stop(name)

    x_values <- x_values[!is.na(x_values)]
    x_values <- x_values[!(x_values == "")]

    wqx_name <- column_mapping[[name]]
    wqx_values <- wqx_domain[[wqx_name]][, 1]

    invalid <- setdiff(x_values, wqx_values)

    if (length(invalid) > 0) {
      cat('FAIL\n')
      warning(paste0('Invalid value(s) in locations column ', name,
                     ': ', paste(paste0('"', invalid, '"'), collapse=', ')))
      failed <- TRUE
    } else {
      cat('OK\n')
    }
  }

  if (failed) {
    cat('\nValidation Failed\n')
  } else {
    cat('\nValidation Complete (OK)\n')
  }
}
