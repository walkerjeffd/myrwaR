#' myrwaR: R Tools for the Mystic River Watershed Association
#'
#' The myrwaR package contains sets of functions for loading data from the MyRWA
#' Access database, and for analyzing hydrologic and water quality data. The package
#' also includes \code{rmarkdown} templates for automated report generation.
#'
#' @section Database Functions:
#' \itemize{
#'   \item{\code{\link{db_connect}}}{: Creates a connection handler for the Access database}
#'   \item{\code{\link{db_results}}}{: Fetches water quality results and joins them with the visit table}
#'   \item{\code{\link{db_table}}}{: Fetches all rows for a single table}
#'   \item{\code{\link{db_list_tables}}}{: Lists all tables in the database}
#'   \item{\code{\link{db_table_fields}}}{: Lists all column names for a single table}
#'   \item{\code{\link{db_locations}}}{: Fetches the locations joined with water body and location type information}
#'   \item{\code{\link{load_wq}}}{: Generates a dataframe of water quality Results merged with both the Visit and Location tables}
#' }
#'
#' @section Database QAQC Functions:
#' \itemize{
#'   \item{\code{\link{compare_database}}}{: Compare two database versions and summarize differences between them}
#'   \item{\code{\link{qaqc_table_results}}}{: Runs QAQC tests on Results table}
#' }
#'
#' @section Precipitation Functions:
#' \itemize{
#'   \item{\code{\link{load_precip_from_xls}}}{: Loads an hourly precipitation dataset from the LoganPrecip.xls Excel file}
#'   \item{\code{\link{load_precip_from_usgs}}}{: Retrieves hourly precipitation from USGS NWIS}
#'   \item{\code{\link{antecedent_precip}}}{: Computes hourly antecedent precipitation (e.g. 48-hour total precipitation)}
#'   \item{\code{\link{append_weather}}}{: Appends antecedent precipitation and weather condition (Dry/Wet) to a water quality data frame}
#'   \item{\code{\link{assign_precip_events}}}{: Delineates an hourly precipitation timeseries into discrete dry and wet events}
#'   \item{\code{\link{precip_event_summary}}}{: Creates a summary table of precipitation events (e.g. duration, total depth, peak rate, etc.)}
#' }
#'
#' @section WQX Export:
#' \itemize{
#'   \item{\code{\link{wqx_projects}}}{: Convert Projects to WQX Format}
#'   \item{\code{\link{wqx_locations}}}{: Convert Locations to WQX Format}
#'   \item{\code{\link{wqx_results}}}{: Convert Results to WQX Format}
#'   \item{\code{\link{wqx_validate_locations}}}{: Validate converted Locations table against WQX requirements}
#'   \item{\code{\link{wqx_validate_results}}}{: Validate converted Results table against WQX requirements}
#' }
#'
#' @section Report Templates:
#' \itemize{
#'   \item{\code{\link{hotspot_report}}}{: Hotspot Report Template}
#' }
#'
#' @section Miscellaneous:
#' \itemize{
#'   \item{\code{\link{water_year}}}{: Returns water year of specified date(s)}
#' }
#'
#' @docType package
#' @name myrwaR
NULL
