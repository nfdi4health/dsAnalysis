#### which studies have at least one non-missing value of a variable
#' @title Find the studies that have data for a variable
#' @description Returns the names of the datasources in which a server-side variable has at least one non-missing value.
#' @details Calls dsBaseClient::ds.length() (type "split") and dsBaseClient::ds.numNA() on the given datasources; both return one number per server, in the order of the datasources. A study whose values are all missing counts as not having the variable: its quantiles would be NA and turn the pooled range into NA, and an analysis can't use the variable there. If either call fails (e.g. a disclosure control refusal, caught via tryCatch), all given studies are returned unchanged.
#' @param x The variable on the servers, e.g. "D$LAB_TSC".
#' @param datasources A named list of DSConnection-class objects: the studies whose data frame has the column.
#' @return A character vector of server names, a subset of names(datasources).
#' @noRd
internal_studies_with_data <- function(x, datasources){

  servers <- names(datasources)
  lengths <- tryCatch(unlist(dsBaseClient::ds.length(x, type = "split", datasources = datasources)),
                      error = function(e) NULL)
  missing <- tryCatch(unlist(dsBaseClient::ds.numNA(x, datasources = datasources)),
                      error = function(e) NULL)
  if (length(lengths) != length(servers) || length(missing) != length(servers)) {
    return(servers)
  }
  servers[unname(lengths) > unname(missing)]

}
