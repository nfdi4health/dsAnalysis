#### which studies have at least one non-missing value of a variable
#' @title Find the studies that have data for a variable
#' @description Returns the names of the datasources in which a server-side variable has at least one non-missing value.
#' @details Calls `dsBaseClient::ds.length()` (type "split") and `dsBaseClient::ds.numNA()` on the given datasources, each returning one count per server in the order of datasources. A server is kept only if its non-missing count (length minus NA count) is greater than zero; if the lengths of either result don't match the number of datasources (e.g. because a call errored, caught via tryCatch, or returned something unexpected), all given studies are returned unchanged.
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
