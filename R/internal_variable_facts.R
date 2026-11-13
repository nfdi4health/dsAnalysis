#### type, categories or range of one server-side variable, from aggregates only
#' @title Describe one server-side variable
#' @description Works out a variable's type and its categories or range from DataSHIELD aggregates, for the studies that have it.
#' @details Calls dsBaseClient::ds.class() to determine the variable's class on each server, then (depending on the result) dsBaseClient::ds.levels() for categorical variables or dsBaseClient::ds.quantileMean(type = "combine") for integer/continuous variables, on the given connections only. Server function called: `ds.class`, `ds.levels`, `ds.quantileMean`. The range is the pooled 5% and 95% quantile, not the minimum and maximum, which could identify a person; it is rounded (whole numbers for integers, two decimals otherwise). Any of these calls failing (e.g. a disclosure control refusal, caught via tryCatch) simply leaves the corresponding part (levels or range) as NULL. Types other than factor, integer and numeric are reported as "other".
#' @param x The variable on the servers, e.g. "D$LAB_TSC".
#' @param datasources A list of DSI::DSConnection-class objects: the studies that have the variable.
#' @return A list with type ("categorical", "integer", "continuous" or "other"), levels (character vector, only when type is "categorical" and the server call succeeded, otherwise NULL) and range (numeric vector of length 2, only when type is "integer" or "continuous" and the quantiles were obtainable, otherwise NULL).
internal_variable_facts <- function(x, datasources){

  classes <- tryCatch(unique(unlist(dsBaseClient::ds.class(x, datasources = datasources))),
                      error = function(e) character(0))
  type <- if ("factor" %in% classes) {
    "categorical"
  } else if (length(classes) > 0 && all(classes == "integer")) {
    "integer"
  } else if (length(classes) > 0 && all(classes %in% c("numeric", "integer"))) {
    "continuous"
  } else {
    "other"
  }

  facts <- list(type = type, levels = NULL, range = NULL)
  if (type == "categorical") {
    levels <- tryCatch(dsBaseClient::ds.levels(x, datasources = datasources), error = function(e) NULL)
    facts$levels <- unique(unlist(lapply(levels, function(l) l$Levels)))
  } else if (type %in% c("integer", "continuous")) {
    #### ds.quantileMean() prints a heading for every call
    quantiles <- NULL
    utils::capture.output(
      quantiles <- tryCatch(dsBaseClient::ds.quantileMean(x, type = "combine", datasources = datasources),
                            error = function(e) NULL))
    if (!is.null(quantiles) && all(c("5%", "95%") %in% names(quantiles)) && !anyNA(quantiles[c("5%", "95%")])) {
      digits <- if (type == "integer") 0 else 2
      facts$range <- round(unname(quantiles[c("5%", "95%")]), digits)
    }
  }
  facts
}
