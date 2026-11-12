#### type, categories or range of one server-side variable, from aggregates only
#' @title Describe one server-side variable
#' @description Works out a variable's type and its categories or range from DataSHIELD aggregates, for the studies that have it.
#' @details Calls dsBaseClient's ds.class(), and then ds.levels() for factors or ds.quantileMean(type = "combine") for numbers, on the given connections only. The range is the pooled 5% and 95% quantile, not the minimum and maximum, which could identify a person; it is rounded (whole numbers for integers, two decimals otherwise). A call the servers refuse (e.g. disclosure control) leaves that part out. Types other than factor, integer and numeric are reported as "other".
#' @param x The variable on the servers, e.g. "D$LAB_TSC".
#' @param datasources A list of DSI::DSConnection-class objects: the studies that have the variable.
#' @return A list with type ("categorical", "integer", "continuous" or "other"), levels (character, categorical only) and range (numeric of length 2, or NULL).
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
    if (!is.null(quantiles) && all(c("5%", "95%") %in% names(quantiles))) {
      digits <- if (type == "integer") 0 else 2
      facts$range <- round(unname(quantiles[c("5%", "95%")]), digits)
    }
  }
  facts
}
