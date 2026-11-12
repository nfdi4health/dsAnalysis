#### a variable's label from its server-side attributes
#' @title Read a variable's label through DataSHIELD
#' @description Returns the label a server attached to a variable, as dsBaseClient's ds.metadata() reports it.
#' @details This function calls dsBaseClient::ds.metadata() to retrieve metadata (including any label) for the variable from each datasource, wrapped in tryCatch so errors yield NULL instead of stopping execution. Any Opal-style language prefix, e.g. "(en) ", is stripped locally in R from each returned label. Only non-disclosive attributes are returned by the server function ds.metadata(); label extraction and cleanup happen client-side after that call.
#' @param x The variable on the servers, e.g. "D$LAB_TSC".
#' @param datasources A list of DSI::DSConnection-class objects to ask; the first non-empty label is used.
#' @return A character string, empty if no study has a label.
internal_metadata_label <- function(x, datasources){

  metadata <- tryCatch(dsBaseClient::ds.metadata(x, datasources = datasources), error = function(e) NULL)
  labels <- unlist(lapply(metadata, function(m) m$label))
  labels <- trimws(sub("^\\([A-Za-z_-]+\\)\\s*", "", labels))
  labels <- labels[nzchar(labels)]

  if (length(labels) == 0) "" else labels[[1]]
}
