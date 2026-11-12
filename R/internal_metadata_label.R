#### a variable's label from its server-side attributes
#' @title Read a variable's label through DataSHIELD
#' @description Returns the label a server attached to a variable, as dsBaseClient's ds.metadata() reports it.
#' @details Opal attaches its dictionary's labels to the variables it assigns, in the form "(en) Label"; the language prefix is removed. Used when an Opal dictionary can't be read, and for other servers (DSLite, Armadillo), where there may be no label at all. Only non-disclosive attributes are returned by the server function.
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
