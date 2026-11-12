#### an Opal table's dictionary: variable labels, category labels, missing codes
#' @title Read an Opal table's dictionary
#' @description Reads the variable labels and categories of one Opal table through the Opal connection behind a DataSHIELD login.
#' @details Uses opalr::opal.table_dictionary_get() with the opal object of a DSOpal connection; this needs the "view dictionary" permission on the table, not data access. Nothing is changed on the server. Labels are taken in the given locale, else without a locale. If the connection isn't an Opal one, the table is unknown, or the dictionary can't be read, NULL is returned and the caller falls back to other sources.
#' @param connection One DSI::DSConnection-class object of the login (an OpalConnection for this to work).
#' @param table The table as "project.table", e.g. "CNSIM.CNSIM1"; NA if unknown.
#' @param locale Language of the labels to use, e.g. "en".
#' @return NULL, or a list with labels (named character vector, variable name to label) and categories (data frame: variable, name, label, missing).
internal_opal_dictionary <- function(connection, table, locale = "en"){

  if (!methods::is(connection, "OpalConnection") || is.na(table) || !grepl(".", table, fixed = TRUE) ||
      !requireNamespace("opalr", quietly = TRUE)) {
    return(NULL)
  }
  project <- sub("\\..*$", "", table)
  table_name <- sub("^[^.]*\\.", "", table)
  dictionary <- tryCatch(opalr::opal.table_dictionary_get(connection@opal, project, table_name),
                         error = function(e) NULL)
  if (is.null(dictionary) || is.null(dictionary$variables)) {
    return(NULL)
  }

  pick_label <- function(frame){
    for (column in c(paste0("label:", locale), "label")) {
      if (column %in% names(frame)) {
        return(ifelse(is.na(frame[[column]]), "", as.character(frame[[column]])))
      }
    }
    rep("", nrow(frame))
  }

  variables <- dictionary$variables
  labels <- stats::setNames(pick_label(variables), variables$name)

  categories <- data.frame(variable = character(0), name = character(0), label = character(0), missing = logical(0))
  if (!is.null(dictionary$categories) && nrow(dictionary$categories) > 0) {
    cats <- dictionary$categories
    categories <- data.frame(variable = as.character(cats$variable),
                             name = as.character(cats$name),
                             label = pick_label(cats),
                             missing = if ("missing" %in% names(cats)) as.logical(cats$missing) else FALSE)
  }

  list(labels = labels, categories = categories)
}
