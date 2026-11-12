#### an Opal table's dictionary: variable labels, category labels, missing codes
#' @title Read an Opal table's dictionary
#' @description Reads the variable labels and categories of one Opal table through the Opal connection behind a DataSHIELD login.
#' @details Calls opalr::opal.table_dictionary_get() directly on the OpalConnection's underlying opal object (connection@opal), splitting table into project and table_name on the first ".". This bypasses DSI's aggregate/assign dispatch and talks to Opal's R client API directly; it requires the "view dictionary" permission, not data access, and changes nothing on the server. Labels are taken for the given locale (column "label:<locale>"), falling back to the unlocalized "label" column if present, else blank. Returns NULL (so callers fall back to other sources) if connection isn't an OpalConnection, table is NA or lacks a ".", the opalr package isn't installed, or the dictionary lookup fails or has no variables.
#' @param connection One DSI::DSConnection-class object of the login (an OpalConnection for this to work).
#' @param table Name of the table on the server as "project.table" (must contain a literal "." separating the two parts), e.g. "CNSIM.CNSIM1"; pass NA if unknown, which makes the function return NULL immediately.
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
