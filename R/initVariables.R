#' @title Describe the servers' variables in config/variables.yml
#' @description Reads which variables the studies hold, their types, categories, ranges and labels, and writes them to config/variables.yml, the variable list the analysis plan builds on.
#' @details Everything comes from aggregates and metadata, never from individual data:
#' \itemize{
#'   \item which study has which variable: server function called: `ds.colnames`, without the studies where all its values are missing (checked via `ds.length` and `ds.numNA`); a variable missing everywhere is left out and named in a message;
#'   \item the type: `ds.class` (factor = categorical, integer, numeric = continuous); variables of other types (e.g. text) are left out and named in a message;
#'   \item categories: `ds.levels`; ranges: the pooled 5\% and 95\% quantiles from `ds.quantileMean`, not the minimum and maximum, which could identify a person;
#'   \item labels, category labels and missing-value codes: the Opal dictionary of each study's table (opalr, needs the "view dictionary" permission; the table is read from R/01_DS_Login.R or given in tables); where it can't be read, the variable label from `ds.metadata`; category labels then stay empty.
#' }
#' If config/variables.yml exists, labels you filled in and fields you added are kept; variables no longer on any server are dropped, with a message. Comments in the file are not kept: it gets a new header on every run.
#' The file's content (names, labels, categories, ranges, which study has what) is sent to the Claude API by the datashield-analysis-suggest workflow, together with the analysis plan.
#' @param df String naming the data frame as it exists on each server (e.g. "D"), not a local R data frame; used to build `df$variable` expressions passed to the server; defaults to "D".
#' @param datasources A list of DSI::DSConnection-class objects obtained after login; if NULL (the default), the connections found by DSI::datashield.connections_find() are used.
#' @param tables Optional named character vector, server name to Opal table ("project.table"), for reading the dictionaries; by default read from R/01_DS_Login.R.
#' @param locale Language of the labels, e.g. "en"; defaults to "en".
#' @param file Path of the file to write, relative to the project; defaults to "config/variables.yml".
#' @return Invisibly returns the path of the written file; the variables are written to it as a side effect.
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @importFrom DSI datashield.connections_find
#' @importFrom methods is
#' @examples
#' \dontrun{
#' # after logging in, in an analysis project made with initProject()
#' initVariables(df = "D", datasources = conns)
#' }
#' @export
initVariables <- function(df = "D", datasources = NULL, tables = NULL, locale = "en", file = "config/variables.yml"){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!(is.list(datasources) && length(datasources) > 0 &&
        all(vapply(datasources, function(d) methods::is(d, "DSConnection"), logical(1))))) {
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }
  servers <- names(datasources)

  columns <- dsBaseClient::ds.colnames(df, datasources = datasources)
  names_all <- unique(unlist(columns))

  if (is.null(tables)) {
    tables <- internal_login_tables()
  }
  dictionaries <- lapply(stats::setNames(servers, servers), function(s){
    internal_opal_dictionary(datasources[[s]], if (s %in% names(tables)) tables[[s]] else NA_character_, locale)
  })
  unread <- servers[vapply(dictionaries, is.null, logical(1))]

  skipped <- character(0)
  empty <- character(0)
  entries <- list()
  for (name in names_all) {
    studies <- servers[vapply(servers, function(s) name %in% columns[[s]], logical(1))]
    x <- paste0(df, "$", name)
    #### a study whose values are all missing doesn't count as having the variable
    studies <- internal_studies_with_data(x, datasources[studies])
    if (length(studies) == 0) {
      empty <- c(empty, name)
      next
    }
    facts <- internal_variable_facts(x, datasources[studies])
    if (facts$type == "other") {
      skipped <- c(skipped, name)
      next
    }

    known <- Filter(Negate(is.null), dictionaries[studies])
    label <- ""
    for (d in known) {
      if (name %in% names(d$labels) && nzchar(d$labels[[name]])) {
        label <- d$labels[[name]]
        break
      }
    }
    if (!nzchar(label)) {
      label <- internal_metadata_label(x, datasources[studies])
    }

    entry <- list(name = name, label = label, type = facts$type)
    if (facts$type == "categorical") {
      categories <- do.call(rbind, lapply(known, function(d) d$categories[d$categories$variable == name, ]))
      missing_codes <- if (is.null(categories)) character(0) else unique(categories$name[categories$missing])
      codes <- setdiff(facts$levels, missing_codes)
      entry$categories <- stats::setNames(lapply(codes, function(code){
        hit <- if (is.null(categories)) character(0) else categories$label[categories$name == code & nzchar(categories$label)]
        if (length(hit) > 0) hit[[1]] else ""
      }), codes)
      if (length(missing_codes) > 0) {
        entry$missing_codes <- as.list(missing_codes)
      }
    } else if (!is.null(facts$range)) {
      entry$range <- as.list(facts$range)
    }
    entry$studies <- as.list(studies)
    entries[[length(entries) + 1]] <- entry
  }

  path <- here::here(file)
  old <- if (file.exists(path)) yaml::read_yaml(path)$variables else NULL
  merged <- internal_variables_merge(entries, old)

  header <- c(
    paste0("# Written by dsAnalysis::initVariables() on ", format(Sys.Date()), " from the servers' metadata."),
    "# The analysis plan (config/analysis-plan.yml) builds on these variables. Its",
    "# content and this file's (names, labels, categories, ranges, which study has",
    "# what) are sent to the Claude API by the datashield-analysis-suggest workflow.",
    "# Fill in empty labels freely: a re-run keeps them, but not comments.",
    "# range: the pooled 5% and 95% quantiles, not the minimum and maximum.",
    ""
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(header, internal_variables_yaml(merged$variables)), path)

  message(sprintf("Wrote %d variables to %s.", length(merged$variables), file))
  if (length(empty) > 0) {
    message("Left out (all values missing in every study): ", paste(empty, collapse = ", "))
  }
  if (length(skipped) > 0) {
    message("Left out (not numeric, integer or factor): ", paste(skipped, collapse = ", "))
  }
  if (length(merged$dropped) > 0) {
    message("No longer on any server, removed: ", paste(merged$dropped, collapse = ", "))
  }
  if (length(unread) > 0) {
    message("No Opal dictionary read for ", paste(unread, collapse = ", "),
            "; labels came from the data's attributes, category labels are empty.")
  }

  invisible(path)
}
