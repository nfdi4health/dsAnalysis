#### the lines of config/variables.yml, compact and readable
#' @title Format variable entries as YAML lines
#' @description Turns the entries of initVariables() into the lines of config/variables.yml, with short lists on one line (e.g. range: [4.1, 8.11]) so the file stays easy to read and edit.
#' @details Strings are quoted with escapes (R's escaping matches how YAML reads them); numbers are written as formatted plain values. Known fields (name, label, type, categories, missing_codes, range, studies) are written first in a fixed order; categories are written as one "code": "label" line per entry; missing_codes, range and studies are written as flow-style lists (e.g. [4.1, 8.11]), with studies always included even if empty. Any other fields present on the entry are appended below via yaml::as.yaml(). This is a local formatting helper: it builds and returns lines of YAML text and does not contact a DataSHIELD server.
#' @param variables A list of variable entries (name, label, type, categories, missing_codes, range, studies, and any other fields).
#' @return A character vector of lines, starting with "variables:".
internal_variables_yaml <- function(variables){

  quote <- function(x) encodeString(as.character(x), quote = "\"")
  flow <- function(x, numeric = FALSE){
    values <- unlist(x)
    paste0("[", paste(if (numeric) format(values, trim = TRUE, drop0trailing = TRUE) else quote(values), collapse = ", "), "]")
  }
  known <- c("name", "label", "type", "categories", "missing_codes", "range", "studies")

  lines <- "variables:"
  for (v in variables) {
    lines <- c(lines,
               paste0("  - name: ", quote(v$name)),
               paste0("    label: ", quote(if (is.null(v$label)) "" else v$label)),
               paste0("    type: ", v$type))
    if (length(v$categories) > 0) {
      lines <- c(lines, "    categories:",
                 paste0("      ", quote(names(v$categories)), ": ", quote(unlist(v$categories))))
    }
    if (length(v$missing_codes) > 0) {
      lines <- c(lines, paste0("    missing_codes: ", flow(v$missing_codes)))
    }
    if (length(v$range) > 0) {
      lines <- c(lines, paste0("    range: ", flow(v$range, numeric = TRUE)))
    }
    lines <- c(lines, paste0("    studies: ", flow(v$studies)))
    extra <- v[setdiff(names(v), known)]
    if (length(extra) > 0) {
      lines <- c(lines, paste0("    ", strsplit(trimws(yaml::as.yaml(extra), "right"), "\n")[[1]]))
    }
  }
  lines
}
