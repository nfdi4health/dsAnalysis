#### the lines of config/variables.yml, compact and readable
#' @title Format variable entries as YAML lines
#' @description Turns the entries of initVariables() into the lines of config/variables.yml, with short lists on one line (e.g. range: [4.1, 8.11]) so the file stays easy to read and edit.
#' @details Texts are written in double quotes (with R's escapes, which YAML reads the same way), numbers as they are. Fields an analyst added to an entry are written with yaml::as.yaml() below the known ones. Writes nothing itself.
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
