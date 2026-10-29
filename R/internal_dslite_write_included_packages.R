#### rewrites the dslite.server$config(...) call in step 4 with these server packages
#' @title Replace included-package list in DSLite config lines
#' @description Rewrites the `include=c(...)` argument of a `DSLite::defaultDSConfiguration()` call within a vector of R source lines, substituting the given package names.
#' @details This is an internal helper with no file or session side effects; it only manipulates a character vector in memory. It locates the existing `dslite.server$config(DSLite::defaultDSConfiguration(include=c(...)))` block via `internal_dslite_config_lines()`, and replaces it with a newly formatted block listing `packages`, keeping the surrounding lines unchanged. The result is typically written back to a script file by the caller, not by this function itself.
#' @param codelines A character vector of R source code lines (e.g. read from a local script file) that contains a DSLite configuration call to be rewritten; this is a plain local R value, not a server-side object.
#' @param packages A character vector of package names to include in the rewritten `DSLite::defaultDSConfiguration(include=c(...))` call; a plain local R value.
#' @return A character vector of R source code lines, identical to `codelines` except that the `include=c(...)` block of the DSLite configuration call has been replaced with one listing `packages`.
internal_dslite_write_included_packages <- function(codelines, packages){

  config_lines <- internal_dslite_config_lines(codelines)
  config_start <- "dslite.server$config(DSLite::defaultDSConfiguration(include=c("
  indent <- strrep(" ", nchar(config_start))

  quoted <- paste0("\"", packages, "\"")
  config_new <- paste0(c(config_start, rep(indent, length(quoted) - 1)),
                       quoted,
                       c(rep(",", length(quoted) - 1), ")))"))

  c(codelines[seq_len(config_lines[1] - 1)],
    config_new,
    codelines[(config_lines[length(config_lines)] + 1):length(codelines)])
}
