#### the server packages in include=c(...) of step 4
#' @title Extract included package names from DSLite setup code
#' @description Finds the DSLite configuration call within a vector of R source lines and extracts the package names listed in its `include` argument.
#' @details This is a local, pure text-parsing helper that works on R code supplied as a character vector; it reads no files and has no side effects on the analyst's machine. It relies on `internal_dslite_config_lines()` to locate the relevant configuration lines within `codelines`, then uses regular expressions to pull out the quoted package names from the `include = c(...)` argument.
#' @param codelines A character vector of R source code lines (e.g. read from a local script file) in which to search for the DSLite configuration call.
#' @return A character vector of package names found inside the `include = c(...)` argument of the DSLite configuration code; returns `character(0)` if no `include` argument is present.
internal_dslite_included_packages <- function(codelines){

  config_code <- paste(codelines[internal_dslite_config_lines(codelines)], collapse = "")
  include_code <- stringr::str_match(config_code, "include\\s*=\\s*c\\(([^)]*)\\)")[1, 2]

  if(is.na(include_code)){
    return(character(0))
  }

  stringr::str_match_all(include_code, "\"([^\"]+)\"")[[1]][, 2]
}
