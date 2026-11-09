#### the catalogue's name of a package, ignoring case; NA if it isn't listed
#' @title Find canonical catalogue name ignoring case
#' @description Looks up a package name in a catalogue's names, matching case-insensitively, and returns the catalogue's stored spelling.
#' @param package A character string giving the package name to look up, compared case-insensitively; this is a plain local R value, not a server-side object.
#' @param catalogue A named list or vector (e.g. a package catalogue) whose names are searched for a case-insensitive match to `package`.
#' @return A single character string with the matching name as spelled in `names(catalogue)`, or `NA_character_` if no case-insensitive match is found.
internal_catalogue_name <- function(package, catalogue){

  hit <- match(tolower(package), tolower(names(catalogue)))
  if(is.na(hit)) NA_character_ else names(catalogue)[hit]
}
