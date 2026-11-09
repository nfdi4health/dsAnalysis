#### the client package of a server package in the catalogue; NA if there is none
#### (<server>Client, or <server without Base>Client as in dsMTLBase -> dsMTLClient,
#### or <server without Server> as in dsQueryLibraryServer -> dsQueryLibrary)
#' @title Guess the client-side package name from a catalogue
#' @description Derives likely client package names for a given (often server-side) package name and looks them up in a catalogue.
#' @details The function builds candidate client package names by appending "Client" to the package name, appending "Client" after stripping a trailing "Base", and stripping a trailing "Server", then removes any candidate identical to the original package name. Each candidate is checked against the supplied catalogue via internal_catalogue_name, and the first matching hit is returned. This function only performs in-memory string manipulation and lookups; it does not read or write any files, install packages, or contact DataSHIELD servers.
#' @param package Character string: the name of a package (typically a DataSHIELD server-side package name) for which a corresponding client package name should be guessed. This is a plain local R value, not an object on a DataSHIELD server.
#' @param catalogue A local data structure (as used by internal_catalogue_name) listing known packages, against which the candidate client names are matched.
#' @return A single character string giving the first matching client package name found in the catalogue, or NA_character_ if none of the candidate names are found.
internal_catalogue_client <- function(package, catalogue){

  candidates <- unique(c(paste0(package, "Client"),
                         paste0(sub("Base$", "", package), "Client"),
                         sub("Server$", "", package)))
  candidates <- setdiff(candidates, package)

  hits <- vapply(candidates, internal_catalogue_name, "", catalogue = catalogue)
  hits <- hits[!is.na(hits)]

  if(length(hits) == 0) NA_character_ else hits[[1]]
}
