#' @title Register DataSHIELD packages in the DSLite setup script
#' @description Adds one or more DataSHIELD server-side packages (and their client-side counterparts) to the project's DSLite setup script and dependency tracking file.
#' @details Reads and rewrites the local file utils/setup/01_DSLite_Setup.R (located via here::here), using internal helper functions to detect packages already listed, insert library() calls for the client packages, and update the vector of server packages passed to DSLite's configuration. It also calls internal_dependencies_set_dsPackages() to add corresponding library calls to the project's dependencies file so renv tracks both server and client packages. Packages already present in the setup file are skipped with a message and excluded from the writes; nothing is written to either file if all requested packages are already included.
#' @param dsPackage Character vector of names of the DataSHIELD server-side packages to add to the local DSLite instance; required, the function stops with an error if left NULL.
#' @param client Character vector of matching client-side package names, one per element of dsPackage; if NULL (the default), each client name is derived by appending "Client" to the corresponding dsPackage name, and an error is raised if the lengths don't match.
#' @return Returns invisibly NULL if every requested package was already present, otherwise the character vector of newly added dsPackage names (those not already listed). As a side effect it overwrites utils/setup/01_DSLite_Setup.R with updated library calls and server package list, and updates the project's dependencies file via internal_dependencies_set_dsPackages().
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import stringr
#' @examples
#' \dontrun{
#' ## This function modifies utils/setup/01_DSLite_Setup.R in the current
#' ## DataSHIELD analysis project located via here::here(), so it should
#' ## only be run inside such a project.
#' ## Not run in a temporary directory because add_dsPackage always
#' ## targets the project's own setup file rather than an arbitrary path.
#' ## \dontrun{
#' ## add_dsPackage(dsPackage = "dsBase", client = "dsBaseClient")
#' ## }
#' }
#' @export


add_dsPackage <- function(dsPackage = NULL, client = NULL){

  if(is.null(dsPackage)){
    stop("No package name has been given.",call.=FALSE)
  }

  if(is.null(client)){
    client <- paste0(dsPackage, "Client")
  }

  if(!(length(client) == length(dsPackage))){
    stop("Please provide one client package per DataSHIELD package.", call. = FALSE)
  }

  setup_file <- here::here("utils/setup", "01_DSLite_Setup.R")
  dslite_setup_codelines <- readLines(con = setup_file)

  included <- internal_dslite_included_packages(dslite_setup_codelines)

  dupl_int <- dsPackage %in% included
  for (p in dsPackage[dupl_int]){
    message(paste0("The DataSHIELD package ", p, " is already included in the DSLite Setup."))
  }

  #### nothing to add when all packages are already included
  if(all(dupl_int)){
    return(invisible(NULL))
  }

  dsPackage_unique <- dsPackage[!dupl_int]
  client_unique <- client[!dupl_int]

  #### step 1: library calls of the client packages
  dslite_setup_codelines <- internal_dslite_set_library_calls(dslite_setup_codelines,
                                                              add = client_unique[!is.na(client_unique)])

  #### step 4: server packages in the DSLite configuration
  dslite_setup_codelines <- internal_dslite_write_included_packages(dslite_setup_codelines,
                                                                    c(included, dsPackage_unique))

  writeLines(text = dslite_setup_codelines, con = setup_file)

  #### dependencies.R: library calls so that renv tracks server and client packages
  internal_dependencies_set_dsPackages(add = dsPackage_unique, client = client_unique)

  invisible(dsPackage_unique)

}
