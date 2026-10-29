#' @title Removes DataSHIELD packages from the DSLite setup and dependencies.R
#' @description Removes a DataSHIELD server-side package (and its matching client package) from the project's DSLite setup script and dependency records, optionally uninstalling them from the project library.
#' @details Edits utils/setup/01_DSLite_Setup.R on the analyst's machine to drop the package(s) from the DSLite-included packages list and remove matching library() calls, using internal helpers for parsing/writing included packages and library calls. Updates the project's dependencies.R record and refreshes the renv lockfile record via an internal renv-record helper. If uninstall = TRUE, also calls renv::remove() to uninstall the packages from the project library; dsBase can never be removed since every DSLite setup requires it.
#' @param dsPackage Character vector of DataSHIELD server-side package name(s) to remove from the DSLite setup; required (the function stops if NULL), and must not include "dsBase".
#' @param client Character vector of matching client-side package name(s), one per element of dsPackage; if NULL (the default), the client name is taken from dependencies.R if recorded there, otherwise guessed as '<dsPackage>Client'.
#' @param uninstall Logical, defaults to FALSE; if TRUE, also uninstalls the server and client packages from the project's renv library via renv::remove().
#' @return Returns the dsPackage argument invisibly (a character vector); its main effect is updating utils/setup/01_DSLite_Setup.R and dependencies.R in the project, and optionally uninstalling packages from the project library via renv.
#' @examples
#' \dontrun{
#' project_dir <- tempdir()
#' dir.create(file.path(project_dir, "utils/setup"), recursive = TRUE, showWarnings = FALSE)
#' writeLines(c("included_packages <- c('dsMediationClient')", "library(dsMediationClient)"),
#'            file.path(project_dir, "utils/setup/01_DSLite_Setup.R"))
#' old_wd <- getwd()
#' setwd(project_dir)
#' 
#' remove_dsPackage(dsPackage = "dsMediation", client = "dsMediationClient", uninstall = FALSE)
#' 
#' setwd(old_wd)
#' }
#' @export

remove_dsPackage <- function(dsPackage = NULL, client = NULL, uninstall = FALSE){

  if(is.null(dsPackage)){
    stop("No package name has been given.", call. = FALSE)
  }

  if("dsBase" %in% dsPackage){
    stop("dsBase can't be removed: every DataSHIELD setup needs it.", call. = FALSE)
  }

  if(!is.null(client) && !(length(client) == length(dsPackage))){
    stop("Please provide one client package per DataSHIELD package.", call. = FALSE)
  }

  #### the client package: as given, else as recorded in dependencies.R, else <server>Client
  if(is.null(client)){

    client <- paste0(dsPackage, "Client")
    dependencies_file <- here::here("dependencies.R")

    if(file.exists(dependencies_file)){
      recorded <- internal_dependencies_dsPackages(readLines(con = dependencies_file))
      known <- dsPackage %in% recorded$server
      client[known] <- recorded$client[match(dsPackage[known], recorded$server)]
    }

  }

  setup_file <- here::here("utils/setup", "01_DSLite_Setup.R")
  dslite_setup_codelines <- readLines(con = setup_file)
  included <- internal_dslite_included_packages(dslite_setup_codelines)

  for (p in setdiff(dsPackage, included)){
    message(paste0("The DataSHIELD package ", p, " is not included in the DSLite Setup."))
  }

  dslite_setup_codelines <- internal_dslite_write_included_packages(dslite_setup_codelines,
                                                                    setdiff(included, dsPackage))
  dslite_setup_codelines <- internal_dslite_set_library_calls(dslite_setup_codelines,
                                                              remove = client[!is.na(client)])

  writeLines(text = dslite_setup_codelines, con = setup_file)

  internal_dependencies_set_dsPackages(remove = dsPackage)

  #### renv: uninstall if asked, and record that the project no longer uses them
  project <- here::here()

  if(uninstall){
    renv::remove(c(dsPackage, client[!is.na(client)]), project = project)
  }

  internal_renv_record(project)

  invisible(dsPackage)

}
