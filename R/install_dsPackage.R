#' @title Installs DataSHIELD packages and adds them to the project
#' @description Installs one or more DataSHIELD server-side packages, optionally with a matching client package, into the current renv project and registers them with the local DSLite setup.
#' @details Resolves package specifications with the internal resolve_dsPackage() helper and installs them with renv::install() into the project found by here::here(), updating that project's renv library and lockfile. Also updates the local DSLite setup via add_dsPackage() and appends the new dependencies to dependencies.R via an internal renv_record() helper. When dsPackage has length greater than one, the function calls itself once per name with no version/ref/source/client options, since those only make sense for a single package.
#' @param dsPackage Character vector giving the name(s) of the DataSHIELD server-side package(s) to install; if more than one name is given, version/ref/source/client arguments must be left NULL.
#' @param version Single character string giving the CRAN/release version of the server package to install; only usable when dsPackage has length one.
#' @param ref Single character string naming a GitHub branch, tag or commit of the server package to install; only usable when dsPackage has length one.
#' @param source Single character string giving the GitHub repository of the server package in "owner/repo" form; only usable when dsPackage has length one.
#' @param client Single character string giving the name of the client-side R package to install alongside the server package; only usable when dsPackage has length one.
#' @param client_version Single character string giving the version of the client package to install; only usable when dsPackage has length one.
#' @param client_source Single character string giving the GitHub repository of the client package in "owner/repo" form; only usable when dsPackage has length one.
#' @return Returns invisibly: the character vector of package names supplied, when multiple packages were installed in a loop, or the list of resolved package specifications (server and, if given, client) for a single-package call. As a side effect it installs packages into the current renv project's library (updating its lockfile), updates the local DSLite setup, and appends records to dependencies.R in the project.
#' @examples
#' \dontrun{
#' proj <- tempdir()
#' old_wd <- setwd(proj)
#' on.exit(setwd(old_wd), add = TRUE)
#' 
#' ## Not run because it installs packages and requires network access
#' ## install_dsPackage("dsBase")
#' }
#' @export

install_dsPackage <- function(dsPackage = NULL, version = NULL, ref = NULL, source = NULL,
                              client = NULL, client_version = NULL, client_source = NULL){

  if(is.null(dsPackage)){
    stop("No package name has been given.", call. = FALSE)
  }

  options_given <- !all(vapply(list(version, ref, source, client, client_version, client_source), is.null, TRUE))

  if(length(dsPackage) > 1){

    if(options_given){
      stop("Versions, refs, sources and clients can only be given for one package at a time.", call. = FALSE)
    }

    for (p in dsPackage){
      install_dsPackage(p)
    }

    return(invisible(dsPackage))
  }

  packages <- internal_resolve_dsPackage(dsPackage, version = version, ref = ref, source = source,
                                         client = client, client_version = client_version,
                                         client_source = client_source)

  project <- here::here()
  specs <- c(packages$server$spec, packages$client$spec)

  message("Installing ", paste(specs, collapse = " and "), " ...")
  renv::install(specs, project = project, prompt = FALSE)

  client_name <- if (is.null(packages$client)) NA_character_ else packages$client$package
  add_dsPackage(packages$server$package, client = client_name)

  internal_renv_record(project)

  message("Done: ", paste(c(packages$server$package, stats::na.omit(client_name)), collapse = " and "),
          " installed, added to the DSLite setup and to dependencies.R.")

  invisible(packages)

}
