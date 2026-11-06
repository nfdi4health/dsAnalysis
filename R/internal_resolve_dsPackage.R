#### what to install for a DataSHIELD package: the server and the client package,
#### each with its name and renv::install() specification (client is NULL if there is none)
#' @title Resolve install specs for a DataSHIELD server/client package pair
#' @description Looks up a DataSHIELD server package (and its matching client package, if any) in the package catalogue and builds the information needed to install each, including source and version.
#' @details Loads the DataSHIELD package catalogue (via internal_ds_catalogue()) to find the canonical name and source of dsPackage and, unless given explicitly, of its matching client package. If dsPackage is not in the catalogue and no source is given, it stops with an error; if no client package can be identified, it emits a message and returns only the server spec. When client_version is not given but version is, it tries to reuse version for the client and falls back to the latest client version with a message if that version is not available for the client. This function has no side effects on disk; it only performs lookups and returns specifications used elsewhere to drive package installation.
#' @param dsPackage Name of the DataSHIELD server-side package to resolve, as known in the DataSHIELD package catalogue or as a plain package name if source is given.
#' @param version Desired released version of the server package; if NULL, the latest version is used.
#' @param ref Desired git branch, tag, or commit of the server package when installing from a repository rather than CRAN; if NULL, the default branch is used.
#' @param source Optional override for where to install the server package from, e.g. "owner/repo" for GitHub; if NULL, the source is looked up in the catalogue.
#' @param client Name of the client package matching dsPackage; if NULL, it is looked up in the catalogue, and if none is found only the server package is resolved.
#' @param client_version Desired released version of the client package; if NULL, it defaults to version (if given and available for the client) or otherwise the latest version.
#' @param client_source Optional override for where to install the client package from, e.g. "owner/repo"; if NULL, it is looked up in the catalogue or inferred from the server package's repository owner.
#' @return A list with elements server and (if found) client, each a list with package (the resolved package name) and spec (the install specification, e.g. source and version/ref, as produced by internal_install_spec); client is NULL if no client package could be identified.
internal_resolve_dsPackage <- function(dsPackage, version = NULL, ref = NULL, source = NULL,
                              client = NULL, client_version = NULL, client_source = NULL){

  catalogue <- internal_ds_catalogue()

  #### server package
  name <- if (is.null(catalogue)) NA_character_ else internal_catalogue_name(dsPackage, catalogue)

  if(!is.null(source)){
    server_src <- list(cran = FALSE, repo = source)
  } else if(!is.na(name)){
    server_src <- internal_catalogue_source(catalogue[[name]])
  } else {
    stop(dsPackage, " is not in the DataSHIELD package catalogue. ",
         "If it is on GitHub, give its repository with source = \"owner/repo\".", call. = FALSE)
  }

  if(is.na(name)){
    name <- dsPackage
  }

  server <- list(package = name, spec = internal_install_spec(name, server_src, version, ref))

  #### client package: as given, else from the catalogue
  if(is.null(client) && !is.null(catalogue)){
    client <- internal_catalogue_client(name, catalogue)
  }

  if(is.null(client) || is.na(client)){
    message("No client package found for ", name, "; only the server package is installed. ",
            "Give client = \"...\" if it has one.")
    return(list(server = server, client = NULL))
  }

  client_name <- if (is.null(catalogue)) NA_character_ else internal_catalogue_name(client, catalogue)

  if(!is.null(client_source)){
    client_src <- list(cran = FALSE, repo = client_source)
  } else if(!is.na(client_name)){
    client_src <- internal_catalogue_source(catalogue[[client_name]])
  } else if(!is.na(server_src$repo)){
    #### same owner as the server package
    client_src <- list(cran = FALSE, repo = paste0(sub("/.*$", "", server_src$repo), "/", client))
  } else {
    stop("No CRAN or GitHub location is known for ", client, ". Give client_source = \"owner/repo\".", call. = FALSE)
  }

  if(is.na(client_name)){
    client_name <- client
  }

  #### the client's version: as given, else the server's version if the client has it, else the latest
  if(is.null(client_version) && !is.null(version)){
    client_spec <- tryCatch(internal_install_spec(client_name, client_src, version),
                            error = function(e) NULL)
    if(is.null(client_spec)){
      message("Version ", version, " was not found for ", client_name, "; its latest version is installed. ",
              "Give client_version = \"...\" to choose one.")
      client_spec <- internal_install_spec(client_name, client_src)
    }
  } else {
    client_spec <- internal_install_spec(client_name, client_src, client_version)
  }

  list(server = server, client = list(package = client_name, spec = client_spec))
}
