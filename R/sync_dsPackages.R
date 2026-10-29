#' @title Matches the local packages and DSLite setup to the DataSHIELD servers
#' @description Compares package versions installed locally with those available on the connected DataSHIELD servers, and installs any that are missing or outdated.
#' @details Side effects on the analyst's machine: calls install_dsPackage() to install or update packages in the local R library when install = TRUE, and if utils/setup/01_DSLite_Setup.R exists, may append newly-available packages to it via add_dsPackage(). Uses DSI::datashield.profiles() to report which DataSHIELD profiles the servers use, DSI::datashield.pkg_status() to learn which packages and versions each server offers, and utils::packageVersion() to check what is installed locally. When a package has different versions across servers, the lowest common version is chosen for local installation so the client works with all connected servers; informative messages are printed about such mismatches and about packages available on only some servers.
#' @param conns DataSHIELD connections; found in the session if not given
#' @param install Logical, default TRUE; if TRUE missing or outdated packages are installed locally, if FALSE only a report of what would be installed is printed and returned, with nothing installed.
#' @return Returns (invisibly) a data frame with one row per server-side package, giving the servers it is on, the versions found, the version chosen for local installation, and the locally installed version (NA if absent); when install = FALSE it is also printed to the console as a report, and no packages are installed.
#' @examples
#' \dontrun{
#' require('DSI')
#' require('DSOpal')
#' require('dsAnalysis')
#' 
#' builder <- DSI::newDSLoginBuilder()
#' builder$append(server = "study1",
#'                url = "https://opal-demo.obiba.org/",
#'                user = "dsuser", password = "P@ssw0rd",
#'                table = "CNSIM.CNSIM1", driver = "OpalDriver")
#' builder$append(server = "study2",
#'                url = "https://opal-demo.obiba.org/",
#'                user = "dsuser", password = "P@ssw0rd",
#'                table = "CNSIM.CNSIM2", driver = "OpalDriver")
#' builder$append(server = "study3",
#'                url = "https://opal-demo.obiba.org/",
#'                user = "dsuser", password = "P@ssw0rd",
#'                table = "CNSIM.CNSIM3", driver = "OpalDriver")
#' logindata <- builder$build()
#' connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#' 
#' sync_dsPackages(conns = connections, install = FALSE)
#' 
#' datashield.logout(connections)
#' }
#' @export

sync_dsPackages <- function(conns = NULL, install = TRUE){

  if(is.null(conns)){
    conns <- DSI::datashield.connections_find()
  }

  if(length(conns) == 0){
    stop("No DataSHIELD connections found. Log in first (R/01_DS_Login.R with R_CONFIG_ACTIVE = 'production').",
         call. = FALSE)
  }

  #### the profiles the servers use: they decide which packages a server offers
  profiles <- tryCatch(DSI::datashield.profiles(conns)$current, error = function(e) NULL)

  if(!is.null(profiles) && any(profiles$profile != "default")){
    message("The servers use these DataSHIELD profiles: ",
            paste0(rownames(profiles), " = ", profiles$profile, collapse = ", "), ". ",
            "The packages below are the ones of these profiles; to log in with the same profiles next time, ",
            "give profile = \"...\" in builder$append() in R/01_DS_Login.R.")
  }

  status <- DSI::datashield.pkg_status(conns)
  on_server <- status$package_status
  versions <- status$version_status

  servers_packages <- data.frame(package = rownames(on_server),
                                 servers = apply(on_server, 1, function(x) paste(colnames(on_server)[x], collapse = ", ")),
                                 versions = apply(versions, 1, function(x) paste(unique(stats::na.omit(x)), collapse = ", ")),
                                 row.names = NULL)

  #### one version per package: the lowest the servers have, so that the client works with all of them
  servers_packages$version <- apply(versions, 1, function(x){
    x <- unique(stats::na.omit(x))
    x[order(numeric_version(x, strict = FALSE))][1]
  })

  servers_packages$installed <- vapply(servers_packages$package, function(p){
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  }, "")

  for (i in which(grepl(",", servers_packages$versions))){
    message(servers_packages$package[i], " has different versions on the servers (", servers_packages$versions[i],
            "); version ", servers_packages$version[i], " is used here, so that it works with all of them.")
  }

  for (i in which(!(servers_packages$servers == paste(colnames(on_server), collapse = ", ")))){
    message(servers_packages$package[i], " is only on ", servers_packages$servers[i], ".")
  }

  to_install <- is.na(servers_packages$installed) | !(servers_packages$installed == servers_packages$version)

  if(!install){
    print(servers_packages, row.names = FALSE)
    message(if (any(to_install)) paste0("To install: ", paste(servers_packages$package[to_install], collapse = ", "),
                                        ". Run sync_dsPackages() to install them.")
            else "All packages of the servers are installed in the same versions.")
    return(invisible(servers_packages))
  }

  for (i in which(to_install)){

    package <- servers_packages$package[i]
    version <- servers_packages$version[i]

    done <- tryCatch({ install_dsPackage(package, version = version); TRUE },
                     error = function(e){ message("Version ", version, " of ", package, " can't be installed: ",
                                                  conditionMessage(e)); FALSE })

    #### a development version on a server (e.g. 6.3.6.9000) has no release: fall back to the latest
    if(!done){
      tryCatch({ install_dsPackage(package)
                 message(package, ": installed its latest version instead.") },
               error = function(e) message(package, " was not installed: ", conditionMessage(e)))
    }

  }

  #### packages that are installed already, but not yet in the DSLite setup
  setup_file <- here::here("utils/setup", "01_DSLite_Setup.R")

  if(file.exists(setup_file)){
    included <- internal_dslite_included_packages(readLines(setup_file))
    missing <- servers_packages$package[!to_install & !(servers_packages$package %in% included)]
    if(length(missing) > 0){
      catalogue <- internal_ds_catalogue()
      clients <- if (is.null(catalogue)) paste0(missing, "Client") else
        vapply(missing, internal_catalogue_client, "", catalogue = catalogue, USE.NAMES = FALSE)
      add_dsPackage(missing, client = clients)
    }
  }

  invisible(servers_packages)

}
