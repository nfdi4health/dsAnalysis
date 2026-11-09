#' @title Lists the DataSHIELD packages in the DataSHIELD package catalogue
#' @description Returns a tibble of DataSHIELD packages from the package catalogue, optionally filtered by search text and status.
#' @details Calls internal_ds_catalogue() to obtain the catalogue, caching it for the session unless refresh = TRUE forces a reload. Packages that are clients of another package in the catalogue (see internal_catalogue_client()) are merged into that package's row rather than listed separately. Prints a message to the console reporting how many packages were found, with a hint to use install_dsPackage() for the first match, or that none were found.
#' @param search Optional character string to search for (case-insensitive, fixed match) in the package name, client name and description; if NULL (the default), no text filtering is applied.
#' @param status Optional character vector of statuses to keep, e.g. "production" or c("production", "development"); matching is case-insensitive and packages with no recorded status are labelled "unknown"; if NULL (the default), all statuses are kept.
#' @param refresh Logical, defaults to FALSE; if TRUE, re-downloads the package catalogue via ds_catalogue() instead of reusing the copy cached earlier in the session.
#' @return A tibble, sorted by package name, with one row per package and columns package, client, status, github_version, source and description; if the catalogue cannot be obtained, an empty tibble is returned invisibly. No disclosure control is relevant since this only reflects publicly available catalogue metadata, and no files are written.
#' @examples
#' \dontrun{
#' list_dsPackages()
#' 
#' list_dsPackages(search = "diabetes", status = "production")
#' }
#' @export

list_dsPackages <- function(search = NULL, status = NULL, refresh = FALSE){

  catalogue <- internal_ds_catalogue(refresh = refresh)

  if(is.null(catalogue)){
    return(invisible(tibble::tibble()))
  }

  #### one row per package that isn't the client of another one
  clients <- vapply(names(catalogue), internal_catalogue_client, "", catalogue = catalogue)
  packages <- setdiff(names(catalogue), stats::na.omit(clients))

  field <- function(entry, part, name){
    value <- entry[[part]][[name]]
    if (is.null(value)) "" else as.character(value)
  }

  packages_info <- tibble::tibble(
    package = packages,
    client = unname(ifelse(is.na(clients[packages]), "", clients[packages])),
    status = vapply(catalogue[packages], field, "", part = "input", name = "status"),
    github_version = vapply(catalogue[packages], field, "", part = "repo", name = "Version"),
    source = vapply(catalogue[packages], function(entry){
      src <- internal_catalogue_source(entry)
      if (src$cran) "CRAN" else if (!is.na(src$repo)) paste0("GitHub: ", src$repo) else ""
    }, ""),
    description = vapply(catalogue[packages], field, "", part = "input", name = "description")
  )

  packages_info$status[!nzchar(packages_info$status)] <- "unknown"

  if(!is.null(status)){
    packages_info <- packages_info[tolower(packages_info$status) %in% tolower(status), ]
  }

  if(!is.null(search)){
    text <- tolower(paste(packages_info$package, packages_info$client, packages_info$description))
    packages_info <- packages_info[grepl(tolower(search), text, fixed = TRUE), ]
  }

  packages_info <- packages_info[order(packages_info$package), ]

  if(nrow(packages_info) == 0){
    message("No DataSHIELD packages found. Try another search or status.")
  } else {
    message(nrow(packages_info), " DataSHIELD package(s) found. Install one with install_dsPackage(\"",
            packages_info$package[1], "\").")
  }

  packages_info

}
