#### Where DataSHIELD packages are installed from: the DataSHIELD package
#### catalogue (https://packages.datashield.org, built from
#### FederatedMethods/packages) gives each package's CRAN or GitHub location.

#### the catalogue, read once per session
dsAnalysis_cache <- new.env(parent = emptyenv())

#### the catalogue as a named list (one entry per package); NULL if it can't be read
#' @title Retrieve the DataSHIELD package catalogue
#' @description Fetches the DataSHIELD package catalogue from a remote URL, using a cached copy when available.
#' @details The catalogue is read from the URL in the `dsAnalysis.catalogue` option (defaulting to the official DataSHIELD packages.json), and the result is stored in the internal `dsAnalysis_cache` environment on the analyst's machine for reuse by later calls. No files are written to disk. If the catalogue cannot be downloaded or parsed, a message is printed and NULL is returned instead of raising an error.
#' @param refresh Logical; if FALSE (the default) a previously cached catalogue is reused when available, if TRUE the catalogue is always re-downloaded.
#' @return A list of package catalogue entries parsed from JSON (with simplifyVector = FALSE), or NULL if the catalogue could not be retrieved or parsed.
internal_ds_catalogue <- function(refresh = FALSE){

  url <- getOption("dsAnalysis.catalogue", "https://packages.datashield.org/packages.json")

  if(!refresh && !is.null(dsAnalysis_cache$catalogue)){
    return(dsAnalysis_cache$catalogue)
  }

  catalogue <- tryCatch(jsonlite::fromJSON(url, simplifyVector = FALSE),
                        error = function(e) NULL)

  if(is.null(catalogue)){
    message("The DataSHIELD package catalogue (", url, ") could not be read. ",
            "Give the package's GitHub repository with source = \"owner/repo\".")
    return(NULL)
  }

  dsAnalysis_cache$catalogue <- catalogue
  catalogue
}
