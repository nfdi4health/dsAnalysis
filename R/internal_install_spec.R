#### the renv::install() specification of one package
#### src: list(cran, repo) from internal_catalogue_source() or a user's "owner/repo"
#' @title Build a remotes-style install specification string
#' @description Resolves a package name, source, version and/or ref into a single string suitable for remotes::install_* functions. Chooses between CRAN and GitHub forms and looks up GitHub tags when needed.
#' @details This is a local, offline-except-for-GitHub-API-calls helper used while assembling install instructions for a package; it does not install anything or write any files itself. When a GitHub repo is known and no explicit ref or version is given, it queries GitHub tags (via internal_github_tags) to find the latest release tag, falling back to the repository's default branch if no release tags exist or the lookup fails. It signals an error with rlang-free base `stop()` (call. = FALSE) when a ref or version is requested but no GitHub repository is available, or when the requested version cannot be found among GitHub tags.
#' @param package Name of the package, as a plain string, used in messages and as the CRAN install spec.
#' @param src A local list describing where the package comes from, with a logical `cran` element and a `repo` element giving the "owner/repo" GitHub location (or NA if none is known); this is not an object on any DataSHIELD server.
#' @param version Optional version string to install; if NULL, the latest CRAN version or the latest GitHub release tag (else the default branch) is used.
#' @param ref Optional GitHub ref (branch, commit, or tag) to install; if given, `src$repo` must be a known GitHub repository and the ref is used as-is without checking it exists.
#' @return A single character string: the package name (optionally with "@version") for CRAN packages, or "owner/repo" optionally with "@ref"/"@tag" for GitHub packages, suitable for passing to remotes::install_version or remotes::install_github. The function has no side effects on disk.
internal_install_spec <- function(package, src, version = NULL, ref = NULL){

  if(!is.null(ref)){
    if(is.na(src$repo)){
      stop("A ref needs the GitHub repository of ", package, ": give source = \"owner/repo\".", call. = FALSE)
    }
    return(paste0(src$repo, "@", ref))
  }

  if(src$cran){
    return(if (is.null(version)) package else paste0(package, "@", version))
  }

  if(is.na(src$repo)){
    stop("No CRAN or GitHub location is known for ", package, ". Give source = \"owner/repo\".", call. = FALSE)
  }

  #### the GitHub tags; without a version, a failed lookup falls back to the default branch
  tags <- if (is.null(version)) tryCatch(internal_github_tags(src$repo), error = function(e) character(0)) else internal_github_tags(src$repo)

  #### the release tags (1.2.3 or v1.2.3, no release candidates), newest first
  released <- tags[grepl("^v?[0-9]+(\\.[0-9]+)+$", tags)]
  released <- released[order(numeric_version(sub("^v", "", released)), decreasing = TRUE)]

  #### no version: the latest release, else the default branch
  if(is.null(version)){
    if(length(released) == 0){
      message("No released version of ", package, " found on GitHub; installing its default branch.")
      return(src$repo)
    }
    return(paste0(src$repo, "@", released[[1]]))
  }

  #### a version: the tag 1.2.3 or v1.2.3
  tag <- tags[tags %in% c(version, paste0("v", version))][1]

  if(is.na(tag)){
    stop("Version ", version, " of ", package, " was not found on GitHub (", src$repo, "). ",
         "Available versions: ", paste(sub("^v", "", released), collapse = ", "), call. = FALSE)
  }

  paste0(src$repo, "@", tag)
}
