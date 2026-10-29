#### the tags of a GitHub repository (first 100)
#' @title Get GitHub repository tag names
#' @description Fetches the list of tag names (e.g. version tags) available for a GitHub repository via the GitHub API.
#' @details Uses the GitHub REST API to list up to 100 tags for the given repository. If the environment variable GITHUB_PAT is set, it is sent as a bearer token to authenticate the request and avoid rate limits. No files are written and no packages are installed; this only performs a network request. If the request fails or the repository cannot be read, the function stops with an informative error instead of returning NULL.
#' @param repo Character string giving the GitHub repository in 'owner/name' form (a local R value, not a server-side object), used to query the GitHub tags API.
#' @return A character vector of tag names (e.g. version strings) defined in the GitHub repository; the function stops with an error if the tags cannot be retrieved.
internal_github_tags <- function(repo){

  headers <- c(Accept = "application/vnd.github+json")
  if(nzchar(Sys.getenv("GITHUB_PAT"))){
    headers <- c(headers, Authorization = paste("token", Sys.getenv("GITHUB_PAT")))
  }

  tags <- tryCatch({
    con <- url(paste0("https://api.github.com/repos/", repo, "/tags?per_page=100"), headers = headers)
    on.exit(close(con))
    jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = ""))$name
  }, error = function(e) NULL)

  if(is.null(tags)){
    stop("Could not read the versions (tags) of https://github.com/", repo,
         ". Check the repository name, or give a ref instead of a version.", call. = FALSE)
  }

  tags
}
