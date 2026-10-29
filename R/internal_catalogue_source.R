#### where a catalogue entry is installed from: list(cran = TRUE/FALSE, repo = "owner/repo" or NA)
#' @title Extract CRAN and GitHub source info from a catalogue entry
#' @description Derives whether a package catalogue entry has a CRAN link and extracts a "owner/repo" string from its GitHub link, if any.
#' @param entry A list representing one package catalogue entry, expected to contain an `input` element with optional `cran_link` and `github_link` character strings; this is a plain local R value, not a server-side object.
#' @return A list with two elements: `cran`, a logical indicating whether `entry$input$cran_link` is non-null and non-empty, and `repo`, a character string with the GitHub "owner/repo" path extracted from `entry$input$github_link` (stripping the "https://github.com/" prefix, trailing slashes, and an optional ".git" suffix), or NA_character_ if no GitHub link is present.
internal_catalogue_source <- function(entry){

  cran_link <- entry[["input"]][["cran_link"]]
  github_link <- entry[["input"]][["github_link"]]

  repo <- NA_character_
  if(!is.null(github_link) && nzchar(github_link)){
    repo <- sub("^https?://github\\.com/", "", github_link)
    repo <- sub("(\\.git)?/*$", "", repo)
  }

  list(cran = !is.null(cran_link) && nzchar(cran_link), repo = repo)
}
