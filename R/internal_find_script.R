#' @title Internal Function for pathing of files that will be provided
#' @description This is an internal function which will find the correct path for files.
#' @details This function looks up the installed location of a template file within a package's "templates" directory on the analyst's local machine, using fs::path_package(). It does not write, copy, or modify any files; it only returns a path. If the path cannot be resolved (e.g. the package or template file is missing), it stops with an informative error instead of returning an empty string.
#' @param script_name specifies which script is called.
#' @param package Character string giving the name of the locally installed R package that contains the template files; defaults to "dsAnalysis".
#' @return A single character string giving the local file system path to the requested template script inside the installed package; no files are created or modified. If the path cannot be resolved, the function throws an error instead of returning a value.
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import fs
#' @import usethis

internal_find_script <- function(script_name, package = "dsAnalysis") {

  path <- tryCatch(
    fs::path_package(package = package, "templates", script_name),
    error = function(e) ""
  )

  if (identical(path, "")) {
    stop(paste0("Could not find the file: ", script_name,
                ". Please contact the developer team."),
         call. = FALSE)
  }

  path

}
