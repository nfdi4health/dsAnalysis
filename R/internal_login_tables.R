#### the table of each server in the login file, e.g. c(study1 = "CNSIM.CNSIM1")
#' @title Read the server tables from the login script
#' @description Reads which Opal table each server's login assigns, from the builder$append() calls of the project's login script (R/01_DS_Login.R), without running it.
#' @details The login script is read as text and parsed (not sourced), so no connection is made, no credentials are evaluated, and no server-side call is constructed or dispatched by this function. Only `builder$append()` calls that name both `server` and `table` as plain character strings are captured; a table supplied another way (e.g. via `Sys.getenv()`) is skipped, leaving the caller with no table for that server. This is a purely local, static-analysis helper used to prepare arguments for later DataSHIELD calls elsewhere in the package.
#' @param login_file Path of the login script on the analyst's machine; defaults to "R/01_DS_Login.R" in the project.
#' @return A named character vector, server name to table ("project.table"); empty if the file doesn't exist or names no tables.
internal_login_tables <- function(login_file = here::here("R", "01_DS_Login.R")){

  if (!file.exists(login_file)) {
    return(character(0))
  }
  exprs <- tryCatch(parse(login_file, keep.source = FALSE), error = function(e) expression())

  tables <- character(0)
  visit <- function(x){
    if (!is.call(x)) {
      return(invisible())
    }
    head <- x[[1]]
    is_append <- is.call(head) && identical(head[[1]], as.name("$")) && identical(head[[3]], as.name("append"))
    if (is_append && is.character(x$server) && is.character(x$table)) {
      tables[[x$server]] <<- x$table
    }
    for (part in as.list(x)[-1]) {
      visit(part)
    }
  }
  for (e in exprs) {
    visit(e)
  }

  tables
}
