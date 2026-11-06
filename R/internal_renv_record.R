#### records the project's packages in renv.lock and says in plain words whether all is in sync
#' @title Record current package state into renv.lock
#' @description Updates a project's renv.lock file to match the currently installed packages, then checks whether the project is fully synchronized.
#' @details If the project does not use renv (no renv.lock file present), the function does nothing but emit a message. Otherwise it calls renv::snapshot() to rewrite renv.lock in the project folder to reflect the currently installed packages, suppressing renv's own console output. It then re-checks synchronization status and reports via message() whether the recorded lockfile now matches the library, suggesting check_project() if discrepancies remain.
#' @param project Path to the local project directory on the analyst's machine, expected to contain (or potentially contain) an renv.lock file.
#' @return Returns, invisibly, a logical: TRUE if renv.lock was updated and the project is now synchronized, FALSE if the project does not use renv or remains out of sync after the update. As a side effect, it may overwrite the renv.lock file in the project directory.
internal_renv_record <- function(project){

  if(!file.exists(file.path(project, "renv.lock"))){
    message("This project doesn't use renv (no renv.lock), so nothing was recorded.")
    return(invisible(FALSE))
  }

  internal_renv_quietly(renv::snapshot(project = project, prompt = FALSE))

  if(internal_renv_compare(project)$synchronized){
    message("Recorded in renv.lock.")
    return(invisible(TRUE))
  }

  message("renv.lock was updated, but something is still out of sync: run check_project() to see what.")
  invisible(FALSE)
}
