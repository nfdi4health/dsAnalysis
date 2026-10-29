#' @title Checks that the installed packages match the project's renv.lock
#' @description Compares the packages installed in the current project library with those recorded in renv.lock and reports any differences. Optionally restores or snapshots the project to resolve the differences.
#' @details Uses here::here() to locate the project and an internal helper (built on renv::status()) to classify differences such as missing, unrecorded, or mismatched-version packages. If fix is NULL and the session is interactive, utils::menu() asks whether to run renv::restore() or renv::snapshot(); either call can change the packages installed in the project library or the contents of renv.lock on disk. If the project has no renv.lock file, the function only messages and returns invisible NULL without checking anything.
#' @param fix Character string, one of "restore" or "snapshot", or NULL (the default); if NULL and the session is interactive, the user is asked to choose via a menu, while in a non-interactive session with fix left NULL no restore or snapshot action is taken and any other value is silently ignored.
#' @return Invisibly returns the comparison list produced by the internal helper (with elements such as synchronized, used_not_installed, recorded_not_installed, used_not_recorded, other_version, and unexplained), or invisible NULL if the project has no renv.lock file; the function is called mainly for its console messages and the side effects of fix (changes to the installed packages or to renv.lock on disk) rather than for its return value.
#' @examples
#' \dontrun{
#' proj <- tempfile(\"demo_project\")\ndir.create(proj)\nold <- setwd(proj)\n\n# check_project() looks up the project with here::here(); without an\n# renv.lock file it simply reports that there is nothing to check\ncheck_project(fix = \"none\")\n\nsetwd(old)\nunlink(proj, recursive = TRUE)
#' }
#' @export

check_project <- function(fix = NULL){

  project <- here::here()

  if(!file.exists(file.path(project, "renv.lock"))){
    message("This project doesn't use renv (no renv.lock), so there is nothing to check.")
    return(invisible(NULL))
  }

  sync <- internal_renv_compare(project)

  if(sync$synchronized){
    message("All good: the installed packages match renv.lock.")
    return(invisible(sync))
  }

  if(length(sync$used_not_installed) > 0){
    recorded <- intersect(sync$used_not_installed, sync$recorded)
    other <- setdiff(sync$used_not_installed, sync$recorded)
    message("Used by the project but not installed: ", paste(sync$used_not_installed, collapse = ", "), ".")
    if(length(recorded) > 0){
      message("  -> renv::restore() installs ", paste(recorded, collapse = ", "), " in the versions recorded in renv.lock.")
    }
    if(length(other) > 0){
      message("  -> ", paste(other, collapse = ", "), " must be installed first: install_dsPackage() for DataSHIELD packages, ",
              "renv::install() for others.")
    }
  }

  if(length(sync$recorded_not_installed) > 0){
    message("Recorded in renv.lock but not installed: ", paste(sync$recorded_not_installed, collapse = ", "), ".\n",
            "  -> renv::restore() installs them in the recorded versions.")
  }

  if(length(sync$used_not_recorded) > 0){
    message("Used by the project but not recorded in renv.lock: ", paste(sync$used_not_recorded, collapse = ", "), ".\n",
            "  -> renv::snapshot() records them.")
  }

  if(length(sync$other_version) > 0){
    message("Installed in another version than recorded in renv.lock: ", paste(sync$other_version, collapse = ", "), ".\n",
            "  -> renv::restore() goes back to the recorded versions, renv::snapshot() records the installed ones.")
  }

  if(sync$unexplained){
    message("renv reports other differences (e.g. in the packages' own dependencies); renv::snapshot() usually fixes them. ",
            "renv::status() shows the details.")
  }

  if(is.null(fix) && interactive()){
    choice <- utils::menu(c("Install what renv.lock records (renv::restore())",
                            "Record what is installed in renv.lock (renv::snapshot())",
                            "Nothing for now"),
                          title = "What should be done?")
    fix <- c("restore", "snapshot", "none")[choice]
  }

  if(identical(fix, "restore")){
    renv::restore(project = project, prompt = FALSE)
  } else if(identical(fix, "snapshot")){
    internal_renv_quietly(renv::snapshot(project = project, prompt = FALSE))
  }

  if(identical(fix, "restore") || identical(fix, "snapshot")){
    if(internal_renv_compare(project)$synchronized){
      message("All good now: the installed packages match renv.lock.")
    } else {
      message("Still not in sync. Run check_project() again to see what is left.")
    }
  }

  invisible(sync)

}
