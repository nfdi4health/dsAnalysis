#### compares what the project uses, what is installed and what renv.lock records
#' @title Compare used, recorded, and installed packages in a project
#' @description Checks whether packages actually used in the project's code match those recorded in the renv lockfile and those installed in the project library.
#' @details Calls renv::status() and renv::dependencies() on the given project to determine package usage, lockfile records, and installed library contents, suppressing their normal console output via internal_renv_quietly(). It does not write, modify, or delete any files; it only reads the project's renv state and returns a summary in R. Base R packages and "renv" itself are excluded from the set of used packages before comparison.
#' @param project Local file path to the root directory of the renv project to check; not a DataSHIELD connection or server-side object.
#' @return A list with a logical `synchronized` flag (renv's own sync status), character vectors `recorded`, `used_not_installed`, `recorded_not_installed`, `used_not_recorded`, and `other_version` identifying mismatches between used, recorded, and installed packages, and a logical `unexplained` flag that is TRUE when renv reports the project as out of sync but none of these specific mismatches account for it. No files are created or modified on disk.
internal_renv_compare <- function(project){

  status <- internal_renv_quietly(renv::status(project = project))
  used <- internal_renv_quietly(renv::dependencies(project, progress = FALSE))$Package

  base_packages <- rownames(utils::installed.packages(priority = "base"))
  used <- setdiff(unique(used), c(base_packages, "renv"))

  recorded_packages <- status$lockfile$Packages
  installed_packages <- status$library$Packages
  recorded <- names(recorded_packages)
  installed <- names(installed_packages)

  versions <- function(x) vapply(x, function(p) if (is.null(p$Version)) NA_character_ else p$Version, "")
  common <- intersect(recorded, installed)

  sync <- list(synchronized = isTRUE(status$synchronized),
               recorded = recorded,
               used_not_installed = sort(setdiff(used, installed)),
               recorded_not_installed = sort(setdiff(setdiff(recorded, installed), used)),
               used_not_recorded = sort(setdiff(intersect(used, installed), recorded)),
               other_version = sort(common[versions(recorded_packages[common]) != versions(installed_packages[common])]))

  sync$unexplained <- !sync$synchronized &&
    all(lengths(sync[c("used_not_installed", "recorded_not_installed", "used_not_recorded", "other_version")]) == 0)

  sync
}
