#### the packages in the block of dependencies.R that add_dsPackage() and remove_dsPackage()
#### maintain, one line per server package: library(<server>); library(<client>)
#### returns data.frame(server, client), with the attributes "markers" (the block's
#### start and end line texts) and "block" (their line numbers; NULL without a block)
internal_dependencies_dsPackages <- function(codelines){

  markers <- c(start = "#### DataSHIELD packages (managed by add_dsPackage and remove_dsPackage)",
               end = "#### DataSHIELD packages end")

  start <- which(codelines == markers[["start"]])
  end <- which(codelines == markers[["end"]])
  block <- if (length(start) > 0 && length(end) > 0 && end[1] > start[1]) c(start[1], end[1])
  inside <- if (!is.null(block) && block[2] > block[1] + 1) (block[1] + 1):(block[2] - 1) else integer(0)

  calls <- stringr::str_match_all(codelines[inside], "library\\(([^)]+)\\)")
  packages <- data.frame(server = vapply(calls, function(x) if (nrow(x) > 0) x[1, 2] else NA_character_, ""),
                         client = vapply(calls, function(x) if (nrow(x) > 1) x[2, 2] else NA_character_, ""))
  packages <- packages[!is.na(packages$server), , drop = FALSE]

  attr(packages, "markers") <- markers
  attr(packages, "block") <- block
  packages
}
