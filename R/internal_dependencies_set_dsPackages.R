#### adds and removes packages in the dependencies.R block (creates the block if needed)
#' @title Update recorded DataSHIELD package dependencies in dependencies.R
#' @description Adds, removes, or updates entries in the project's dependencies.R file that record which server and client R packages are used for the DataSHIELD analysis.
#' @details If no dependencies.R file exists in the current project (located via here::here()), the function does nothing except emit a message. Otherwise it reads the file, locates the existing block of library() calls previously written by this function (recognised by start/end markers), and rebuilds that block from the current package list plus the requested additions/removals. If no such block exists yet, a new one is inserted just before the block maintained by the datashield-analysis-suggest bot (if present) or appended to the end of the file. The updated lines are written back to dependencies.R on disk, overwriting it.
#' @param add Character vector of server-side package names to add to the recorded dependencies, local R values rather than anything evaluated on the server.
#' @param client Character vector of client-side package names paired positionally with add, used to record the companion client package for each added server package.
#' @param remove Character vector of server-side package names to remove from the recorded dependencies.
#' @return Invisibly returns a data frame with columns server and client listing the package names recorded after the update; as a side effect it overwrites the project's dependencies.R file with the updated package list (and leaves it unchanged if the file did not exist).
internal_dependencies_set_dsPackages <- function(add = character(0), client = character(0), remove = character(0)){

  #### marker line of the block the datashield-analysis-suggest workflow maintains
  bot_marker <- "#### bot-suggest: packages (updated by datashield-analysis-suggest)"

  dependencies_file <- here::here("dependencies.R")

  if(!file.exists(dependencies_file)){
    message("No dependencies.R found in the project, so it was not updated.")
    return(invisible(NULL))
  }

  codelines <- readLines(con = dependencies_file)
  recorded <- internal_dependencies_dsPackages(codelines)
  markers <- attr(recorded, "markers")
  block <- attr(recorded, "block")

  packages <- recorded[!(recorded$server %in% c(remove, add)), , drop = FALSE]
  packages <- rbind(packages, data.frame(server = add, client = client))

  block_new <- c(markers[["start"]],
                 ifelse(is.na(packages$client) | !nzchar(packages$client),
                        paste0("library(", packages$server, ")"),
                        paste0("library(", packages$server, "); library(", packages$client, ")")),
                 markers[["end"]])

  if(!is.null(block)){

    codelines <- c(codelines[seq_len(block[1] - 1)],
                   block_new,
                   codelines[-seq_len(block[2])])

  } else {

    #### a new block goes before the bot's block, or at the end
    bot_line <- which(codelines == bot_marker)

    if(length(bot_line) > 0){
      codelines <- append(codelines, c(block_new, ""), after = bot_line[1] - 1)
    } else {
      codelines <- c(codelines, "", block_new)
    }

  }

  writeLines(text = codelines, con = dependencies_file)
  invisible(packages)
}
