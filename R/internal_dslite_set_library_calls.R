#### adds and removes library() calls in step 1 of 01_DSLite_Setup.R; added ones go after
#### the last non-empty line of step 1, a call that is already there isn't added again
#' @title Add or remove library() calls in DSLite setup script
#' @description Edits the block of library() calls located between step 1 and step 2 markers in a DSLite setup script, adding or removing packages as requested.
#' @details This is an internal helper used while generating or editing a DSLite setup script held in memory as a character vector; it does not read or write any files itself. It locates the line range between the step-1 and step-2 markers (via internal_dslite_step_line), drops any library() lines for packages named in remove, then appends library() lines for any packages in add that are not already present, inserting them after the last non-blank line of that range. The caller is expected to write the returned lines back to disk if persistence is needed.
#' @param codelines Character vector of lines of R code making up a DSLite setup script, held locally in the analyst's session (not a file path).
#' @param add Character vector of package names to ensure are loaded via library() calls in the script; defaults to an empty character vector, meaning nothing is added.
#' @param remove Character vector of package names whose existing library() calls should be removed from the script; defaults to an empty character vector, meaning nothing is removed.
#' @return A character vector of code lines with the requested library() calls removed and/or added; if no additions are needed after removals, the (possibly shortened) input vector is returned unchanged.
internal_dslite_set_library_calls <- function(codelines, add = character(0), remove = character(0)){

  step1_line <- internal_dslite_step_line(codelines, 1)
  step2_line <- internal_dslite_step_line(codelines, 2)
  step1_range <- step1_line:(step2_line - 1)

  drop <- step1_range[trimws(codelines[step1_range]) %in% paste0("library(", remove, ")")]
  if(length(drop) > 0){
    codelines <- codelines[-drop]
    step2_line <- step2_line - length(drop)
    step1_range <- step1_line:(step2_line - 1)
  }

  library_lines <- setdiff(paste0("library(", add, ")"), codelines[step1_range])
  if(length(library_lines) == 0){
    return(codelines)
  }

  last_line <- max(step1_range[nzchar(trimws(codelines[step1_range]))])
  append(codelines, library_lines, after = last_line)
}
