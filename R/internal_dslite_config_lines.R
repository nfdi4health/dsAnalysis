#### lines of the dslite.server$config(...) call in step 4
#' @title Locate config block lines in DSLite setup script
#' @description Finds the line indices of the dslite.server$config(...) call within step 4 of the 01_DSLite_Setup.R script.
#' @details This is an internal helper used while parsing an in-memory character vector of script lines; it does not read or write any files itself. It locates step 4 and step 5 markers via internal_dslite_step_line, then searches between them for the lines starting the config(...) call and the profile() call. It throws an error if either the start or end marker is not found exactly once within that range.
#' @param codelines A character vector holding the lines of the 01_DSLite_Setup.R script, as read locally on the analyst's machine (e.g. via readLines).
#' @return An integer vector of line indices (positions within codelines) spanning from the dslite.server$config( line up to, but not including, the dslite.server$profile() line.
internal_dslite_config_lines <- function(codelines){

  step4_line <- internal_dslite_step_line(codelines, 4)
  step5_line <- internal_dslite_step_line(codelines, 5)
  step4_range <- step4_line:(step5_line - 1)

  config_start <- step4_range[startsWith(codelines[step4_range], "dslite.server$config(")]
  config_end <- step4_range[startsWith(codelines[step4_range], "dslite.server$profile()")]

  if(!(length(config_start) == 1) || !(length(config_end) == 1)){
    stop("Could not find the dslite.server$config(...) call in step 4 of 01_DSLite_Setup.R.",
         call. = FALSE)
  }

  config_start:(config_end - 1)
}
