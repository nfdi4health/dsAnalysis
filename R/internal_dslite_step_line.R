#### line number of a step marker in 01_DSLite_Setup.R; stops if the file doesn't have it
#' @title Locate a step marker line in setup script
#' @description Finds the line number of a specific step-marker comment within the lines of the "01_DSLite_Setup.R" script, used internally when programmatically editing that file.
#' @details This is an internal helper used while generating or editing the "01_DSLite_Setup.R" setup script on the analyst's machine; it has no side effects of its own (no files are read or written here). It relies on a fixed, hard-coded vector of seven step-marker strings corresponding to steps 1 through 7 of that script. If the expected marker text is not found exactly once in codelines (e.g. because a marker was edited or removed), the function stops with an informative error.
#' @param codelines A character vector containing the lines of the local '01_DSLite_Setup.R' script (e.g. as read by readLines()), not a DataSHIELD connections object.
#' @param step An integer from 1 to 7 selecting which of the seven predefined step markers to search for in codelines.
#' @return An integer giving the index (line number) within codelines at which the requested step marker occurs; the function stops with an error if the marker is not found exactly once.
internal_dslite_step_line <- function(codelines, step){

  markers <- c("#### Step 1: Loading necessary libraries",
               "#### Step 2: Import of mock data files",
               "#### Step 3: Defining the server-side data in a new dslite server",
               "#### Step 4: Defining the server-side settings",
               "#### Step 5: Building the logindata object",
               "#### Step 6: Login to the different DSLite Servers",
               "#### Step 7: Cleaning the environment")

  step_line <- which(codelines == markers[step])

  if(!(length(step_line) == 1)){
    stop(paste0("Could not find the line '", markers[step],
                "' in 01_DSLite_Setup.R. Please don't edit the step markers."),
         call. = FALSE)
  }

  step_line
}
