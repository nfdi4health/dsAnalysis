#### asks the user a yes/no question in an interactive session
#' @title Ask the user to confirm
#' @description Asks a yes/no question in the R console and returns whether the answer was yes.
#' @details Only asks in an interactive session (utils::askYesNo()); in scripts, CI, tests and CRAN's checks it returns FALSE without asking, so nothing is done there without an explicit argument. A cancelled question counts as no. Used where CRAN's policy allows writing outside the temporary directory only with the user's confirmation.
#' @param question The question to show, ending with a question mark.
#' @param ask Whether the session can ask; defaults to interactive(). An argument so tests can set it: testthat can't replace base functions such as interactive().
#' @return TRUE if the user answered yes, FALSE otherwise.
internal_confirm <- function(question, ask = interactive()){

  if (!isTRUE(ask)) {
    return(FALSE)
  }
  isTRUE(utils::askYesNo(question))
}
