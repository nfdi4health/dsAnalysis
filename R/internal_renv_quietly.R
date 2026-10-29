#### runs a renv call without its printout (the functions here report in their own words)
#' @title Evaluate an expression suppressing output, messages, and warnings
#' @description Runs an R expression while silencing printed output, messages, and warnings, returning its value invisibly.
#' @details This is a local helper used to quiet noisy calls (such as renv operations) on the analyst's machine; it has no effect on any DataSHIELD server. It captures and discards anything the expression would print via utils::capture.output, and suppresses any messages or warnings raised during evaluation. No files are created and no connections are used; any side effects of expr itself (such as files written by renv) still occur.
#' @param expr An R expression to evaluate locally; its printed output, messages, and warnings are suppressed.
#' @return Returns, invisibly, the value produced by evaluating expr; if expr raises an error, that error still propagates.
internal_renv_quietly <- function(expr){
  result <- NULL
  utils::capture.output(result <- suppressMessages(suppressWarnings(expr)))
  invisible(result)
}
