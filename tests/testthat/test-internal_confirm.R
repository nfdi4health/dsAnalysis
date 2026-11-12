test_that("internal_confirm returns FALSE without asking in a non-interactive session", {
  asked <- new.env(parent = emptyenv())
  asked$n <- 0L
  testthat::local_mocked_bindings(askYesNo = function(msg, ...) { asked$n <- asked$n + 1L; TRUE }, .package = "utils")
  testthat::expect_false(dsAnalysis:::internal_confirm("Use the home directory?", ask = FALSE))
  testthat::expect_identical(asked$n, 0L)
})

test_that("internal_confirm returns TRUE in an interactive session when the user answers yes and passes the question through", {
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(askYesNo = function(msg, ...) { seen$msg <- msg; TRUE }, .package = "utils")
  testthat::expect_true(dsAnalysis:::internal_confirm("Use the home directory?", ask = TRUE))
  testthat::expect_identical(seen$msg, "Use the home directory?")
})

test_that("internal_confirm returns FALSE in an interactive session when the user answers no", {
  testthat::local_mocked_bindings(askYesNo = function(msg, ...) FALSE, .package = "utils")
  testthat::expect_false(dsAnalysis:::internal_confirm("Use the home directory?", ask = TRUE))
})

test_that("internal_confirm returns FALSE in an interactive session when the question is cancelled (NA)", {
  testthat::local_mocked_bindings(askYesNo = function(msg, ...) NA, .package = "utils")
  res <- dsAnalysis:::internal_confirm("Use the home directory?", ask = TRUE)
  testthat::expect_identical(res, FALSE)
})

test_that("internal_confirm returns FALSE when askYesNo returns NULL or a non-logical answer", {
  testthat::local_mocked_bindings(askYesNo = function(msg, ...) NULL, .package = "utils")
  testthat::expect_identical(dsAnalysis:::internal_confirm("Use the home directory?", ask = TRUE), FALSE)
})

test_that("internal_confirm returns FALSE without asking when ask is NA or a non-TRUE value", {
  asked <- new.env(parent = emptyenv())
  asked$n <- 0L
  testthat::local_mocked_bindings(askYesNo = function(msg, ...) { asked$n <- asked$n + 1L; TRUE }, .package = "utils")
  testthat::expect_identical(dsAnalysis:::internal_confirm("Use the home directory?", ask = NA), FALSE)
  testthat::expect_identical(dsAnalysis:::internal_confirm("Use the home directory?", ask = NULL), FALSE)
  testthat::expect_identical(dsAnalysis:::internal_confirm("Use the home directory?", ask = c(TRUE, TRUE)), FALSE)
  testthat::expect_identical(asked$n, 0L)
})

test_that("internal_confirm defaults ask to interactive() and so returns FALSE without asking in the test session", {
  asked <- new.env(parent = emptyenv())
  asked$n <- 0L
  testthat::local_mocked_bindings(askYesNo = function(msg, ...) { asked$n <- asked$n + 1L; TRUE }, .package = "utils")
  testthat::expect_identical(formals(dsAnalysis:::internal_confirm)$ask, quote(interactive()))
  testthat::expect_false(interactive())
  testthat::expect_identical(dsAnalysis:::internal_confirm("Use the home directory?"), FALSE)
  testthat::expect_identical(asked$n, 0L)
})

test_that("internal_confirm returns FALSE when askYesNo answers with a non-logical value and asks exactly once", {
  asked <- new.env(parent = emptyenv())
  asked$n <- 0L
  testthat::local_mocked_bindings(askYesNo = function(msg, ...) { asked$n <- asked$n + 1L; "yes" }, .package = "utils")
  testthat::expect_identical(dsAnalysis:::internal_confirm("Use the home directory?", ask = TRUE), FALSE)
  testthat::expect_identical(asked$n, 1L)
})

test_that("internal_confirm passes only the question to askYesNo and returns a single TRUE", {
  seen <- new.env(parent = emptyenv())
  seen$n_args <- NA_integer_
  testthat::local_mocked_bindings(askYesNo = function(msg, ...) { seen$n_args <- length(list(...)); seen$msg <- msg; TRUE }, .package = "utils")
  res <- dsAnalysis:::internal_confirm("Overwrite the project folder?", ask = TRUE)
  testthat::expect_identical(res, TRUE)
  testthat::expect_length(res, 1L)
  testthat::expect_identical(seen$msg, "Overwrite the project folder?")
  testthat::expect_identical(seen$n_args, 0L)
})

test_that("internal_confirm propagates an error raised by askYesNo", {
  testthat::local_mocked_bindings(askYesNo = function(msg, ...) stop("no console available"), .package = "utils")
  testthat::expect_error(dsAnalysis:::internal_confirm("Use the home directory?", ask = TRUE), "no console available")
  testthat::expect_identical(dsAnalysis:::internal_confirm("Use the home directory?", ask = FALSE), FALSE)
})
