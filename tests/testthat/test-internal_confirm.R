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
