test_that("remove_dsPackage errors when no package name is given", {
  err <- testthat::expect_error(dsAnalysis::remove_dsPackage())
  testthat::expect_equal(err$message, "No package name has been given.")
  testthat::expect_null(err$call)
})

test_that("remove_dsPackage refuses to remove dsBase", {
  err <- testthat::expect_error(dsAnalysis::remove_dsPackage(dsPackage = c("dsBase", "dsSurvival")))
  testthat::expect_equal(err$message, "dsBase can't be removed: every DataSHIELD setup needs it.")
  testthat::expect_null(err$call)
})

test_that("remove_dsPackage errors when the number of client packages does not match the number of server packages", {
  err <- testthat::expect_error(dsAnalysis::remove_dsPackage(dsPackage = c("dsSurvival", "dsMediation"),
                                                             client = "dsSurvivalClient"))
  testthat::expect_equal(err$message, "Please provide one client package per DataSHIELD package.")
  testthat::expect_null(err$call)
})
