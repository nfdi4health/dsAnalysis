test_that("internal_dependencies_dsPackages returns a zero-row data.frame and a NULL block attribute when no markers are present", {
  codelines <- c("library(here)", "library(dsBase); library(dsBaseClient)")
  res <- dsAnalysis:::internal_dependencies_dsPackages(codelines)
  testthat::expect_equal(nrow(res), 0L)
  testthat::expect_identical(names(res), c("server", "client"))
  testthat::expect_null(attr(res, "block"))
})

test_that("internal_dependencies_dsPackages returns no rows but the block attribute for an empty block", {
  codelines <- c("#### DataSHIELD packages (managed by add_dsPackage and remove_dsPackage)", "#### DataSHIELD packages end")
  res <- dsAnalysis:::internal_dependencies_dsPackages(codelines)
  testthat::expect_equal(nrow(res), 0L)
  testthat::expect_identical(attr(res, "block"), c(1L, 2L))
})

test_that("internal_dependencies_dsPackages gives NA client for a line with only a server library call", {
  codelines <- c("#### DataSHIELD packages (managed by add_dsPackage and remove_dsPackage)", "library(dsBase)", "#### DataSHIELD packages end")
  res <- dsAnalysis:::internal_dependencies_dsPackages(codelines)
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_identical(res$server, "dsBase")
  testthat::expect_true(is.na(res$client))
  testthat::expect_type(res$client, "character")
})

test_that("internal_dependencies_dsPackages drops lines inside the block without any library call", {
  codelines <- c("#### DataSHIELD packages (managed by add_dsPackage and remove_dsPackage)", "#### a comment", "", "library(dsBase); library(dsBaseClient)", "#### DataSHIELD packages end")
  res <- dsAnalysis:::internal_dependencies_dsPackages(codelines)
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_identical(res$server, "dsBase")
  testthat::expect_identical(res$client, "dsBaseClient")
  testthat::expect_identical(attr(res, "block"), c(1L, 5L))
})

test_that("internal_dependencies_dsPackages ignores library calls outside the block and only keeps those between the markers", {
  codelines <- c("library(dsOutsideBefore); library(dsOutsideBeforeClient)", "#### DataSHIELD packages (managed by add_dsPackage and remove_dsPackage)", "library(dsBase); library(dsBaseClient)", "#### DataSHIELD packages end", "library(dsOutsideAfter); library(dsOutsideAfterClient)")
  res <- dsAnalysis:::internal_dependencies_dsPackages(codelines)
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_identical(res$server, "dsBase")
  testthat::expect_false("dsOutsideBefore" %in% res$server)
  testthat::expect_false("dsOutsideAfter" %in% res$server)
})

test_that("internal_dependencies_dsPackages returns no rows and a NULL block when the end marker comes before the start marker", {
  codelines <- c("#### DataSHIELD packages end", "library(dsBase); library(dsBaseClient)", "#### DataSHIELD packages (managed by add_dsPackage and remove_dsPackage)")
  res <- dsAnalysis:::internal_dependencies_dsPackages(codelines)
  testthat::expect_equal(nrow(res), 0L)
  testthat::expect_null(attr(res, "block"))
})

test_that("internal_dependencies_dsPackages uses the first start and the first following end marker when markers repeat", {
  codelines <- c("#### DataSHIELD packages (managed by add_dsPackage and remove_dsPackage)", "library(dsBase); library(dsBaseClient)", "#### DataSHIELD packages end", "#### DataSHIELD packages (managed by add_dsPackage and remove_dsPackage)", "library(dsSurvival); library(dsSurvivalClient)", "#### DataSHIELD packages end")
  res <- dsAnalysis:::internal_dependencies_dsPackages(codelines)
  testthat::expect_identical(attr(res, "block"), c(1L, 3L))
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_identical(res$server, "dsBase")
})

test_that("internal_dependencies_dsPackages returns no rows and a NULL block for a zero-length character vector", {
  res <- dsAnalysis:::internal_dependencies_dsPackages(character(0))
  testthat::expect_equal(nrow(res), 0L)
  testthat::expect_identical(names(res), c("server", "client"))
  testthat::expect_null(attr(res, "block"))
})
