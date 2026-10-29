test_that("internal_renv_compare reports synchronized TRUE, no differences and unexplained FALSE when status says synchronized", {
  tmp <- withr::local_tempdir()
  writeLines("library(dsBase)", file.path(tmp, "script.R"))
  testthat::local_mocked_bindings(
    status = function(project, ...) list(synchronized = TRUE,
                                         lockfile = list(Packages = list(dsBase = list(Version = "6.3.0"))),
                                         library = list(Packages = list(dsBase = list(Version = "6.3.0")))),
    dependencies = function(...) data.frame(Package = "dsBase", stringsAsFactors = FALSE),
    .package = "renv")
  res <- dsAnalysis:::internal_renv_compare(tmp)
  testthat::expect_true(res$synchronized)
  testthat::expect_identical(res$recorded, "dsBase")
  testthat::expect_identical(res$used_not_installed, character(0))
  testthat::expect_identical(res$recorded_not_installed, character(0))
  testthat::expect_identical(res$used_not_recorded, character(0))
  testthat::expect_identical(res$other_version, character(0))
  testthat::expect_false(res$unexplained)
  testthat::expect_identical(names(res), c("synchronized", "recorded", "used_not_installed", "recorded_not_installed", "used_not_recorded", "other_version", "unexplained"))
})

test_that("internal_renv_compare lists a used but not installed package in used_not_installed only", {
  tmp <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    status = function(project, ...) list(synchronized = FALSE,
                                         lockfile = list(Packages = list(dsBase = list(Version = "6.3.0"))),
                                         library = list(Packages = list(dsBase = list(Version = "6.3.0")))),
    dependencies = function(...) data.frame(Package = c("dsBase", "dsSurvival"), stringsAsFactors = FALSE),
    .package = "renv")
  res <- dsAnalysis:::internal_renv_compare(tmp)
  testthat::expect_false(res$synchronized)
  testthat::expect_identical(res$used_not_installed, "dsSurvival")
  testthat::expect_identical(res$recorded_not_installed, character(0))
  testthat::expect_identical(res$used_not_recorded, character(0))
  testthat::expect_identical(res$other_version, character(0))
  testthat::expect_false(res$unexplained)
})

test_that("internal_renv_compare lists a recorded, not installed and unused package in recorded_not_installed only", {
  tmp <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    status = function(project, ...) list(synchronized = FALSE,
                                         lockfile = list(Packages = list(dsBase = list(Version = "6.3.0"), ggplot2 = list(Version = "3.5.1"))),
                                         library = list(Packages = list(dsBase = list(Version = "6.3.0")))),
    dependencies = function(...) data.frame(Package = "dsBase", stringsAsFactors = FALSE),
    .package = "renv")
  res <- dsAnalysis:::internal_renv_compare(tmp)
  testthat::expect_identical(res$recorded, c("dsBase", "ggplot2"))
  testthat::expect_identical(res$recorded_not_installed, "ggplot2")
  testthat::expect_identical(res$used_not_installed, character(0))
  testthat::expect_identical(res$used_not_recorded, character(0))
  testthat::expect_false(res$unexplained)
})

test_that("internal_renv_compare lists an installed and used but unrecorded package in used_not_recorded only", {
  tmp <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    status = function(project, ...) list(synchronized = FALSE,
                                         lockfile = list(Packages = list(dsBase = list(Version = "6.3.0"))),
                                         library = list(Packages = list(dsBase = list(Version = "6.3.0"), ggplot2 = list(Version = "3.5.1")))),
    dependencies = function(...) data.frame(Package = c("dsBase", "ggplot2"), stringsAsFactors = FALSE),
    .package = "renv")
  res <- dsAnalysis:::internal_renv_compare(tmp)
  testthat::expect_identical(res$used_not_recorded, "ggplot2")
  testthat::expect_identical(res$used_not_installed, character(0))
  testthat::expect_identical(res$recorded_not_installed, character(0))
  testthat::expect_identical(res$other_version, character(0))
  testthat::expect_false(res$unexplained)
})

test_that("internal_renv_compare lists packages whose recorded and installed versions differ in other_version, sorted", {
  tmp <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    status = function(project, ...) list(synchronized = FALSE,
                                         lockfile = list(Packages = list(zoo = list(Version = "1.8-12"), dsBase = list(Version = "6.3.0"), ggplot2 = list(Version = "3.5.1"))),
                                         library = list(Packages = list(zoo = list(Version = "1.8-13"), dsBase = list(Version = "6.2.0"), ggplot2 = list(Version = "3.5.1")))),
    dependencies = function(...) data.frame(Package = character(0), stringsAsFactors = FALSE),
    .package = "renv")
  res <- dsAnalysis:::internal_renv_compare(tmp)
  testthat::expect_identical(res$other_version, c("dsBase", "zoo"))
  testthat::expect_identical(res$used_not_installed, character(0))
  testthat::expect_identical(res$recorded_not_installed, character(0))
  testthat::expect_identical(res$used_not_recorded, character(0))
  testthat::expect_false(res$unexplained)
})

test_that("internal_renv_compare sets unexplained TRUE when status is out of sync but no difference is found", {
  tmp <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    status = function(project, ...) list(synchronized = FALSE,
                                         lockfile = list(Packages = list(dsBase = list(Version = "6.3.0"))),
                                         library = list(Packages = list(dsBase = list(Version = "6.3.0")))),
    dependencies = function(...) data.frame(Package = "dsBase", stringsAsFactors = FALSE),
    .package = "renv")
  res <- dsAnalysis:::internal_renv_compare(tmp)
  testthat::expect_false(res$synchronized)
  testthat::expect_true(res$unexplained)
  testthat::expect_identical(res$recorded, "dsBase")
})
