test_that("internal_catalogue_source returns cran FALSE for an empty cran_link and NA_character_ repo for an empty github_link", {
  entry <- list(input = list(cran_link = "", github_link = ""))
  res <- dsAnalysis:::internal_catalogue_source(entry)
  testthat::expect_false(res$cran)
  testthat::expect_identical(res$repo, NA_character_)
})

test_that("internal_catalogue_source returns cran FALSE and NA repo when both links are missing from the entry", {
  entry <- list(input = list())
  res <- dsAnalysis:::internal_catalogue_source(entry)
  testthat::expect_false(res$cran)
  testthat::expect_identical(res$repo, NA_character_)
  testthat::expect_equal(length(res), 2L)
})

test_that("internal_catalogue_source strips a .git suffix and trailing slashes from the github link", {
  entry_git <- list(input = list(cran_link = NULL, github_link = "https://github.com/neelsoumya/dsSurvival.git"))
  entry_slash <- list(input = list(cran_link = NULL, github_link = "https://github.com/datashield/dsBaseClient/"))
  testthat::expect_equal(dsAnalysis:::internal_catalogue_source(entry_git)$repo, "neelsoumya/dsSurvival")
  testthat::expect_false(dsAnalysis:::internal_catalogue_source(entry_git)$cran)
  testthat::expect_equal(dsAnalysis:::internal_catalogue_source(entry_slash)$repo, "datashield/dsBaseClient")
})

test_that("internal_catalogue_source strips an http:// github prefix as well as https://", {
  entry <- list(input = list(cran_link = "", github_link = "http://github.com/datashield/dsMediation"))
  res <- dsAnalysis:::internal_catalogue_source(entry)
  testthat::expect_equal(res$repo, "datashield/dsMediation")
  testthat::expect_false(res$cran)
})

test_that("internal_catalogue_source leaves a plain owner/repo string unchanged", {
  entry <- list(input = list(cran_link = "", github_link = "datashield/dsBase"))
  res <- dsAnalysis:::internal_catalogue_source(entry)
  testthat::expect_equal(res$repo, "datashield/dsBase")
})
