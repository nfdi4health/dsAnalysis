test_that("internal_catalogue_name returns the catalogue's spelling of the name for a case-different package", {
  catalogue <- list(dsBase = list(input = list(cran_link = "")), dsBaseClient = list(input = list(cran_link = "")), dsSurvival = list(input = list(cran_link = "")))
  testthat::expect_identical(dsAnalysis:::internal_catalogue_name("DSBASE", catalogue), "dsBase")
  testthat::expect_identical(dsAnalysis:::internal_catalogue_name("dsbaseclient", catalogue), "dsBaseClient")
  testthat::expect_identical(dsAnalysis:::internal_catalogue_name("dsSurvival", catalogue), "dsSurvival")
})

test_that("internal_catalogue_name returns NA_character_ for a package that is not in the catalogue", {
  catalogue <- list(dsBase = list(input = list(cran_link = "")), dsBaseClient = list(input = list(cran_link = "")))
  res <- dsAnalysis:::internal_catalogue_name("notAPackage", catalogue)
  testthat::expect_identical(res, NA_character_)
  testthat::expect_true(is.na(res))
  testthat::expect_type(res, "character")
  testthat::expect_length(res, 1L)
})

test_that("internal_catalogue_name returns NA_character_ when the catalogue is NULL or an empty list", {
  testthat::expect_identical(dsAnalysis:::internal_catalogue_name("dsBase", NULL), NA_character_)
  testthat::expect_identical(dsAnalysis:::internal_catalogue_name("dsBase", list()), NA_character_)
})

test_that("internal_catalogue_name matches the first catalogue entry when two names differ only in case", {
  catalogue <- stats::setNames(list(list(input = list(id = 1)), list(input = list(id = 2))), c("dsBase", "DSBASE"))
  testthat::expect_identical(dsAnalysis:::internal_catalogue_name("dsbase", catalogue), "dsBase")
  testthat::expect_equal(length(catalogue), 2L)
})

test_that("internal_catalogue_name returns NA_character_ for NA and for an empty-string package name", {
  catalogue <- list(dsBase = list(input = list(cran_link = "")))
  testthat::expect_identical(dsAnalysis:::internal_catalogue_name(NA_character_, catalogue), NA_character_)
  testthat::expect_identical(dsAnalysis:::internal_catalogue_name("", catalogue), NA_character_)
})
