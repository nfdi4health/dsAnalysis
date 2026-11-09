test_that("internal_catalogue_client returns the <package>Client entry when the catalogue holds it", {
  catalogue <- list(dsBase = list(input = list(cran_link = "")), dsBaseClient = list(input = list(cran_link = "")), dsSurvival = list(input = list(cran_link = "")))
  testthat::expect_identical(dsAnalysis:::internal_catalogue_client("dsBase", catalogue), "dsBaseClient")
})

test_that("internal_catalogue_client strips a trailing Base and returns the <package without Base>Client entry", {
  catalogue <- list(dsMTLBase = list(input = list(cran_link = "")), dsMTLClient = list(input = list(cran_link = "")))
  testthat::expect_identical(dsAnalysis:::internal_catalogue_client("dsMTLBase", catalogue), "dsMTLClient")
})

test_that("internal_catalogue_client strips a trailing Server and returns the <package without Server> entry", {
  catalogue <- list(dsQueryLibraryServer = list(input = list(cran_link = "")), dsQueryLibrary = list(input = list(cran_link = "")))
  testthat::expect_identical(dsAnalysis:::internal_catalogue_client("dsQueryLibraryServer", catalogue), "dsQueryLibrary")
})

test_that("internal_catalogue_client returns NA_character_ when no candidate client is in the catalogue", {
  catalogue <- list(dsBase = list(input = list(cran_link = "")), dsSurvival = list(input = list(cran_link = "")))
  res <- dsAnalysis:::internal_catalogue_client("dsBase", catalogue)
  testthat::expect_true(is.na(res))
  testthat::expect_type(res, "character")
  testthat::expect_length(res, 1L)
})

test_that("internal_catalogue_client prefers the <package>Client candidate over the Base-stripped one when both are in the catalogue", {
  catalogue <- list(dsMTLBase = list(input = list(cran_link = "")), dsMTLBaseClient = list(input = list(cran_link = "")), dsMTLClient = list(input = list(cran_link = "")))
  testthat::expect_identical(dsAnalysis:::internal_catalogue_client("dsMTLBase", catalogue), "dsMTLBaseClient")
})

test_that("internal_catalogue_client matches catalogue names case-insensitively and returns the catalogue's spelling", {
  catalogue <- list(dsbaseclient = list(input = list(cran_link = "")))
  testthat::expect_identical(dsAnalysis:::internal_catalogue_client("dsBase", catalogue), dsAnalysis:::internal_catalogue_name("dsBaseClient", catalogue))
})

test_that("internal_catalogue_client returns NA_character_ for an empty catalogue", {
  res <- dsAnalysis:::internal_catalogue_client("dsBase", list())
  testthat::expect_true(is.na(res))
  testthat::expect_length(res, 1L)
})

test_that("internal_catalogue_client never returns the package itself when the catalogue holds only that package", {
  catalogue <- list(dsQueryLibrary = list(input = list(cran_link = "")))
  res <- dsAnalysis:::internal_catalogue_client("dsQueryLibrary", catalogue)
  testthat::expect_true(is.na(res))
  testthat::expect_length(res, 1L)
})
