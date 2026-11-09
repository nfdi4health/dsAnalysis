test_that("internal_install_spec returns repo@ref when a ref is given, ignoring cran and version", {
  testthat::expect_identical(dsAnalysis:::internal_install_spec("dsBase", list(cran = TRUE, repo = "datashield/dsBase"), ref = "feature-x"),
                             "datashield/dsBase@feature-x")
  testthat::expect_identical(dsAnalysis:::internal_install_spec("dsBase", list(cran = FALSE, repo = "datashield/dsBase"), version = "1.0.0", ref = "abc123"),
                             "datashield/dsBase@abc123")
})

test_that("internal_install_spec errors when a ref is given but the repo is NA", {
  testthat::expect_error(dsAnalysis:::internal_install_spec("mypkg", list(cran = TRUE, repo = NA_character_), ref = "main"),
                         "A ref needs the GitHub repository of mypkg: give source = \"owner/repo\".",
                         fixed = TRUE)
})

test_that("internal_install_spec returns the package name for a CRAN source and package@version with a version", {
  testthat::expect_identical(dsAnalysis:::internal_install_spec("dsBaseClient", list(cran = TRUE, repo = NA_character_)),
                             "dsBaseClient")
  testthat::expect_identical(dsAnalysis:::internal_install_spec("dsBaseClient", list(cran = TRUE, repo = "datashield/dsBaseClient"), version = "6.3.0"),
                             "dsBaseClient@6.3.0")
})

test_that("internal_install_spec errors when the source is neither CRAN nor a known repo", {
  testthat::expect_error(dsAnalysis:::internal_install_spec("unknownpkg", list(cran = FALSE, repo = NA_character_)),
                         "No CRAN or GitHub location is known for unknownpkg. Give source = \"owner/repo\".",
                         fixed = TRUE)
})

test_that("internal_install_spec returns the newest release tag when no version is given", {
  testthat::local_mocked_bindings(internal_github_tags = function(repo) c("v1.2.0", "v1.10.0", "v1.9.3", "v2.0.0-rc1", "main-snapshot"), .package = "dsAnalysis")
  testthat::expect_identical(dsAnalysis:::internal_install_spec("dsBase", list(cran = FALSE, repo = "datashield/dsBase")),
                             "datashield/dsBase@v1.10.0")
})

test_that("internal_install_spec messages and returns the bare repo when no release tags exist and no version is given", {
  testthat::local_mocked_bindings(internal_github_tags = function(repo) c("nightly", "v2.0.0-rc1"), .package = "dsAnalysis")
  testthat::expect_message(res <- dsAnalysis:::internal_install_spec("dsBase", list(cran = FALSE, repo = "datashield/dsBase")),
                           "No released version of dsBase found on GitHub; installing its default branch.",
                           fixed = TRUE)
  testthat::expect_identical(res, "datashield/dsBase")
})

test_that("internal_install_spec falls back to the default branch with a message when the tag lookup fails and no version is given", {
  testthat::local_mocked_bindings(internal_github_tags = function(repo) stop("no network"), .package = "dsAnalysis")
  testthat::expect_message(res <- dsAnalysis:::internal_install_spec("dsBase", list(cran = FALSE, repo = "datashield/dsBase")),
                           "No released version of dsBase found on GitHub; installing its default branch.",
                           fixed = TRUE)
  testthat::expect_identical(res, "datashield/dsBase")
})

test_that("internal_install_spec matches a requested version to its tag with or without a leading v", {
  testthat::local_mocked_bindings(internal_github_tags = function(repo) c("v1.2.0", "1.1.0", "v1.0.0"), .package = "dsAnalysis")
  testthat::expect_identical(dsAnalysis:::internal_install_spec("dsBase", list(cran = FALSE, repo = "datashield/dsBase"), version = "1.2.0"),
                             "datashield/dsBase@v1.2.0")
  testthat::expect_identical(dsAnalysis:::internal_install_spec("dsBase", list(cran = FALSE, repo = "datashield/dsBase"), version = "1.1.0"),
                             "datashield/dsBase@1.1.0")
})

test_that("internal_install_spec errors listing the available versions newest first when the requested version has no tag", {
  testthat::local_mocked_bindings(internal_github_tags = function(repo) c("v1.0.0", "v1.2.0", "v1.1.0", "nightly"), .package = "dsAnalysis")
  testthat::expect_error(dsAnalysis:::internal_install_spec("dsBase", list(cran = FALSE, repo = "datashield/dsBase"), version = "9.9.9"),
                         "Version 9.9.9 of dsBase was not found on GitHub (datashield/dsBase). Available versions: 1.2.0, 1.1.0, 1.0.0",
                         fixed = TRUE)
})

test_that("internal_install_spec propagates the tag lookup error when a version is requested", {
  testthat::local_mocked_bindings(internal_github_tags = function(repo) stop("Could not read the versions (tags) of https://github.com/datashield/dsBase"), .package = "dsAnalysis")
  testthat::expect_error(dsAnalysis:::internal_install_spec("dsBase", list(cran = FALSE, repo = "datashield/dsBase"), version = "1.0.0"),
                         "Could not read the versions (tags) of https://github.com/datashield/dsBase",
                         fixed = TRUE)
})
