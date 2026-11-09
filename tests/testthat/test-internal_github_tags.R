test_that("internal_github_tags returns the tag names from the JSON the connection yields", {
  tmp <- withr::local_tempdir()
  json_file <- file.path(tmp, "tags.json")
  writeLines('[{"name":"v1.0.0"},{"name":"v0.9.1"},{"name":"v0.9.0"}]', json_file)
  testthat::local_mocked_bindings(url = function(description, ...) base::file(json_file, open = "r"), .package = "base")
  res <- dsAnalysis:::internal_github_tags("datashield/dsBase")
  testthat::expect_identical(res, c("v1.0.0", "v0.9.1", "v0.9.0"))
  testthat::expect_length(res, 3L)
})

test_that("internal_github_tags errors with the repo name in the message when the connection cannot be read", {
  testthat::local_mocked_bindings(url = function(description, ...) stop("no network"), .package = "base")
  testthat::expect_error(dsAnalysis:::internal_github_tags("datashield/doesnotexist"),
                         "Could not read the versions (tags) of https://github.com/datashield/doesnotexist. Check the repository name, or give a ref instead of a version.",
                         fixed = TRUE)
})

test_that("internal_github_tags errors when the JSON has no name field (GitHub error object)", {
  tmp <- withr::local_tempdir()
  json_file <- file.path(tmp, "err.json")
  writeLines('{"message":"Not Found","status":"404"}', json_file)
  testthat::local_mocked_bindings(url = function(description, ...) base::file(json_file, open = "r"), .package = "base")
  testthat::expect_error(dsAnalysis:::internal_github_tags("datashield/missing"),
                         "Could not read the versions (tags) of https://github.com/datashield/missing",
                         fixed = TRUE)
})

test_that("internal_github_tags requests the repo's tags endpoint with per_page=100 and the GitHub Accept header", {
  tmp <- withr::local_tempdir()
  json_file <- file.path(tmp, "tags.json")
  writeLines('[{"name":"1.2.3"}]', json_file)
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(url = function(description, ..., headers = NULL) {
    seen$description <- description
    seen$headers <- headers
    base::file(json_file, open = "r")
  }, .package = "base")
  withr::local_envvar(GITHUB_PAT = "")
  testthat::expect_identical(dsAnalysis:::internal_github_tags("datashield/dsBase"), "1.2.3")
  testthat::expect_identical(seen$description,
                             "https://api.github.com/repos/datashield/dsBase/tags?per_page=100")
  testthat::expect_identical(seen$headers, c(Accept = "application/vnd.github+json"))
})

test_that("internal_github_tags adds an Authorization token header when GITHUB_PAT is set", {
  tmp <- withr::local_tempdir()
  json_file <- file.path(tmp, "tags.json")
  writeLines('[{"name":"6.3.0"}]', json_file)
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(url = function(description, ..., headers = NULL) {
    seen$headers <- headers
    base::file(json_file, open = "r")
  }, .package = "base")
  withr::local_envvar(GITHUB_PAT = "abc123")
  testthat::expect_identical(dsAnalysis:::internal_github_tags("datashield/dsBase"), "6.3.0")
  testthat::expect_identical(seen$headers,
                             c(Accept = "application/vnd.github+json",
                               Authorization = "token abc123"))
})

test_that("internal_github_tags errors when the repository has no tags (empty JSON array)", {
  tmp <- withr::local_tempdir()
  json_file <- file.path(tmp, "empty.json")
  writeLines('[]', json_file)
  testthat::local_mocked_bindings(url = function(description, ...) base::file(json_file, open = "r"), .package = "base")
  testthat::expect_error(dsAnalysis:::internal_github_tags("datashield/notags"),
                         "Could not read the versions (tags) of https://github.com/datashield/notags",
                         fixed = TRUE)
})
