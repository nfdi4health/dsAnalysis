test_that("internal_resolve_dsPackage errors with a catalogue hint when the package is not in the catalogue and no source is given", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(github = "datashield/dsBase", client = "dsBaseClient")))
  testthat::expect_error(dsAnalysis:::internal_resolve_dsPackage("dsUnknown"),
                         "dsUnknown is not in the DataSHIELD package catalogue. If it is on GitHub, give its repository with source = \"owner/repo\".",
                         fixed = TRUE)
})

test_that("internal_resolve_dsPackage derives the client repository from the server's owner when the client is unknown to the catalogue", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(github = "datashield/dsBase", client = "dsBaseClient")),
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) paste0(name, "@", src$repo))
  res <- dsAnalysis:::internal_resolve_dsPackage("dsMine", source = "myorg/dsMine", client = "dsMineClient")
  testthat::expect_identical(res$server$spec, "dsMine@myorg/dsMine")
  testthat::expect_identical(res$client$package, "dsMineClient")
  testthat::expect_identical(res$client$spec, "dsMineClient@myorg/dsMineClient")
})

test_that("internal_resolve_dsPackage errors when the client is unknown and the server package comes from CRAN without a repo", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(cran = TRUE, github = NA_character_)),
                                  internal_catalogue_source = function(entry) list(cran = TRUE, repo = NA_character_),
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) name)
  testthat::expect_error(dsAnalysis:::internal_resolve_dsPackage("dsBase", client = "dsBaseClient"),
                         "No CRAN or GitHub location is known for dsBaseClient. Give client_source = \"owner/repo\".",
                         fixed = TRUE)
})

test_that("internal_resolve_dsPackage passes the server version on to the client when client_version is not given", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(github = "datashield/dsBase", client = "dsBaseClient"),
                                                                          dsBaseClient = list(github = "datashield/dsBaseClient")),
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) paste0(name, "-", if (is.null(version)) "latest" else version))
  res <- dsAnalysis:::internal_resolve_dsPackage("dsBase", version = "6.3.0")
  testthat::expect_identical(res$server$spec, "dsBase-6.3.0")
  testthat::expect_identical(res$client$spec, "dsBaseClient-6.3.0")
})

test_that("internal_resolve_dsPackage uses client_version instead of the server version when both are given", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(github = "datashield/dsBase", client = "dsBaseClient"),
                                                                          dsBaseClient = list(github = "datashield/dsBaseClient")),
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) paste0(name, "-", if (is.null(version)) "latest" else version))
  res <- dsAnalysis:::internal_resolve_dsPackage("dsBase", version = "6.3.0", client_version = "6.2.0")
  testthat::expect_identical(res$server$spec, "dsBase-6.3.0")
  testthat::expect_identical(res$client$spec, "dsBaseClient-6.2.0")
})
