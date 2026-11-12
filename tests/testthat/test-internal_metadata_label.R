test_that("internal_metadata_label returns an empty string when ds.metadata() errors", {
  setup <- local_cnsim_project()
  testthat::local_mocked_bindings(ds.metadata = function(...) stop("no such variable"), .package = "dsBaseClient")
  testthat::expect_identical(dsAnalysis:::internal_metadata_label("D$DOES_NOT_EXIST", datasources = setup$conns), "")
})
