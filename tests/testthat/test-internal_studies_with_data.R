test_that("internal_studies_with_data drops a study in which the variable is all missing", {
  setup <- local_cnsim_project()
  conns <- setup$conns
  DSI::datashield.assign.expr(conns[1], "allNA", quote(as.numeric(D$LAB_TSC * NA)))
  DSI::datashield.assign.expr(conns[-1], "allNA", quote(as.numeric(D$LAB_TSC)))
  res <- dsAnalysis:::internal_studies_with_data("allNA", conns)
  testthat::expect_identical(res, names(conns)[-1])
  testthat::expect_length(res, 2L)
})

test_that("internal_studies_with_data returns all given servers unchanged when the variable does not exist on the servers", {
  setup <- local_cnsim_project()
  conns <- setup$conns
  res <- dsAnalysis:::internal_studies_with_data("D$DOES_NOT_EXIST", conns)
  testthat::expect_identical(res, names(conns))
  testthat::expect_length(res, 3L)
})

test_that("internal_studies_with_data works on a subset of the datasources and returns only those server names", {
  setup <- local_cnsim_project()
  conns <- setup$conns
  res <- dsAnalysis:::internal_studies_with_data("D$LAB_TSC", conns[2])
  testthat::expect_identical(res, names(conns)[2])
  testthat::expect_length(res, 1L)
})

test_that("internal_studies_with_data returns an empty character vector when the variable is all missing on every study", {
  setup <- local_cnsim_project()
  conns <- setup$conns
  DSI::datashield.assign.expr(conns, "allNA", quote(as.numeric(D$LAB_TSC * NA)))
  res <- dsAnalysis:::internal_studies_with_data("allNA", conns)
  testthat::expect_length(res, 0L)
  testthat::expect_type(res, "character")
})
