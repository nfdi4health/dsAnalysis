test_that("internal_opal_dictionary returns NULL for a non-Opal (DSLite) connection", {
  setup <- local_cnsim_project()
  testthat::expect_null(dsAnalysis:::internal_opal_dictionary(setup$conns[[1]], "CNSIM.CNSIM1"))
})
