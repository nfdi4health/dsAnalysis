test_that("internal_variable_facts reports a factor variable as categorical with its levels and a NULL range", {
  setup <- local_cnsim_project()
  conns <- setup$conns
  res <- dsAnalysis:::internal_variable_facts("D$GENDER", datasources = conns)
  testthat::expect_identical(res$type, "categorical")
  testthat::expect_identical(sort(as.character(res$levels)), c("0", "1"))
  testthat::expect_null(res$range)
  testthat::expect_identical(names(res), c("type", "levels", "range"))
})

test_that("internal_variable_facts reports a numeric variable as continuous with a two-decimal 5%/95% range and no levels", {
  setup <- local_cnsim_project()
  conns <- setup$conns
  res <- dsAnalysis:::internal_variable_facts("D$LAB_TSC", datasources = conns)
  testthat::expect_identical(res$type, "continuous")
  testthat::expect_null(res$levels)
  testthat::expect_length(res$range, 2L)
  testthat::expect_type(res$range, "double")
  testthat::expect_true(res$range[1] < res$range[2])
  testthat::expect_equal(res$range, round(res$range, 2))
  testthat::expect_null(names(res$range))
})

test_that("internal_variable_facts reports an unknown variable as type other with NULL levels and NULL range", {
  setup <- local_cnsim_project()
  conns <- setup$conns
  res <- dsAnalysis:::internal_variable_facts("D$NOT_A_VARIABLE", datasources = conns)
  testthat::expect_identical(res$type, "other")
  testthat::expect_null(res$levels)
  testthat::expect_null(res$range)
})

test_that("internal_variable_facts reports a data.frame as type other with NULL levels and NULL range", {
  setup <- local_cnsim_project()
  conns <- setup$conns
  res <- dsAnalysis:::internal_variable_facts("D", datasources = conns)
  testthat::expect_identical(res$type, "other")
  testthat::expect_null(res$levels)
  testthat::expect_null(res$range)
})

test_that("internal_variable_facts uses only the given connections and returns the same type for a single server as for all three", {
  setup <- local_cnsim_project()
  conns <- setup$conns
  one <- dsAnalysis:::internal_variable_facts("D$LAB_TSC", datasources = conns[1])
  all3 <- dsAnalysis:::internal_variable_facts("D$LAB_TSC", datasources = conns)
  testthat::expect_identical(one$type, "continuous")
  testthat::expect_identical(all3$type, "continuous")
  testthat::expect_length(one$range, 2L)
  testthat::expect_false(identical(one$range, all3$range))
})

test_that("internal_variable_facts reports an integer variable as integer with a whole-number range", {
  setup <- local_cnsim_project()
  conns <- setup$conns
  dsBaseClient::ds.asInteger("D$LAB_TSC", newobj = "tsc_int", datasources = conns)
  res <- dsAnalysis:::internal_variable_facts("tsc_int", datasources = conns)
  testthat::expect_identical(res$type, "integer")
  testthat::expect_null(res$levels)
  testthat::expect_length(res$range, 2L)
  testthat::expect_equal(res$range, round(res$range))
  testthat::expect_true(res$range[1] < res$range[2])
})

test_that("internal_variable_facts prints nothing when describing a continuous variable", {
  setup <- local_cnsim_project()
  conns <- setup$conns
  printed <- utils::capture.output(res <- dsAnalysis:::internal_variable_facts("D$LAB_TSC", datasources = conns))
  testthat::expect_identical(printed, character(0))
  testthat::expect_identical(res$type, "continuous")
})
