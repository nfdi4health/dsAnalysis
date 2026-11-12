test_that("initProject needs a folder and a name, and creates nothing without them", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  err <- testthat::expect_error(dsAnalysis::initProject(name = "my-analysis"))
  testthat::expect_match(err$message, "provide the folder the project is created in")
  testthat::expect_null(err$call)

  for (bad in list(NULL, "", c("a", "b"), 42)) {
    testthat::expect_error(dsAnalysis::initProject(path = bad, name = "my-analysis"), "provide the folder")
  }
  testthat::expect_error(dsAnalysis::initProject(path = tmp), "provide a path name for the project")

  testthat::expect_length(list.files(tmp, all.files = TRUE, no.. = TRUE), 0)
})

test_that("initProject stops before creating anything when the project folder exists", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "taken"))
  err <- testthat::expect_error(dsAnalysis::initProject(path = tmp, name = "taken"))
  testthat::expect_match(err$message, "would overwrite an existing")
  testthat::expect_equal(list.files(file.path(tmp, "taken")), character(0))
})
