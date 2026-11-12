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

test_that("initProject rejects a non-logical switch_to_proj and creates nothing", {
  tmp <- withr::local_tempdir()
  err <- testthat::expect_error(dsAnalysis::initProject(path = tmp, name = "my-analysis", switch_to_proj = "yes"))
  testthat::expect_match(err$message, "switch_to_proj has to be logical")
  testthat::expect_null(err$call)
  testthat::expect_length(list.files(tmp, all.files = TRUE, no.. = TRUE), 0)
})

test_that("initProject reports the expanded path in the overwrite error when path is \"home\"", {
  home_dir <- fs::path_expand("~")
  existing <- basename(list.dirs(home_dir, recursive = FALSE))[1]
  testthat::skip_if(is.na(existing) || length(existing) == 0)
  err <- testthat::expect_error(dsAnalysis::initProject(path = "home", name = existing))
  testthat::expect_match(err$message, "would overwrite an existing")
  testthat::expect_true(grepl(paste0(home_dir, "/", existing), err$message, fixed = TRUE))
})

test_that("initProject expands a tilde path in the overwrite error message", {
  home_dir <- fs::path_expand("~")
  existing <- basename(list.dirs(home_dir, recursive = FALSE))[1]
  testthat::skip_if(is.na(existing) || length(existing) == 0)
  err <- testthat::expect_error(dsAnalysis::initProject(path = "~", name = existing))
  testthat::expect_match(err$message, "would overwrite an existing")
  testthat::expect_true(grepl(paste0(home_dir, "/", existing), err$message, fixed = TRUE))
  testthat::expect_false(grepl("~", err$message, fixed = TRUE))
})

test_that("initProject rejects a logical NA for switch_to_proj only after path and name are valid", {
  tmp <- withr::local_tempdir()
  testthat::expect_error(dsAnalysis::initProject(path = tmp, name = "my-analysis", switch_to_proj = 1), "switch_to_proj has to be logical")
  testthat::expect_error(dsAnalysis::initProject(path = list(TRUE), name = "my-analysis", switch_to_proj = TRUE), "provide the folder")
  testthat::expect_length(list.files(tmp, all.files = TRUE, no.. = TRUE), 0)
})
