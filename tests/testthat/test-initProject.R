test_that("initProject without a path stops outside an interactive session and creates nothing", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  err <- testthat::expect_error(dsAnalysis::initProject(name = "my-analysis"))
  testthat::expect_match(err$message, "No project created")
  testthat::expect_null(err$call)

  for (bad in list(NULL, "", c("a", "b"), 42)) {
    testthat::expect_error(dsAnalysis::initProject(path = bad, name = "my-analysis"), "provide the folder")
  }
  testthat::expect_error(dsAnalysis::initProject(path = tmp), "provide a path name for the project")

  testthat::expect_length(list.files(tmp, all.files = TRUE, no.. = TRUE), 0)
})

test_that("initProject without a path asks before using the home directory", {
  home <- withr::local_tempdir()
  withr::local_envvar(HOME = home, R_USER = home)
  asked <- NULL
  testthat::local_mocked_bindings(internal_confirm = function(question){
    asked <<- question
    FALSE
  })

  testthat::expect_error(dsAnalysis::initProject(name = "my-analysis"), "No project created")
  testthat::expect_equal(asked, paste0("Create the project in ", file.path(fs::path_expand("~"), "my-analysis"), "?"))
  testthat::expect_false(dir.exists(file.path(home, "my-analysis")))

  #### a yes goes on with the home directory: here up to the existing-folder check
  dir.create(file.path(home, "my-analysis"))
  testthat::local_mocked_bindings(internal_confirm = function(question) TRUE)
  testthat::expect_error(dsAnalysis::initProject(name = "my-analysis"), "would overwrite an existing")
})

test_that("internal_confirm never asks outside an interactive session", {
  testthat::expect_false(internal_confirm("Create it?"))
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
