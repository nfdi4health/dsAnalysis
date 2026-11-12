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

test_that("initProject checks name before path, so a missing name errors even with a bad path", {
  err <- testthat::expect_error(dsAnalysis::initProject(path = 42, name = NULL))
  testthat::expect_match(err$message, "provide a path name for the project")
  testthat::expect_null(err$call)
})

test_that("initProject rejects a non-logical switch_to_proj after a confirmed home directory and creates nothing", {
  home <- withr::local_tempdir()
  withr::local_envvar(HOME = home, R_USER = home)
  testthat::local_mocked_bindings(internal_confirm = function(question) TRUE)
  err <- testthat::expect_error(dsAnalysis::initProject(name = "my-analysis", switch_to_proj = "yes"))
  testthat::expect_match(err$message, "switch_to_proj has to be logical")
  testthat::expect_null(err$call)
  testthat::expect_false(dir.exists(file.path(home, "my-analysis")))
})

test_that("initProject treats path = 'home' like the home directory when checking for an existing folder", {
  home <- withr::local_tempdir()
  withr::local_envvar(HOME = home, R_USER = home)
  dir.create(file.path(home, "taken"))
  err <- testthat::expect_error(dsAnalysis::initProject(path = "home", name = "taken"))
  testthat::expect_match(err$message, "would overwrite an existing")
  testthat::expect_match(err$message, file.path(fs::path_expand("~"), "taken"), fixed = TRUE)
  testthat::expect_equal(list.files(file.path(home, "taken")), character(0))
})

test_that("initProject with a path never asks for confirmation", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "taken"))
  asked <- 0L
  testthat::local_mocked_bindings(internal_confirm = function(question){
    asked <<- asked + 1L
    FALSE
  })
  testthat::expect_error(dsAnalysis::initProject(path = tmp, name = "taken"), "would overwrite an existing")
  testthat::expect_equal(asked, 0L)
})

test_that("initProject rejects a non-logical switch_to_proj before the existing-folder check", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "taken"))
  err <- testthat::expect_error(dsAnalysis::initProject(path = tmp, name = "taken", switch_to_proj = 1))
  testthat::expect_match(err$message, "switch_to_proj has to be logical")
  testthat::expect_null(err$call)
  testthat::expect_equal(list.files(tmp, all.files = TRUE, no.. = TRUE), "taken")
})
