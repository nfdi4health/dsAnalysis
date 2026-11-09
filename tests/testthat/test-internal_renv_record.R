test_that("internal_renv_record messages that the project doesn't use renv and returns invisible FALSE when renv.lock is missing", {
  tmp <- withr::local_tempdir()
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp),
                           "This project doesn't use renv (no renv.lock), so nothing was recorded.",
                           fixed = TRUE)
  testthat::expect_false(res)
  testthat::expect_false(file.exists(file.path(tmp, "renv.lock")))
})

test_that("internal_renv_record calls renv::snapshot with the project and prompt = FALSE and reports being recorded when the comparison is synchronized", {
  tmp <- withr::local_tempdir()
  writeLines("{}", file.path(tmp, "renv.lock"))
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) {
    seen$project <- project
    seen$prompt <- prompt
    invisible(NULL)
  }, .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) list(synchronized = TRUE))
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp),
                           "Recorded in renv.lock.", fixed = TRUE)
  testthat::expect_true(res)
  testthat::expect_identical(seen$project, tmp)
  testthat::expect_false(seen$prompt)
})

test_that("internal_renv_record messages that something is still out of sync and returns invisible FALSE when the comparison is not synchronized", {
  tmp <- withr::local_tempdir()
  writeLines("{}", file.path(tmp, "renv.lock"))
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) invisible(NULL), .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) list(synchronized = FALSE))
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp),
                           "renv.lock was updated, but something is still out of sync: run check_project() to see what.",
                           fixed = TRUE)
  testthat::expect_false(res)
})

test_that("internal_renv_record returns its value invisibly in both the no-lockfile and the synchronized case", {
  tmp_nolock <- withr::local_tempdir()
  tmp_lock <- withr::local_tempdir()
  writeLines("{}", file.path(tmp_lock, "renv.lock"))
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) invisible(NULL), .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) list(synchronized = TRUE))
  out_nolock <- testthat::capture_output(testthat::expect_message(dsAnalysis:::internal_renv_record(tmp_nolock)))
  testthat::expect_identical(out_nolock, "")
  out_lock <- testthat::capture_output(testthat::expect_message(dsAnalysis:::internal_renv_record(tmp_lock)))
  testthat::expect_identical(out_lock, "")
})

test_that("internal_renv_record does not call renv::snapshot or internal_renv_compare when renv.lock is missing", {
  tmp <- withr::local_tempdir()
  calls <- new.env(parent = emptyenv())
  calls$snapshot <- 0L
  calls$compare <- 0L
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) {
    calls$snapshot <- calls$snapshot + 1L
    invisible(NULL)
  }, .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) {
    calls$compare <- calls$compare + 1L
    list(synchronized = TRUE)
  })
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp),
                           "nothing was recorded", fixed = TRUE)
  testthat::expect_false(res)
  testthat::expect_identical(calls$snapshot, 0L)
  testthat::expect_identical(calls$compare, 0L)
})

test_that("internal_renv_record passes the same project path to internal_renv_compare that it passes to renv::snapshot", {
  tmp <- withr::local_tempdir()
  writeLines("{}", file.path(tmp, "renv.lock"))
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) {
    seen$snapshot_project <- project
    invisible(NULL)
  }, .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) {
    seen$compare_project <- project
    list(synchronized = TRUE)
  })
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp), "Recorded in renv.lock.", fixed = TRUE)
  testthat::expect_true(res)
  testthat::expect_identical(seen$compare_project, tmp)
  testthat::expect_identical(seen$compare_project, seen$snapshot_project)
})
