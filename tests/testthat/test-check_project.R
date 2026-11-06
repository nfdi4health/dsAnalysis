test_that("check_project messages that there is nothing to check and returns NULL when the project has no renv.lock", {
  tmp_proj <- withr::local_tempdir("no-renv-")
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  testthat::expect_false(file.exists(file.path(tmp_proj, "renv.lock")))
  testthat::expect_message(res <- dsAnalysis::check_project(),
                           "This project doesn't use renv (no renv.lock), so there is nothing to check.",
                           fixed = TRUE)
  testthat::expect_null(res)
})

test_that("check_project reports a synchronized project and returns the comparison invisibly", {
  tmp_proj <- withr::local_tempdir("renv-sync-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  sync_ok <- list(synchronized = TRUE,
                  recorded = character(0),
                  used_not_installed = character(0),
                  recorded_not_installed = character(0),
                  used_not_recorded = character(0),
                  other_version = character(0),
                  unexplained = FALSE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) sync_ok)
  testthat::expect_message(res <- dsAnalysis::check_project(),
                           "All good: the installed packages match renv.lock.",
                           fixed = TRUE)
  testthat::expect_identical(res, sync_ok)
  testthat::expect_true(withVisible(dsAnalysis::check_project())$visible == FALSE) |> suppressMessages()
})

test_that("check_project lists used-but-not-installed packages, splitting recorded ones (restore) from unrecorded ones (install)", {
  tmp_proj <- withr::local_tempdir("renv-missing-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  sync_bad <- list(synchronized = FALSE,
                   recorded = c("dplyr"),
                   used_not_installed = c("dplyr", "dsBaseClient"),
                   recorded_not_installed = character(0),
                   used_not_recorded = character(0),
                   other_version = character(0),
                   unexplained = FALSE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) sync_bad)
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "none"))
  testthat::expect_true(any(grepl("Used by the project but not installed: dplyr, dsBaseClient.", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("renv::restore() installs dplyr in the versions recorded in renv.lock.", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("dsBaseClient must be installed first: install_dsPackage() for DataSHIELD packages, renv::install() for others.", msgs, fixed = TRUE)))
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project reports recorded-not-installed, used-not-recorded, other-version and unexplained differences in one run", {
  tmp_proj <- withr::local_tempdir("renv-diff-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  sync_bad <- list(synchronized = FALSE,
                   recorded = c("jsonlite", "glue"),
                   used_not_installed = character(0),
                   recorded_not_installed = c("jsonlite"),
                   used_not_recorded = c("stringr"),
                   other_version = c("glue"),
                   unexplained = TRUE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) sync_bad)
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "none"))
  testthat::expect_true(any(grepl("Recorded in renv.lock but not installed: jsonlite.", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("Used by the project but not recorded in renv.lock: stringr.", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("Installed in another version than recorded in renv.lock: glue.", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("renv reports other differences", msgs, fixed = TRUE)))
  testthat::expect_false(any(grepl("All good", msgs, fixed = TRUE)))
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project(fix = \"restore\") calls renv::restore on the project and reports that everything matches afterwards", {
  tmp_proj <- withr::local_tempdir("renv-restore-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  restore_calls <- new.env(parent = emptyenv())
  restore_calls$n <- 0L
  restore_calls$project <- NULL
  restore_calls$prompt <- NULL
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  sync_bad <- list(synchronized = FALSE,
                   recorded = "jsonlite",
                   used_not_installed = character(0),
                   recorded_not_installed = "jsonlite",
                   used_not_recorded = character(0),
                   other_version = character(0),
                   unexplained = FALSE)
  sync_ok <- list(synchronized = TRUE,
                  recorded = "jsonlite",
                  used_not_installed = character(0),
                  recorded_not_installed = character(0),
                  used_not_recorded = character(0),
                  other_version = character(0),
                  unexplained = FALSE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) {
                                    calls$n <- calls$n + 1L
                                    if (calls$n == 1L) sync_bad else sync_ok
                                  })
  testthat::local_mocked_bindings(restore = function(project, prompt, ...) {
                                    restore_calls$n <- restore_calls$n + 1L
                                    restore_calls$project <- project
                                    restore_calls$prompt <- prompt
                                    invisible(TRUE)
                                  }, .package = "renv")
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "restore"))
  testthat::expect_equal(restore_calls$n, 1L)
  testthat::expect_equal(restore_calls$project, tmp_proj)
  testthat::expect_false(restore_calls$prompt)
  testthat::expect_true(any(grepl("All good now: the installed packages match renv.lock.", msgs, fixed = TRUE)))
  testthat::expect_equal(calls$n, 2L)
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project(fix = \"snapshot\") calls renv::snapshot and warns that the project is still not in sync when it stays unsynchronized", {
  tmp_proj <- withr::local_tempdir("renv-snapshot-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  snap <- new.env(parent = emptyenv())
  snap$n <- 0L
  snap$project <- NULL
  sync_bad <- list(synchronized = FALSE,
                   recorded = character(0),
                   used_not_installed = character(0),
                   recorded_not_installed = character(0),
                   used_not_recorded = "stringr",
                   other_version = character(0),
                   unexplained = FALSE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) sync_bad)
  testthat::local_mocked_bindings(snapshot = function(project, prompt, ...) {
                                    snap$n <- snap$n + 1L
                                    snap$project <- project
                                    invisible(TRUE)
                                  }, .package = "renv")
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "snapshot"))
  testthat::expect_equal(snap$n, 1L)
  testthat::expect_equal(snap$project, tmp_proj)
  testthat::expect_true(any(grepl("Still not in sync. Run check_project() again to see what is left.", msgs, fixed = TRUE)))
  testthat::expect_false(any(grepl("All good now", msgs, fixed = TRUE)))
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project(fix = \"none\") neither restores nor snapshots and prints no follow-up sync message", {
  tmp_proj <- withr::local_tempdir("renv-none-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  lock_before <- readLines(file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  counters <- new.env(parent = emptyenv())
  counters$compare <- 0L
  counters$restore <- 0L
  counters$snapshot <- 0L
  sync_bad <- list(synchronized = FALSE,
                   recorded = character(0),
                   used_not_installed = character(0),
                   recorded_not_installed = character(0),
                   used_not_recorded = "stringr",
                   other_version = character(0),
                   unexplained = FALSE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) {
                                    counters$compare <- counters$compare + 1L
                                    sync_bad
                                  })
  testthat::local_mocked_bindings(restore = function(...) counters$restore <- counters$restore + 1L,
                                  snapshot = function(...) counters$snapshot <- counters$snapshot + 1L,
                                  .package = "renv")
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "none"))
  testthat::expect_equal(counters$restore, 0L)
  testthat::expect_equal(counters$snapshot, 0L)
  testthat::expect_equal(counters$compare, 1L)
  testthat::expect_false(any(grepl("All good now", msgs, fixed = TRUE)))
  testthat::expect_false(any(grepl("Still not in sync", msgs, fixed = TRUE)))
  testthat::expect_identical(readLines(file.path(tmp_proj, "renv.lock")), lock_before)
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project without renv.lock returns NULL invisibly and never calls internal_renv_compare, renv::restore or renv::snapshot", {
  tmp_proj <- withr::local_tempdir("no-renv-calls-")
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  counters <- new.env(parent = emptyenv())
  counters$compare <- 0L
  counters$restore <- 0L
  counters$snapshot <- 0L
  testthat::local_mocked_bindings(internal_renv_compare = function(project) {
    counters$compare <- counters$compare + 1L
    list(synchronized = TRUE, recorded = character(0), used_not_installed = character(0),
         recorded_not_installed = character(0), used_not_recorded = character(0),
         other_version = character(0), unexplained = FALSE)
  })
  testthat::local_mocked_bindings(restore = function(...) counters$restore <- counters$restore + 1L,
                                  snapshot = function(...) counters$snapshot <- counters$snapshot + 1L,
                                  .package = "renv")
  vis <- NULL
  testthat::expect_message(vis <- withVisible(dsAnalysis::check_project()),
                           "nothing to check", fixed = TRUE)
  testthat::expect_false(vis$visible)
  testthat::expect_null(vis$value)
  testthat::expect_equal(counters$compare, 0L)
  testthat::expect_equal(counters$restore, 0L)
  testthat::expect_equal(counters$snapshot, 0L)
})

test_that("check_project on a synchronized project calls internal_renv_compare once and emits only the all-good message", {
  tmp_proj <- withr::local_tempdir("renv-sync-once-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  counters <- new.env(parent = emptyenv())
  counters$compare <- 0L
  counters$restore <- 0L
  counters$snapshot <- 0L
  sync_ok <- list(synchronized = TRUE, recorded = c("glue"), used_not_installed = character(0),
                  recorded_not_installed = character(0), used_not_recorded = character(0),
                  other_version = character(0), unexplained = FALSE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) {
    counters$compare <- counters$compare + 1L
    sync_ok
  })
  testthat::local_mocked_bindings(restore = function(...) counters$restore <- counters$restore + 1L,
                                  snapshot = function(...) counters$snapshot <- counters$snapshot + 1L,
                                  .package = "renv")
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "restore"))
  testthat::expect_equal(length(msgs), 1L)
  testthat::expect_true(grepl("All good: the installed packages match renv.lock.", msgs[1], fixed = TRUE))
  testthat::expect_equal(counters$compare, 1L)
  testthat::expect_equal(counters$restore, 0L)
  testthat::expect_equal(counters$snapshot, 0L)
  testthat::expect_identical(res, sync_ok)
})

test_that("check_project omits the restore hint when no used-but-not-installed package is recorded in renv.lock", {
  tmp_proj <- withr::local_tempdir("renv-only-unrecorded-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  sync_bad <- list(synchronized = FALSE,
                   recorded = c("glue"),
                   used_not_installed = c("dsBaseClient"),
                   recorded_not_installed = character(0),
                   used_not_recorded = character(0),
                   other_version = character(0),
                   unexplained = FALSE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) sync_bad)
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "none"))
  testthat::expect_equal(length(msgs), 2L)
  testthat::expect_true(any(grepl("Used by the project but not installed: dsBaseClient.", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("dsBaseClient must be installed first:", msgs, fixed = TRUE)))
  testthat::expect_false(any(grepl("renv::restore() installs", msgs, fixed = TRUE)))
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project omits the install-first hint when all used-but-not-installed packages are recorded in renv.lock", {
  tmp_proj <- withr::local_tempdir("renv-only-recorded-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  sync_bad <- list(synchronized = FALSE,
                   recorded = c("dplyr", "glue"),
                   used_not_installed = c("dplyr", "glue"),
                   recorded_not_installed = character(0),
                   used_not_recorded = character(0),
                   other_version = character(0),
                   unexplained = FALSE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) sync_bad)
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "none"))
  testthat::expect_equal(length(msgs), 2L)
  testthat::expect_true(any(grepl("Used by the project but not installed: dplyr, glue.", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("renv::restore() installs dplyr, glue in the versions recorded in renv.lock.", msgs, fixed = TRUE)))
  testthat::expect_false(any(grepl("must be installed first", msgs, fixed = TRUE)))
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project reports only the unexplained-differences message when no category of difference is listed", {
  tmp_proj <- withr::local_tempdir("renv-unexplained-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  sync_bad <- list(synchronized = FALSE,
                   recorded = character(0),
                   used_not_installed = character(0),
                   recorded_not_installed = character(0),
                   used_not_recorded = character(0),
                   other_version = character(0),
                   unexplained = TRUE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) sync_bad)
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "none"))
  testthat::expect_equal(length(msgs), 1L)
  testthat::expect_true(grepl("renv reports other differences", msgs[1], fixed = TRUE))
  testthat::expect_true(grepl("renv::status() shows the details.", msgs[1], fixed = TRUE))
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project(fix = \"snapshot\") runs the snapshot through internal_renv_quietly, does not call renv::restore and reports the project in sync afterwards", {
  tmp_proj <- withr::local_tempdir("renv-snapshot-ok-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  state <- new.env(parent = emptyenv())
  state$compare <- 0L
  state$quietly <- 0L
  state$snapshot <- 0L
  state$restore <- 0L
  state$prompt <- NULL
  sync_bad <- list(synchronized = FALSE, recorded = character(0), used_not_installed = character(0),
                   recorded_not_installed = character(0), used_not_recorded = "stringr",
                   other_version = character(0), unexplained = FALSE)
  sync_ok <- list(synchronized = TRUE, recorded = "stringr", used_not_installed = character(0),
                  recorded_not_installed = character(0), used_not_recorded = character(0),
                  other_version = character(0), unexplained = FALSE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) {
    state$compare <- state$compare + 1L
    if (state$compare == 1L) sync_bad else sync_ok
  },
  internal_renv_quietly = function(expr) {
    state$quietly <- state$quietly + 1L
    invisible(expr)
  })
  testthat::local_mocked_bindings(snapshot = function(project, prompt, ...) {
    state$snapshot <- state$snapshot + 1L
    state$prompt <- prompt
    invisible(TRUE)
  },
  restore = function(...) state$restore <- state$restore + 1L,
  .package = "renv")
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "snapshot"))
  testthat::expect_equal(state$quietly, 1L)
  testthat::expect_equal(state$snapshot, 1L)
  testthat::expect_equal(state$restore, 0L)
  testthat::expect_false(state$prompt)
  testthat::expect_equal(state$compare, 2L)
  testthat::expect_true(any(grepl("All good now: the installed packages match renv.lock.", msgs, fixed = TRUE)))
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project ignores an unknown fix value: it reports the differences, calls no renv action and compares only once", {
  tmp_proj <- withr::local_tempdir("renv-unknown-fix-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  counters <- new.env(parent = emptyenv())
  counters$compare <- 0L
  counters$restore <- 0L
  counters$snapshot <- 0L
  sync_bad <- list(synchronized = FALSE, recorded = "glue", used_not_installed = character(0),
                   recorded_not_installed = character(0), used_not_recorded = character(0),
                   other_version = "glue", unexplained = FALSE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) {
    counters$compare <- counters$compare + 1L
    sync_bad
  })
  testthat::local_mocked_bindings(restore = function(...) counters$restore <- counters$restore + 1L,
                                  snapshot = function(...) counters$snapshot <- counters$snapshot + 1L,
                                  .package = "renv")
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "something-else"))
  testthat::expect_equal(length(msgs), 1L)
  testthat::expect_true(grepl("Installed in another version than recorded in renv.lock: glue.", msgs[1], fixed = TRUE))
  testthat::expect_equal(counters$compare, 1L)
  testthat::expect_equal(counters$restore, 0L)
  testthat::expect_equal(counters$snapshot, 0L)
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project returns the unsynchronized comparison invisibly", {
  tmp_proj <- withr::local_tempdir("renv-invisible-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  sync_bad <- list(synchronized = FALSE, recorded = character(0), used_not_installed = character(0),
                   recorded_not_installed = character(0), used_not_recorded = "stringr",
                   other_version = character(0), unexplained = FALSE)
  testthat::local_mocked_bindings(internal_renv_compare = function(project) sync_bad)
  vis <- NULL
  msgs <- testthat::capture_messages(vis <- withVisible(dsAnalysis::check_project(fix = "none")))
  testthat::expect_false(vis$visible)
  testthat::expect_identical(vis$value, sync_bad)
  testthat::expect_equal(length(msgs), 1L)
})
