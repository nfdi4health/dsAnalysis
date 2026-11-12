test_that("internal_login_tables returns an empty character vector when the login file does not exist", {
  tmp <- withr::local_tempdir()
  missing_file <- file.path(tmp, "no_such_login.R")
  res <- dsAnalysis:::internal_login_tables(missing_file)
  testthat::expect_identical(res, character(0))
  testthat::expect_length(res, 0L)
})

test_that("internal_login_tables leaves out append() calls whose table is not a plain string", {
  tmp <- withr::local_tempdir()
  login_file <- file.path(tmp, "01_DS_Login.R")
  writeLines(c("builder <- DSI::newDSLoginBuilder()",
               "builder$append(server = 'study1', url = 'https://opal1', table = Sys.getenv('TBL'))",
               "builder$append(server = 'study2', url = 'https://opal2', table = 'CNSIM.CNSIM2')"), login_file)
  res <- dsAnalysis:::internal_login_tables(login_file)
  testthat::expect_length(res, 1L)
  testthat::expect_identical(res, c(study2 = "CNSIM.CNSIM2"))
  testthat::expect_false("study1" %in% names(res))
})

test_that("internal_login_tables ignores calls that are not an append() on an object", {
  tmp <- withr::local_tempdir()
  login_file <- file.path(tmp, "01_DS_Login.R")
  writeLines(c("library(DSI)",
               "append(server = 'study9', table = 'CNSIM.CNSIM9')",
               "builder$add(server = 'study8', table = 'CNSIM.CNSIM8')",
               "builder$append(server = 'study1', table = 'CNSIM.CNSIM1')"), login_file)
  res <- dsAnalysis:::internal_login_tables(login_file)
  testthat::expect_identical(res, c(study1 = "CNSIM.CNSIM1"))
  testthat::expect_length(res, 1L)
})

test_that("internal_login_tables returns an empty character vector for a login file that parses but names no tables", {
  tmp <- withr::local_tempdir()
  login_file <- file.path(tmp, "01_DS_Login.R")
  writeLines(c("builder <- DSI::newDSLoginBuilder()",
               "logindata <- builder$build()"), login_file)
  res <- dsAnalysis:::internal_login_tables(login_file)
  testthat::expect_identical(res, character(0))
  testthat::expect_length(res, 0L)
})

test_that("internal_login_tables returns an empty character vector when the login file cannot be parsed", {
  tmp <- withr::local_tempdir()
  login_file <- file.path(tmp, "01_DS_Login.R")
  writeLines(c("builder$append(server = 'study1', table = 'CNSIM.CNSIM1'",
               "this is ) not R"), login_file)
  res <- dsAnalysis:::internal_login_tables(login_file)
  testthat::expect_identical(res, character(0))
  testthat::expect_length(res, 0L)
})

test_that("internal_login_tables finds append() calls nested inside other calls and keeps the last table for a repeated server", {
  tmp <- withr::local_tempdir()
  login_file <- file.path(tmp, "01_DS_Login.R")
  writeLines(c("builder <- DSI::newDSLoginBuilder()",
               "if (TRUE) {",
               "  builder$append(server = 'study1', table = 'CNSIM.CNSIM1')",
               "}",
               "for (i in 1) builder$append(server = 'study1', table = 'CNSIM.OTHER')"), login_file)
  res <- dsAnalysis:::internal_login_tables(login_file)
  testthat::expect_length(res, 1L)
  testthat::expect_identical(res, c(study1 = "CNSIM.OTHER"))
})

test_that("internal_login_tables reads R/01_DS_Login.R of the project by default", {
  project <- withr::local_tempdir()
  dir.create(file.path(project, "R"), recursive = TRUE)
  writeLines(c("builder <- DSI::newDSLoginBuilder()",
               "builder$append(server = 'sim1', url = 'https://opal1', table = 'CNSIM.CNSIM1')"),
             file.path(project, "R", "01_DS_Login.R"))
  testthat::local_mocked_bindings(here = function(...) file.path(project, ...), .package = "here")
  res <- dsAnalysis:::internal_login_tables()
  testthat::expect_identical(res, c(sim1 = "CNSIM.CNSIM1"))
  testthat::expect_length(res, 1L)
})

test_that("internal_login_tables leaves out an append() call that gives no server argument", {
  tmp <- withr::local_tempdir()
  login_file <- file.path(tmp, "01_DS_Login.R")
  writeLines(c("builder$append(url = 'https://opal1', table = 'CNSIM.CNSIM1')",
               "builder$append(server = 'study2', table = 'CNSIM.CNSIM2')"), login_file)
  res <- dsAnalysis:::internal_login_tables(login_file)
  testthat::expect_identical(res, c(study2 = "CNSIM.CNSIM2"))
  testthat::expect_length(res, 1L)
})
