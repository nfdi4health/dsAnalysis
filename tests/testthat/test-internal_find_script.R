test_that("internal_find_script errors with the developer-team message when the script does not exist in the package templates", {
  testthat::expect_error(
    dsAnalysis:::internal_find_script("definitely_not_a_template_file_12345.R"),
    "Could not find the file: definitely_not_a_template_file_12345\\.R\\. Please contact the developer team\\.",
    fixed = FALSE
  )
  err <- testthat::expect_error(dsAnalysis:::internal_find_script("definitely_not_a_template_file_12345.R"))
  testthat::expect_identical(err$message,
                             "Could not find the file: definitely_not_a_template_file_12345.R. Please contact the developer team.")
  testthat::expect_null(conditionCall(err))
})

test_that("internal_find_script errors when the package has no templates directory at all", {
  lib <- withr::local_tempdir()
  pkg <- file.path(lib, "pkgWithoutTemplates")
  dir.create(file.path(pkg, "Meta"), recursive = TRUE)
  writeLines(c("Package: pkgWithoutTemplates", "Version: 0.0.1", "Type: Package",
               "Title: Test", "Description: Test.", "License: MIT", "Author: t",
               "Maintainer: t <t@t.org>", "Built: R 4.0.0; ; now; unix"),
             file.path(pkg, "DESCRIPTION"))
  withr::local_libpaths(lib, action = "prefix")
  testthat::expect_error(
    dsAnalysis:::internal_find_script("anything.R", package = "pkgWithoutTemplates"),
    "Could not find the file: anything.R. Please contact the developer team.",
    fixed = TRUE
  )
})

test_that("internal_find_script returns the existing path of a template file inside an installed package", {
  lib <- withr::local_tempdir()
  pkg <- file.path(lib, "pkgWithTemplates")
  dir.create(file.path(pkg, "templates"), recursive = TRUE)
  writeLines(c("Package: pkgWithTemplates", "Version: 0.0.1", "Type: Package",
               "Title: Test", "Description: Test.", "License: MIT", "Author: t",
               "Maintainer: t <t@t.org>", "Built: R 4.0.0; ; now; unix"),
             file.path(pkg, "DESCRIPTION"))
  writeLines("#### template content", file.path(pkg, "templates", "my_script.R"))
  withr::local_libpaths(lib, action = "prefix")
  res <- dsAnalysis:::internal_find_script("my_script.R", package = "pkgWithTemplates")
  testthat::expect_type(res, "character")
  testthat::expect_length(res, 1L)
  testthat::expect_true(file.exists(res))
  testthat::expect_identical(basename(res), "my_script.R")
  testthat::expect_identical(basename(dirname(res)), "templates")
  testthat::expect_identical(readLines(res), "#### template content")
})

test_that("internal_find_script errors for a package that is not installed", {
  testthat::expect_error(
    dsAnalysis:::internal_find_script("some_script.R", package = "thisPackageDoesNotExist12345"),
    "Could not find the file: some_script.R. Please contact the developer team.",
    fixed = TRUE
  )
})
