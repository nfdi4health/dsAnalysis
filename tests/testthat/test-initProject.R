test_that("initProject errors when name is NULL", {
  testthat::expect_error(dsAnalysis::initProject(path = withr::local_tempdir()),
                         "Please provide a path name for the project to be created.",
                         fixed = TRUE)
})

test_that("initProject errors when switch_to_proj is not logical", {
  tmp <- withr::local_tempdir()
  testthat::expect_error(dsAnalysis::initProject(path = tmp, name = "proj", switch_to_proj = "yes"),
                         "switch_to_proj has to be logical, i.e. either TRUE or FALSE.",
                         fixed = TRUE)
  testthat::expect_false(dir.exists(file.path(tmp, "proj")))
})

test_that("initProject errors without creating anything when the target directory already exists", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "existing"))
  testthat::expect_error(dsAnalysis::initProject(path = tmp, name = "existing"),
                         "would overwrite an existing",
                         fixed = TRUE)
  testthat::expect_identical(list.files(file.path(tmp, "existing")), character(0))
})

test_that("initProject creates the full folder structure, the standard scripts, config and README, and returns the project path invisibly", {
  tmp <- withr::local_tempdir()
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeLines("mock", destfile)
    0L
  }, .package = "utils")
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(...) invisible(NULL),
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  dependencies = function(...) data.frame(Package = character(0)),
                                  .package = "renv")
  res <- withr::with_output_sink(tempfile(), dsAnalysis::initProject(path = tmp, name = "myproj"))
  proj <- file.path(tmp, "myproj")
  testthat::expect_identical(res, paste0(tmp, "/myproj"))
  testthat::expect_true(dir.exists(proj))
  for (d in c("R", "results", "results/tables", "results/figures", "utils",
              "utils/mock_data", "utils/mock_data/demo_obiba", "utils/data_dictionary",
              "utils/setup", "citations", "config", ".github/workflows")) {
    testthat::expect_true(dir.exists(file.path(proj, d)))
  }
  for (f in c("R/main.R", "R/01_DS_Login.R", "R/99_DSLiteLearning.R", "R/99_package_citations.R",
              "results/tables/placeholder.txt", "results/figures/placeholder.txt",
              "utils/setup/01_DSLite_Setup.R", "config.yml", "config/analysis-plan.yml",
              ".github/workflows/datashield-analysis-suggest.yml", "README.md", "dependencies.R",
              ".gitignore", ".Renviron")) {
    testthat::expect_true(file.exists(file.path(proj, f)))
  }
  testthat::expect_equal(length(list.files(proj, pattern = "[.]Rproj$")), 1L)
})

test_that("initProject writes the .Renviron with the three OBIBA demo servers and adds the results rules to .gitignore", {
  tmp <- withr::local_tempdir()
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeLines("mock", destfile)
    0L
  }, .package = "utils")
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(...) invisible(NULL),
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  dependencies = function(...) data.frame(Package = character(0)),
                                  .package = "renv")
  invisible(withr::with_output_sink(tempfile(), dsAnalysis::initProject(path = tmp, name = "envproj")))
  proj <- file.path(tmp, "envproj")
  renviron <- readLines(file.path(proj, ".Renviron"))
  testthat::expect_true("R_CONFIG_ACTIVE = 'production'" %in% renviron)
  testthat::expect_true("OBIBA1_TABLE = 'CNSIM.CNSIM1'" %in% renviron)
  testthat::expect_true("OBIBA2_TABLE = 'CNSIM.CNSIM2'" %in% renviron)
  testthat::expect_true("OBIBA3_TABLE = 'CNSIM.CNSIM3'" %in% renviron)
  testthat::expect_equal(sum(grepl("^OBIBA[0-9]_URL", renviron)), 3L)
  gitignore <- readLines(file.path(proj, ".gitignore"))
  testthat::expect_true(all(c(".Renviron", "results/figures/*", "!results/figures/placeholder.txt",
                              "results/tables/*", "!results/tables/placeholder.txt") %in% gitignore))
})

test_that("initProject installs the nfdi4health dsAnalysis and dsSupportClient repositories into the new project's own library", {
  tmp <- withr::local_tempdir()
  seen <- new.env(parent = emptyenv())
  seen$pkgs <- list()
  seen$libs <- character(0)
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeLines("mock", destfile)
    0L
  }, .package = "utils")
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(packages, library = NULL, ...) {
                                    seen$pkgs <- c(seen$pkgs, list(packages))
                                    seen$libs <- c(seen$libs, as.character(library))
                                    invisible(NULL)
                                  },
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  dependencies = function(...) data.frame(Package = character(0)),
                                  .package = "renv")
  invisible(withr::with_output_sink(tempfile(), dsAnalysis::initProject(path = tmp, name = "libproj")))
  proj <- file.path(tmp, "libproj")
  testthat::expect_true(length(seen$pkgs) >= 1L)
  first <- seen$pkgs[[1]]
  testthat::expect_identical(first,
                             c("dsBaseClient", "nfdi4health/dsSupportClient", "nfdi4health/dsAnalysis",
                               "config", "DSLite", "grateful"))
  testthat::expect_false(any(grepl("FlorianSchw", unlist(seen$pkgs))))
  testthat::expect_true(all(grepl(proj, seen$libs, fixed = TRUE)))
})

test_that("initProject calls usethis::proj_activate with the new project path when switch_to_proj is TRUE", {
  tmp <- withr::local_tempdir()
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeLines("mock", destfile)
    0L
  }, .package = "utils")
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(...) invisible(NULL),
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  dependencies = function(...) data.frame(Package = character(0)),
                                  .package = "renv")
  testthat::local_mocked_bindings(proj_activate = function(path) {
    seen$path <- path
    invisible(TRUE)
  }, .package = "usethis")
  res <- withr::with_output_sink(tempfile(), dsAnalysis::initProject(path = tmp, name = "switchproj", switch_to_proj = TRUE))
  testthat::expect_identical(seen$path, paste0(tmp, "/switchproj"))
  testthat::expect_identical(res, paste0(tmp, "/switchproj"))
  testthat::expect_true(dir.exists(file.path(tmp, "switchproj")))
})

test_that("initProject does not call usethis::proj_activate when switch_to_proj is FALSE", {
  tmp <- withr::local_tempdir()
  seen <- new.env(parent = emptyenv())
  seen$called <- FALSE
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeLines("mock", destfile)
    0L
  }, .package = "utils")
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(...) invisible(NULL),
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  dependencies = function(...) data.frame(Package = character(0)),
                                  .package = "renv")
  testthat::local_mocked_bindings(proj_activate = function(path) {
    seen$called <- TRUE
    invisible(TRUE)
  }, .package = "usethis")
  invisible(withr::with_output_sink(tempfile(), dsAnalysis::initProject(path = tmp, name = "noswitch", switch_to_proj = FALSE)))
  testthat::expect_false(seen$called)
  testthat::expect_true(dir.exists(file.path(tmp, "noswitch")))
})
