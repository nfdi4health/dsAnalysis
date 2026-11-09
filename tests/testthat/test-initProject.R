test_that("initProject errors when name is NULL", {
  testthat::expect_error(dsAnalysis::initProject(path = withr::local_tempdir()),
                         "Please provide a path name for the project to be created.",
                         fixed = TRUE)
})

test_that("initProject errors when switch_to_proj is not logical", {
  tmp <- withr::local_tempdir()
  testthat::expect_error(dsAnalysis::initProject(path = tmp, name = "myproj", switch_to_proj = "yes"),
                         "switch_to_proj has to be logical, i.e. either TRUE or FALSE.",
                         fixed = TRUE)
  testthat::expect_false(dir.exists(file.path(tmp, "myproj")))
})

test_that("initProject errors and does not touch the directory when the target directory already exists", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "existing"))
  writeLines("keep me", file.path(tmp, "existing", "keep.txt"))
  testthat::expect_error(dsAnalysis::initProject(path = tmp, name = "existing"),
                         "would overwrite an existing",
                         fixed = TRUE)
  testthat::expect_identical(list.files(file.path(tmp, "existing")), "keep.txt")
  testthat::expect_identical(readLines(file.path(tmp, "existing", "keep.txt")), "keep me")
})

test_that("initProject creates the full folder structure and returns the new project path invisibly", {
  mock_dl <- function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) }
  mock_renv_init <- function(...) invisible(NULL)
  mock_renv_install <- function(...) invisible(NULL)
  mock_renv_hydrate <- function(...) invisible(NULL)
  mock_renv_snapshot <- function(...) invisible(NULL)
  mock_renv_deps <- function(...) data.frame(Package = character(0), stringsAsFactors = FALSE)
  tmp <- withr::local_tempdir()
  lib <- file.path(tmp, "renv-lib")
  testthat::local_mocked_bindings(download.file = mock_dl, .package = "utils")
  testthat::local_mocked_bindings(init = mock_renv_init, install = mock_renv_install,
                                  hydrate = mock_renv_hydrate, snapshot = mock_renv_snapshot,
                                  dependencies = mock_renv_deps,
                                  paths = list(library = function(project = NULL) lib),
                                  .package = "renv")
  res <- testthat::expect_invisible(dsAnalysis::initProject(path = tmp, name = "newproj"))
  testthat::expect_identical(res, paste0(tmp, "/newproj"))
  proj <- file.path(tmp, "newproj")
  expected_dirs <- c("R", "results", "results/tables", "results/figures", "utils",
                     "utils/mock_data", "utils/mock_data/demo_obiba", "utils/data_dictionary",
                     "utils/setup", "citations", "config", ".github/workflows")
  testthat::expect_true(all(dir.exists(file.path(proj, expected_dirs))))
})

test_that("initProject copies the standard R scripts, config, analysis plan, workflow, README and placeholders into the new project", {
  mock_dl <- function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) }
  mock_renv_init <- function(...) invisible(NULL)
  mock_renv_install <- function(...) invisible(NULL)
  mock_renv_hydrate <- function(...) invisible(NULL)
  mock_renv_snapshot <- function(...) invisible(NULL)
  mock_renv_deps <- function(...) data.frame(Package = character(0), stringsAsFactors = FALSE)
  tmp <- withr::local_tempdir()
  lib <- file.path(tmp, "renv-lib")
  testthat::local_mocked_bindings(download.file = mock_dl, .package = "utils")
  testthat::local_mocked_bindings(init = mock_renv_init, install = mock_renv_install,
                                  hydrate = mock_renv_hydrate, snapshot = mock_renv_snapshot,
                                  dependencies = mock_renv_deps,
                                  paths = list(library = function(project = NULL) lib),
                                  .package = "renv")
  dsAnalysis::initProject(path = tmp, name = "newproj")
  proj <- file.path(tmp, "newproj")
  expected_files <- c("R/main.R", "R/01_DS_Login.R", "R/99_DSLiteLearning.R",
                      "R/99_package_citations.R", "results/tables/placeholder.txt",
                      "results/figures/placeholder.txt", "utils/setup/01_DSLite_Setup.R",
                      "config.yml", "config/analysis-plan.yml",
                      ".github/workflows/datashield-analysis-suggest.yml",
                      "README.md", "dependencies.R")
  testthat::expect_true(all(file.exists(file.path(proj, expected_files))))
  testthat::expect_identical(readLines(file.path(proj, "R/main.R")),
                             readLines(dsAnalysis:::internal_find_script("datashield/main.R")))
  testthat::expect_identical(readLines(file.path(proj, "config/analysis-plan.yml")),
                             readLines(dsAnalysis:::internal_find_script("utils/analysis-plan.yml")))
})

test_that("initProject writes the .gitignore additions and the full .Renviron content", {
  mock_dl <- function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) }
  mock_renv_init <- function(...) invisible(NULL)
  mock_renv_install <- function(...) invisible(NULL)
  mock_renv_hydrate <- function(...) invisible(NULL)
  mock_renv_snapshot <- function(...) invisible(NULL)
  mock_renv_deps <- function(...) data.frame(Package = character(0), stringsAsFactors = FALSE)
  tmp <- withr::local_tempdir()
  lib <- file.path(tmp, "renv-lib")
  testthat::local_mocked_bindings(download.file = mock_dl, .package = "utils")
  testthat::local_mocked_bindings(init = mock_renv_init, install = mock_renv_install,
                                  hydrate = mock_renv_hydrate, snapshot = mock_renv_snapshot,
                                  dependencies = mock_renv_deps,
                                  paths = list(library = function(project = NULL) lib),
                                  .package = "renv")
  dsAnalysis::initProject(path = tmp, name = "newproj")
  proj <- file.path(tmp, "newproj")
  gitignore <- readLines(file.path(proj, ".gitignore"))
  testthat::expect_true(all(c(".Renviron", "results/figures/*", "!results/figures/placeholder.txt",
                              "results/tables/*", "!results/tables/placeholder.txt") %in% gitignore))
  renviron <- readLines(file.path(proj, ".Renviron"))
  testthat::expect_identical(renviron[1], "R_CONFIG_ACTIVE = 'production'")
  testthat::expect_true(all(c("OBIBA1_TABLE = 'CNSIM.CNSIM1'",
                              "OBIBA2_TABLE = 'CNSIM.CNSIM2'",
                              "OBIBA3_TABLE = 'CNSIM.CNSIM3'") %in% renviron))
  testthat::expect_equal(sum(grepl("OBIBA", renviron)), 12L)
})

test_that("initProject installs dsAnalysis from the nfdi4health repository into the project library", {
  mock_renv_init <- function(...) invisible(NULL)
  mock_renv_hydrate <- function(...) invisible(NULL)
  mock_renv_snapshot <- function(...) invisible(NULL)
  mock_renv_deps <- function(...) data.frame(Package = character(0), stringsAsFactors = FALSE)
  mock_dl <- function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) }
  tmp <- withr::local_tempdir()
  lib <- file.path(tmp, "renv-lib")
  seen <- new.env(parent = emptyenv())
  seen$pkgs <- list()
  seen$libs <- character(0)
  testthat::local_mocked_bindings(download.file = mock_dl, .package = "utils")
  testthat::local_mocked_bindings(init = mock_renv_init,
                                  install = function(packages, library = NULL, project = NULL, ...) {
                                    seen$pkgs <- c(seen$pkgs, list(packages))
                                    seen$libs <- c(seen$libs, library)
                                    invisible(NULL)
                                  },
                                  hydrate = mock_renv_hydrate, snapshot = mock_renv_snapshot,
                                  dependencies = mock_renv_deps,
                                  paths = list(library = function(project = NULL) lib),
                                  .package = "renv")
  dsAnalysis::initProject(path = tmp, name = "newproj")
  first_install <- seen$pkgs[[1]]
  testthat::expect_identical(first_install,
                             c("dsBaseClient", "nfdi4health/dsSupportClient", "nfdi4health/dsAnalysis",
                               "config", "DSLite", "grateful"))
  testthat::expect_false(any(grepl("FlorianSchw", unlist(seen$pkgs))))
  testthat::expect_identical(seen$libs[1], lib)
  testthat::expect_true(dir.exists(lib))
})

test_that("initProject calls usethis::proj_activate with the new project path when switch_to_proj is TRUE", {
  mock_dl <- function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) }
  mock_renv_init <- function(...) invisible(NULL)
  mock_renv_install <- function(...) invisible(NULL)
  mock_renv_hydrate <- function(...) invisible(NULL)
  mock_renv_snapshot <- function(...) invisible(NULL)
  mock_renv_deps <- function(...) data.frame(Package = character(0), stringsAsFactors = FALSE)
  tmp <- withr::local_tempdir()
  lib <- file.path(tmp, "renv-lib")
  seen <- new.env(parent = emptyenv())
  seen$path <- NULL
  testthat::local_mocked_bindings(download.file = mock_dl, .package = "utils")
  testthat::local_mocked_bindings(init = mock_renv_init, install = mock_renv_install,
                                  hydrate = mock_renv_hydrate, snapshot = mock_renv_snapshot,
                                  dependencies = mock_renv_deps,
                                  paths = list(library = function(project = NULL) lib),
                                  .package = "renv")
  testthat::local_mocked_bindings(proj_activate = function(path) { seen$path <- path; invisible(TRUE) },
                                  .package = "usethis")
  res <- dsAnalysis::initProject(path = tmp, name = "switched", switch_to_proj = TRUE)
  testthat::expect_identical(seen$path, paste0(tmp, "/switched"))
  testthat::expect_identical(res, paste0(tmp, "/switched"))
})

test_that("initProject creates an RStudio .Rproj file for the new project", {
  mock_dl <- function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) }
  mock_renv_init <- function(...) invisible(NULL)
  mock_renv_install <- function(...) invisible(NULL)
  mock_renv_hydrate <- function(...) invisible(NULL)
  mock_renv_snapshot <- function(...) invisible(NULL)
  mock_renv_deps <- function(...) data.frame(Package = character(0), stringsAsFactors = FALSE)
  tmp <- withr::local_tempdir()
  lib <- file.path(tmp, "renv-lib")
  testthat::local_mocked_bindings(download.file = mock_dl, .package = "utils")
  testthat::local_mocked_bindings(init = mock_renv_init, install = mock_renv_install,
                                  hydrate = mock_renv_hydrate, snapshot = mock_renv_snapshot,
                                  dependencies = mock_renv_deps,
                                  paths = list(library = function(project = NULL) lib),
                                  .package = "renv")
  dsAnalysis::initProject(path = tmp, name = "rprojtest")
  proj <- file.path(tmp, "rprojtest")
  testthat::expect_true(file.exists(file.path(proj, "rprojtest.Rproj")))
  testthat::expect_identical(list.files(proj, pattern = "[.]Rproj$"), "rprojtest.Rproj")
})
