

test_that("project setup structure", {

  testthat::expect_error(dsAnalysis::initProject(), regexp = "Please provide a path name for the project to be created.")
  testthat::expect_error(dsAnalysis::initProject(name = "ZZZZ", switch_to_proj = "abc"),
                           regexp = "switch_to_proj has to be logical, i.e. either TRUE or FALSE.")

  #### Testing that all top-level folders and files are created as expected
  test_name <- "zzz-testproj-123"
  tmp_path <- fs::path_temp()
  tmp_path_to_proj <- dsAnalysis::initProject(path = tmp_path, name = test_name)

  file_structure_top <- fs::dir_ls(path = tmp_path_to_proj,
                                   all = TRUE)

  expect_elements <- c(".github",
                       ".gitignore",
                       ".Renviron",
                       ".Rprofile",
                       "config.yml",
                       "citations",
                       "config",
                       "dependencies.R",
                       "R",
                       "README.md",
                       "renv",
                       "renv.lock",
                       "results",
                       "utils",
                       paste0(test_name, ".Rproj"))

  expected_paths <- paste0(tmp_path_to_proj, "/", expect_elements)

  testthat::expect_setequal(file_structure_top,
                            expected_paths)

  #### Testing that the analysis plan is in place
  testthat::expect_true(file.exists(paste0(tmp_path_to_proj, "/config/analysis-plan.yml")))
  testthat::expect_true(file.exists(paste0(tmp_path_to_proj, "/R/99_DSLiteLearning.R")))
  testthat::expect_true(file.exists(paste0(tmp_path_to_proj, "/R/99_package_citations.R")))
  testthat::expect_true(file.exists(paste0(tmp_path_to_proj, "/.github/workflows/datashield-analysis-suggest.yml")))


  #### Testing whether important elements exist in the .gitignore file
  gitignore_lines_expected <- c(".Renviron",
                                "results/figures/*",
                                "!results/figures/placeholder.txt",
                                "results/tables/*",
                                "!results/tables/placeholder.txt")


  gitignore_lines_created <- readLines(con = paste0(tmp_path_to_proj, "/", ".gitignore"))

  testthat::expect_true(all(gitignore_lines_expected %in% gitignore_lines_created))

  #### Testing that the mock data is not excluded (the datashield-analysis-suggest workflow commits it)
  testthat::expect_false(any(stringr::str_detect(gitignore_lines_created, "utils|mock_data|\\.rda")))

  #### Testing that the blocks of the datashield-analysis-suggest workflow are in place
  main_lines_created <- readLines(con = paste0(tmp_path_to_proj, "/R/main.R"))
  testthat::expect_true(all(c("#### bot-suggest: scripts (updated by datashield-analysis-suggest)",
                              "#### bot-suggest: scripts end") %in% main_lines_created))

  dependencies_lines_created <- readLines(con = paste0(tmp_path_to_proj, "/dependencies.R"))
  testthat::expect_true(all(c("#### bot-suggest: packages (updated by datashield-analysis-suggest)",
                              "#### bot-suggest: packages end") %in% dependencies_lines_created))

  #### Testing whether important elements exist in the .Renviron file
  renviron_lines_expected <- c("R_CONFIG_ACTIVE = 'production'",
                               "OBIBA1_URL = 'https://opal-demo.obiba.org/'",
                               "OBIBA1_USER = 'dsuser'",
                               "OBIBA1_PWD = 'P@ssw0rd'",
                               "OBIBA1_TABLE = 'CNSIM.CNSIM1'")


  renviron_lines_created <- readLines(con = paste0(tmp_path_to_proj, "/", ".Renviron"))

  testthat::expect_true(all(renviron_lines_expected %in% renviron_lines_created))


  #### testing whether important packages have been written to the renv.lock file

  renv_lock_file <- paste0(tmp_path_to_proj, "/", "renv.lock")
  renv_lock_data <- rjson::fromJSON(paste(readLines(renv_lock_file), collapse = ""))[[2]]
  installed_package_names <- names(renv_lock_data)

  renv_lock_expected <- c("dsBaseClient",
                          "dsBase",
                          "grateful")

  testthat::expect_true(all(renv_lock_expected %in% installed_package_names))

  #### Testing whether the function stops when no name has been provided
  error_message <- testthat::expect_error(dsAnalysis::initProject(path = tmp_path))

  #### Testing that the error message is consistent
  testthat::expect_equal(error_message$message,
                         paste0("Please provide a path name for the project to be created."))


  #### Testing whether the function stops when it would overwrite a folder directory
  error_message2 <- testthat::expect_error(dsAnalysis::initialize_project(path = tmp_path, name = test_name))
  error_message2_message <- stringr::str_replace_all(string = error_message2$message,
                                                     pattern = "\\n",
                                                     replacement = "")
  error_message2_message <- stringr::str_squish(error_message2_message)

  if(cfg_dir_overwrite){

    #### Testing that the error message is consistent
    testthat::expect_equal(error_message2_message,
                           paste0("The path and name you have provided would overwrite an existing directory (", tmp_path_to_proj , "). Setup aborted."))

  }

})

test_that("initProject() creates the full folder structure, placeholder files and returns the project path invisibly", {
  tmp_root <- withr::local_tempdir()
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(...) invisible(NULL),
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  .package = "renv")
  testthat::local_mocked_bindings(proj_activate = function(...) invisible(NULL),
                                  .package = "usethis")
  local_dl <- function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) }
  testthat::local_mocked_bindings(download.file = local_dl, .package = "utils")
  proj_path <- withVisible(dsAnalysis::initProject(path = tmp_root, name = "proj-structure"))
  
  testthat::expect_false(proj_path$visible)
  testthat::expect_equal(proj_path$value, paste0(tmp_root, "/proj-structure"))
  
  p <- proj_path$value
  
  expected_dirs <- c("results", "results/tables", "results/figures",
                     "utils", "utils/mock_data", "utils/mock_data/demo_obiba",
                     "utils/data_dictionary", "utils/setup",
                     "citations", "config", ".github/workflows", "R")
  testthat::expect_equal(as.logical(fs::dir_exists(paste0(p, "/", expected_dirs))),
                         rep(TRUE, length(expected_dirs)))
  
  expected_files <- c("R/main.R", "R/01_DS_Login.R", "R/99_DSLiteLearning.R",
                      "R/99_package_citations.R",
                      "results/tables/placeholder.txt", "results/figures/placeholder.txt",
                      "utils/setup/01_DSLite_Setup.R", "config.yml",
                      "config/analysis-plan.yml",
                      ".github/workflows/datashield-analysis-suggest.yml",
                      "README.md", "dependencies.R", ".gitignore", ".Renviron",
                      "proj-structure.Rproj")
  testthat::expect_equal(as.logical(fs::file_exists(paste0(p, "/", expected_files))),
                         rep(TRUE, length(expected_files)))
  
  testthat::expect_equal(as.logical(fs::file_exists(paste0(p, "/utils/mock_data/demo_obiba/",
                                                           c("CNSIM1.rda", "CNSIM2.rda", "CNSIM3.rda")))),
                         c(TRUE, TRUE, TRUE))
})

test_that("initProject() copies a non-empty README.md into the new project", {
  tmp_root <- withr::local_tempdir()
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(...) invisible(NULL),
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  .package = "renv")
  testthat::local_mocked_bindings(proj_activate = function(...) invisible(NULL),
                                  .package = "usethis")
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) },
                                  .package = "utils")
  p <- dsAnalysis::initProject(path = tmp_root, name = "proj-readme")
  readme_path <- paste0(p, "/README.md")
  testthat::expect_true(file.exists(readme_path))
  
  readme_lines <- readLines(readme_path)
  testthat::expect_gt(length(readme_lines), 0)
  testthat::expect_identical(readme_lines,
                             readLines(dsAnalysis:::internal_find_script("utils/README.md")))
})

test_that("initProject() stops with the overwrite message when the target directory already exists", {
  tmp_root <- withr::local_tempdir()
  dir.create(paste0(tmp_root, "/already-there"))
  err <- testthat::expect_error(dsAnalysis::initProject(path = tmp_root, name = "already-there"))
  msg <- stringr::str_squish(stringr::str_replace_all(err$message, "\\n", ""))
  testthat::expect_equal(msg,
                         paste0("The path and name you have provided would overwrite an existing directory (",
                                tmp_root, "/already-there). Setup aborted."))
})

test_that("initProject() with switch_to_proj = TRUE calls usethis::proj_activate() with the new project path", {
  tmp_root <- withr::local_tempdir()
  activated <- new.env(parent = emptyenv())
  activated$path <- NULL
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(...) invisible(NULL),
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  .package = "renv")
  testthat::local_mocked_bindings(proj_activate = function(path, ...) { activated$path <- path; invisible(NULL) },
                                  .package = "usethis")
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) },
                                  .package = "utils")
  p <- dsAnalysis::initProject(path = tmp_root, name = "proj-switch", switch_to_proj = TRUE)
  
  testthat::expect_equal(p, paste0(tmp_root, "/proj-switch"))
  testthat::expect_equal(activated$path, paste0(tmp_root, "/proj-switch"))
})

test_that("initProject() writes the complete .Renviron with all three OBIBA server blocks and keeps the gitignore placeholder exceptions", {
  tmp_root <- withr::local_tempdir()
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(...) invisible(NULL),
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  .package = "renv")
  testthat::local_mocked_bindings(proj_activate = function(...) invisible(NULL),
                                  .package = "usethis")
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) },
                                  .package = "utils")
  p <- dsAnalysis::initProject(path = tmp_root, name = "proj-env")
  
  renviron_lines <- readLines(paste0(p, "/.Renviron"))
  testthat::expect_equal(renviron_lines[1], "R_CONFIG_ACTIVE = 'production'")
  testthat::expect_true(all(c("OBIBA1_TABLE = 'CNSIM.CNSIM1'",
                              "OBIBA2_TABLE = 'CNSIM.CNSIM2'",
                              "OBIBA3_TABLE = 'CNSIM.CNSIM3'") %in% renviron_lines))
  testthat::expect_equal(sum(stringr::str_detect(renviron_lines, "OBIBA[0-9]_URL = 'https://opal-demo.obiba.org/'")), 3)
  
  gitignore_lines <- readLines(paste0(p, "/.gitignore"))
  testthat::expect_true(all(c(".Renviron",
                              "results/figures/*",
                              "!results/figures/placeholder.txt",
                              "results/tables/*",
                              "!results/tables/placeholder.txt") %in% gitignore_lines))
})

test_that("initProject() copies config.yml, analysis-plan.yml and the workflow file identically to the packaged templates", {
  tmp_root <- withr::local_tempdir()
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(...) invisible(NULL),
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  .package = "renv")
  testthat::local_mocked_bindings(proj_activate = function(...) invisible(NULL),
                                  .package = "usethis")
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) },
                                  .package = "utils")
  p <- dsAnalysis::initProject(path = tmp_root, name = "proj-templates")
  testthat::expect_identical(readLines(paste0(p, "/config.yml")),
                             readLines(dsAnalysis:::internal_find_script("utils/config.yml")))
  testthat::expect_identical(readLines(paste0(p, "/config/analysis-plan.yml")),
                             readLines(dsAnalysis:::internal_find_script("utils/analysis-plan.yml")))
  testthat::expect_identical(readLines(paste0(p, "/.github/workflows/datashield-analysis-suggest.yml")),
                             readLines(dsAnalysis:::internal_find_script("github/datashield-analysis-suggest.yml")))
})

test_that("initProject() copies the same placeholder.txt template into results/tables and results/figures", {
  tmp_root <- withr::local_tempdir()
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(...) invisible(NULL),
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  .package = "renv")
  testthat::local_mocked_bindings(proj_activate = function(...) invisible(NULL),
                                  .package = "usethis")
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) },
                                  .package = "utils")
  p <- dsAnalysis::initProject(path = tmp_root, name = "proj-placeholder")
  placeholder_template <- readLines(dsAnalysis:::internal_find_script("utils/placeholder.txt"))
  testthat::expect_identical(readLines(paste0(p, "/results/tables/placeholder.txt")), placeholder_template)
  testthat::expect_identical(readLines(paste0(p, "/results/figures/placeholder.txt")), placeholder_template)
})

test_that("initProject() does not call usethis::proj_activate() when switch_to_proj is FALSE", {
  tmp_root <- withr::local_tempdir()
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(...) invisible(NULL),
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  .package = "renv")
  testthat::local_mocked_bindings(proj_activate = function(...) { calls$n <- calls$n + 1L; invisible(NULL) },
                                  .package = "usethis")
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) },
                                  .package = "utils")
  p <- dsAnalysis::initProject(path = tmp_root, name = "proj-noswitch", switch_to_proj = FALSE)
  testthat::expect_equal(calls$n, 0L)
  testthat::expect_equal(p, paste0(tmp_root, "/proj-noswitch"))
  testthat::expect_true(fs::dir_exists(p))
})

test_that("initProject() passes the new project path to renv::init(), renv::install() and renv::snapshot()", {
  tmp_root <- withr::local_tempdir()
  rec <- new.env(parent = emptyenv())
  rec$init <- NULL
  rec$install_pkgs <- NULL
  rec$install_proj <- NULL
  rec$hydrate <- NULL
  rec$snapshot <- NULL
  testthat::local_mocked_bindings(init = function(project, ...) { rec$init <- project; invisible(NULL) },
                                  install = function(packages, library, project, ...) {
                                    rec$install_pkgs <- packages
                                    rec$install_proj <- project
                                    invisible(NULL)
                                  },
                                  hydrate = function(library, project, ...) { rec$hydrate <- project; invisible(NULL) },
                                  snapshot = function(project, ...) { rec$snapshot <- project; invisible(NULL) },
                                  .package = "renv")
  testthat::local_mocked_bindings(proj_activate = function(...) invisible(NULL),
                                  .package = "usethis")
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) },
                                  .package = "utils")
  p <- dsAnalysis::initProject(path = tmp_root, name = "proj-renv")
  testthat::expect_equal(rec$init, p)
  testthat::expect_equal(rec$install_proj, p)
  testthat::expect_equal(rec$hydrate, p)
  testthat::expect_equal(rec$snapshot, p)
  testthat::expect_equal(rec$install_pkgs,
                         c("dsBaseClient", "nfdi4health/dsSupportClient", "nfdi4health/dsAnalysis"))
})

test_that("initProject() errors on a non-logical switch_to_proj before creating the project directory", {
  tmp_root <- withr::local_tempdir()
  testthat::expect_error(dsAnalysis::initProject(path = tmp_root, name = "proj-badflag", switch_to_proj = "yes"),
                         regexp = "switch_to_proj has to be logical, i.e. either TRUE or FALSE.")
  testthat::expect_false(fs::dir_exists(paste0(tmp_root, "/proj-badflag")))
  testthat::expect_equal(length(fs::dir_ls(tmp_root, all = TRUE)), 0)
})

test_that("initProject() copies main.R and dependencies.R identically to the packaged templates including the bot-suggest markers", {
  tmp_root <- withr::local_tempdir()
  testthat::local_mocked_bindings(init = function(...) invisible(NULL),
                                  install = function(...) invisible(NULL),
                                  hydrate = function(...) invisible(NULL),
                                  snapshot = function(...) invisible(NULL),
                                  .package = "renv")
  testthat::local_mocked_bindings(proj_activate = function(...) invisible(NULL),
                                  .package = "usethis")
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) { writeLines("mock", destfile); invisible(0L) },
                                  .package = "utils")
  p <- dsAnalysis::initProject(path = tmp_root, name = "proj-scripts")
  main_lines <- readLines(paste0(p, "/R/main.R"))
  dep_lines <- readLines(paste0(p, "/dependencies.R"))
  testthat::expect_identical(main_lines, readLines(dsAnalysis:::internal_find_script("datashield/main.R")))
  testthat::expect_identical(dep_lines, readLines(dsAnalysis:::internal_find_script("utils/dependencies.R")))
  testthat::expect_true(all(c("#### bot-suggest: scripts (updated by datashield-analysis-suggest)",
                              "#### bot-suggest: scripts end") %in% main_lines))
  testthat::expect_true(all(c("#### bot-suggest: packages (updated by datashield-analysis-suggest)",
                              "#### bot-suggest: packages end") %in% dep_lines))
})
