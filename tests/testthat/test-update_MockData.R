#
#
# test_that("project setup structure", {
#
#   #### Testing that all top-level folders and files are created as expected
#   test_name <- "testproj-123"
#   tmp_path <- fs::path_temp()
#   tmp_path_to_proj <- mepr::initialize_project(path = tmp_path, name = test_name)
#
#   file_structure_top <- fs::dir_ls(path = tmp_path_to_proj,
#                                    all = TRUE)
#
#   expect_elements <- c(".gitignore",
#                        #".Renviron",
#                        ".Rprofile",
#                        #"config.yml",
#                        "citations",
#                        "data",
#                        "data-raw",
#                        "R",
#                        "README.md",
#                        "renv",
#                        "renv.lock",
#                        "results",
#                        paste0(test_name, ".Rproj"))
#
#   expected_paths <- paste0(tmp_path_to_proj, "/", expect_elements)
#
#   testthat::expect_setequal(file_structure_top,
#                             expected_paths)
#
#
#   #### Testing whether important elements exist in the .gitignore file
#   gitignore_lines_expected <- c(".Renviron",
#                                 "data/*",
#                                 "!data/placeholder.txt",
#                                 "data-raw/*",
#                                 "!data-raw/placeholder.txt",
#                                 "results/plots/*",
#                                 "!results/plots/placeholder.txt",
#                                 "results/tables/*",
#                                 "!results/tables/placeholder.txt")
#
#   gitignore_lines_created <- readLines(con = paste0(tmp_path_to_proj, "/", ".gitignore"))
#
#   testthat::expect_true(all(gitignore_lines_expected %in% gitignore_lines_created))
#
#
#   #### testing whether important packages have been written to the renv.lock file
#
#   renv_lock_file <- paste0(tmp_path_to_proj, "/", "renv.lock")
#   renv_lock_data <- rjson::fromJSON(paste(readLines(renv_lock_file), collapse = ""))[[2]]
#   installed_package_names <- names(renv_lock_data)
#
#   renv_lock_expected <- c("survival",
#                           "tidyverse",
#                           "here",
#                           "pak")
#
#   testthat::expect_true(all(renv_lock_expected %in% installed_package_names))
#
#
#   #### Testing whether the function stops when no name has been provided
#   error_message <- testthat::expect_error(mepr::initialize_project(path = tmp_path))
#
#   #### Testing that the error message is consistent
#   testthat::expect_equal(error_message$message,
#                          paste0("Please provide a path name for the project to be created."))
#
#
#   #### Testing whether the function stops when it would overwrite a folder directory
#   error_message2 <- testthat::expect_error(mepr::initialize_project(path = tmp_path, name = test_name))
#   error_message2_message <- stringr::str_replace_all(string = error_message2$message,
#                                                      pattern = "\\n",
#                                                      replacement = "")
#   error_message2_message <- stringr::str_squish(error_message2_message)
#
#   if(cfg_dir_overwrite){
#
#     #### Testing that the error message is consistent
#     testthat::expect_equal(error_message2_message,
#                            paste0("The path and name you have provided would overwrite an existing directory (", tmp_path_to_proj , "). Setup aborted."))
#
#   }
#
# })
#
#
#
#
#
#

test_that("update_MockData() errors when folder_name is NULL", {
  err <- expect_error(update_MockData())
  expect_equal(err$message, "You need to name a folder that contains the mock data.")
})

test_that("update_MockData() errors when table_names has a different length than the mock data files", {
  setup <- local_cnsim_project()
  project <- setup$project
  mock_dir <- file.path(project, "utils", "mock_data", "myfolder")
  dir.create(mock_dir, recursive = TRUE)
  sim1 <- setup$real$sim1
  sim2 <- setup$real$sim2
  save(sim1, file = file.path(mock_dir, "sim1.rda"))
  save(sim2, file = file.path(mock_dir, "sim2.rda"))
  err <- expect_error(update_MockData(folder_name = "myfolder", table_names = c("tab1")))
  expect_equal(err$message, "The length of the provided table names and mock data files are not the same.")
})

test_that("update_MockData() with table_names rewrites blocks 2, 3 and 5 of 01_DSLite_Setup.R and keeps the other blocks", {
  setup <- local_cnsim_project()
  project <- setup$project
  mock_dir <- file.path(project, "utils", "mock_data", "myfolder")
  dir.create(mock_dir, recursive = TRUE)
  sim1 <- setup$real$sim1
  sim2 <- setup$real$sim2
  save(sim1, file = file.path(mock_dir, "sim1.rda"))
  save(sim2, file = file.path(mock_dir, "sim2.rda"))
  setup_dir <- file.path(project, "utils", "setup")
  dir.create(setup_dir, recursive = TRUE)
  setup_file <- file.path(setup_dir, "01_DSLite_Setup.R")
  writeLines(c("#### header line",
               "#### Step 1: Loading necessary libraries",
               "library(DSLite)",
               "",
               "#### Step 2: Import of mock data files",
               "load(file = 'old')",
               "",
               "#### Step 3: Defining the server-side data in a new dslite server",
               "dslite.server <<- DSLite::newDSLiteServer(tables=list(old = old))",
               "",
               "#### Step 4: Defining the server-side settings",
               "dslite.server$config(DSLite::defaultDSConfiguration(include = 'dsBase'))",
               "",
               "#### Step 5: Building the logindata object",
               "logindata.dslite.data <- data.frame(server = c('old'))",
               "",
               "#### Step 6: Login to the different DSLite Servers",
               "conns <- DSI::datashield.login(logins = logindata.dslite.data)",
               "",
               "#### Step 7: Cleaning the environment",
               "rm(list = ls())"),
             con = setup_file)
  update_MockData(folder_name = "myfolder", table_names = c("tabA", "tabB"))
  new_lines <- readLines(setup_file)
  expect_equal(new_lines[1], "#### header line")
  expect_equal(new_lines[2], "#### Step 1: Loading necessary libraries")
  expect_true("library(DSLite)" %in% new_lines)
  expect_true(paste0("load(file = here::here(\"utils/mock_data/myfolder\", \"sim1.rda\"))") %in% new_lines)
  expect_true(paste0("load(file = here::here(\"utils/mock_data/myfolder\", \"sim2.rda\"))") %in% new_lines)
  expect_false("load(file = 'old')" %in% new_lines)
  expect_true("dslite.server <<- DSLite::newDSLiteServer(tables=list(tabA = sim1," %in% new_lines)
  expect_true(any(grepl("^ +tabB = sim2\\)\\)$", new_lines)))
  expect_true("logindata.dslite.data <- data.frame(server = c(\"sim1\"," %in% new_lines)
  expect_true(any(grepl("^ +\"sim2\"\\),$", new_lines)))
  expect_true(any(grepl("table = c\\(\"tabA\",$", new_lines)))
  expect_true(any(grepl("^ +\"tabB\"\\),$", new_lines)))
  expect_true(any(grepl("driver = \"DSLiteDriver\"\\)$", new_lines)))
  expect_true(any(grepl("datashield.privacyControlLevel", new_lines)))
  expect_true("#### Step 4: Defining the server-side settings" %in% new_lines)
  expect_true("conns <- DSI::datashield.login(logins = logindata.dslite.data)" %in% new_lines)
  expect_equal(new_lines[length(new_lines)], "rm(list = ls())")
})

test_that("update_MockData() without table_names messages and takes server and table names from R/01_DS_Login.R", {
  setup <- local_cnsim_project()
  project <- setup$project
  mock_dir <- file.path(project, "utils", "mock_data", "myfolder")
  dir.create(mock_dir, recursive = TRUE)
  sim1 <- setup$real$sim1
  sim2 <- setup$real$sim2
  save(sim1, file = file.path(mock_dir, "sim1.rda"))
  save(sim2, file = file.path(mock_dir, "sim2.rda"))
  dir.create(file.path(project, "R"), recursive = TRUE)
  writeLines(c("builder <- DSI::newDSLoginBuilder()",
               "builder$append(server = \"sim1\",",
               "               url = \"https://opal.example.org\",",
               "               user = \"user\",",
               "               password = \"pw\",",
               "               table = \"CNSIM.CNSIM1\",",
               "               driver = \"OpalDriver\")",
               "builder$append(server = \"sim2\",",
               "               url = \"https://opal.example.org\",",
               "               user = \"user\",",
               "               password = \"pw\",",
               "               table = \"CNSIM.CNSIM2\",",
               "               driver = \"OpalDriver\")",
               "logindata <- builder$build()"),
             con = file.path(project, "R", "01_DS_Login.R"))
  setup_dir <- file.path(project, "utils", "setup")
  dir.create(setup_dir, recursive = TRUE)
  setup_file <- file.path(setup_dir, "01_DSLite_Setup.R")
  writeLines(c("#### header line",
               "#### Step 1: Loading necessary libraries",
               "library(DSLite)",
               "",
               "#### Step 2: Import of mock data files",
               "load(file = 'old')",
               "",
               "#### Step 3: Defining the server-side data in a new dslite server",
               "dslite.server <<- DSLite::newDSLiteServer(tables=list(old = old))",
               "",
               "#### Step 4: Defining the server-side settings",
               "dslite.server$config(DSLite::defaultDSConfiguration(include = 'dsBase'))",
               "",
               "#### Step 5: Building the logindata object",
               "logindata.dslite.data <- data.frame(server = c('old'))",
               "",
               "#### Step 6: Login to the different DSLite Servers",
               "conns <- DSI::datashield.login(logins = logindata.dslite.data)",
               "",
               "#### Step 7: Cleaning the environment",
               "rm(list = ls())"),
             con = setup_file)
  expect_message(update_MockData(folder_name = "myfolder"),
                 "No table names were given")
  new_lines <- readLines(setup_file)
  expect_true("dslite.server <<- DSLite::newDSLiteServer(tables=list(CNSIM1 = sim1," %in% new_lines)
  expect_true(any(grepl("^ +CNSIM2 = sim2\\)\\)$", new_lines)))
  expect_true(any(grepl("table = c\\(\"CNSIM1\",$", new_lines)))
  expect_true(any(grepl("^ +\"CNSIM2\"\\),$", new_lines)))
  expect_true("logindata.dslite.data <- data.frame(server = c(\"sim1\"," %in% new_lines)
})
