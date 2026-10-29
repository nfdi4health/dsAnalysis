#' @title Initiate a new project environment dedicated for DataSHIELD analysis
#' @description Sets up a new local R project pre-configured for DataSHIELD analyses, including folder structure, starter scripts, and environment/config files for switching between live servers and local DSLite testing.
#' @details Creates the project with usethis::create_project, manages the project's R package library with renv (renv::init, renv::install, renv::hydrate, renv::snapshot), and downloads example CNSIM mock datasets from the dsBaseClient GitHub repository into the new project with utils::download.file. It also writes .Renviron, .gitignore, and config.yml files in the new project folder to support switching between a live DataSHIELD server setup and a local DSLite testing setup, and copies in template R scripts, a GitHub Actions workflow, and a README. All of these files and folders are created under the new project path on the analyst's machine; no existing files outside that new folder are modified, and the function stops before creating anything if that folder already exists.
#' @param path Character string giving the local directory in which to create the new project folder; defaults to "home", which is expanded to the user's home directory via fs::path_expand("~").
#' @param name Character string giving the name of the new project; required (the function stops with an error if left NULL), and is used as the name of the new subfolder created under path.
#' @param switch_to_proj Logical flag indicating whether to activate the newly created project in the current R session via usethis::proj_activate once setup is complete; defaults to FALSE.
#' @return Invisibly returns the character string giving the path to the newly created project folder; the main effect is the creation of that folder together with its subfolders (results, utils, citations, config, .github/workflows, etc.), template R scripts, config/.Renviron/.gitignore files, downloaded mock data, and an renv-managed package library.
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import renv
#' @import fs
#' @import usethis
#' @importFrom utils download.file
#' @examples
#' \dontrun{
#' tmp_dir <- tempdir()
#'
#' project_path <- initProject(path = tmp_dir,
#'                              name = "demo_ds_project",
#'                              switch_to_proj = FALSE)
#'
#' list.files(project_path)
#' }
#' @export

initProject <- function(path = "home",
                        name = NULL,
                        switch_to_proj = FALSE){

  if (is.null(name)) {
    stop("Please provide a path name for the project to be created.",
         call. = FALSE)
  }

  if (!(is.logical(switch_to_proj))) {
    stop("switch_to_proj has to be logical, i.e. either TRUE or FALSE.",
         call. = FALSE)
  }


  if (path == "home") {

    new_project_path <- paste0(fs::path_expand("~"), "/", name)

  } else {

    new_project_path <- paste0(path, "/", name)

  }

  if (fs::dir_exists(new_project_path)) {
    stop(paste0("The path and name you have provided would overwrite an existing
                directory (", new_project_path, "). Setup aborted."),
         call. = FALSE)
  }

  #### sets up normal R Project
  usethis::create_project(new_project_path,
                          open = FALSE,
                          rstudio = TRUE)

  #### creates additional folder structure
  dir.create(paste0(new_project_path, "/results"))
  dir.create(paste0(new_project_path, "/results/tables"))
  dir.create(paste0(new_project_path, "/results/figures"))
  dir.create(paste0(new_project_path, "/utils"))
  dir.create(paste0(new_project_path, "/utils/mock_data"))
  dir.create(paste0(new_project_path, "/utils/mock_data/demo_obiba"))
  dir.create(paste0(new_project_path, "/utils/data_dictionary"))
  dir.create(paste0(new_project_path, "/utils/setup"))
  dir.create(paste0(new_project_path, "/citations"))
  dir.create(paste0(new_project_path, "/config"))
  dir.create(paste0(new_project_path, "/.github/workflows"), recursive = TRUE)

  #### copies over standardised R scripts for start
  file.copy(from = internal_find_script("datashield/main.R"),
            to = paste0(new_project_path, "/R/main.R"))
  file.copy(from = internal_find_script("datashield/01_DS_Login.R"),
            to = paste0(new_project_path, "/R/01_DS_Login.R"))
  file.copy(from = internal_find_script("datashield/99_DSLiteLearning.R"),
            to = paste0(new_project_path, "/R/99_DSLiteLearning.R"))
  file.copy(from = internal_find_script("datashield/99_package_citations.R"),
            to = paste0(new_project_path, "/R/99_package_citations.R"))

  #### copies over placeholder files to keep folder structure in place for GitHub
  #### for folders that should not be shared (e.g. results)
  file.copy(from = internal_find_script("utils/placeholder.txt"),
            to = paste0(new_project_path, "/results/tables/placeholder.txt"))
  file.copy(from = internal_find_script("utils/placeholder.txt"),
            to = paste0(new_project_path, "/results/figures/placeholder.txt"))

  #### copies over standardised R scripts for DSLite
  file.copy(from = internal_find_script("dslite/01_DSLite_Setup.R"),
            to = paste0(new_project_path, "/utils/setup/01_DSLite_Setup.R"))


  #### copies over initial config.yml file
  file.copy(from = internal_find_script("utils/config.yml"),
            to = paste0(new_project_path, "/config.yml"))

  #### copies over the analysis plan and the datashield-analysis-suggest workflow that reads it
  file.copy(from = internal_find_script("utils/analysis-plan.yml"),
            to = paste0(new_project_path, "/config/analysis-plan.yml"))
  file.copy(from = internal_find_script("github/datashield-analysis-suggest.yml"),
            to = paste0(new_project_path, "/.github/workflows/datashield-analysis-suggest.yml"))

  #### copies over the project README (credentials, testing mode, help)
  file.copy(from = internal_find_script("utils/README.md"),
            to = paste0(new_project_path, "/README.md"))

  #### copies over dependencies file for renv
  file.copy(from = internal_find_script("utils/dependencies.R"),
            to = paste0(new_project_path, "/dependencies.R"))


  #### modify .gitignore file
  gitignore_lines <- c("",
                       ".Renviron",
                       " ",
                       "results/figures/*",
                       "!results/figures/placeholder.txt",
                       "  ",
                       "results/tables/*",
                       "!results/tables/placeholder.txt")

  usethis::write_union(path = paste0(new_project_path, "/.gitignore"),
                       lines = gitignore_lines)


  #### initiate and fill standard .Renviron file

  r_environ_lines <- c("R_CONFIG_ACTIVE = 'production'",
                       "",
                       "OBIBA1_URL = 'https://opal-demo.obiba.org/'",
                       "OBIBA1_USER = 'dsuser'",
                       "OBIBA1_PWD = 'P@ssw0rd'",
                       "OBIBA1_TABLE = 'CNSIM.CNSIM1'",
                       " ",
                       "OBIBA2_URL = 'https://opal-demo.obiba.org/'",
                       "OBIBA2_USER = 'dsuser'",
                       "OBIBA2_PWD = 'P@ssw0rd'",
                       "OBIBA2_TABLE = 'CNSIM.CNSIM2'",
                       "  ",
                       "OBIBA3_URL = 'https://opal-demo.obiba.org/'",
                       "OBIBA3_USER = 'dsuser'",
                       "OBIBA3_PWD = 'P@ssw0rd'",
                       "OBIBA3_TABLE = 'CNSIM.CNSIM3'")

  usethis::write_over(path = paste0(new_project_path, "/.Renviron"),
                      lines = r_environ_lines)


  download.file(url = "https://github.com/datashield/dsBaseClient/raw/master/tests/testthat/data_files/CNSIM/CNSIM1.rda",
                destfile = paste0(new_project_path, "/utils/mock_data/demo_obiba/CNSIM1.rda"))
  download.file(url = "https://github.com/datashield/dsBaseClient/raw/master/tests/testthat/data_files/CNSIM/CNSIM2.rda",
                destfile = paste0(new_project_path, "/utils/mock_data/demo_obiba/CNSIM2.rda"))
  download.file(url = "https://github.com/datashield/dsBaseClient/raw/master/tests/testthat/data_files/CNSIM/CNSIM3.rda",
                destfile = paste0(new_project_path, "/utils/mock_data/demo_obiba/CNSIM3.rda"))

  #### renv: the packages go into the new project's own library; the library of the
  #### R session that runs initProject() (the analyst's, or a test run's) stays untouched
  renv::init(project = new_project_path,
             bare = TRUE,
             load = FALSE,
             restart = FALSE)

  project_library <- renv::paths$library(project = new_project_path)
  dir.create(project_library, recursive = TRUE, showWarnings = FALSE)

  renv::install(c("dsBaseClient",
                  "nfdi4health/dsSupportClient",
                  "FlorianSchw/dsAnalysis@ci/setup",
                  "config",
                  "DSLite",
                  "grateful"),
                library = project_library,
                project = new_project_path,
                prompt = FALSE)

  used <- unique(renv::dependencies(new_project_path, quiet = TRUE)$Package)

  base_pkgs      <- rownames(installed.packages(priority = "base"))
  already_there  <- rownames(installed.packages(lib.loc = project_library))
  used           <- setdiff(used, c(base_pkgs, already_there))

  if (length(used) > 0) {
    renv::install(used,
                  library = project_library,
                  project = new_project_path,
                  prompt = FALSE)
  }

  #### everything else the project's scripts use (DSLite, here, config, grateful, ...)
  renv::hydrate(library = project_library,
                project = new_project_path,
                packages = c("dsBaseClient", "dsSupportClient", "dsAnalysis"),
                prompt = FALSE)

  ip <- installed.packages(lib.loc = project_library)

  deps <- unique(unlist(
    tools::package_dependencies(rownames(ip), db = ip,
                                which = c("Depends", "Imports", "LinkingTo"))
  ))

  known   <- c(rownames(ip), rownames(installed.packages(priority = "base")), "R")
  missing <- setdiff(deps, known)

  if (length(missing) > 0) {
    renv::install(missing,
                  library = project_library,
                  project = new_project_path,
                  prompt = FALSE)
  }

  renv::snapshot(project = new_project_path,
                 library = project_library,
                 prompt = FALSE)

  if (switch_to_proj) {
    usethis::proj_activate(new_project_path)
  }

  invisible(new_project_path)


}
