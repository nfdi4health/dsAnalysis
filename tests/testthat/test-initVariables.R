test_that("initVariables writes each variable's type, range or categories, studies and label from the servers", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()

  testthat::expect_message(path <- dsAnalysis::initVariables(datasources = cnsim$conns), "Wrote 11 variables")
  testthat::expect_equal(path, file.path(cnsim$project, "config/variables.yml"))
  written <- yaml::read_yaml(path)$variables
  by_name <- stats::setNames(written, vapply(written, function(v) v$name, character(1)))

  tsc <- by_name$LAB_TSC
  testthat::expect_equal(tsc$type, "continuous")
  testthat::expect_equal(tsc$label, "Total Serum Cholesterol")    # "(en) " removed
  testthat::expect_equal(unlist(tsc$studies), c("sim1", "sim2", "sim3"))
  #### the 5% and 95% quantiles lie inside the real values, never at their ends
  real <- c(cnsim$real$sim1$LAB_TSC, cnsim$real$sim2$LAB_TSC, cnsim$real$sim3$LAB_TSC)
  testthat::expect_length(tsc$range, 2)
  testthat::expect_gt(tsc$range[[1]], min(real, na.rm = TRUE))
  testthat::expect_lt(tsc$range[[2]], max(real, na.rm = TRUE))

  gender <- by_name$GENDER
  testthat::expect_equal(gender$type, "categorical")
  testthat::expect_equal(names(gender$categories), c("0", "1"))
  testthat::expect_null(gender$range)

  #### the file starts with the header saying what it is sent to
  testthat::expect_match(readLines(path)[1], "^# Written by dsAnalysis::initVariables\\(\\)")
})

test_that("initVariables keeps the analyst's labels and added fields on a re-run", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()
  path <- file.path(cnsim$project, "config", "variables.yml")
  dir.create(dirname(path))
  writeLines(c("variables:",
               "  - name: GENDER",
               "    label: Sex at birth",
               "    type: categorical",
               "    categories: {'0': male, '1': ''}",
               "    note: coded by the study",
               "  - name: GONE",
               "    label: Not on the servers any more",
               "    type: continuous"), path)

  testthat::expect_message(dsAnalysis::initVariables(datasources = cnsim$conns), "removed: GONE")
  written <- yaml::read_yaml(path)$variables
  gender <- Find(function(v) v$name == "GENDER", written)
  testthat::expect_equal(gender$label, "Sex at birth")
  testthat::expect_equal(gender$categories[["0"]], "male")
  testthat::expect_equal(gender$note, "coded by the study")
  testthat::expect_null(Find(function(v) v$name == "GONE", written))
})

test_that("initVariables rejects datasources that aren't DSConnection objects", {
  err <- testthat::expect_error(dsAnalysis::initVariables(datasources = list(a = "not_a_connection")))
  testthat::expect_equal(err$message, "The 'datasources' were expected to be a list of DSConnection-class objects")
})

test_that("internal_login_tables reads the tables of the login template without running it", {
  login <- tempfile(fileext = ".R")
  on.exit(unlink(login))
  file.copy(internal_find_script("datashield/01_DS_Login.R"), login)
  write(c("builder$append(server = 'secret_one', url = 'x', table = Sys.getenv('T'), driver = 'OpalDriver')"),
        login, append = TRUE)

  tables <- internal_login_tables(login)
  testthat::expect_equal(tables[["DEMO_OBIBA_1"]], "CNSIM.CNSIM1")
  testthat::expect_length(tables, 3)
  testthat::expect_equal(internal_login_tables(tempfile()), character(0))
})

test_that("internal_opal_dictionary reads labels, category labels and missing codes of an Opal table", {
  testthat::skip_if_not_installed("opalr")
  connection <- methods::new("OpalConnection", name = "study1", opal = structure(list(), class = "opal"))
  seen <- NULL
  testthat::local_mocked_bindings(opal.table_dictionary_get = function(opal, project, table){
    seen <<- c(project, table)
    list(variables = data.frame(name = c("SMOKE", "AGE"), `label:en` = c("Smoking status", NA), check.names = FALSE),
         categories = data.frame(variable = "SMOKE", name = c("0", "1", "99"), missing = c(FALSE, FALSE, TRUE),
                                 `label:en` = c("never", "ever", "unknown"), check.names = FALSE))
  }, .package = "opalr")

  dictionary <- internal_opal_dictionary(connection, "STUDY.TABLE_A", "en")
  testthat::expect_equal(seen, c("STUDY", "TABLE_A"))
  testthat::expect_equal(dictionary$labels[["SMOKE"]], "Smoking status")
  testthat::expect_equal(dictionary$labels[["AGE"]], "")
  testthat::expect_equal(dictionary$categories$missing, c(FALSE, FALSE, TRUE))
  testthat::expect_equal(dictionary$categories$label[2], "ever")

  #### no table known, or not an Opal connection: nothing to read
  testthat::expect_null(internal_opal_dictionary(connection, NA_character_))
  testthat::expect_null(internal_opal_dictionary(list(), "STUDY.TABLE_A"))
})

test_that("internal_variables_yaml writes a file that reads back as the same entries", {
  entries <- list(list(name = "SMOKE", label = "Smoking \"status\"", type = "categorical",
                       categories = list(`0` = "never", `1` = ""), missing_codes = list("99"),
                       studies = list("s1", "s2"), note = "added by hand"),
                  list(name = "AGE", label = "", type = "integer", range = list(18, 79), studies = list("s1")))
  lines <- internal_variables_yaml(entries)
  testthat::expect_true("    range: [18, 79]" %in% lines)
  back <- yaml::yaml.load(paste(lines, collapse = "\n"))$variables
  testthat::expect_equal(back[[1]]$label, "Smoking \"status\"")
  testthat::expect_equal(back[[1]]$categories, list(`0` = "never", `1` = ""))
  testthat::expect_equal(back[[1]]$missing_codes, "99")
  testthat::expect_equal(back[[1]]$note, "added by hand")
  testthat::expect_equal(back[[2]]$range, c(18L, 79L))
})
