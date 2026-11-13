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

test_that("initVariables messages about servers without an Opal dictionary and writes the file at a given relative path", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()
  
  testthat::expect_message(path <- dsAnalysis::initVariables(datasources = cnsim$conns, file = "meta/vars.yml"),
                           "No Opal dictionary read for sim1, sim2, sim3")
  testthat::expect_equal(path, file.path(cnsim$project, "meta/vars.yml"))
  testthat::expect_true(file.exists(path))
  written <- yaml::read_yaml(path)$variables
  testthat::expect_length(written, 11)
  #### no Opal dictionary: every category label stays empty
  gender <- Find(function(v) v$name == "GENDER", written)
  testthat::expect_equal(unlist(gender$categories, use.names = FALSE), c("", ""))
  testthat::expect_null(gender$missing_codes)
})

test_that("initVariables writes every column of CNSIM with its type from the servers", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()
  
  testthat::expect_message(path <- dsAnalysis::initVariables(datasources = cnsim$conns), "Wrote 11 variables")
  written <- yaml::read_yaml(path)$variables
  names_written <- vapply(written, function(v) v$name, character(1))
  testthat::expect_equal(sort(names_written), sort(colnames(cnsim$real$sim1)))
  
  types <- stats::setNames(vapply(written, function(v) v$type, character(1)), names_written)
  testthat::expect_equal(types[["GENDER"]], "categorical")
  testthat::expect_equal(types[["PM_BMI_CATEGORICAL"]], "categorical")
  testthat::expect_equal(types[["LAB_HDL"]], "continuous")
  testthat::expect_equal(types[["PM_BMI_CONTINUOUS"]], "continuous")
  
  #### categorical entries carry levels and no range, continuous ones a range of two
  bmi_cat <- Find(function(v) v$name == "PM_BMI_CATEGORICAL", written)
  testthat::expect_equal(names(bmi_cat$categories), c("1", "2", "3"))
  testthat::expect_null(bmi_cat$range)
  hdl <- Find(function(v) v$name == "LAB_HDL", written)
  testthat::expect_length(hdl$range, 2)
  testthat::expect_lt(hdl$range[[1]], hdl$range[[2]])
  testthat::expect_equal(unlist(hdl$studies), c("sim1", "sim2", "sim3"))
})

test_that("initVariables leaves out variables that are neither numeric, integer nor factor and names them in a message", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()
  
  testthat::local_mocked_bindings(internal_variable_facts = function(x, datasources){
    if (grepl("GENDER$", x)) list(type = "other") else list(type = "continuous", range = c(1, 2))
  }, .package = "dsAnalysis")
  
  testthat::expect_message(path <- dsAnalysis::initVariables(datasources = cnsim$conns),
                           "Left out \\(not numeric, integer or factor\\): GENDER")
  written <- yaml::read_yaml(path)$variables
  testthat::expect_length(written, 10)
  testthat::expect_null(Find(function(v) v$name == "GENDER", written))
})

test_that("initVariables rejects an empty list of datasources", {
  err <- testthat::expect_error(dsAnalysis::initVariables(datasources = list()))
  testthat::expect_equal(err$message, "The 'datasources' were expected to be a list of DSConnection-class objects")
})

test_that("initVariables replaces the old header and keeps no comments on a re-run", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()
  path <- file.path(cnsim$project, "config", "variables.yml")
  dir.create(dirname(path))
  writeLines(c("# a comment of my own",
               "variables:",
               "  - name: GENDER",
               "    label: Sex at birth",
               "    type: categorical",
               "    categories: {'0': male, '1': ''}"), path)
  
  testthat::expect_message(dsAnalysis::initVariables(datasources = cnsim$conns), "Wrote 11 variables")
  lines <- readLines(path)
  testthat::expect_false("# a comment of my own" %in% lines)
  testthat::expect_match(lines[1], "^# Written by dsAnalysis::initVariables\\(\\)", fixed = FALSE)
  testthat::expect_true(any(grepl("^# range: the pooled 5% and 95% quantiles", lines)))
  testthat::expect_equal(sum(grepl("^# Written by", lines)), 1)
})

test_that("internal_studies_with_data leaves out studies where all values are missing", {
  conns <- list(a = "conn_a", b = "conn_b", c = "conn_c")
  testthat::local_mocked_bindings(
    ds.length = function(x, type, datasources) list(`length of D$X in a` = 10, `length of D$X in b` = 5, `length of D$X in c` = 8),
    ds.numNA = function(x, datasources) list(a = 2, b = 5, c = 0),
    .package = "dsBaseClient")
  testthat::expect_equal(internal_studies_with_data("D$X", conns), c("a", "c"))
})

test_that("internal_studies_with_data keeps all studies when the counts can't be read", {
  conns <- list(a = "conn_a", b = "conn_b")
  testthat::local_mocked_bindings(
    ds.length = function(x, type, datasources) stop("disclosure control"),
    ds.numNA = function(x, datasources) list(a = 0, b = 0),
    .package = "dsBaseClient")
  testthat::expect_equal(internal_studies_with_data("D$X", conns), c("a", "b"))
})

test_that("initVariables leaves out a study whose values are all missing, and a variable missing everywhere", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()
  #### stand-ins: LAB_TSC has no values in sim2, LAB_HDL none anywhere
  testthat::local_mocked_bindings(internal_studies_with_data = function(x, datasources){
    if (x == "D$LAB_HDL") return(character(0))
    if (x == "D$LAB_TSC") return(setdiff(names(datasources), "sim2"))
    names(datasources)
  })

  testthat::expect_message(path <- dsAnalysis::initVariables(datasources = cnsim$conns),
                           "all values missing in every study\\): LAB_HDL")
  written <- yaml::read_yaml(path)$variables
  by_name <- stats::setNames(written, vapply(written, function(v) v$name, character(1)))
  testthat::expect_null(by_name$LAB_HDL)
  testthat::expect_equal(unlist(by_name$LAB_TSC$studies), c("sim1", "sim3"))
  testthat::expect_length(by_name$LAB_TSC$range, 2)
})

test_that("initVariables uses the tables argument for the Opal dictionary and reports only the servers left without one", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()
  seen <- list()
  testthat::local_mocked_bindings(internal_opal_dictionary = function(connection, table, locale = "en"){
    seen[[length(seen) + 1]] <<- table
    if (is.na(table)) return(NULL)
    list(labels = list(GENDER = "Sex of the participant"),
         categories = data.frame(variable = "GENDER", name = c("0", "1"), missing = c(FALSE, FALSE),
                                 label = c("female", "male"), stringsAsFactors = FALSE))
  })
  
  testthat::expect_message(path <- dsAnalysis::initVariables(datasources = cnsim$conns,
                                                             tables = list(sim1 = "CNSIM.CNSIM1")),
                           "No Opal dictionary read for sim2, sim3")
  testthat::expect_equal(unlist(seen), c("CNSIM.CNSIM1", NA, NA))
  written <- yaml::read_yaml(path)$variables
  gender <- Find(function(v) v$name == "GENDER", written)
  testthat::expect_equal(gender$label, "Sex of the participant")
  testthat::expect_equal(gender$categories, list(`0` = "female", `1` = "male"))
  testthat::expect_null(gender$missing_codes)
})

test_that("initVariables drops a dictionary's missing codes from the categories and lists them under missing_codes", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()
  testthat::local_mocked_bindings(internal_opal_dictionary = function(connection, table, locale = "en"){
    list(labels = list(),
         categories = data.frame(variable = "PM_BMI_CATEGORICAL", name = c("1", "2", "3"),
                                 missing = c(FALSE, FALSE, TRUE), label = c("normal", "overweight", "unknown"),
                                 stringsAsFactors = FALSE))
  })
  
  testthat::expect_message(path <- dsAnalysis::initVariables(datasources = cnsim$conns), "Wrote 11 variables")
  written <- yaml::read_yaml(path)$variables
  bmi <- Find(function(v) v$name == "PM_BMI_CATEGORICAL", written)
  testthat::expect_equal(names(bmi$categories), c("1", "2"))
  testthat::expect_equal(unlist(bmi$categories, use.names = FALSE), c("normal", "overweight"))
  testthat::expect_equal(unlist(bmi$missing_codes), "3")
})

test_that("initVariables keeps a variable held by only some servers and lists just those studies", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()
  columns_real <- dsBaseClient::ds.colnames("D", datasources = cnsim$conns)
  testthat::local_mocked_bindings(ds.colnames = function(x, datasources){
    columns_real$sim2 <- setdiff(columns_real$sim2, "LAB_HDL")
    columns_real
  }, .package = "dsBaseClient")
  
  testthat::expect_message(path <- dsAnalysis::initVariables(datasources = cnsim$conns), "Wrote 11 variables")
  written <- yaml::read_yaml(path)$variables
  hdl <- Find(function(v) v$name == "LAB_HDL", written)
  testthat::expect_equal(unlist(hdl$studies), c("sim1", "sim3"))
  tsc <- Find(function(v) v$name == "LAB_TSC", written)
  testthat::expect_equal(unlist(tsc$studies), c("sim1", "sim2", "sim3"))
})

test_that("initVariables does not message about empty or skipped variables when every variable has data", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()
  
  messages <- testthat::capture_messages(dsAnalysis::initVariables(datasources = cnsim$conns))
  testthat::expect_false(any(grepl("all values missing in every study", messages)))
  testthat::expect_false(any(grepl("not numeric, integer or factor", messages)))
  testthat::expect_false(any(grepl("No longer on any server", messages)))
  testthat::expect_true(any(grepl("Wrote 11 variables to config/variables.yml\\.", messages)))
})

test_that("initVariables asks for the facts and the label of only the studies that have data for a variable", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()
  asked_facts <- list()
  asked_label <- list()
  testthat::local_mocked_bindings(
    internal_studies_with_data = function(x, datasources){
      if (x == "D$LAB_TSC") return(setdiff(names(datasources), "sim2"))
      names(datasources)
    },
    internal_variable_facts = function(x, datasources){
      asked_facts[[x]] <<- names(datasources)
      list(type = "continuous", range = c(1, 2))
    },
    internal_metadata_label = function(x, datasources){
      asked_label[[x]] <<- names(datasources)
      ""
    })
  
  testthat::expect_message(dsAnalysis::initVariables(datasources = cnsim$conns), "Wrote 11 variables")
  testthat::expect_equal(asked_facts[["D$LAB_TSC"]], c("sim1", "sim3"))
  testthat::expect_equal(asked_label[["D$LAB_TSC"]], c("sim1", "sim3"))
  testthat::expect_equal(asked_facts[["D$GENDER"]], c("sim1", "sim2", "sim3"))
})

test_that("initVariables writes the file into a directory that does not exist yet and returns its path invisibly", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()
  
  testthat::expect_message(path <- dsAnalysis::initVariables(datasources = cnsim$conns,
                                                             file = "a/b/c/vars.yml"),
                           "Wrote 11 variables to a/b/c/vars.yml\\.")
  testthat::expect_equal(path, file.path(cnsim$project, "a/b/c/vars.yml"))
  testthat::expect_true(dir.exists(file.path(cnsim$project, "a", "b", "c")))
  testthat::expect_length(yaml::read_yaml(path)$variables, 11)
})
