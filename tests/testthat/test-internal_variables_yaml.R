test_that("internal_variables_yaml returns only the header line for an empty variable list", {
  res <- dsAnalysis:::internal_variables_yaml(list())
  testthat::expect_identical(res, "variables:")
  testthat::expect_length(res, 1L)
})

test_that("internal_variables_yaml writes name, empty label, unquoted type and studies for a minimal entry", {
  v <- list(list(name = "LAB_TSC", type = "numeric", studies = c("sim1", "sim2")))
  res <- dsAnalysis:::internal_variables_yaml(v)
  testthat::expect_identical(res, c("variables:",
                                    "  - name: \"LAB_TSC\"",
                                    "    label: \"\"",
                                    "    type: numeric",
                                    "    studies: [\"sim1\", \"sim2\"]"))
  testthat::expect_length(res, 5L)
})

test_that("internal_variables_yaml writes categories as an indented block of quoted key/value pairs", {
  v <- list(list(name = "GENDER", label = "Sex", type = "factor",
                 categories = list(`0` = "female", `1` = "male"),
                 studies = "sim1"))
  res <- dsAnalysis:::internal_variables_yaml(v)
  testthat::expect_identical(res, c("variables:",
                                    "  - name: \"GENDER\"",
                                    "    label: \"Sex\"",
                                    "    type: factor",
                                    "    categories:",
                                    "      \"0\": \"female\"",
                                    "      \"1\": \"male\"",
                                    "    studies: [\"sim1\"]"))
  testthat::expect_length(res, 8L)
})

test_that("internal_variables_yaml writes range as an unquoted flow list and missing_codes as a quoted flow list", {
  v <- list(list(name = "LAB_HDL", label = "HDL", type = "numeric",
                 missing_codes = c(-99, -98), range = c(0.05, 4.1),
                 studies = c("sim1", "sim2", "sim3")))
  res <- dsAnalysis:::internal_variables_yaml(v)
  testthat::expect_identical(res, c("variables:",
                                    "  - name: \"LAB_HDL\"",
                                    "    label: \"HDL\"",
                                    "    type: numeric",
                                    "    missing_codes: [\"-99\", \"-98\"]",
                                    "    range: [0.05, 4.1]",
                                    "    studies: [\"sim1\", \"sim2\", \"sim3\"]"))
})

test_that("internal_variables_yaml writes unknown extra fields with yaml::as.yaml below the known fields", {
  v <- list(list(name = "PM_BMI_CONTINUOUS", label = "BMI", type = "numeric",
                 studies = "sim1", source = "registry"))
  res <- dsAnalysis:::internal_variables_yaml(v)
  testthat::expect_identical(res, c("variables:",
                                    "  - name: \"PM_BMI_CONTINUOUS\"",
                                    "    label: \"BMI\"",
                                    "    type: numeric",
                                    "    studies: [\"sim1\"]",
                                    "    source: registry"))
  testthat::expect_length(res, 6L)
})

test_that("internal_variables_yaml escapes double quotes and backslashes in labels", {
  v <- list(list(name = "X", label = 'a "quoted" \\ label', type = "character", studies = "sim1"))
  res <- dsAnalysis:::internal_variables_yaml(v)
  testthat::expect_identical(res[3], "    label: \"a \\\"quoted\\\" \\\\ label\"")
  testthat::expect_identical(yaml::yaml.load(paste(res, collapse = "\n"))$variables[[1]]$label,
                             'a "quoted" \\ label')
})

test_that("internal_variables_yaml omits categories, missing_codes and range lines when they are NULL or empty", {
  v <- list(list(name = "X", label = "L", type = "integer",
                 categories = list(), missing_codes = NULL, range = numeric(0),
                 studies = "sim3"))
  res <- dsAnalysis:::internal_variables_yaml(v)
  testthat::expect_length(res, 5L)
  testthat::expect_false(any(grepl("categories", res, fixed = TRUE)))
  testthat::expect_false(any(grepl("missing_codes", res, fixed = TRUE)))
  testthat::expect_false(any(grepl("range", res, fixed = TRUE)))
  testthat::expect_identical(res[5], "    studies: [\"sim3\"]")
})

test_that("internal_variables_yaml drops trailing zeros of a single-value range and keeps it unquoted", {
  v <- list(list(name = "AGE", type = "integer", range = c(18.0, 100.0), studies = "sim1"))
  res <- dsAnalysis:::internal_variables_yaml(v)
  testthat::expect_identical(res[5], "    range: [18, 100]")
  testthat::expect_identical(yaml::yaml.load(paste(res, collapse = "\n"))$variables[[1]]$range, c(18L, 100L))
})
