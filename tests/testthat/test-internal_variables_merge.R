test_that("internal_variables_merge returns new unchanged and an empty character dropped when old is NULL or empty", {
  new <- list(list(name = "LAB_TSC", label = "", type = "numeric"))
  res_null <- dsAnalysis:::internal_variables_merge(new, NULL)
  testthat::expect_identical(res_null$variables, new)
  testthat::expect_identical(res_null$dropped, character(0))
  res_empty <- dsAnalysis:::internal_variables_merge(new, list())
  testthat::expect_identical(res_empty$variables, new)
  testthat::expect_identical(res_empty$dropped, character(0))
})

test_that("internal_variables_merge keeps the earlier label and overwrites type, range and studies with the new ones", {
  new <- list(list(name = "LAB_TSC", label = "", type = "numeric", range = c(2, 10), studies = c("sim1", "sim2")))
  old <- list(list(name = "LAB_TSC", label = "Total serum cholesterol", type = "character", range = c(0, 1), studies = "sim1"))
  res <- dsAnalysis:::internal_variables_merge(new, old)
  testthat::expect_length(res$variables, 1L)
  testthat::expect_identical(res$variables[[1]]$label, "Total serum cholesterol")
  testthat::expect_identical(res$variables[[1]]$type, "numeric")
  testthat::expect_identical(res$variables[[1]]$range, c(2, 10))
  testthat::expect_identical(res$variables[[1]]$studies, c("sim1", "sim2"))
  testthat::expect_identical(res$dropped, character(0))
})

test_that("internal_variables_merge keeps the new label when the earlier label is empty or NULL", {
  new <- list(list(name = "GENDER", label = "Gender", type = "factor"),
              list(name = "PM_BMI_CONTINUOUS", label = "BMI", type = "numeric"))
  old <- list(list(name = "GENDER", label = ""),
              list(name = "PM_BMI_CONTINUOUS", label = NULL))
  res <- dsAnalysis:::internal_variables_merge(new, old)
  testthat::expect_identical(res$variables[[1]]$label, "Gender")
  testthat::expect_identical(res$variables[[2]]$label, "BMI")
  testthat::expect_identical(res$dropped, character(0))
})

test_that("internal_variables_merge keeps filled earlier category labels per code and the new ones where the earlier is empty", {
  new <- list(list(name = "GENDER", label = "", type = "factor",
                   categories = list(`0` = "", `1` = "code 1")))
  old <- list(list(name = "GENDER", label = "Sex",
                   categories = list(`0` = "female", `1` = "")))
  res <- dsAnalysis:::internal_variables_merge(new, old)
  testthat::expect_identical(res$variables[[1]]$categories, list(`0` = "female", `1` = "code 1"))
  testthat::expect_identical(res$variables[[1]]$label, "Sex")
  testthat::expect_identical(names(res$variables[[1]]$categories), c("0", "1"))
})

test_that("internal_variables_merge ignores earlier categories that are no longer present in the new entry", {
  new <- list(list(name = "GENDER", label = "", type = "factor",
                   categories = list(`0` = "", `1` = "")))
  old <- list(list(name = "GENDER", label = "",
                   categories = list(`0` = "female", `1` = "male", `9` = "unknown")))
  res <- dsAnalysis:::internal_variables_merge(new, old)
  testthat::expect_identical(res$variables[[1]]$categories, list(`0` = "female", `1` = "male"))
  testthat::expect_length(res$variables[[1]]$categories, 2L)
})

test_that("internal_variables_merge appends extra fields the analyst added and does not copy earlier reserved fields", {
  new <- list(list(name = "LAB_HDL", label = "", type = "numeric", range = c(0.2, 3)))
  old <- list(list(name = "LAB_HDL", label = "HDL", type = "character", range = c(0, 1),
                   missing_codes = c(-99), studies = "sim1",
                   unit = "mmol/l", source = "lab"))
  res <- dsAnalysis:::internal_variables_merge(new, old)
  v <- res$variables[[1]]
  testthat::expect_identical(names(v), c("name", "label", "type", "range", "unit", "source"))
  testthat::expect_identical(v$unit, "mmol/l")
  testthat::expect_identical(v$source, "lab")
  testthat::expect_identical(v$range, c(0.2, 3))
  testthat::expect_null(v$missing_codes)
  testthat::expect_null(v$studies)
})

test_that("internal_variables_merge returns the names of earlier variables that are not in new as dropped", {
  new <- list(list(name = "GENDER", label = "Gender"),
              list(name = "LAB_TSC", label = "TSC"))
  old <- list(list(name = "LAB_TSC", label = "Cholesterol"),
              list(name = "OLD_ONE", label = "gone"),
              list(name = "ANOTHER_OLD", label = "also gone"))
  res <- dsAnalysis:::internal_variables_merge(new, old)
  testthat::expect_identical(res$dropped, c("OLD_ONE", "ANOTHER_OLD"))
  testthat::expect_length(res$variables, 2L)
  testthat::expect_identical(vapply(res$variables, function(v) v$name, character(1)), c("GENDER", "LAB_TSC"))
  testthat::expect_identical(res$variables[[1]]$label, "Gender")
  testthat::expect_identical(res$variables[[2]]$label, "Cholesterol")
})

test_that("internal_variables_merge ignores earlier entries without a name and omits them from dropped", {
  new <- list(list(name = "GENDER", label = "Gender"))
  old <- list(list(label = "no name here"),
              list(name = "GENDER", label = "Sex"),
              list(name = "GONE", label = "x"))
  res <- dsAnalysis:::internal_variables_merge(new, old)
  testthat::expect_identical(res$variables[[1]]$label, "Sex")
  testthat::expect_identical(res$dropped, "GONE")
})

test_that("internal_variables_merge keeps new entries unchanged when no earlier entry has the same name", {
  new <- list(list(name = "NEW_VAR", label = "", type = "numeric"))
  old <- list(list(name = "OTHER", label = "Other", unit = "kg"))
  res <- dsAnalysis:::internal_variables_merge(new, old)
  testthat::expect_identical(res$variables, new)
  testthat::expect_identical(res$dropped, "OTHER")
})

test_that("internal_variables_merge returns variables in the order of new and preserves the new order of overlapping names", {
  new <- list(list(name = "A", label = ""), list(name = "B", label = ""), list(name = "C", label = ""))
  old <- list(list(name = "C", label = "c label"), list(name = "A", label = "a label"))
  res <- dsAnalysis:::internal_variables_merge(new, old)
  testthat::expect_identical(vapply(res$variables, function(v) v$name, character(1)), c("A", "B", "C"))
  testthat::expect_identical(res$variables[[1]]$label, "a label")
  testthat::expect_identical(res$variables[[2]]$label, "")
  testthat::expect_identical(res$variables[[3]]$label, "c label")
  testthat::expect_identical(res$dropped, character(0))
})
