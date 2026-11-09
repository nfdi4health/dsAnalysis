test_that("internal_renv_quietly suppresses messages and warnings of the expression and still returns its value", {
  testthat::expect_silent(res <- dsAnalysis:::internal_renv_quietly({message("a message"); warning("a warning"); "done"}))
  testthat::expect_identical(res, "done")
})

test_that("internal_renv_quietly propagates an error raised by the expression", {
  testthat::expect_error(dsAnalysis:::internal_renv_quietly(stop("boom")), "boom", fixed = TRUE)
})

test_that("internal_renv_quietly returns NULL when the expression evaluates to NULL", {
  res <- dsAnalysis:::internal_renv_quietly({cat("noise\n"); NULL})
  testthat::expect_null(res)
})

test_that("internal_renv_quietly evaluates the expression exactly once and keeps its side effects on files", {
  tmp <- withr::local_tempdir()
  counter <- 0L
  res <- dsAnalysis:::internal_renv_quietly({
    counter <- counter + 1L
    writeLines(as.character(counter), file.path(tmp, "count.txt"))
    cat("writing\n")
    file.path(tmp, "count.txt")
  })
  testthat::expect_identical(counter, 1L)
  testthat::expect_true(file.exists(file.path(tmp, "count.txt")))
  testthat::expect_identical(readLines(file.path(tmp, "count.txt")), "1")
  testthat::expect_identical(res, file.path(tmp, "count.txt"))
})

test_that("internal_renv_quietly returns a complex value such as a data.frame unchanged", {
  df <- data.frame(server = c("dsBase", "dsSurvival"), client = c("dsBaseClient", "dsSurvivalClient"), stringsAsFactors = FALSE)
  res <- dsAnalysis:::internal_renv_quietly({print(df); df})
  testthat::expect_identical(res, df)
  testthat::expect_equal(nrow(res), 2L)
  testthat::expect_identical(names(res), c("server", "client"))
})
