test_that("internal_dslite_step_line errors naming the missing marker when the step marker is absent", {
  codelines <- c("#### Step 1: Loading necessary libraries", "library(DSLite)")
  testthat::expect_error(dsAnalysis:::internal_dslite_step_line(codelines, 3),
                         "Could not find the line '#### Step 3: Defining the server-side data in a new dslite server' in 01_DSLite_Setup.R. Please don't edit the step markers.",
                         fixed = TRUE)
})

test_that("internal_dslite_step_line errors when the step marker occurs more than once", {
  codelines <- c("#### Step 5: Building the logindata object", "x <- 1", "#### Step 5: Building the logindata object")
  testthat::expect_error(dsAnalysis:::internal_dslite_step_line(codelines, 5),
                         "Could not find the line '#### Step 5: Building the logindata object'",
                         fixed = TRUE)
})

test_that("internal_dslite_step_line errors for an empty character vector of codelines", {
  testthat::expect_error(dsAnalysis:::internal_dslite_step_line(character(0), 1),
                         "Could not find the line '#### Step 1: Loading necessary libraries'",
                         fixed = TRUE)
})

test_that("internal_dslite_step_line errors with an NA marker name when step is out of range", {
  codelines <- c("#### Step 1: Loading necessary libraries")
  testthat::expect_error(dsAnalysis:::internal_dslite_step_line(codelines, 8),
                         "Could not find the line 'NA' in 01_DSLite_Setup.R.",
                         fixed = TRUE)
})

test_that("internal_dslite_step_line requires an exact match and errors on markers with trailing whitespace or different case", {
  codelines <- c("#### Step 2: Import of mock data files ", "#### step 6: Login to the different DSLite Servers")
  testthat::expect_error(dsAnalysis:::internal_dslite_step_line(codelines, 2),
                         "Could not find the line '#### Step 2: Import of mock data files'",
                         fixed = TRUE)
  testthat::expect_error(dsAnalysis:::internal_dslite_step_line(codelines, 6),
                         "Could not find the line '#### Step 6: Login to the different DSLite Servers'",
                         fixed = TRUE)
})

test_that("internal_dslite_step_line raises its error without a call context", {
  err <- testthat::expect_error(dsAnalysis:::internal_dslite_step_line(c("a", "b"), 4))
  testthat::expect_null(conditionCall(err))
  testthat::expect_true(grepl("#### Step 4: Defining the server-side settings", conditionMessage(err), fixed = TRUE))
})
