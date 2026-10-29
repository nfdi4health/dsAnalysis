test_that("list_dsPackages returns an empty tibble invisibly and no message when the catalogue is NULL", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function(refresh = FALSE) NULL)
  res <- withVisible(dsAnalysis::list_dsPackages())
  testthat::expect_false(res$visible)
  testthat::expect_s3_class(res$value, "tbl_df")
  testthat::expect_equal(nrow(res$value), 0L)
  testthat::expect_equal(ncol(res$value), 0L)
})

test_that("list_dsPackages filters by status case-insensitively and accepts several statuses", {
  fake_catalogue <- list(
    dsA = list(input = list(status = "Active", description = "a"), repo = list(Version = "1")),
    dsB = list(input = list(status = "beta", description = "b"), repo = list(Version = "2")),
    dsC = list(input = list(status = "retired", description = "c"), repo = list(Version = "3"))
  )
  testthat::local_mocked_bindings(
    internal_ds_catalogue = function(refresh = FALSE) fake_catalogue,
    internal_catalogue_client = function(name, catalogue) NA_character_,
    internal_catalogue_source = function(entry) list(cran = FALSE, repo = NA_character_)
  )
  testthat::expect_message(one <- dsAnalysis::list_dsPackages(status = "ACTIVE"), "1 DataSHIELD package")
  testthat::expect_equal(one$package, "dsA")
  testthat::expect_message(two <- dsAnalysis::list_dsPackages(status = c("active", "Beta")), "2 DataSHIELD package")
  testthat::expect_equal(two$package, c("dsA", "dsB"))
})

test_that("list_dsPackages search matches package name, client name and description case-insensitively", {
  fake_catalogue <- list(
    dsOmics = list(input = list(status = "active", description = "Genomics tools", client = "dsOmicsClient"),
                   repo = list(Version = "1.0")),
    dsOmicsClient = list(input = list(status = "active", description = "client side"), repo = list(Version = "1.0")),
    dsSurvival = list(input = list(status = "active", description = "Cox models for survival"), repo = list(Version = "2.0"))
  )
  testthat::local_mocked_bindings(
    internal_ds_catalogue = function(refresh = FALSE) fake_catalogue,
    internal_catalogue_client = function(name, catalogue) {
      cl <- catalogue[[name]]$input$client
      if (is.null(cl)) NA_character_ else cl
    },
    internal_catalogue_source = function(entry) list(cran = FALSE, repo = NA_character_)
  )
  testthat::expect_message(by_name <- dsAnalysis::list_dsPackages(search = "SURV"), "1 DataSHIELD package")
  testthat::expect_equal(by_name$package, "dsSurvival")
  testthat::expect_message(by_client <- dsAnalysis::list_dsPackages(search = "omicsclient"), "1 DataSHIELD package")
  testthat::expect_equal(by_client$package, "dsOmics")
  testthat::expect_message(by_desc <- dsAnalysis::list_dsPackages(search = "genomics"), "1 DataSHIELD package")
  testthat::expect_equal(by_desc$package, "dsOmics")
})

test_that("list_dsPackages returns a zero-row tibble with all six columns and a 'No DataSHIELD packages found' message when nothing matches", {
  fake_catalogue <- list(
    dsA = list(input = list(status = "active", description = "a"), repo = list(Version = "1"))
  )
  testthat::local_mocked_bindings(
    internal_ds_catalogue = function(refresh = FALSE) fake_catalogue,
    internal_catalogue_client = function(name, catalogue) NA_character_,
    internal_catalogue_source = function(entry) list(cran = FALSE, repo = NA_character_)
  )
  testthat::expect_message(res <- dsAnalysis::list_dsPackages(search = "zzz-no-such-thing"),
                           "No DataSHIELD packages found")
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 0L)
  testthat::expect_equal(names(res), c("package", "client", "status", "github_version", "source", "description"))
})

test_that("list_dsPackages names the alphabetically first package of the sorted result in the install hint message", {
  fake_catalogue <- list(
    dsZeta = list(input = list(status = "active", description = "z"), repo = list(Version = "1")),
    dsAlpha = list(input = list(status = "active", description = "a"), repo = list(Version = "2"))
  )
  testthat::local_mocked_bindings(
    internal_ds_catalogue = function(refresh = FALSE) fake_catalogue,
    internal_catalogue_client = function(name, catalogue) NA_character_,
    internal_catalogue_source = function(entry) list(cran = FALSE, repo = NA_character_)
  )
  testthat::expect_message(res <- dsAnalysis::list_dsPackages(),
                           'install_dsPackage\\("dsAlpha"\\)', fixed = FALSE)
  testthat::expect_equal(res$package, c("dsAlpha", "dsZeta"))
})

test_that("list_dsPackages passes refresh through to internal_ds_catalogue", {
  seen <- NULL
  testthat::local_mocked_bindings(
    internal_ds_catalogue = function(refresh = FALSE) { seen <<- refresh; NULL }
  )
  res <- dsAnalysis::list_dsPackages(refresh = TRUE)
  testthat::expect_true(seen)
  testthat::expect_equal(nrow(res), 0L)
})
