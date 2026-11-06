#### For tests of functions that use DataSHIELD connections: a temporary project folder
#### (here::here() points to it) and three DSLite servers with DSLite's CNSIM data, logged in
#### with `symbol`. Everything is cleaned up when the calling test ends. Returns
#### list(project, conns, real): `real` is each server's data (sim1, sim2, sim3).
local_cnsim_project <- function(symbol = "D", env = parent.frame()){

  project <- tempfile("cnsim-project-")
  dir.create(file.path(project, "utils", "mock_data"), recursive = TRUE)
  withr::defer(unlink(project, recursive = TRUE), envir = env)
  testthat::local_mocked_bindings(here = function(...) file.path(project, ...), .package = "here", .env = env)

  cnsim <- new.env()
  utils::data("CNSIM1", "CNSIM2", "CNSIM3", "logindata.dslite.cnsim", package = "DSLite", envir = cnsim)

  #### the DSLite driver finds the server object by its name in the global environment
  assign("dslite.server",
         DSLite::newDSLiteServer(tables = list(CNSIM1 = cnsim$CNSIM1,
                                               CNSIM2 = cnsim$CNSIM2,
                                               CNSIM3 = cnsim$CNSIM3),
                                 config = DSLite::defaultDSConfiguration(include = "dsBase")),
         envir = globalenv())
  withr::defer(rm("dslite.server", envir = globalenv()), envir = env)

  conns <- DSI::datashield.login(logins = cnsim$logindata.dslite.cnsim, assign = TRUE, symbol = symbol)
  withr::defer(DSI::datashield.logout(conns), envir = env)

  list(project = project, conns = conns, real = list(sim1 = cnsim$CNSIM1, sim2 = cnsim$CNSIM2, sim3 = cnsim$CNSIM3))
}
