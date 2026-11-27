# Points the home directory at a temporary folder for the calling test:
# HOME and R_USER, and USERPROFILE, which fs::path_expand("~") reads on
# Windows. Stops the test if "~" still leads elsewhere, so initProject()
# never writes into the real home directory.
local_home <- function(env = parent.frame()) {
  home <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(HOME = home, R_USER = home, USERPROFILE = home, .local_envir = env)
  if (normalizePath(fs::path_expand("~"), mustWork = FALSE) != normalizePath(home, mustWork = FALSE)) {
    stop("The home directory couldn't be redirected to a temporary folder; test not run.")
  }
  home
}
