APPS_DIR <- "apps"

run_all_tests <- function() {
  no_tests <- NULL
  test_results <- NULL
  for (app in get_apps()) {
    if (!has_tests(app)) {
      no_tests <- c(no_tests, app)
      next
    }
    test_results <- c(test_results, run_one(app))
  }
  failed <- report_status(test_results, no_tests)
  quit(status = if(failed) 1 else 0)
}

run_one <- function(app) {
  res <- try(shiny::runTests(app))
  status <- !inherits(res, "try-error")
  setNames(status, app)
}

report_status <- function(results, no_tests) {
  failed <- FALSE
  message("============")
  message("Test report:")
  message("============")
  for (app in names(results)) {
    status <- results[app]
    failed <- failed | !status
    message(paste0(app, ": ", if (status) "PASS" else "FAIL"))
  }
  message(paste0("The following apps have no tests: ",
                 paste0(no_tests, collapse = ", ")))
  return(failed)
}

get_apps <- function() {
  apps <- Filter(function(appname) file.exists(file.path(appname, "app.R")),
                 list.files('.'))
  sort(apps)
}

has_tests <- function(app) {
  file.exists(file.path(app, "tests"))
}

in_apps_dir <- function(expr) {
  current_wd <- getwd()
  on.exit(setwd(current_wd))
  setwd(APPS_DIR)
  evalq(expr)
}

in_apps_dir(run_all_tests())