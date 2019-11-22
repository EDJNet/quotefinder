#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny runApp
qf_dashboard <- function(path) {
  # https://github.com/rstudio/shiny/issues/440
  .GlobalEnv$.quotefinder.path <- path
  on.exit(rm(.quotefinder.path, envir=.GlobalEnv))
  shiny::runApp(appDir = system.file("app", package = "quotefinder"))
}
  