#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny runApp
qf_dashboard <- function(path) {
  # https://github.com/rstudio/shiny/issues/440
  .GlobalEnv$.quotefinder.path <- path
  on.exit(rm(.quotefinder.path, envir=.GlobalEnv))
  
  emm_df <- readRDS(file = file.path(path,"qf_data", "emm_newsbrief_processed", "emm_newsbrief_processed.rds"))
  emm_languages <- readRDS(file = file.path(path,"qf_data", "emm_newsbrief_processed", "emm_newsbrief_languages.rds"))
  
  shiny::runApp(appDir = system.file("app", package = "quotefinder"))
}
  