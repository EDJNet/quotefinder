#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(title = "EdjNet's QuoteFinder"),
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(text = "MEPs on Twitter",
                                   tabName = "tab_twitter_mep",
                                   icon = shiny::icon("twitter")),
          shinydashboard::menuItem(text = "Quotes from EU institutions",
                                   tabName = "tab_eu_official",
                                   icon = shiny::icon("globe"), badgeLabel = "new"),
          disable = FALSE,
          id = "tabs"
        )
      ),
      body = shinydashboard::dashboardBody(
        
        shinydashboard::tabItems(
          
          ##### TwitterMEP tab ####
          shinydashboard::tabItem(tabName = "tab_twitter_mep", 
                                  shiny::h3("placeholder")
          ),
          ##### TwitterMEP tab ####
          shinydashboard::tabItem(tabName = "tab_eu_official", 
                                  shiny::h3("placeholder")
          )
          
        )
      ), title = "EDJNet's QuoteFinder",
      skin = "purple"
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'edjnetquotefinder')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
