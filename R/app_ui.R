#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(title = tags$a(href='https://www.europeandatajournalism.eu/',                                                              tags$img(src='logo_50_230_w.png'))), # for animated version of the logo, check https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
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
          
          ##### tab_twitter_mep ####
          shinydashboard::tabItem(tabName = "tab_twitter_mep", 
                                  shiny::h3("Twitter MEP"),
                                  mod_qf_show_tweets_ui("qf_show_tweets_ui_1", tweets_r)
          ),
          ##### tab_eu_official ####
          shinydashboard::tabItem(tabName = "tab_eu_official",
                                  shiny::textInput(inputId = 'term',
                                                   label = 'Search',
                                                   value = ""),
                                  shiny::radioButtons(inputId = "freq",
                                                      label = NULL,
                                                      choices = c("Absolute frequency",
                                                                  "Relative frequency")),
                                  shiny::sliderInput(inputId = "rollingAverage",
                                                     label = "Apply rolling average for ... days",
                                                     min = 1,
                                                     max = 91,
                                                     value = 91,
                                                     round = TRUE),
                                  shiny::uiOutput(outputId = "dateRangeInput_UI"),
                                  shiny::actionButton("go", "Go!"),

                                  shiny::fluidRow(DT::dataTableOutput(outputId = "kwic"))
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
