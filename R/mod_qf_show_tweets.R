# Module UI
  
#' @title   mod_qf_show_tweets_ui and mod_qf_show_tweets_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_qf_show_tweets
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_qf_show_tweets_ui <- function(id, tweets_r){
  ns <- NS(id)
  tagList(
    shiny::textOutput(outputId = ns("letters")),
    checkboxInput(ns("heading"), "Has heading"),
    shiny::textOutput(outputId = ns("head"))
  )
}
    
# Module Server
    
#' @rdname mod_qf_show_tweets
#' @export
#' @keywords internal
    
mod_qf_show_tweets_server <- function(input, output, session, tweets){
  ns <- session$ns
  output$letters <- shiny::renderText(expr = paste(head(tweets$screen_name), collapse = ", "))
  output$head <- shiny::renderText(expr = input$heading)
  reactive({head(base::letters)})
}
    
## To be copied in the UI
# mod_qf_show_tweets_ui("qf_show_tweets_ui_1")
    
## To be copied in the server
# callModule(mod_qf_show_tweets_server, "qf_show_tweets_ui_1")
 
