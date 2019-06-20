# Module UI
  
#' @title   mod_qf_ca_kwic_r_ui and mod_qf_ca_kwic_r_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_qf_ca_kwic_r
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_qf_ca_kwic_r_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_qf_ca_kwic_r
#' @export
#' @keywords internal
    
mod_qf_ca_kwic_r_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_qf_ca_kwic_r_ui("qf_ca_kwic_r_ui_1")
    
## To be copied in the server
# callModule(mod_qf_ca_kwic_r_server, "qf_ca_kwic_r_ui_1")
 
