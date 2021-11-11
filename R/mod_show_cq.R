#' show_cq UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_show_cq_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' show_cq Server Functions
#'
#' @noRd 
mod_show_cq_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_show_cq_ui("show_cq_ui_1")
    
## To be copied in the server
# mod_show_cq_server("show_cq_ui_1")
