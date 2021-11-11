#' rever_trans UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rever_trans_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' rever_trans Server Functions
#'
#' @noRd 
mod_rever_trans_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_rever_trans_ui("rever_trans_ui_1")
    
## To be copied in the server
# mod_rever_trans_server("rever_trans_ui_1")
