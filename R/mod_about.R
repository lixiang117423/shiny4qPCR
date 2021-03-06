#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_6(
      includeMarkdown(
        system.file("app/www/about.md", package = "shiny4qPCR")
      )
    ), 
    col_6(
      includeMarkdown(
        system.file("app/www/tech.md", package = "shiny4qPCR")
      )
    )
  )
}
    
#' about Server Functions
#'
#' @noRd 
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_about_ui("about_ui_1")
    
## To be copied in the server
# mod_about_server("about_ui_1")
