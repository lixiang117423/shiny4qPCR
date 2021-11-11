#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  mod_cal_curve_server("cal_curve")
  mod_cal_expre_server("cal_expre")
}
