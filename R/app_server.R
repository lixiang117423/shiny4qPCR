#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  mod_cal_curve_server("cal_curve")
  mod_cal_expre_server("cal_expre")
  mod_cal_expre_ddct_server("cal_expre_ddct")
  mod_stat_server("stat")
  mod_rever_trans_server("rever_trans")
}
