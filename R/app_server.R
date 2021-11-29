#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  mod_cal_curve_server("cal_curve")
  mod_primer_server("primer")
  mod_cal_expre_server("cal_expre")
  mod_cal_expre_ddct_server("cal_expre_ddct")
  mod_cal_expre_RqPCR_server("cal_expre_RqPCR")
  mod_stat_server("stat")
  mod_rever_trans_server("rever_trans")
}
