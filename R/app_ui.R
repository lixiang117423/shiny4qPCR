#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    tagList(
      tags$style(
        "h1 {
        color: #00b76d;
        }"
      ),
      nav_(
        h1("shiny4qPCR"),
        c(
          "home" = "首页",
          "rever_trans" = "反转录计算",
          #"show_cq" = "展示Cq值",
          "cal_curve" = "计算标曲",
          "cal_expre" = "计算表达量(标曲法)",
          "cal_expre_ddct" = "计算表达量(2^-ΔΔCt法)",
          "cal_expre_RqPCR" = "计算表达量(RqPCR法)",
          "stat" = "差异表达统计",
          #"visualization" = "数据可视化",
          "about" = "About"
        )
      ),
      tags$div(
        class = "container",
        
        fluidRow(
          id = "home", mod_home_ui("home")
        ) %>% tagAppendAttributes(
          style = "display::nline-block"
        ),
        fluidRow(
          id = "rever_trans", mod_rever_trans_ui("rever_trans")
        ) %>% tagAppendAttributes(
          style = "display::nline-block"
        ),
        fluidRow(
          id = "show_cq", mod_show_cq_ui("show_cq")
        ) %>% tagAppendAttributes(
          style = "display::nline-block"
        ),
        fluidRow(
          id = "cal_curve", mod_cal_curve_ui("cal_curve")
        ) %>% tagAppendAttributes(
          style = "display::nline-block"
        ),
        fluidRow(
          id = "cal_expre", mod_cal_expre_ui("cal_expre")
        ) %>% tagAppendAttributes(
          style = "display::nline-block"
        ),
        fluidRow(
          id = "cal_expre_ddct", mod_cal_expre_ddct_ui("cal_expre_ddct")
        ) %>% tagAppendAttributes(
          style = "display::nline-block"
        ),
        fluidRow(
          id = "cal_expre_RqPCR", mod_cal_expre_RqPCR_ui("cal_expre_RqPCR")
        ) %>% tagAppendAttributes(
          style = "display::nline-block"
        ),
        fluidRow(
          id = "stat", mod_stat_ui("stat")
        ) %>% tagAppendAttributes(
          style = "display::nline-block"
        ),
        fluidRow(
          id = "visualization", mod_visualization_ui("visualization")
        ) %>% tagAppendAttributes(
          style = "display::nline-block"
        ),
        fluidRow(
          id = "about", mod_about_ui("about")
        ) %>% tagAppendAttributes(
          style = "display::nline-block"
        )
      )
    )
  )
}




#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'shiny4qPCR')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$title("shiny4qPCR"),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(
      rel="stylesheet", 
      type="text/css", 
      href="www/bootstrap.min.css",
      integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T",
      crossorigin="anonymous"
    ), 
    tags$script(
      src="www/bootstrap.min.css",
      integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM",
      crossorigin="anonymous"
    ), 
    tags$link(
      rel="stylesheet", 
      type="text/css", 
      href="www/custom.css"
    ), 
    tags$script(src="www/script.js")
  )
}

