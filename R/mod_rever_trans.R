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
    col_3(
      h4("Parameter Setting"),
      # 上传数据
      fileInput(
        ns("uploadfile"),
        label = h6("上传数据"),
        accept = NULL,
        buttonLabel = "View..."
      ),
      
      # 下载示例数据
      downloadLink(ns("dl_demo"),
                   label = h6("下载示例数据")
      ),
      # 输入对照处理
      textInput(
        ns("volume"),
        label = h6("反转录体积"),
        value = "2"
      ),
      # 提交按钮
      col_12(
        actionButton(
          ns("submit"),
          label = "Submit",
          icon = icon("arrow-up")
        ) %>%
          tags$div(align = "center", style = "padding-right:0em;")
      ),
      HTML("&nbsp;"),
      col_12(
        downloadButton(ns("dl_table"),
                       label = "下载表格"
        ) %>%
          tags$div(align = "center")
      )
    ),
    column(
      width = 9,
      col_12(
        tags$p(
          "Preview"
        ) %>%
          tags$div(align = "center", style = "font-size:30px")
      ),
      col_12(
        col_12(
          dataTableOutput(ns("preview")) %>%
            tags$div(style = "height:400px") %>%
            tagAppendAttributes(
              onclick = sprintf(
                "Shiny.setInputValue('%s', true, {priority : 'event'})",
                ns("show")
              )
            )
        )
      )
    )
  )
}
    
#' rever_trans Server Functions
#'
#' @noRd 
mod_rever_trans_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # 预设返回值
    r <- rv(
      df_out = NULL
    )
    
    # 下载示例数据
    output$dl_demo <- downloadHandler(
      filename = "反转录计算示例数据.tsv",
      content = function(file) {
        file.copy("./data/反转录计算示例数据.tsv", file)
      }
    )
    
    observeEvent(input$submit, {
      r$df_out <- data.table::fread(
        input$uploadfile$datapath,
        header = TRUE
      ) %>% 
        dplyr::select("Sample ID", "Nucleic Acid", "260/280") %>% 
        dplyr::rename(Sample = `Sample ID`,
                      Conc = `Nucleic Acid`) %>% 
        dplyr::group_by(Sample) %>% 
        dplyr::mutate(mean1 = mean(Conc),
                      mean2 = mean(`260/280`)) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(RNA体积 = round(as.numeric(input$volume)*1000/Conc, 2),
                      Mix体积 = as.numeric(input$volume)*4,
                      gDNARemover体积 = as.numeric(input$volume),
                      ddH2O体积 = 20*as.numeric(input$volume) - RNA体积 - Mix体积 - gDNARemover体积) %>% 
        dplyr::select(-Conc, -`260/280`) %>% 
        dplyr::rename(浓度均值 = mean1, `260/280均值` = mean2)
        
        
      r$df_out = r$df_out[!duplicated(r$df_out$Sample),]
      
       
      # 展示结果
      output$preview <- shiny::renderDataTable(options = list(pageLength = 6),{
        r$df_out
      })
      
      
      # 下载结果
      output$dl_table <- downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "-反转计算结果.xlsx")
        },
        content = function(file) {
          xlsx::write.xlsx(as.data.frame(r$df_out),
                           file,
                           # col.names = FALSE,
                           row.names = FALSE
          )
        }
      )

    })
  })
}
    
## To be copied in the UI
# mod_rever_trans_ui("rever_trans_ui_1")
    
## To be copied in the server
# mod_rever_trans_server("rever_trans_ui_1")
