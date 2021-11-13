#' stat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stat_ui <- function(id) {
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
        ns("reftreatment"),
        label = h6("对照处理 (严格区分大小写)"),
        value = "CK"
      ),
      # 选择统计方法
      selectInput(
        ns("method"),
        label = h6("选择统计方法"),
        choices = list(
          "t-检验" = "ttest",
          "方差分析" = "anova"
        ),
        selected = "ttest"
      ),
      # 选择统计检验水平
      selectInput(
        ns("level"),
        label = h6("选择统计检验水平"),
        choices = list(
          "0.95" = 0.95,
          "0.99" = 0.99
        ),
        selected = 0.95
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

#' stat Server Functions
#'
#' @noRd
mod_stat_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # 预设返回值
    r <- rv(
      df_user_exp = NULL,
      df_out = NULL
    )
    # 下载示例数据
    output$dl_demo <- downloadHandler(
      filename = "表达量差异统计示例数据.xlsx",
      content = function(file) {
        file.copy("./data/表达量差异统计示例数据.xlsx", file)
      }
    )

    observeEvent(input$submit, {
      # 读入用户数据
      r$df_user_exp <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 1
      )

      if (input$method == "ttest") {
        res <- multGroupTtest(
          data = r$df_user_exp,
          group1 = "Gene",
          group2 = "Treatment",
          CK = input$reftreatment,
          value = "Expression",
          level = as.numeric(input$level)
        )
      } else {
        res <- mult.aov(
          data = r$df_user_exp,
          group1 = "Gene",
          group2 = "Treatment",
          value = "Expression",
          level = as.numeric(input$level)
        ) %>%
          dplyr::select(1, 2, 4, 5) %>%
          dplyr::mutate(temp = paste0(Gene, Treatment))

        res <- res[!duplicated(res$temp), ] %>%
          dplyr::select(-temp)
      }

      r$df_out <- res
      colnames(r$df_out) <- c("Gene", "Treatment", "Pvalue", "Significance")

      # 输出结果
      output$preview <- shiny::renderDataTable(options = list(pageLength = 6), {
        r$df_out
      })

      # 下载结果
      output$dl_table <- downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "-统计检验结果.xlsx")
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
# mod_stat_ui("stat_ui_1")

## To be copied in the server
# mod_stat_server("stat_ui_1")
