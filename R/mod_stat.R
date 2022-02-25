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
      h4("Parameter Setting",align="center"),
      HTML("<hr style='background-color: #282828'>"),
      # 上传数据
      fileInput(
        ns("uploadfile"),
        label = h6("Upload data"),
        accept = NULL,
        buttonLabel = "View..."
      ),

      # 下载示例数据
      downloadLink(ns("dl_demo"),
        label = h6("Doanload demo data")
      ),
      # sheet index
      selectInput(
        ns("sheetindex"),
        label = h6("Sheet index"),
        choices = list(
          "1" = 1,
          "2" = 2,
          "3" = 3,
          "4" = 4,
          "5" = 5
        ),
        selected = "2"
      ),
      # 输入对照处理
      textInput(
        ns("reftreatment"),
        label = h6("CK"),
        value = "CK"
      ),
      # 选择统计方法
      selectInput(
        ns("method"),
        label = h6("Statistical method"),
        choices = list(
          "t-test" = "ttest",
          "Anova" = "anova"
        ),
        selected = "ttest"
      ),
      # 选择统计检验水平
      numericInput(
        ns("level"),
        label = h6("Level"),
        min = 0,
        max = 1,
        value = 0.95,
        step = 0.01
      ),
      if (FALSE) {
        # 是否绘图
        selectInput(
          ns('plot'),
          label = h6("Plot or not"),
          choices = list(
            "Yes" = "yes",
            "No" = "no"
          ),
          selected = "是"
        )
        # 选择绘图样式
        selectInput(
          ns("type"),
          label = h6("选择绘图样式"),
          choices = list(
            "箱线图" = "box",
            "柱状图" = "bar",
            "小提琴图" = "violin"
            #"热图" = "heatmap"
          ),
          selected = "box"
        )
        # 是否加上散点
        selectInput(
          ns("point"),
          label = h6("是否添加散点"),
          choices = list(
            "是" = "yes",
            "否" = "no"
          ),
          selected = "yes"
        )
      },
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
          label = "Download Excel"
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
      df_out = NULL,
      plot = NULL,
      df_out_2 = NULL
    )
    # 下载示例数据
    output$dl_demo <- downloadHandler(
      filename = "DemoData.xlsx",
      content = function(file) {
        file.copy("./data/表达量差异统计分析示例数据.xlsx", file)
      }
    )

    observeEvent(input$submit, {
      # 读入用户数据
      r$df_user_exp <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = as.numeric(input$sheetindex)
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
      
      # 合并输入数据和统计检验结果
      df_temp <- r$df_out %>% 
        dplyr::mutate(temp = paste0(Gene,Treatment)) %>% 
        dplyr::select(temp, Pvalue, Significance)
      
      r$df_user_exp %>% 
        dplyr::mutate(temp = paste0(Gene,Treatment)) %>% 
        merge(df_temp, by = "temp", all.x = TRUE) %>% 
        #dplyr::group_by(temp) %>% 
        #dplyr::mutate(max = max(Expression)) %>% 
        #dplyr::ungroup() %>% 
        #dplyr::select(-temp,-max) %>% 
        dplyr::select(-temp) %>%
        dplyr::select(colnames(r$df_user_exp), "Pvalue", "Significance") -> r$df_out_2 
      
      colnames(r$df_out_2)[(ncol(r$df_out_2 ) - 1)] = paste0("Pvalue_",input$method)
      colnames(r$df_out_2)[ncol(r$df_out_2)] = paste0("Significance_",input$method)
      
      r$df_out <- r$df_out_2 %>% 
        dplyr::mutate(temp = paste0(Gene,Treatment))
      
      r$df_out <- r$df_out[!duplicated(r$df_out$temp),] %>% 
        dplyr::select(Treatment,	Gene, paste0("Significance_",input$method))
      
        
      # 绘图
      if(F){
        if (input$plot == "yes") {
          r$plot <- ggplot2::ggplot(df_plot, ggplot2::aes(Treatment, Expression))
          # 选择绘图方式
          if (input$type == "box") {
            r$plot <- r$plot + ggplot2::geom_boxplot(ggplot2::aes(fill = Treatmen)) # 箱线图
          }else if (input$type == "bar") {
            r$plot <- r$plot + ggplot2::geom_bar(ggplot2::aes(fill = Treatmen)) +
              ggplot2::geom_errorbar(ggplot2::aes(x = Treatment, ymin = Mean - SD, ymax = MEan + SD), width = 0.2)# 柱状图
          }else {
            r$plot <- r$plot + ggplot2::geom_violin(ggplot2::aes(fill = Treatmen)) # 小提琴图
          }
          
          # 是否画上散点
          if (input$point == "yes") {
            r$plot <- r$plot + ggplot2::geom_jitter(size = 1.2, width = 0.1)
          }else{
            r$plot <- r$plot
          }
          
          r$plot <- r$plot + 
            scale_fill_aaas() +
            theme_bw()
          
        }else{
          r$plot <- NULL
        }
      }
      
      df_list <- list("statistical result" = r$df_out, "raw data+results" = r$df_out_2)

      # 输出结果
      output$preview <- shiny::renderDataTable(options = list(pageLength = 6), {
        r$df_out
      })
      
      

      # 下载结果
      output$dl_table <- downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "-results.xlsx")
        },
        content = function(file) {
          openxlsx::write.xlsx(df_list, file)
        }
      )
    })
  })
}

## To be copied in the UI
# mod_stat_ui("stat_ui_1")

## To be copied in the server
# mod_stat_server("stat_ui_1")
