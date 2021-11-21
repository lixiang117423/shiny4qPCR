#' cal_expre_ddct UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cal_expre_ddct_ui <- function(id) {
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
      # 是否剔除空值
      selectInput(
        ns("dropNA"),
        label = h6("是否剔除空值"),
        choices = list(
          "是" = "TRUE",
          "否" = "FALSE"
        ),
        selected = "TRUE"
      ),
      # 空值填充方法
      selectInput(
        ns("fillNA"),
        label = h6("空值填充方法"),
        choices = list(
          "均值填充" = "mean",
          "最大值填充" = "max"
        ),
        selected = "mean"
      ),
      # 输入对照处理
      textInput(
        ns("reftreatment"),
        label = h6("对照处理 (严格区分大小写)"),
        value = "CK"
      ),
      # 输入内参基因
      textInput(
        ns("refgene"),
        label = h6("内参基因 (严格区分大小写)"),
        value = "OsUBQ"
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

#' cal_expre_ddct Server Functions
#'
#' @noRd
#' @importFrom readxl read_excel
#' @importFrom shiny NS tagList renderDataTable downloadHandler
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate rename filter group_by ungroup n
#' @importFrom stringr str_sub
#' @importFrom reshape2 melt
#' @importFrom xlsx write.xlsx
mod_cal_expre_ddct_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # 预设返回值
    r <- rv(
      df_user_exp = NULL,
      df_user_design_treat = NULL,
      df_user_design_gene = NULL,
      df_out = NULL,
      df_out_2 = NULL,
      df_list = NULL
    )
    # 下载示例数据
    output$dl_demo <- downloadHandler(
      filename = "表达量计算示例数据_2-ΔΔCt法.xlsx",
      content = function(file) {
        file.copy("./data/表达量计算示例数据_2-ΔΔCt法.xlsx", file)
      }
    )

    # 正式程序
    observeEvent(input$submit, {
      # 读入Cq矩阵
      r$df_user_exp <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 1
      ) %>%
        dplyr::select(Position, Cq, Batch) %>%
        dplyr::mutate(
          P = stringr::str_sub(Position, 1, 1),
          N = as.numeric(stringr::str_sub(Position, 2, nchar(Position)))
        ) %>%
        dplyr::select(N, P, Cq, Position, Batch) %>% 
        dplyr::mutate(temp = paste0(Position, Batch))

      # 读入处理矩阵
      r$df_user_design_treat <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 2
      ) %>%
        reshape2::melt(id.vars = 1:2) %>%
        dplyr::mutate(Position = paste0(N, variable)) %>%
        dplyr::rename(Treatment = value) %>%
        dplyr::select(Position, Treatment, Batch) %>% 
        dplyr::mutate(temp = paste0(Position, Batch))

      # 读入基因矩阵
      r$df_user_design_gene <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 3
      ) %>%
        reshape2::melt(id.vars = 1:2) %>%
        dplyr::mutate(Position = paste0(N, variable)) %>%
        dplyr::rename(Gene = value) %>%
        dplyr::select(Position, Gene, Batch) %>% 
        dplyr::mutate(temp = paste0(Position, Batch))

      # 开始计算
      df <- merge(r$df_user_exp, r$df_user_design_treat, by = "temp") %>%
        merge(r$df_user_design_gene, by = "temp")
      
    
      # 填充空值
      df %>% dplyr::mutate(group = paste0(Treatment, Gene, Batch)) -> df
      
      df_new <- NULL

      for (i in unique(df$group)) {
        df_temp <- df %>%
          dplyr::filter(group == i) %>%
          fill_NA(value = "Cq", fill.by = input$fillNA)
        df_new <- rbind(df_new, df_temp)
      }
      
      df <- df_new

      # 先提取对照处理
      df_ck_treatment <- df %>%
        dplyr::filter(Treatment == input$reftreatment)
      
      # 计算对照的内参基因的均值
      df_ck_refgene_mean <- df_ck_treatment %>%
        dplyr::filter(Gene == input$refgene) %>%
        dplyr::select(Cq)
      
      
      # 提取所有处理
      all_treatment <- unique(df$Treatment) %>% setdiff(input$reftreatment)
      # 提取所有目的基因
      all_gene <- unique(df$Gene) %>% setdiff(input$refgene)
      
      r$df_out <- NULL
      
      for (treat in all_treatment) {
        
        # 按照内参进行循环计算
        for (gene in all_gene) {
          # 计算对照的目的基因的均值
          df_ck_target_gene_mean <- df_ck_treatment %>%
            dplyr::filter(Gene == gene) %>%
            dplyr::select(Cq)
          
          dt2 <- mean(df_ck_target_gene_mean$Cq) - mean(df_ck_refgene_mean$Cq)
          
          # 选择目的基因,处理的内参基因和目的基因
          df_target_gene <- df %>%
            dplyr::filter(Treatment == treat) %>%
            dplyr::filter(Gene == c(gene, input$refgene))
          
          # 计算处理的内参的均值
          df_target_gene_refgene_mean <- df_target_gene %>%
            dplyr::filter(Gene == input$refgene) %>%
            dplyr::select(Cq)
          
          # 处理的目的基因的表达量
          df_treatment_target_gene <- df_target_gene %>%
            dplyr::filter(Gene == gene) %>%
            dplyr::mutate(
              dt1 = Cq - mean(df_target_gene_refgene_mean$Cq),
              dt2 = dt2,
              Expression = 2^(-(dt1 - dt2))
            )
          
          # 计算CK的目的基因的表达量
          df_ck_target_gene <- df_ck_treatment %>%
            dplyr::filter(Gene == gene) %>%
            dplyr::mutate(
              dt1 = Cq - mean(df_ck_refgene_mean$Cq),
              dt2 = dt2,
              Expression = 2^(-(dt1 - dt2))
            )
          
          df_target_expre <- rbind(df_treatment_target_gene, df_ck_target_gene) %>%
            dplyr::select(Treatment, Gene, Cq, Expression) %>%
            dplyr::group_by(Treatment) %>%
            dplyr::mutate(
              N = dplyr::n(),
              Mean = mean(Expression),
              SD = sd(Expression),
              SE = SD / sqrt(N)
            )
          
          r$df_out <- rbind(r$df_out, df_target_expre)
        }
      }
      
      r$df_out_2 <- r$df_out %>% 
        dplyr::mutate(temp = paste0(Treatment, Gene))
        
        
      r$df_out_2 <- r$df_out_2[!duplicated(r$df_out_2$temp),] %>% 
        dplyr::select(-Cq, -temp)
      
      r$df_list <- list("平均值表达量" = r$df_out_2, "原始表达量" = r$df_out)

      # 输出结果
      output$preview <- shiny::renderDataTable(options = list(pageLength = 6), {
        r$df_out_2
      })
    })

    # 下载结果
    output$dl_table <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "-表达量(2^-ΔΔCt法).xlsx")
      },
      content = function(file) {
        openxlsx::write.xlsx(r$df_list, file)
      }
    )
  })
}

## To be copied in the UI
# mod_cal_expre_ddct_ui("cal_expre_ddct_ui_1")

## To be copied in the server
# mod_cal_expre_ddct_server("cal_expre_ddct_ui_1")
