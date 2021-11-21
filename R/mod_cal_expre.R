#' cal_expre UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cal_expre_ui <- function(id) {
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
      # 是否用内参进行校正
      selectInput(
        ns("correction"),
        label = h6("是否用内参进行校正"),
        choices = list(
          "是" = "yes",
          "否" = "no"
        ),
        selected = "yes"
      ),
      # 输入内参基因
      textInput(
        ns("refgene"),
        label = h6("内参基因名称 (严格区分大小写)"),
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

#' cal_expre Server Functions
#'
#' @noRd
#' @importFrom readxl read_excel
#' @importFrom shiny NS tagList renderDataTable downloadHandler
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate rename filter group_by ungroup n
#' @importFrom stringr str_sub
#' @importFrom reshape2 melt
#' @importFrom xlsx write.xlsx
mod_cal_expre_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # 预设返回值
    r <- rv(
      df_user_exp = NULL,
      df_user_design_treat = NULL,
      df_user_design_gene = NULL,
      df_user_sd = NULL,
      df_out = NULL,
      df_out_2 = NULL,
      df_lit = NULL
    )

    # 下载示例数据
    output$dl_demo <- downloadHandler(
      filename = "表达量计算示例数据_标曲法.xlsx",
      content = function(file) {
        file.copy("./data/表达量计算示例数据_标曲法.xlsx", file)
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
        dplyr::select(N, P, Cq, Position, Batch)

      # 读入处理矩阵
      r$df_user_design_treat <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 2
      ) %>%
        reshape2::melt(id.vars = 1:2) %>%
        dplyr::mutate(Position = paste0(N, variable)) %>%
        dplyr::rename(Treatment = value) %>%
        dplyr::select(Position, Treatment,Batch)

      # 读入基因矩阵
      r$df_user_design_gene <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 3
      ) %>%
        reshape2::melt(id.vars = 1:2) %>%
        dplyr::mutate(Position = paste0(N, variable)) %>%
        dplyr::rename(Gene = value) %>%
        dplyr::select(Position, Gene,Batch)

      # 读入标曲
      r$df_user_sd <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 4
      )
      
      
      # 根据batch分批计算
      for (i in unique(r$df_user_exp$Batch)) {
        df_user_exp = r$df_user_exp %>% dplyr::filter(Batch == i)
        df_user_design_treat = r$df_user_design_treat %>% dplyr::filter(Batch == i)
        df_user_design_gene = r$df_user_design_gene %>% dplyr::filter(Batch == i)
        
        # 计算表达量
        # 数据处理
        df <- merge(r$df_user_exp, r$df_user_design_treat, by = "Position") %>%
          merge(r$df_user_design_gene, by = "Position") %>%
          dplyr::mutate(group = paste0(Gene, "-", Treatment))
        
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
        
        df.3 <- df %>%
          dplyr::filter(Cq == "-") %>%
          dplyr::mutate(temp = paste0(Gene, N, P))
        
        df.4 <- df %>%
          dplyr::filter(Cq != "-") %>%
          merge(r$df_user_sd, by = "Gene") %>%
          dplyr::mutate(out = "")
        
        for (i in 1:nrow(df.4)) {
          if (df.4$Cq[i] > df.4$max[i] | df.4$Cq[i] < df.4$min[i]) {
            df.4$out[i] <- 1
          } else {
            df.4$out[i] <- 0
          }
        }
        
        df.5 <- df.4 %>%
          dplyr::filter(out == 0) %>%
          dplyr::mutate(Cq = as.numeric(Cq)) %>%
          dplyr::group_by(group) %>%
          dplyr::mutate(mean = mean(Cq)) %>%
          dplyr::ungroup()
        
        df.6 <- df.4 %>%
          dplyr::filter(out == 1) %>%
          dplyr::mutate(temp = paste0(Gene, N, P))
        
        # 填充缺失值
        df.5.1 <- df.5 %>% dplyr::filter(group %in% unique(df.3$group))
        df.5.1 <- df.5.1[!duplicated(df.5.1$group), ]
        
        df.3 <- merge(df.3[, c("group", "temp")], df.5.1, by = "group")
        df.3 <- df.3[!duplicated(df.3$temp), ] %>%
          dplyr::mutate(Cq = mean) %>%
          dplyr::select(-temp) %>%
          dplyr::select(colnames(df.5))
        
        
        df.5.1 <- df.5 %>% dplyr::filter(group %in% unique(df.6$group))
        df.5.1 <- df.5.1[!duplicated(df.5.1$group), ]
        
        df.6 <- merge(df.6[, c("group", "temp")], df.5.1, by = "group")
        
        df.6 <- df.6[!duplicated(df.6$temp), ] %>%
          dplyr::mutate(Cq = mean) %>%
          dplyr::select(-temp) %>%
          dplyr::select(colnames(df.5))
        
        exp.all <- rbind(df.3, df.5, df.6) %>%
          dplyr::mutate(group = paste0(Treatment, Gene)) %>%
          dplyr::group_by(group) %>%
          dplyr::mutate(
            N = dplyr::n(),
            Mean = mean(Cq),
            SD = sd(Cq),
            SE = SD / sqrt(N),
            Expression = Cq * Slope + Intercept
          ) %>%
          dplyr::ungroup() %>%
          dplyr::select(Treatment, Gene, Cq, N, Mean, SD, SE, Formula, R2, P.value, Expression, Date)
        # 合并计算结果
        r$df_out <- rbind(r$df_out, exp.all)
      }
      
      # 用内参进行校正
      if (input$correction == "yes") {
        df_rengene <- r$df_out %>% 
          dplyr::filter(Gene == input$refgene) %>% 
          dplyr::select(Treatment,Expression) %>% 
          dplyr::group_by(Treatment) %>% 
          dplyr::mutate(mean_ref = mean(Expression)) %>% 
          dplyr::select(Treatment,mean_ref) %>% 
          dplyr::ungroup()
        
        df_rengene <- df_rengene[!duplicated(df_rengene$Treatment),]
        
        r$df_out <- merge(r$df_out, df_rengene, by = "Treatment") %>% 
          dplyr::mutate(Corrected_expressions =  Expression / mean_ref) %>% 
          dplyr::select(-mean_ref) %>% 
          dplyr::select(Treatment, Gene, Cq, N, SD, SE, 
                        Formula, R2, P.value, Expression,
                        Corrected_expressions, Date) %>% 
          dplyr::mutate(temp = paste0(Treatment, Gene)) %>% 
          dplyr::group_by(temp) %>% 
          dplyr::mutate(Mean = mean(Corrected_expressions)) %>% 
          dplyr::ungroup() %>% 
          dplyr::select(Treatment, Gene, Cq, Mean, N, SD, SE, 
                        Formula, R2, P.value,
                        Corrected_expressions, Date, temp) %>% 
          dplyr::rename(Expression = Corrected_expressions)
        
        r$df_out_2 <- r$df_out %>%
          dplyr::select(temp,Treatment, Gene, Cq, N, Mean, SD, SE, 
                        Formula, R2, P.value) 
        
        r$df_out <- r$df_out %>% 
          dplyr::select(-temp)
        
      }else{
        r$df_out <- r$df_out 
        
        r$df_out_2 <- r$df_out %>% 
          dplyr::mutate(temp = paste0(Treatment, Gene)) %>% 
          dplyr::group_by(temp) %>% 
          dplyr::mutate(Mean = mean(Expression)) %>% 
          dplyr::ungroup() %>% 
          dplyr::select(temp,Treatment, Gene, Cq,N, Mean, SD, SE, 
                        Formula, R2, P.value)
      }

      r$df_out_2 <- r$df_out_2[!duplicated(r$df_out_2$temp),] %>% 
        dplyr::select(-temp)
      
      
      r$df_list <- list("平均值表达量" = r$df_out_2, "原始表达量" = r$df_out)

      # 输出结果
      output$preview <- shiny::renderDataTable(options = list(pageLength = 6), {
        r$df_out_2
      })
    })
    
    # 下载结果
    output$dl_table <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "-表达量(标曲法).xlsx")
      },
      content = function(file) {
        openxlsx::write.xlsx(r$df_list, file)
      }
    )
  })
}

## To be copied in the UI
# mod_cal_expre_ui("cal_expre_ui_1")

## To be copied in the server
# mod_cal_expre_server("cal_expre_ui_1")
