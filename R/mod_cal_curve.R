#' cal_curve UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList renderDataTable downloadHandler
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom dplyr select mutate rename filter group_by ungroup 
#' @importFrom stringr str_sub
#' @importFrom reshape2 melt 
#' @importFrom broom glance
#' @importFrom ggplot2 ggplot geom_smooth geom_point facet_wrap labs ggsave
#' @importFrom ggplot2 scale_y_continuous  scale_x_continuous theme_bw theme
#' @importFrom ggpmisc stat_poly_eq 
mod_cal_curve_ui <- function(id) {
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
        label = h6("Download demo data")
      ),
      # 稀释倍数
      numericInput(
        ns("dilu"),
        label = h6("Dilution ratio"), # 稀释倍数
        value = 4
      ),
      # 是否剔除空值
      selectInput(
        ns("dropNA"),
        label = h6("Drop NA"), # 剔除空值
        choices = list(
          "Yes" = "TRUE",
          "No" = "FALSE"
        ),
        selected = "TRUE"
      ),
      # 空值填充方法
      selectInput(
        ns("fillNA"),
        label = h6("Fill NA by"),
        choices = list(
          "Mean value" = "mean",
          "Max value" = "max"
        ),
        selected = "mean"
      ),

      # 起始行
      numericInput(
        ns("start"),
        label = h6("Start"),
        min = 1,
        max = 10^10,
        step = 1,
        value = 4096
      ),
      # 终止行
      numericInput(
        ns("end"),
        label = h6("End"),
        min = 1,
        max = 10^10,
        step = 1,
        value = 4,
      ),
      # 图片格式
      selectInput(
        ns("figtype"),
        label = h6("Figure type"),
        choices = list(
          "PDF" = "pdf",
          "eps" = "eps",
          "png" = "png",
          "jpg" = "jpg",
          "tiff" = "tiff"
        ),
        selected = "pdf"
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
          label = "Download Excel"
        ) %>%
          tags$div(align = "center")
      ),
      HTML("&nbsp;"),
      col_12(
        downloadButton(ns("dl_fig"),
          label = "Download Figure"
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

#' cal_curve Server Functions
#' @import shiny
#' @importFrom readxl read_excel
#' @noRd
#' @importFrom shiny NS tagList renderDataTable downloadHandler
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate rename filter group_by ungroup 
#' @importFrom stringr str_sub
#' @importFrom reshape2 melt 
#' @importFrom broom glance
#' @importFrom ggplot2 ggplot geom_smooth geom_point facet_wrap labs ggsave aes
#' @importFrom ggplot2 scale_y_continuous  scale_x_continuous theme_bw theme
#' @importFrom ggpmisc stat_poly_eq 
mod_cal_curve_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 预设返回值
    r <- rv(
      df_user_cq = NULL,
      df_user_design = NULL,
      df_user_conc = NULL,
      df_out = NULL,
      plot_out = NULL
    )

    # 下载示例数据
    output$dl_demo <- downloadHandler(
      filename = "DemoData4CalculateStandardCurve.xlsx",
      content = function(file) {
        file.copy("./data/标曲计算示例数据.xlsx", file)
      }
    )

    # 正式程序
    observeEvent(input$submit, {

      # 读入Cq矩阵
      r$df_user_cq <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 1
      ) %>%
        dplyr::select(Position, Cq)

      # 读入基因矩阵
      r$df_user_design <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 2
      ) %>%
        reshape2::melt(id.vars = 1) %>%
        dplyr::mutate(Position = paste0(N, variable)) %>%
        dplyr::rename(Gene = value) %>%
        dplyr::select(Position, Gene)

      # 读入浓度矩阵
      r$df_user_conc <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 3
      ) %>%
        reshape2::melt(id.vars = 1) %>%
        dplyr::mutate(Position = paste0(N, variable)) %>%
        dplyr::rename(Conc = value) %>%
        dplyr::mutate(Conc = log(Conc, base = input$dilu)) %>% 
        dplyr::select(Position, Conc)

      # 设置位置矩阵
      df_loc <- data.frame(
        P = LETTERS[1:8],
        loc = 1:8
      )

      # 设置起终点
      #start <- df_loc %>% dplyr::filter(Conc < log(input$start, base = input$dilu))
      #start <- start$loc

      #end <- df_loc %>% dplyr::filter(Conc > log(input$end, base = input$dilu))
      #end <- end$loc


      df <- merge(r$df_user_cq, r$df_user_design, by = "Position") %>%
        merge(r$df_user_conc, by = "Position") %>%
        dplyr::mutate(
          P = stringr::str_sub(Position, 1, 1),
          N = stringr::str_sub(Position, 2, nchar(Position))
        ) %>%
        merge(df_loc, by = "P") %>%
        dplyr::mutate(group = paste0(Gene, P))

      # 处理异常值
      df.1 <- df %>% dplyr::filter(Cq == "-")

      df.2 <- df %>%
        dplyr::filter(Cq != "-") %>%
        dplyr::mutate(Cq = as.numeric(Cq)) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(
          mean = mean(Cq), sd = sd(Cq),
          max = max(Cq), min = min(Cq)
        )

      df.3 <- df %>% dplyr::filter(Cq != "-")

      if (input$dropNA == "FALSE") {
        if (input$fillNA == "mean") {
          df.temp <- merge(df.1, df.2[, c("group", "mean")], by = "group") %>%
            dplyr::mutate(group.2 = paste0(Gene, N, P))
          df.temp$Cq <- df.temp$mean
          df.temp <- df.temp %>%
            dplyr::select(c(colnames(df), "group.2"))

          df.temp <- df.temp[!duplicated(df.temp$group.2), ] %>%
            dplyr::select(-group.2)

          df.final <- rbind(df.3, df.temp)
        } else if (input$fillNA == "max") {
          df.temp <- merge(df.1, df.2[, c("group", "max")], by = "group") %>%
            dplyr::mutate(group.2 = paste0(Gene, N, P))
          df.temp$Cq <- df.temp$max
          df.temp <- df.temp %>%
            dplyr::select(c(colnames(df), "group.2"))

          df.temp <- df.temp[!duplicated(df.temp$group.2), ] %>%
            dplyr::select(-group.2)

          df.final <- rbind(df.3, df.temp)
        } else {
          df.temp <- merge(df.1, df.2[, c("group", "min")], by = "group") %>%
            dplyr::mutate(group.2 = paste0(Gene, N, P))
          df.temp$Cq <- df.temp$min
          df.temp <- df.temp %>%
            dplyr::select(c(colnames(df), "group.2"))

          df.temp <- df.temp[!duplicated(df.temp$group.2), ] %>%
            dplyr::select(-group.2)

          df.final <- rbind(df.3, df.temp)
        }

        df.final <- df.final %>%
          dplyr::mutate(Cq = as.numeric(Cq)) %>%
          dplyr::group_by(group) %>%
          dplyr::mutate(
            mean = mean(Cq), sd = sd(Cq),
            max = max(Cq), min = min(Cq)
          ) %>%
          dplyr::ungroup() %>%
          #dplyr::filter(loc >= start & loc <= end)
          dplyr::filter(Conc <= log(input$start, base = input$dilu)) %>% 
          dplyr::filter(Conc >= log(input$end, base = input$dilu))
      } else {
        df.final <- df %>%
          dplyr::filter(Cq != "-") %>%
          dplyr::mutate(Cq = as.numeric(Cq)) %>%
          dplyr::group_by(group) %>%
          dplyr::mutate(
            mean = mean(Cq), sd = sd(Cq),
            max = max(Cq), min = min(Cq)
          ) %>%
          dplyr::ungroup() %>%
          #dplyr::filter(loc >= start & loc <= end) %>% 
          dplyr::filter(Conc <= log(input$start, base = input$dilu)) %>% 
          dplyr::filter(Conc >= log(input$end, base = input$dilu))
          }

      # 构建模型
      fit.res <- NULL

      for (i in unique(df.final$Gene)) {
        df.sub <- df.final %>%
          dplyr::filter(Gene == i)

        fit <- lm(mean  ~  Conc, data = df.sub)
        intercept <- fit[["coefficients"]][["(Intercept)"]] %>%
          round(2)
        slope <- fit[["coefficients"]][["Conc"]] %>%
          round(2)
        
        formula <- paste0("y = ", slope, "*x", " + ", intercept)

        r.2 <- broom::glance(fit)[1, 1] %>%
          round(4) %>%
          as.numeric()

        p.value <- broom::glance(fit)[1, 5] %>%
          round(5) %>%
          as.numeric()

        df.temp <- data.frame(
          Gene = i,
          Formula = formula,
          Slope = slope,
          Intercept = intercept,
          R2 = r.2,
          P.value = p.value,
          max = max(df.sub$max),
          min = min(df.sub$min),
          Date = as.character(Sys.Date())
        )

        fit.res <- rbind(fit.res, df.temp)
      }

      r$df_out <- fit.res %>% 
        dplyr::mutate(Eff = input$dilu^((-1)/Slope),
                      Eff.range = "[1.90, 2.05]") %>% 
        dplyr::select(1:8,10:11,9)

      # 绘图
      # 绘图
      r$plot_out <- ggplot2::ggplot(df.final, ggplot2::aes(Conc, mean, fill = Gene)) +
        ggplot2::geom_smooth(
          formula = y ~ x,
          method = "lm",
          se = TRUE, colour = "black", span = 0.8
        ) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(. ~ Gene, ncol = 2) +
        ggpmisc::stat_poly_eq(ggplot2::aes(label = paste(..eq.label..,
                                                         ..rr.label..,
                                                         ..p.value.label..,
                                                         sep = "~~~~"
        )),
        formula = y ~ x,
        parse = T,
        rr.digits = 5,
        coef.digits = 3,
        label.x = c(0.05),
        label.y = c(0.03)
        ) +
        ggplot2::labs(x = "Relative.Conc", y = "Cq") +
        ggplot2::scale_y_continuous(breaks = round(seq(
          min(df.final$mean),
          max(df.final$mean), 5
        ), 0)) +
        ggplot2::scale_x_continuous(breaks = round(seq(
          min(df.final$Conc),
          max(df.final$Conc) + 1, 1
        ), 0)) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none")

      # 展示结果
      output$preview <- shiny::renderDataTable(options = list(pageLength = 6),{
        r$df_out %>%
          dplyr::select(1:6,9:10)
      })

      # 下载结果
      output$dl_table <- downloadHandler(
        filename = function() {
          paste0(Sys.Date(),input$uploadfile$dataname, "-Standard_Curves.xlsx")
        },
        content = function(file) {
          xlsx::write.xlsx(r$df_out,
                           file,
                           # col.names = FALSE,
                           row.names = FALSE
          )
        }
      )

      # 下载图片
      output$dl_fig <- downloadHandler(
        filename = function() {
          paste0(Sys.Date(),input$uploadfile$dataname, "-Standard_Curves.", input$figtype)
        },
        content = function(file) {
          ggplot2::ggsave(file, r$plot_out, device = input$figtype, width = 12, height = 8)
        }
      )
    })
  })
}

## To be copied in the UI
# mod_cal_curve_ui("cal_curve_ui_1")

## To be copied in the server
# mod_cal_curve_server("cal_curve_ui_1")