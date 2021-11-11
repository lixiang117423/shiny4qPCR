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

      # 起始行
      selectInput(
        ns("start"),
        label = h6("起始行"),
        choices = list(
          "A" = "A",
          "B" = "B",
          "C" = "C",
          "D" = "D",
          "E" = "E",
          "F" = "F",
          "G" = "G",
          "H" = "H"
        ),
        selected = "B"
      ),
      # 终止行
      selectInput(
        ns("end"),
        label = h6("终止行"),
        choices = list(
          "A" = "A",
          "B" = "B",
          "C" = "C",
          "D" = "D",
          "E" = "E",
          "F" = "F",
          "G" = "G",
          "H" = "H"
        ),
        selected = "G"
      ),
      # 图片格式
      selectInput(
        ns("figtype"),
        label = h6("图片格式"),
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
          label = "下载表格"
        ) %>%
          tags$div(align = "center")
      ),
      HTML("&nbsp;"),
      col_12(
        downloadButton(ns("dl_fig"),
          label = "下载图片"
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
#' @importFrom ggplot2 ggplot geom_smooth geom_point facet_wrap labs ggsave
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
      filename = "标曲计算示例数据.xlsx",
      content = function(file) {
        file.copy("./data/StandardCurve_demo.xlsx", file)
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
        dplyr::select(Position, Conc)

      # 设置位置矩阵
      df_loc <- data.frame(
        P = LETTERS[1:8],
        loc = 1:8
      )

      # 设置起终点
      start <- df_loc %>% dplyr::filter(P == input$start)
      start <- start$loc

      end <- df_loc %>% dplyr::filter(P == input$end)
      end <- end$loc


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
          dplyr::filter(loc >= start & loc <= end)
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
          dplyr::filter(loc >= start & loc <= end)
      }

      # 构建模型
      fit.res <- NULL

      for (i in unique(df.final$Gene)) {
        df.sub <- df.final %>%
          dplyr::filter(Gene == i)

        fit <- lm(Conc ~ mean, data = df.sub)
        intercept <- fit[["coefficients"]][["(Intercept)"]] %>%
          round(2)
        slope <- fit[["coefficients"]][["mean"]] %>%
          round(2)

        formula <- paste0("RC = ", slope, "xCq", " + ", intercept)

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

      r$df_out <- fit.res

      # 绘图
      # 绘图
      r$plot_out <- ggplot(df.final, aes(mean, Conc, fill = Gene)) +
        geom_smooth(
          formula = y ~ x,
          method = "lm",
          se = TRUE, colour = "black", span = 0.8
        ) +
        geom_point() +
        facet_wrap(. ~ Gene, ncol = 2) +
        stat_poly_eq(aes(label = paste(..eq.label..,
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
        labs(y = "Relative.Conc (log2)", x = "Cq") +
        scale_y_continuous(breaks = round(seq(
          min(df.final$Conc),
          max(df.final$Conc), 1
        ), 2)) +
        scale_x_continuous(breaks = round(seq(
          min(df.final$mean),
          max(df.final$mean) + 1, 1
        ), 1)) +
        theme_bw() +
        theme(legend.position = "none")

      # 展示结果
      output$preview <- shiny::renderDataTable({
        r$df_out %>%
          dplyr::select(1:6)
      })

      # 下载结果
      output$dl_table <- downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "-Standard_Curves.txt")
        },
        content = function(file) {
          write.table(r$df_out,
            file,
            # col.names = FALSE,
            row.names = FALSE,
            sep = "\t",
            quote = FALSE
          )
        }
      )

      # 下载图片
      output$dl_fig <- downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "-Standard_Curves.", input$figtype)
        },
        content = function(file) {
          ggsave(file, r$plot_out, device = input$figtype, width = 12, height = 8)
        }
      )
    })
  })
}

## To be copied in the UI
# mod_cal_curve_ui("cal_curve_ui_1")

## To be copied in the server
# mod_cal_curve_server("cal_curve_ui_1")