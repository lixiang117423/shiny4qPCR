#' cal_expre_RqPCR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cal_expre_RqPCR_ui <- function(id) {
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
      if (FALSE) {
        # 是否剔除空值
        selectInput(
          ns("dropNA"),
          label = h6("是否剔除空值"),
          choices = list(
            "是" = "TRUE",
            "否" = "FALSE"
          ),
          selected = "TRUE"
        )
      },

      # 是否输入参考基因
      selectInput(
        ns("inputrefgene"),
        label = h6("是否指定参考基因"),
        choices = list(
          "是" = "yes",
          "否" = "no"
        ),
        selected = "no"
      ),
      

      # 输入参考基因
      conditionalPanel(
        condition = "input.inputrefgene == 'yes'",
        ns = ns,
        textInput(
          ns("refgenename"),
          label = h6("参考基因名称(英文逗号分隔)"),
          value = ""
        )
      ),
      
      
      # 参考基因数量
      conditionalPanel(
        condition = "input.inputrefgene == 'no'",
        ns = ns,
        numericInput(
          ns("refgennum"),
          label = h6("参考基因数量(≥2)"),
          value = 2
        )
      ),

      # 是否用样品进行校正
      selectInput(
        ns("samplecorrection"),
        label = h6("是否用样品校正表达量"),
        choices = list(
          "是" = "yes",
          "否" = "no"
        ),
        selected = "no"
      ),
      # 输入参考基因
      conditionalPanel(
        condition = "input.samplecorrection == 'yes'",
        ns = ns,
        textInput(
          ns("refsample"),
          label = h6("用于校正的样品"),
          value = ""
        )
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

#' cal_expre_RqPCR Server Functions
#'
#' @noRd
mod_cal_expre_RqPCR_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # 预设返回值
    r <- rv(
      df_user_exp = NULL,
      df_user_design_treat = NULL,
      df_user_design_gene = NULL,
      df_user_biorep = NULL,
      df_user_rep = NULL,
      df_user_eff = NULL,
      df_out = NULL,
      df_out_2 = NULL,
      df_lit = NULL
    )

    # 下载示例数据
    output$dl_demo <- downloadHandler(
      filename = "表达量计算示例数据_RqPCR法.xlsx",
      content = function(file) {
        file.copy("./data/表达量计算示例数据_RqPCR法.xlsx", file)
      }
    )

    observeEvent(input$submit, {
      na.rm = TRUE
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
        dplyr::select(Cq, Position, Batch) %>%
        dplyr::mutate(temp = paste0(Position, Batch)) %>%
        dplyr::select(temp, Cq) %>%
        dplyr::mutate(Cq = as.numeric(Cq))

      # 读入处理矩阵
      r$df_user_design_treat <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 2
      ) %>%
        reshape2::melt(id.vars = 1:2) %>%
        dplyr::mutate(Position = paste0(N, variable)) %>%
        dplyr::rename(Treatment = value) %>%
        dplyr::select(Position, Treatment, Batch) %>%
        dplyr::mutate(temp = paste0(Position, Batch)) %>%
        dplyr::select(temp, Treatment)

      # 读入基因矩阵
      r$df_user_design_gene <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 3
      ) %>%
        reshape2::melt(id.vars = 1:2) %>%
        dplyr::mutate(Position = paste0(N, variable)) %>%
        dplyr::rename(Gene = value) %>%
        dplyr::select(Position, Gene, Batch) %>%
        dplyr::mutate(temp = paste0(Position, Batch)) %>%
        dplyr::select(temp, Gene)

      # 读入生物学重复矩阵
      r$df_user_biorep <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 4
      ) %>%
        reshape2::melt(id.vars = 1:2) %>%
        dplyr::mutate(Position = paste0(N, variable)) %>%
        dplyr::rename(bio_rep = value) %>%
        dplyr::select(Position, bio_rep, Batch) %>%
        dplyr::mutate(temp = paste0(Position, Batch)) %>%
        dplyr::select(temp, bio_rep)
      
      # 读入技术重复矩阵
      r$df_user_rep <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 5
      ) %>%
        reshape2::melt(id.vars = 1:2) %>%
        dplyr::mutate(Position = paste0(N, variable)) %>%
        dplyr::rename(rep = value) %>%
        dplyr::select(Position, rep, Batch) %>%
        dplyr::mutate(temp = paste0(Position, Batch)) %>%
        dplyr::select(temp, rep)

      # 读入扩增效率矩阵
      r$df_user_eff <- readxl::read_excel(
        input$uploadfile$datapath,
        sheet = 6
      )

      ##########################################
      ############### 合并数据

      df_all <- merge(r$df_user_exp, r$df_user_design_treat, by = "temp") %>%
        merge(r$df_user_design_gene, by = "temp") %>%
        merge(r$df_user_biorep, by = "temp") %>%
        merge(r$df_user_rep, by = "temp") %>%
        merge(r$df_user_eff, by = "Gene")


      ############################################
      ################## 开始计算
      df_expre <- df_all %>%
        dplyr::group_by(bio_rep, Treatment, Gene) %>%
        dplyr::mutate(
          MEAN = mean(Cq, na.rm = TRUE),
          SD = sd(Cq, na.rm = TRUE),
          SD = ifelse(is.na(SD), 0, SD)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(bio_rep, Gene) %>%
        dplyr::mutate(
          min_MEAN = min(MEAN),
          QCq = eff^(min_MEAN - MEAN),
          SD_QCq = SD * QCq * log(eff)
        ) %>%
        dplyr::ungroup()

      ###############################################
      ###############  计算参考基因


      if (input$inputrefgene == "yes") {
        ref.gene <- c(input$refgenename)
      } else {
        df.ref <- df_expre %>%
          dplyr::select(Treatment, Gene, Cq, bio_rep, rep) %>%
          dplyr::group_by(Treatment, Gene, bio_rep, rep) %>%
          dplyr::mutate(
            Treatment = paste0(Treatment, rep)
          ) %>%
          tidyr::spread(key = Gene, value = Cq)

        df.temp <- df.ref[, 4:ncol(df.ref)] %>% as.data.frame()

        n <- length(unique(df_expre$Gene))

        M <- numeric(n)

        for (j in 1:n) {
          A <- log2(df.temp[, j] / df.temp[, -j])
          if (n > 2) {
            M[j] <- mean(apply(A, 2, sd, na.rm = na.rm))
          } else {
            M[j] <- sd(A, na.rm = na.rm)
          }
        }

        if (is.data.frame(df.temp)) {
          names(M) <- names(df.temp)
        } else {
          names(M) <- colnames(df.temp)
        }

        geneSymbol <- colnames(df.temp)
        n <- ncol(df.temp)
        
        num.ref <- input$refgennum

        V <- numeric(n - num.ref)
        names(V) <- paste(((n - 1):num.ref), "/", (n:(num.ref + 1)), sep = "")
        meanM <- numeric(n - num.ref + 1)
        names(meanM) <- as.character(n:num.ref)
        R <- character(n)
        names(R) <- as.character(c(rep(1, num.ref), (num.ref + 1):length(R)))

        for (i in n:num.ref) {
          M <- gene.stable(df.temp, na.rm = na.rm)
          ind <- which.max(M)
          meanM[n - i + 1] <- mean(M)
          if (i == num.ref) {
            R[1:num.ref] <- geneSymbol
          } else {
            R[i] <- geneSymbol[ind]
          }
          if (i > 2) {
            NF.old <- apply(df.temp, 1, geometric.mean)
            NF.new <- apply(df.temp[, -ind], 1, geometric.mean)
            V[n - i + 1] <- sd(log2(NF.new / NF.old), na.rm = na.rm)
          }
          df.temp <- df.temp[, -ind]
          geneSymbol <- geneSymbol[-ind]
        }

        ref.gene <- as.character(R[1:num.ref])
      }


      ###################################
      #### 求校正因子等              ####
      ###################################

      df.factor <- df_expre %>%
        dplyr::filter(Gene %in% ref.gene) %>%
        dplyr::mutate(temp_2 = paste0(Treatment, bio_rep, Gene))
      df.factor <- df.factor[!duplicated(df.factor$temp_2), ] %>% as.data.frame()

      factor <- data.frame()

      for (i in unique(df.factor$bio_rep)) {
        df.temp <- df.factor %>% dplyr::filter(bio_rep == i)
        for (j in unique(df.temp$Treatment)) {
          df.temp.2 <- df.temp %>%
            dplyr::filter(Treatment == j) %>%
            dplyr::select(Treatment, QCq)
          fac <- data.frame(Treatment = j, bio_rep = i, factor = geometric.mean(df.temp.2$QCq))
          factor <- rbind(factor, fac)
        }
      }


      factor <- factor %>%
        dplyr::mutate(temp_2 = paste0(Treatment, bio_rep)) %>%
        dplyr::select(temp_2, factor)
      df.factor <- df.factor %>%
        dplyr::mutate(temp_2 = paste0(Treatment, bio_rep)) %>%
        merge(factor, by = "temp_2") %>%
        dplyr::mutate(SD.factor = (SD_QCq / (length(ref.gene) * (QCq)))^2) %>%
        dplyr::group_by(bio_rep, Treatment) %>%
        dplyr::mutate(SD.factor = sqrt(sum(SD.factor)) * factor)

      ###################################
      #### 求校正表达量              ####
      ###################################
      df.goi <- df_expre %>%
        dplyr::filter(!Gene %in% ref.gene) %>%
        dplyr::mutate(temp_2 = paste0(Treatment, bio_rep)) %>%
        merge(df.factor[, c("temp_2", "factor", "SD.factor")], by = "temp_2") %>%
        dplyr::mutate(
          expression = QCq / factor,
          SD_1 = expression * sqrt((SD_QCq / QCq)^2 + (SD.factor / factor)^2),
          SE_1 = SD_1 / sqrt(2)
        )

      ###################################
      #### 求平均表达量              ####
      ###################################
      if (input$samplecorrection == "yes") {
        df.goi <- df.goi %>%
          dplyr::ungroup() %>%
          # dplyr::mutate(Treatment = stringr::str_sub(Sample, 1, nchar(Sample) - 2)) %>%
          dplyr::group_by(Gene, Treatment) %>%
          dplyr::mutate(
            mean_aver = mean(unique(expression), na.rm = TRUE),
            sd_aver = sd(unique(expression), na.rm = TRUE),
            se_aver = sd_aver / sqrt(length(unique(bio_rep)))
          ) %>%
          dplyr::ungroup()

        df.goi_temp <- df.goi %>%
          dplyr::filter(Treatment == input$refsample) %>%
          dplyr::select(Gene, mean_aver) %>%
          dplyr::rename(min_temp = mean_aver)
        df.goi_temp <- df.goi_temp[!duplicated(df.goi_temp$Gene), ]

        df.goi <- merge(df.goi, df.goi_temp, by = "Gene")


        r$df_out <- df.goi %>%
          dplyr::group_by(Gene) %>%
          dplyr::mutate(
            RE_expr = mean_aver / min_temp,
            RE_sd = sd_aver / min_temp,
            RE_se = se_aver / min_temp
          ) %>%
          dplyr::select(Treatment, Gene, Cq, eff, bio_rep, RE_expr, RE_sd, RE_se) %>%
          dplyr::rename(Expression = RE_expr, SD = RE_sd, SE = RE_se) %>%
          dplyr::mutate(temp = paste0(Treatment, Gene, bio_rep))

        r$df_out <- r$df_out[!duplicated(r$df_out$temp), ] %>%
          dplyr::select(-temp)
      } else {
        r$df_out <- df.goi %>%
          dplyr::ungroup() %>%
          # dplyr::mutate(Treatment = stringr::str_sub(Sample, 1, nchar(Sample) - 2)) %>%
          dplyr::group_by(Gene, Treatment) %>%
          dplyr::mutate(
            mean_aver = mean(unique(expression), na.rm = TRUE),
            sd_aver = sd(unique(expression), na.rm = TRUE),
            se_aver = sd_aver / sqrt(length(unique(bio_rep)))
          ) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(Gene) %>%
          dplyr::mutate(
            min_mean = min(mean_aver),
            RE_expr = mean_aver / min_mean,
            RE_sd = sd_aver / min_mean,
            RE_se = se_aver / min_mean
          ) %>%
          dplyr::select(Treatment, Gene, eff, expression, bio_rep, RE_expr, RE_sd, RE_se) %>%
          dplyr::rename(Expre4Stat = expression, Expression = RE_expr, SD = RE_sd, SE = RE_se) %>%
          dplyr::mutate(temp = paste0(Treatment, Gene, bio_rep))

        r$df_out <- r$df_out[!duplicated(r$df_out$temp), ] %>%
          dplyr::select(-temp)
      }

      # 输出结果
      output$preview <- shiny::renderDataTable(options = list(pageLength = 8), {
        r$df_out
      })

      # 下载结果
      output$dl_table <- downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "-表达量(RqPCR).xlsx")
        },
        content = function(file) {
          openxlsx::write.xlsx(r$df_out, file)
        }
      )
    })
  })
}

## To be copied in the UI
# mod_cal_expre_RqPCR_ui("cal_expre_RqPCR_ui_1")

## To be copied in the server
# mod_cal_expre_RqPCR_server("cal_expre_RqPCR_ui_1")
