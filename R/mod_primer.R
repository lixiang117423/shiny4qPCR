#' primer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_primer_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_3(
      h4("Parameter Setting"),
      # 单条序列还是多条序列
      selectInput(
        ns("seqnum"),
        label = h6("是否单条序列"),
        choices = list(
          "单条序列" = "1",
          "多条序列" = "2"
        ),
        selected = "1"
      ),
      
      # 单条序列时直接复制粘贴
      conditionalPanel(
        condition = "input.seqnum == '1'",
        ns = ns,
        textInput(
          ns("seqinput"),
          label = h6("输入序列"),
          value = ""
        )
      ),
      
      # 多条序列时上传数据
      conditionalPanel(
        condition = "input.seqnum == '2'",
        ns = ns,
        fileInput(
          ns("uploadfile"),
          label = h6("上传数据"),
          accept = NULL,
          buttonLabel = "View..."
        )
      ),
      
      # 引物长度
      numericInput(
        ns("length"),
        label = h6("引物最佳长度"),
        value = 18
      ),
      
      # 引物长度范围
      sliderInput(
        ns("length_room"),
        label = h6("引物长度范围"),
        min = 10,
        max = 50,
        value = c(18,22)
      ),
      
      # 引物产物质量范围
      sliderInput(
        ns("product"),
        label = h6("引物产物大小范围"),
        min = 50,
        max = 200,
        value = c(75,150)
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
      HTML("&nbsp;")
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
    
#' primer Server Functions
#'
#' @noRd 
mod_primer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # 预存数据
    r <- rv(
      df_user_seq = NULL,
      df_out = NULL
    )
    
    observeEvent(input$submit, {
      dir = Sys.time() %>% 
        stringr::str_replace_all(" ","-") %>% 
        stringr::str_replace_all(":","-")
      
      par_mkdir = paste0("mkdir UserResults/",dir)
      
      data.table::fwrite(as.data.frame(par_mkdir), 
                         file = paste0("UserResults/mkdir.sh"), 
                         col.names = FALSE, 
                         row.names = FALSE, 
                         quote = FALSE)
      
      
      system2(command = "sh", args = " UserResults/mkdir.sh")
      
      
      
      # 存储用户输入的数据
      if (input$seqnum == "1") {
        r$df_user_seq = input$seqinput
        
        # 构造输入文件
        par_file = matrix(ncol = 1, nrow = 12)
        
        par_file[1,] = paste0("SEQUENCE_ID=","YourSeq")
        par_file[2,] = paste0("SEQUENCE_TEMPLATE=",r$df_user_seq)
        par_file[3,] = "PRIMER_TASK=generic"
        par_file[4,] = "PRIMER_PICK_LEFT_PRIMER=1"
        par_file[5,] = "PRIMER_PICK_INTERNAL_OLIGO=0"
        par_file[6,] = "PRIMER_PICK_RIGHT_PRIMER=1"
        par_file[7,] = paste0("PRIMER_OPT_SIZE=",input$length)
        par_file[8,] = paste0("PRIMER_MIN_SIZE=",input$length_room[1])
        par_file[9,] = paste0("PRIMER_MAX_SIZE=",input$length_room[2])
        par_file[10,] = paste0("PRIMER_PRODUCT_SIZE_RANGE=",input$product[1],"-",input$product[2])
        par_file[11,] = "PRIMER_EXPLAIN_FLAG=1"
        par_file[12,] = "="
        
        # 输出参数文件
        data.table::fwrite(as.data.frame(par_file), 
                           file = paste0("UserResults/",dir,"/seq.txt"), 
                           col.names = FALSE, 
                           row.names = FALSE, 
                           quote = FALSE)
        
        # 输出运行命令
        data.table::fwrite(as.data.frame(paste0("primer3_core ", 
                                                paste0("UserResults/",dir,"/seq.txt"),
                                                paste0(" > ","UserResults/",dir,"/result.txt"))),
                           file = paste0("UserResults/",dir,"/run.sh"), 
                           col.names = FALSE, 
                           row.names = FALSE, 
                           quote = FALSE)
        
        system2(command = "sh", args =paste0("UserResults/",dir,"/run.sh"))
        
        
      }else{
        
        # 下载示例数据
        output$dl_demo <- downloadHandler(
          filename = "引物设计示例数据.fa",
          content = function(file) {
            file.copy("./data/引物设计示例数据.fa", file)
          }
        )
        
        df_user_seq = data.table::fread(
          input$uploadfile$datapath,
          header = FALSE
        ) %>% fasta2df() %>% as.data.frame()
        
        # 构造输入文件
        par_file_final = NULL
        
        for (i in 1:nrow(df_user_seq)) {
          df_temp = df_user_seq[i,]
          par_file = matrix(ncol = 1, nrow = 12)
          
          par_file[1,] = paste0("SEQUENCE_ID=",stringr::str_replace(df_temp[1,1], ">",""))
          par_file[2,] = paste0("SEQUENCE_TEMPLATE=",df_temp[1,2])
          par_file[3,] = "PRIMER_TASK=generic"
          par_file[4,] = "PRIMER_PICK_LEFT_PRIMER=1"
          par_file[5,] = "PRIMER_PICK_INTERNAL_OLIGO=0"
          par_file[6,] = "PRIMER_PICK_RIGHT_PRIMER=1"
          par_file[7,] = paste0("PRIMER_OPT_SIZE=",input$length)
          par_file[8,] = paste0("PRIMER_MIN_SIZE=",input$length_room[1])
          par_file[9,] = paste0("PRIMER_MAX_SIZE=",input$length_room[2])
          par_file[10,] = paste0("PRIMER_PRODUCT_SIZE_RANGE=",input$product[1],"-",input$product[2])
          par_file[11,] = "PRIMER_EXPLAIN_FLAG=1"
          par_file[12,] = "="
          
          par_file_final = rbind(par_file_final, par_file)
        }
        
        # 输出参数文件
        data.table::fwrite(as.data.frame(par_file_final), 
                           file = paste0("UserResults/",dir,"/seq.txt"), 
                           col.names = FALSE, 
                           row.names = FALSE, 
                           quote = FALSE)
        
        # 输出运行命令
        data.table::fwrite(as.data.frame(paste0("primer3_core ", 
                                                paste0("UserResults/",dir,"/seq.txt"),
                                                paste0(" > ","UserResults/",dir,"/result.txt"))),
                           file = paste0("UserResults/",dir,"/run.sh"), 
                           col.names = FALSE, 
                           row.names = FALSE, 
                           quote = FALSE)
        
        system2(command = "sh", args =paste0("UserResults/",dir,"/run.sh"))
      }
      
      r$df_out <- get_res_from_primer3(data = paste0("UserResults/",dir,"/result.txt"))
      #r$df_out <- data.table::fread(file = paste0("UserResults/",dir,"/result.txt"), sep = "\n")
      
      output$preview <- shiny::renderDataTable(options = list(pageLength = 8),{
        r$df_out %>%dplyr::select(1,3,7,9,11,12)
      })
      
      # 下载结果
      output$dl_table <- downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "-引物设计结果.xlsx")
        },
        content = function(file) {
          openxlsx::write.xlsx(r$df_out, file)
        }
      )

    })
  })
}
    
## To be copied in the UI
# mod_primer_ui("primer_ui_1")
    
## To be copied in the server
# mod_primer_server("primer_ui_1")
