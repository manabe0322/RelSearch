#' example_ui
#'
#' @description The function to create the ui module for downloading example files
example_ui <- function(id){
  ns <- NS(id)

  tabPanel("Example files",
           titlePanel("Example files"),
           br(),
           downloadButton(ns("download_v_auto"), "STR victim database"),
           br(),
           br(),
           downloadButton(ns("download_r_auto"), "STR reference database"),
           br(),
           br(),
           downloadButton(ns("download_af"), "Allele frequencies"),
           br(),
           br(),
           downloadButton(ns("download_v_y"), "Y-STR victim database"),
           br(),
           br(),
           downloadButton(ns("download_r_y"), "Y-STR reference database"),
           br(),
           br(),
           downloadButton(ns("download_v_mt"), "mtDNA victim database"),
           br(),
           br(),
           downloadButton(ns("download_r_mt"), "mtDNA reference database")
  )
}

#' example_server
#'
#' @description The function to create the server module for downloading example files
example_server <- function(id, path_pack){
  moduleServer(
    id,
    function(input, output, session){
      output$download_v_auto <- downloadHandler(
        filename = "str_victim_example.csv",
        content = function(file){
          csvfile <- read.csv(paste0(path_pack, "/extdata/examples/str_victim_example.csv"))
          mk <- setdiff(colnames(csvfile), "SampleName")
          id_mk <- 1:length(mk)
          mk <- mk[id_mk[id_mk %% 2 == 1]]
          colnames(csvfile) <- c("SampleName", as.vector(sapply(mk, rep, 2)))
          csvfile[is.na(csvfile)] <- ""
          write.csv(csvfile, file, row.names = FALSE)
        }
      )

      output$download_r_auto <- downloadHandler(
        filename = "str_ref_example.csv",
        content = function(file){
          csvfile <- read.csv(paste0(path_pack, "/extdata/examples/str_ref_example.csv"))
          mk <- setdiff(colnames(csvfile), c("SampleName", "Relationship"))
          id_mk <- 1:length(mk)
          mk <- mk[id_mk[id_mk %% 2 == 1]]
          colnames(csvfile) <- c("SampleName", "Relationship", as.vector(sapply(mk, rep, 2)))
          csvfile[is.na(csvfile)] <- ""
          write.csv(csvfile, file, row.names = FALSE)
        }
      )

      output$download_af <- downloadHandler(
        filename = "str_af_example.csv",
        content = function(file){
          csvfile <- read.csv(paste0(path_pack, "/extdata/examples/str_af_example.csv"))
          csvfile[is.na(csvfile)] <- ""
          write.csv(csvfile, file, row.names = FALSE)
        }
      )

      output$download_v_y <- downloadHandler(
        filename = "y_victim_example.csv",
        content = function(file){
          csvfile <- read.csv(paste0(path_pack, "/extdata/examples/y_victim_example.csv"))
          csvfile[is.na(csvfile)] <- ""
          write.csv(csvfile, file, row.names = FALSE)
        }
      )

      output$download_r_y <- downloadHandler(
        filename = "y_ref_example.csv",
        content = function(file){
          csvfile <- read.csv(paste0(path_pack, "/extdata/examples/y_ref_example.csv"))
          csvfile[is.na(csvfile)] <- ""
          write.csv(csvfile, file, row.names = FALSE)
        }
      )

      output$download_v_mt <- downloadHandler(
        filename = "mt_victim_example.csv",
        content = function(file){
          csvfile <- read.csv(paste0(path_pack, "/extdata/examples/mt_victim_example.csv"))
          csvfile[is.na(csvfile)] <- ""
          write.csv(csvfile, file, row.names = FALSE)
        }
      )

      output$download_r_mt <- downloadHandler(
        filename = "mt_ref_example.csv",
        content = function(file){
          csvfile <- read.csv(paste0(path_pack, "/extdata/examples/mt_ref_example.csv"))
          csvfile[is.na(csvfile)] <- ""
          write.csv(csvfile, file, row.names = FALSE)
        }
      )
    }
  )
}
