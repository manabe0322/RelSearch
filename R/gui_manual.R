#' manual_ui
#'
#' @description The function to create the ui module for the manual
manual_ui <- function(id){
  ns <- NS(id)

  tabPanel("Manual",
           titlePanel("Manual"),
           br(),
           downloadButton(ns("download_manual"), "Download", class = "btn btn-primary btn-lg")
           )
}

#' manual_server
#'
#' @description The function to create the server module for the manual
manual_server <- function(id, path_pack){
  moduleServer(
    id,
    function(input, output, session){
      output$download_manual <- downloadHandler(
        filename = "relsearch_manual.html",
        content = function(file){
          manual_tmp <- file.path(tempdir(), "relsearch_manual.Rmd")
          file.copy(paste0(path_pack, "/extdata/manual/relsearch_manual.Rmd"), manual_tmp, overwrite = TRUE)
          render(manual_tmp, output_file = file, envir = new.env(parent = globalenv()))
        }
      )
    }
  )
}
