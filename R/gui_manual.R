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
#' @param path_pack Package path
manual_server <- function(id, path_pack){
  moduleServer(
    id,
    function(input, output, session){
      output$download_manual <- downloadHandler(
        filename = "RelSearch_manual.pdf",
        content = function(file){
          file.copy(paste0(path_pack, "/extdata/manual/RelSearch_manual.pdf"), file)
        }
      )
    }
  )
}
