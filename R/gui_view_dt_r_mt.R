#' tab_view_dt_r_mt_ui
#'
#' @description The function to create the ui module for viewing reference database of the mtDNA
tab_view_dt_r_mt_ui <- function(id){
  tabPanel("mtDNA : Reference", dataTableOutput(ns("dt_r_mt")))
}

#' tab_view_dt_r_mt_server
#'
#' @description The function to create the server module for viewing reference database of the mtDNA
tab_view_dt_r_mt_server <- function(id, rv_file){
  moduleServer(
    id,
    function(input, output, session){
      dt_r_mt <- rv_file$dt_r_mt

      if(!is.null(dt_r_mt)){
        output$dt_r_mt <- renderDataTable({
          datatable(
            dt_r_mt,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })
      }
    }
  )
}
