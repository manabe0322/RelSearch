#' tab_view_dt_v_mt_ui
#'
#' @description The function to create the ui module for viewing victim database of the mtDNA
tab_view_dt_v_mt_ui <- function(id){
  ns <- NS(id)

  tabPanel("mtDNA : Victim", dataTableOutput(ns("dt_v_mt")))
}

#' tab_view_dt_v_mt_server
#'
#' @description The function to create the server module for viewing victim database of the mtDNA
tab_view_dt_v_mt_server <- function(id, rv_file){
  moduleServer(
    id,
    function(input, output, session){
      dt_v_mt <- rv_file$dt_v_mt

      if(!is.null(dt_v_mt)){
        output$dt_v_mt <- renderDataTable({
          datatable(
            dt_v_mt,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })
      }
    }
  )
}
