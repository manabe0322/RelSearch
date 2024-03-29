#' tab_view_dt_v_auto_ui
#'
#' @description The function to create the ui module for viewing victim database of the autosomal STR
tab_view_dt_v_auto_ui <- function(id){
  ns <- NS(id)

  tabPanel("STR : Victim", dataTableOutput(ns("dt_v_auto")))
}

#' tab_view_dt_v_auto_server
#'
#' @description The function to create the server module for viewing victim database of the autosomal STR
tab_view_dt_v_auto_server <- function(id, rv_file){
  moduleServer(
    id,
    function(input, output, session){
      dt_v_auto <- rv_file$dt_v_auto

      if(!is.null(dt_v_auto)){
        output$dt_v_auto <- renderDataTable({
          datatable(
            dt_v_auto,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })
      }
    }
  )
}
