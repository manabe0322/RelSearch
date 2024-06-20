#' tab_view_dt_r_y_ui
#'
#' @description The function to create the ui module for viewing reference database of the Y-STR
tab_view_dt_r_y_ui <- function(id){
  ns <- NS(id)

  tabPanel("Y-STR : Reference", dataTableOutput(ns("dt_r_y")))
}

#' tab_view_dt_r_y_server
#'
#' @description The function to create the server module for viewing reference database of the Y-STR
#' @param rv_file The reactive valus for the load data and the analysis results
tab_view_dt_r_y_server <- function(id, rv_file){
  moduleServer(
    id,
    function(input, output, session){
      dt_r_y <- rv_file$dt_r_y

      if(!is.null(dt_r_y)){
        output$dt_r_y <- renderDataTable({
          datatable(
            dt_r_y,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })
      }
    }
  )
}
