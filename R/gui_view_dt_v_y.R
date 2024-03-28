#' tab_view_dt_v_y_ui
#'
#' @description The function to create the ui module for viewing victim database of the Y-STR
tab_view_dt_v_y_ui <- function(id){
  tabPanel("Y-STR : Victim", dataTableOutput(ns("dt_v_y")))
}

#' tab_view_dt_v_y_server
#'
#' @description The function to create the server module for viewing victim database of the Y-STR
tab_view_dt_v_y_server <- function(id, rv_file){
  moduleServer(
    id,
    function(input, output, session){
      dt_v_y <- rv_file$dt_v_y

      if(!is.null(dt_v_y)){
        output$dt_v_y <- renderDataTable({
          datatable(
            dt_v_y,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })
      }
    }
  )
}
