#' tab_view_dt_r_auto_ui
#'
#' @description The function to create the ui module for viewing reference database of the autosomal STR
tab_view_dt_r_auto_ui <- function(id){
  tabPanel("STR : Reference", dataTableOutput(ns("dt_r_auto")))
}

#' tab_view_dt_r_auto_server
#'
#' @description The function to create the server module for viewing reference database of the autosomal STR
tab_view_dt_r_auto_server <- function(id, rv_file){
  moduleServer(
    id,
    function(input, output, session){
      dt_r_auto <- rv_file$dt_r_auto

      if(!is.null(dt_r_auto)){
        output$dt_r_auto <- renderDataTable({
          datatable(
            dt_r_auto,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })
      }
    }
  )
}
