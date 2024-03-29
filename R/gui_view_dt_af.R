#' tab_view_dt_af_ui
#'
#' @description The function to create the ui module for viewing allele frequency database of the autosomal STR
tab_view_dt_af_ui <- function(id){
  ns <- NS(id)

  tabPanel("STR : Allele frequencies", dataTableOutput(ns("dt_af")))
}

#' tab_view_dt_af_server
#'
#' @description The function to create the server module for viewing allele frequency database of the autosomal STR
tab_view_dt_af_server <- function(id, rv_file){
  moduleServer(
    id,
    function(input, output, session){
      dt_af <- rv_file$dt_af

      if(!is.null(dt_af)){
        output$dt_af <- renderDataTable({
          datatable(
            dt_af,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })
      }
    }
  )
}
