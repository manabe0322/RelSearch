#' numeric_ui
#'
#' @description The function to create the ui module for numericInput
numeric_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("numeric"))
}

#' numeric_server
#'
#' @description The function to create the server module for numericInput
#' @param lab Label
#' @param val Value
numeric_server <- function(input, output, session, lab, val){

  rv_numeric <- reactiveVal(val)

  output$numeric <- renderUI({numericInput(session$ns("numeric"), label = lab, value = val)})

  observeEvent(input$numeric, {
    rv_numeric(input$numeric)
  })

  return(rv_numeric)
}
