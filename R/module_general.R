#######################################################
# The function to create the generally used ui module #
#######################################################

# numericInput
numeric_ui <- function(id){
  ns <- NS(id)

  uiOutput(ns("numeric"))
}


###########################################################
# The function to create the generally used server module #
###########################################################

# numericInput
numeric_server <- function(input, output, session, lab, val){

  rv_numeric <- reactiveVal(val)

  output$numeric <- renderUI({numericInput(session$ns("numeric"), label = lab, value = val)})

  observeEvent(input$numeric, {
    rv_numeric(input$numeric)
  })

  return(rv_numeric)
}
