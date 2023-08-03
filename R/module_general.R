#######################################################
# The function to create the generally used ui module #
#######################################################

# numericInput
numeric_ui <- function(id){
  ns <- NS(id)

  uiOutput(ns("numeric"))
}

# radioButtons
radio_ui <- function(id){
  ns <- NS(id)

  uiOutput(ns("radio"))
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

# radioButtons
radio_server <- function(input, output, session, lab, choice_list, select){

  rv_radio <- reactiveVal(as.numeric(select))

  output$radio <- renderUI({radioButtons(session$ns("radio"), label = lab, choices = choice_list, selected = select)})

  observeEvent(input$radio, {
    rv_radio(as.numeric(input$radio))
  })

  return(rv_radio)
}
