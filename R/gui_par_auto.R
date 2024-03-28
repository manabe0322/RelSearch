#' tab_criteria_ui
#'
#' @description The function to create the ui module for the parameter
tab_par_auto_ui <- function(id){
  ns <- NS(id)

  tabPanel("Parameter",
           titlePanel("Parameter"),
           br(),
           p("Press the save button to reflect the changes."),
           br(),
           uiOutput(ns("maf")),
           br(),
           actionButton(ns("act_par_auto_save"), label = "Save"),
           br(),
           br(),
           actionButton(ns("act_par_auto_reset"), label = "Reset")
  )
}

#' tab_par_auto_server
#'
#' @description The function to create the server module for the parameter
#' @param init_dt_par_auto The initial data.table of the parameter
#' @param path_pack Package path
tab_par_auto_server <- function(id, init_dt_par_auto, path_pack){
  moduleServer(
    id,
    function(input, output, session){
      ######################################
      # Define the initial reactive values #
      ######################################

      rv_par_auto <- reactiveValues()
      rv_par_auto$maf <- init_dt_par_auto$Value[init_dt_par_auto$Parameter == "maf"]

      #############
      # Output UI #
      #############

      output$maf <- renderUI({numericInput(session$ns("maf"), label = "Minimum allele frequency", value = rv_par_auto$maf)})

      ##########################################
      # Define the input rule of the parameter #
      ##########################################

      iv_maf <- InputValidator$new()
      iv_maf$add_rule("maf", sv_numeric())
      iv_maf$enable()

      ##################
      # Save parameter #
      ##################

      observeEvent(input$act_par_auto_save, {
        if(isTruthy(input$maf)){
          rv_par_auto$maf <- input$maf

          new_dt_par_auto <- data.table(Parameter = c("maf"), Value = c(rv_par_auto$maf))

          write.csv(new_dt_par_auto, paste0(path_pack, "/extdata/parameters/par_auto.csv"), row.names = FALSE)

          showModal(modalDialog(title = "Information", "The parameter has been saved.", easyClose = TRUE, footer = NULL))
        }else{
          showModal(modalDialog(title = "Error", "Set the parameter!", easyClose = TRUE, footer = NULL))
        }
      })

      ###################
      # Reset parameter #
      ###################

      observeEvent(input$act_par_auto_reset, {
        new_dt_par_auto <- create_dt_par_auto(path_pack, FALSE)

        rv_par_auto$maf <- new_dt_par_auto$Value[new_dt_par_auto$Parameter == "maf"]
      })

      return(rv_par_auto)
    }
  )
}
