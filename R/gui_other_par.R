#' tab_other_par_ui
#'
#' @description The function to create the ui module for other parameters
tab_other_par_ui <- function(id){
  ns <- NS(id)

  tabPanel("Other parameters",
           useShinyjs(),
           useShinyFeedback(),
           titlePanel("Other parameters"),
           br(),
           p(HTML("<b>Press the save button to reflect the changes.</b>")),
           br(),
           uiOutput(ns("output_keep_min_lr")),
           br(),
           uiOutput(ns("output_max_data_displayed")),
           br(),
           actionButton(ns("act_other_par_save"), label = "Save"),
           br(),
           br(),
           actionButton(ns("act_other_par_reset"), label = "Reset")
  )
}

tab_other_par_server <- function(id, path_pack){
  moduleServer(
    id,
    function(input, output, session){

      ######################################
      # Define the initial reactive values #
      ######################################

      init_dt_other_par <- create_dt_other_par(path_pack)
      rv_other_par <- reactiveValues()
      rv_other_par$keep_min_lr <- init_dt_other_par$Value[init_dt_other_par$Parameter == "keep_min_lr"]
      rv_other_par$max_data_displayed <- init_dt_other_par$Value[init_dt_other_par$Parameter == "max_data_displayed"]

      #############
      # Output UI #
      #############

      output$output_keep_min_lr <- renderUI({numericInput(session$ns("input_keep_min_lr"), label = "Minimum LR to be stored", value = rv_other_par$keep_min_lr)})
      output$output_max_data_displayed <- renderUI({numericInput(session$ns("input_max_data_displayed"), label = "Maximum number of displayed data", value = rv_other_par$max_data_displayed)})

      #############################################
      # Define the input rule of other parameters #
      #############################################

      observeEvent(input$input_keep_min_lr, {
        keep_min_lr <- input$input_keep_min_lr
        if(!is.numeric(keep_min_lr)){
          hideFeedback("input_keep_min_lr")
          disable("act_other_par_save")
        }else if(keep_min_lr < 0){
          showFeedbackDanger(inputId = "input_keep_min_lr", text = "The negative number is not allowed.")
          disable("act_other_par_save")
        }else{
          hideFeedback("input_keep_min_lr")
          max_data_displayed <- input$input_max_data_displayed
          if(is.integer(max_data_displayed)){
            if(max_data_displayed > 0){
              enable("act_other_par_save")
            }
          }
        }
      })

      observeEvent(input$input_max_data_displayed, {
        max_data_displayed <- input$input_max_data_displayed
        if(!is.numeric(max_data_displayed)){
          hideFeedback("input_max_data_displayed")
          disable("act_other_par_save")
        }else if(!is.integer(max_data_displayed) || max_data_displayed <= 0){
          showFeedbackDanger(inputId = "input_max_data_displayed", text = "The positive interger is allowed.")
          disable("act_other_par_save")
        }else{
          hideFeedback("input_max_data_displayed")
          keep_min_lr <- input$input_keep_min_lr
          if(is.numeric(keep_min_lr)){
            if(keep_min_lr >= 0){
              enable("act_other_par_save")
            }
          }
        }
      })

      ########################
      # Save other parameter #
      ########################

      observeEvent(input$act_other_par_save, {
        rv_other_par$keep_min_lr <- input$input_keep_min_lr
        rv_other_par$max_data_displayed <- input$input_max_data_displayed

        new_dt_other_par <- data.table(Parameter = c("keep_min_lr", "max_data_displayed"),
                                       Value = c(rv_other_par$keep_min_lr, rv_other_par$max_data_displayed))

        write.csv(new_dt_other_par, paste0(path_pack, "/extdata/parameters/other_par.csv"), row.names = FALSE)

        showModal(modalDialog(title = "Information", "Other parameters have been saved.", easyClose = TRUE, footer = NULL))
      }, ignoreInit = TRUE)

      #########################
      # Reset other parameter #
      #########################

      observeEvent(input$act_other_par_reset, {
        new_dt_other_par <- create_dt_other_par(path_pack, FALSE)

        rv_other_par$keep_min_lr <- new_dt_other_par$Value[new_dt_other_par$Parameter == "keep_min_lr"]
        rv_other_par$max_data_displayed <- new_dt_other_par$Value[new_dt_other_par$Parameter == "max_data_displayed"]

        updateNumericInput(session, inputId = "input_keep_min_lr", value = rv_other_par$keep_min_lr)
        updateNumericInput(session, inputId = "input_max_data_displayed", value = rv_other_par$max_data_displayed)
      }, ignoreInit = TRUE)

      return(rv_other_par)
    }
  )
}
