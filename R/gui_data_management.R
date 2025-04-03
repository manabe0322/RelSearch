#' tab_data_manage_ui
#'
#' @description The function to create the ui module for data management
tab_data_manage_ui <- function(id){
  ns <- NS(id)

  tabPanel("Data storage conditions",
           useShinyjs(),
           useShinyFeedback(),
           titlePanel("Data storage conditions"),
           br(),
           uiOutput(ns("output_keep_min_lr")),
           br(),
           uiOutput(ns("output_max_data_displayed")),
           br(),
           actionButton(ns("act_data_manage_save"), label = "Save", class = "btn btn-primary btn-lg"),
           br(),
           br(),
           actionButton(ns("act_data_manage_reset"), label = "Reset", class = "btn btn-primary btn-lg")
  )
}

#' tab_data_manage_server
#'
#' @description The function to create the server module for data management
#' @param path_pack Package path
tab_data_manage_server <- function(id, path_pack){
  moduleServer(
    id,
    function(input, output, session){

      ######################################
      # Define the initial reactive values #
      ######################################

      init_dt_data_manage <- create_dt_data_manage(path_pack)
      rv_data_manage <- reactiveValues()
      rv_data_manage$keep_min_lr <- init_dt_data_manage$Value[init_dt_data_manage$Parameter == "keep_min_lr"]
      rv_data_manage$max_data_displayed <- init_dt_data_manage$Value[init_dt_data_manage$Parameter == "max_data_displayed"]

      #############
      # Output UI #
      #############

      output$output_keep_min_lr <- renderUI({numericInput(session$ns("input_keep_min_lr"), label = "Minimum LR to be stored", value = rv_data_manage$keep_min_lr)})
      output$output_max_data_displayed <- renderUI({numericInput(session$ns("input_max_data_displayed"), label = "Maximum number of displayed data", value = rv_data_manage$max_data_displayed)})

      ############################################
      # Define the input rule of data management #
      ############################################

      observeEvent(input$input_keep_min_lr, {
        keep_min_lr <- input$input_keep_min_lr
        if(!is.numeric(keep_min_lr)){
          hideFeedback("input_keep_min_lr")
          disable("act_data_manage_save")
        }else if(keep_min_lr < 0){
          showFeedbackDanger(inputId = "input_keep_min_lr", text = "The negative number is not allowed.")
          disable("act_data_manage_save")
        }else{
          hideFeedback("input_keep_min_lr")
          max_data_displayed <- input$input_max_data_displayed
          if(is.integer(max_data_displayed)){
            if(max_data_displayed > 0){
              enable("act_data_manage_save")
            }
          }
        }
      })

      observeEvent(input$input_max_data_displayed, {
        max_data_displayed <- input$input_max_data_displayed
        if(!is.numeric(max_data_displayed)){
          hideFeedback("input_max_data_displayed")
          disable("act_data_manage_save")
        }else if(!is.integer(max_data_displayed) || max_data_displayed <= 0){
          showFeedbackDanger(inputId = "input_max_data_displayed", text = "The positive interger is allowed.")
          disable("act_data_manage_save")
        }else{
          hideFeedback("input_max_data_displayed")
          keep_min_lr <- input$input_keep_min_lr
          if(is.numeric(keep_min_lr)){
            if(keep_min_lr >= 0){
              enable("act_data_manage_save")
            }
          }
        }
      })

      ####################################
      # Save setting for data management #
      ####################################

      observeEvent(input$act_data_manage_save, {
        rv_data_manage$keep_min_lr <- input$input_keep_min_lr
        rv_data_manage$max_data_displayed <- input$input_max_data_displayed

        new_dt_data_manage <- data.table(Parameter = c("keep_min_lr", "max_data_displayed"),
                                         Value = c(rv_data_manage$keep_min_lr, rv_data_manage$max_data_displayed))

        write.csv(new_dt_data_manage, paste0(path_pack, "/extdata/parameters/data_manage.csv"), row.names = FALSE)

        showModal(modalDialog(title = "Information", "Settings for data management have been saved.", easyClose = TRUE, footer = NULL))
      }, ignoreInit = TRUE)

      #####################################
      # Reset setting for data management #
      #####################################

      observeEvent(input$act_data_manage_reset, {
        new_dt_data_manage <- create_dt_data_manage(path_pack, FALSE)

        rv_data_manage$keep_min_lr <- new_dt_data_manage$Value[new_dt_data_manage$Parameter == "keep_min_lr"]
        rv_data_manage$max_data_displayed <- new_dt_data_manage$Value[new_dt_data_manage$Parameter == "max_data_displayed"]

        updateNumericInput(session, inputId = "input_keep_min_lr", value = rv_data_manage$keep_min_lr)
        updateNumericInput(session, inputId = "input_max_data_displayed", value = rv_data_manage$max_data_displayed)
      }, ignoreInit = TRUE)

      return(rv_data_manage)
    }
  )
}
