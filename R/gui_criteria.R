#' tab_criteria_ui
#'
#' @description The function to create the ui module for criteria
tab_criteria_ui <- function(id){
  ns <- NS(id)

  tabPanel("Criteria",
           useShinyjs(),
           useShinyFeedback(),
           titlePanel("Criteria"),
           br(),
           p(HTML("<b>Press the save button to reflect the changes.</b>")),
           br(),
           fluidRow(
             column(4,
                    h4("STR"),
                    uiOutput(ns("output_min_lr_auto"))
             ),
             column(4,
                    h4("Y-STR"),
                    uiOutput(ns("output_max_mismatch_y")),
                    uiOutput(ns("output_max_mustep_y"))
             ),
             column(4,
                    h4("mtDNA"),
                    uiOutput(ns("output_max_mismatch_mt"))
             )
           ),
           br(),
           actionButton(ns("act_criteria_save"), label = "Save"),
           br(),
           br(),
           actionButton(ns("act_criteria_reset"), label = "Reset")
  )
}

#' tab_criteria_server
#'
#' @description The function to create the server module for criteria
#' @param path_pack Package path
tab_criteria_server <- function(id, path_pack){
  moduleServer(
    id,
    function(input, output, session){

      ######################################
      # Define the initial reactive values #
      ######################################

      init_dt_criteria <- create_dt_criteria(path_pack)
      rv_criteria <- reactiveValues()
      rv_criteria$min_lr_auto <- init_dt_criteria$Value[init_dt_criteria$Criteria == "min_lr_auto"]
      rv_criteria$max_mismatch_y <- init_dt_criteria$Value[init_dt_criteria$Criteria == "max_mismatch_y"]
      rv_criteria$max_mustep_y <- init_dt_criteria$Value[init_dt_criteria$Criteria == "max_mustep_y"]
      rv_criteria$max_mismatch_mt <- init_dt_criteria$Value[init_dt_criteria$Criteria == "max_mismatch_mt"]

      #############
      # Output UI #
      #############

      output$output_min_lr_auto <- renderUI({numericInput(session$ns("input_min_lr_auto"), label = "Minimum LR", value = rv_criteria$min_lr_auto)})
      output$output_max_mismatch_y <- renderUI({numericInput(session$ns("input_max_mismatch_y"), label = "Maximum number of mismatched loci", value = rv_criteria$max_mismatch_y)})
      output$output_max_mustep_y <- renderUI({numericInput(session$ns("input_max_mustep_y"), label = "Maximum total mutational steps", value = rv_criteria$max_mustep_y)})
      output$output_max_mismatch_mt <- renderUI({numericInput(session$ns("input_max_mismatch_mt"), label = "Maximum number of inconsistency", value = rv_criteria$max_mismatch_mt)})

      #####################################
      # Define the input rule of criteria #
      #####################################

      observeEvent(input$input_min_lr_auto, {
        min_lr_auto <- input$input_min_lr_auto
        if(!is.numeric(min_lr_auto)){
          hideFeedback("input_min_lr_auto")
          disable("act_criteria_save")
        }else if(min_lr_auto < 1){
          showFeedbackDanger(inputId = "input_min_lr_auto", text = "The number smaller than 1 is not allowed.")
          disable("act_criteria_save")
        }else{
          hideFeedback("input_min_lr_auto")
          max_mismatch_y <- input$input_max_mismatch_y
          max_mustep_y <- input$input_max_mustep_y
          max_mismatch_mt <- input$input_max_mismatch_mt
          if(all(is.integer(max_mismatch_y), is.integer(max_mustep_y), is.integer(max_mismatch_mt))){
            if(all(max_mismatch_y >= 0, max_mustep_y >= 0, max_mismatch_mt >= 0)){
              enable("act_criteria_save")
            }
          }
        }
      })

      observeEvent(input$input_max_mismatch_y, {
        max_mismatch_y <- input$input_max_mismatch_y
        if(!is.numeric(max_mismatch_y)){
          hideFeedback("input_max_mismatch_y")
          disable("act_criteria_save")
        }else if(!is.integer(max_mismatch_y) || max_mismatch_y < 0){
          showFeedbackDanger(inputId = "input_max_mismatch_y", text = "An integer greater than or equal to zero is allowed.")
          disable("act_criteria_save")
        }else{
          hideFeedback("input_max_mismatch_y")
          min_lr_auto <- input$input_min_lr_auto
          max_mustep_y <- input$input_max_mustep_y
          max_mismatch_mt <- input$input_max_mismatch_mt
          if(all(is.numeric(min_lr_auto), is.integer(max_mustep_y), is.integer(max_mismatch_mt))){
            if(all(min_lr_auto >= 1, max_mustep_y >= 0, max_mismatch_mt >= 0)){
              enable("act_criteria_save")
            }
          }
        }
      })

      observeEvent(input$input_max_mustep_y, {
        max_mustep_y <- input$input_max_mustep_y
        if(!is.numeric(max_mustep_y)){
          hideFeedback("input_max_mustep_y")
          disable("act_criteria_save")
        }else if(!is.integer(max_mustep_y) || max_mustep_y < 0){
          showFeedbackDanger(inputId = "input_max_mustep_y", text = "An integer greater than or equal to zero is allowed.")
          disable("act_criteria_save")
        }else{
          hideFeedback("input_max_mustep_y")
          min_lr_auto <- input$input_min_lr_auto
          max_mismatch_y <- input$input_max_mismatch_y
          max_mismatch_mt <- input$input_max_mismatch_mt
          if(all(is.numeric(min_lr_auto), is.integer(max_mismatch_y), is.integer(max_mismatch_mt))){
            if(all(min_lr_auto >= 1, max_mismatch_y >= 0, max_mismatch_mt >= 0)){
              enable("act_criteria_save")
            }
          }
        }
      })

      observeEvent(input$input_max_mismatch_mt, {
        max_mismatch_mt <- input$input_max_mismatch_mt
        if(!is.numeric(max_mismatch_mt)){
          hideFeedback("input_max_mismatch_mt")
          disable("act_criteria_save")
        }else if(!is.integer(max_mismatch_mt) || max_mismatch_mt < 0){
          showFeedbackDanger(inputId = "input_max_mismatch_mt", text = "An integer greater than or equal to zero is allowed.")
          disable("act_criteria_save")
        }else{
          hideFeedback("input_max_mismatch_mt")
          min_lr_auto <- input$input_min_lr_auto
          max_mismatch_y <- input$input_max_mismatch_y
          max_mustep_y <- input$input_max_mustep_y
          if(all(is.numeric(min_lr_auto), is.integer(max_mismatch_y), is.integer(max_mustep_y))){
            if(all(min_lr_auto >= 1, max_mismatch_y >= 0, max_mustep_y >= 0)){
              enable("act_criteria_save")
            }
          }
        }
      })

      #################
      # Save criteria #
      #################

      observeEvent(input$act_criteria_save, {
        rv_criteria$min_lr_auto <- input$input_min_lr_auto
        rv_criteria$max_mismatch_y <- input$input_max_mismatch_y
        rv_criteria$max_mustep_y <- input$input_max_mustep_y
        rv_criteria$max_mismatch_mt <- input$input_max_mismatch_mt

        new_dt_criteria <- data.table(Criteria = c("min_lr_auto", "max_mismatch_y", "max_mustep_y", "max_mismatch_mt"),
                                      Value = c(rv_criteria$min_lr_auto, rv_criteria$max_mismatch_y, rv_criteria$max_mustep_y, rv_criteria$max_mismatch_mt))

        write.csv(new_dt_criteria, paste0(path_pack, "/extdata/parameters/criteria.csv"), row.names = FALSE)

        showModal(modalDialog(title = "Information", "The criteria has been saved.", easyClose = TRUE, footer = NULL))
      }, ignoreInit = TRUE)

      ##################
      # Reset criteria #
      ##################

      observeEvent(input$act_criteria_reset, {
        new_dt_criteria <- create_dt_criteria(path_pack, FALSE)

        rv_criteria$min_lr_auto <- new_dt_criteria$Value[new_dt_criteria$Criteria == "min_lr_auto"]
        rv_criteria$max_mismatch_y <- new_dt_criteria$Value[new_dt_criteria$Criteria == "max_mismatch_y"]
        rv_criteria$max_mustep_y <- new_dt_criteria$Value[new_dt_criteria$Criteria == "max_mustep_y"]
        rv_criteria$max_mismatch_mt <- new_dt_criteria$Value[new_dt_criteria$Criteria == "max_mismatch_mt"]

        updateNumericInput(session, inputId = "input_min_lr_auto", value = rv_criteria$min_lr_auto)
        updateNumericInput(session, inputId = "input_max_mismatch_y", value = rv_criteria$max_mismatch_y)
        updateNumericInput(session, inputId = "input_max_mustep_y", value = rv_criteria$max_mustep_y)
        updateNumericInput(session, inputId = "input_max_mismatch_mt", value = rv_criteria$max_mismatch_mt)
      }, ignoreInit = TRUE)

      return(rv_criteria)
    }
  )
}
