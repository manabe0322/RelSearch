#' tab_criteria_ui
#'
#' @description The function to create the ui module for criteria
tab_criteria_ui <- function(id){
  ns <- NS(id)

  tabPanel("Criteria",
           titlePanel("Criteria"),
           br(),
           p("Press the save button to reflect the changes."),
           br(),
           fluidRow(
             column(4,
                    h4("STR"),
                    uiOutput(ns("min_lr_auto"))
             ),
             column(4,
                    h4("Y-STR"),
                    uiOutput(ns("max_mismatch_y")),
                    uiOutput(ns("max_ignore_y")),
                    uiOutput(ns("max_mustep_y"))
             ),
             column(4,
                    h4("mtDNA"),
                    uiOutput(ns("max_mismatch_mt")),
                    uiOutput(ns("min_share_mt"))
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
#' @param init_dt_criteria The initial data.table of criteria
#' @param path_pack Package path
tab_criteria_server <- function(input, output, session, init_dt_criteria, path_pack){

  ######################################
  # Define the initial reactive values #
  ######################################

  rv_criteria <- reactiveValues()
  rv_criteria$min_lr_auto <- init_dt_criteria$Value[init_dt_criteria$Criteria == "min_lr_auto"]
  rv_criteria$max_mismatch_y <- init_dt_criteria$Value[init_dt_criteria$Criteria == "max_mismatch_y"]
  rv_criteria$max_ignore_y <- init_dt_criteria$Value[init_dt_criteria$Criteria == "max_ignore_y"]
  rv_criteria$max_mustep_y <- init_dt_criteria$Value[init_dt_criteria$Criteria == "max_mustep_y"]
  rv_criteria$max_mismatch_mt <- init_dt_criteria$Value[init_dt_criteria$Criteria == "max_mismatch_mt"]
  rv_criteria$min_share_mt <- init_dt_criteria$Value[init_dt_criteria$Criteria == "min_share_mt"]

  output$min_lr_auto <- renderUI({numericInput(session$ns("min_lr_auto"), label = "Minimum LR", value = rv_criteria$min_lr_auto)})
  output$max_mismatch_y <- renderUI({numericInput(session$ns("max_mismatch_y"), label = "Maximum number of mismatched loci", value = rv_criteria$max_mismatch_y)})
  output$max_ignore_y <- renderUI({numericInput(session$ns("max_ignore_y"), label = "Maximum number of ignored loci", value = rv_criteria$max_ignore_y)})
  output$max_mustep_y <- renderUI({numericInput(session$ns("max_mustep_y"), label = "Maximum total mutational steps", value = rv_criteria$max_mustep_y)})
  output$max_mismatch_mt <- renderUI({numericInput(session$ns("max_mismatch_mt"), label = "Maximum number of inconsistency", value = rv_criteria$max_mismatch_mt)})
  output$min_share_mt <- renderUI({numericInput(session$ns("min_share_mt"), label = "Minimum shared length", value = rv_criteria$min_share_mt)})

  iv_min_lr_auto <- InputValidator$new()
  iv_min_lr_auto$add_rule("min_lr_auto", sv_numeric())
  iv_min_lr_auto$enable()

  iv_max_mismatch_y <- InputValidator$new()
  iv_max_mismatch_y$add_rule("max_mismatch_y", sv_numeric())
  iv_max_mismatch_y$enable()

  iv_max_ignore_y <- InputValidator$new()
  iv_max_ignore_y$add_rule("max_ignore_y", sv_numeric())
  iv_max_ignore_y$enable()

  iv_max_mustep_y <- InputValidator$new()
  iv_max_mustep_y$add_rule("max_mustep_y", sv_numeric())
  iv_max_mustep_y$enable()

  iv_max_mismatch_mt <- InputValidator$new()
  iv_max_mismatch_mt$add_rule("max_mismatch_mt", sv_numeric())
  iv_max_mismatch_mt$enable()

  iv_min_share_mt <- InputValidator$new()
  iv_min_share_mt$add_rule("min_share_mt", sv_numeric())
  iv_min_share_mt$enable()

  #################
  # Save criteria #
  #################

  observeEvent(input$act_criteria_save, {
    if(all(isTruthy(input$min_lr_auto), isTruthy(input$max_mismatch_y), isTruthy(input$max_ignore_y), isTruthy(input$max_mustep_y), isTruthy(input$max_mismatch_mt), isTruthy(input$min_share_mt))){
      rv_criteria$min_lr_auto <- input$min_lr_auto
      rv_criteria$max_mismatch_y <- input$max_mismatch_y
      rv_criteria$max_ignore_y <- input$max_ignore_y
      rv_criteria$max_mustep_y <- input$max_mustep_y
      rv_criteria$max_mismatch_mt <- input$max_mismatch_mt
      rv_criteria$min_share_mt <- input$min_share_mt

      new_dt_criteria <- data.table(Criteria = c("min_lr_auto", "max_mismatch_y", "max_ignore_y", "max_mustep_y", "max_mismatch_mt", "min_share_mt"),
                                    Value = c(rv_criteria$min_lr_auto, rv_criteria$max_mismatch_y, rv_criteria$max_ignore_y, rv_criteria$max_mustep_y, rv_criteria$max_mismatch_mt, rv_criteria$min_share_mt))

      write.csv(new_dt_criteria, paste0(path_pack, "/extdata/parameters/criteria.csv"), row.names = FALSE)

      showModal(modalDialog(title = "Information", "The criteria has been saved.", easyClose = TRUE, footer = NULL))
    }else{
      showModal(modalDialog(title = "Error", "Set criteria!", easyClose = TRUE, footer = NULL))
    }
  })

  ##################
  # Reset criteria #
  ##################

  observeEvent(input$act_criteria_reset, {
    new_dt_criteria <- create_dt_criteria(path_pack, FALSE)

    rv_criteria$min_lr_auto <- new_dt_criteria$Value[new_dt_criteria$Criteria == "min_lr_auto"]
    rv_criteria$max_mismatch_y <- new_dt_criteria$Value[new_dt_criteria$Criteria == "max_mismatch_y"]
    rv_criteria$max_ignore_y <- new_dt_criteria$Value[new_dt_criteria$Criteria == "max_ignore_y"]
    rv_criteria$max_mustep_y <- new_dt_criteria$Value[new_dt_criteria$Criteria == "max_mustep_y"]
    rv_criteria$max_mismatch_mt <- new_dt_criteria$Value[new_dt_criteria$Criteria == "max_mismatch_mt"]
    rv_criteria$min_share_mt <- new_dt_criteria$Value[new_dt_criteria$Criteria == "min_share_mt"]
  })

  return(rv_criteria)
}
