#' tab_myu_ui
#'
#' @description The function to create the ui module for mutation rates
tab_myu_ui <- function(id){
  ns <- NS(id)

  tabPanel("Mutation rates",
           titlePanel("Mutation rates"),
           br(),
           sidebarLayout(
             sidebarPanel(
               actionButton(ns("act_myu_edit"), label = "Edit"),
               br(),
               br(),
               actionButton(ns("act_myu_add"), label = "Add"),
               br(),
               br(),
               actionButton(ns("act_myu_del"), label = "Delete"),
               br(),
               br(),
               actionButton(ns("act_myu_reset"), label = "Reset")
             ),
             mainPanel(
               dataTableOutput(ns("dt_myu"))
             )
           )
  )
}

#' tab_myu_server
#'
#' @description The function to create the server module for mutation rates
#' @param init_dt_myu The initial data.table of mutation rates
#' @param path_pack Package path
tab_myu_server <- function(id, init_dt_myu, path_pack){
  moduleServer(
    id,
    function(input, output, session){
      ######################################
      # Define the initial reactive values #
      ######################################

      rv_myu <- reactiveValues()
      rv_myu$mk <- init_dt_myu[, Marker]
      rv_myu$val <- init_dt_myu[, Myu]

      ########################
      # Edit a mutation rate #
      ########################

      observeEvent(input$act_myu_edit, {
        showModal(modalDialog(
          title = "Edit a mutation rate",
          selectInput(session$ns("input_myu_mk_edit"), label = "Select a locus", choices = rv_myu$mk, selected = rv_myu$mk[1]),
          uiOutput(session$ns("output_myu_val_edit")),
          footer = tagList(
            actionButton(session$ns("act_myu_edit_save"), "Save"),
            modalButton("Cancel")
          ),
          size = "l"
        ))
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_mk_edit, {
        output$output_myu_val_edit <- renderUI(numericInput(session$ns("input_myu_val_edit"), label = "Enter a mutation rate", value = rv_myu$val[rv_myu$mk == input$input_myu_mk_edit]))
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_val_edit, {
        myu_val_edit <- input$input_myu_val_edit
        if(!is.numeric(myu_val_edit)){
          hideFeedback("input_myu_val_edit")
          disable("act_myu_edit_save")
        }else if(myu_val_edit < 0 || myu_val_edit > 1){
          showFeedbackDanger(inputId = "input_myu_val_edit", text = "A positive number between 0 to 1 is allowed.")
          disable("act_myu_edit_save")
        }else{
          hideFeedback("input_myu_val_edit")
          enable("act_myu_edit_save")
        }
      }, ignoreInit = TRUE)

      observeEvent(input$act_myu_edit_save, {
        myu_mk_edit <- input$input_myu_mk_edit
        myu_val_edit <- input$input_myu_val_edit

        rv_myu$val[rv_myu$mk == myu_mk_edit] <- myu_val_edit

        new_dt_myu <- data.table(Marker = rv_myu$mk, Myu = rv_myu$val)
        write.csv(new_dt_myu, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)

        output$dt_myu <- renderDataTable({
          datatable(
            new_dt_myu,
            colnames = c("Locus", "Mutation rates"),
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        removeModal()
      }, ignoreInit = TRUE)

      #######################
      # Add a mutation rate #
      #######################

      observeEvent(input$act_myu_add, {
        showModal(modalDialog(
          title = "Add a mutation rate",
          textInput(session$ns("input_myu_mk_add"), label = "Enter a locus name", value = NULL),
          numericInput(session$ns("input_myu_val_add"), label = "Mutation rate", value = NULL),
          footer = tagList(
            disabled(actionButton(session$ns("act_myu_add_save"), "Save")),
            modalButton("Cancel")
          ),
          size = "l"
        ))
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_mk_add, {
        myu_mk_add <- input$input_myu_mk_add
        if(nchar(myu_mk_add) == 0){
          hideFeedback("input_myu_mk_add")
          disable("act_myu_add_save")
        }else if(is.element(myu_mk_add, rv_myu$mk)){
          showFeedbackDanger(inputId = "input_myu_mk_add", text = "The name has been already registered.")
          disable("act_myu_add_save")
        }else{
          hideFeedback("input_myu_mk_add")
          myu_val_add <- input$input_myu_val_add
          if(is.numeric(myu_val_add) && myu_val_add >= 0 && myu_val_add <= 1){
            enable("act_myu_add_save")
          }
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_val_add, {
        myu_val_add <- input$input_myu_val_add
        if(!is.numeric(myu_val_add)){
          hideFeedback("input_myu_val_add")
          disable("act_myu_add_save")
        }else if(myu_val_add < 0 || myu_val_add > 1){
          showFeedbackDanger(inputId = "input_myu_val_add", text = "A positive number between 0 to 1 is allowed.")
          disable("act_myu_add_save")
        }else{
          hideFeedback("input_myu_val_add")
          myu_mk_add <- input$input_myu_mk_add
          if(nchar(myu_mk_add) != 0 && !is.element(myu_mk_add, rv_myu$mk)){
            enable("act_myu_add_save")
          }
        }
      }, ignoreInit = TRUE)

      observeEvent(input$act_myu_add_save, {
        myu_mk_add <- input$input_myu_mk_add
        myu_val_add <- input$input_myu_val_add

        rv_myu$mk <- c(rv_myu$mk, myu_mk_add)
        rv_myu$val <- c(rv_myu$val, myu_val_add)

        new_dt_myu <- data.table(Marker = rv_myu$mk, Myu = rv_myu$val)
        write.csv(new_dt_myu, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)

        output$dt_myu <- renderDataTable({
          datatable(
            new_dt_myu,
            colnames = c("Locus", "Mutation rates"),
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        removeModal()
      }, ignoreInit = TRUE)

      ##########################
      # Delete a mutation rate #
      ##########################

      observeEvent(input$act_myu_del, {
        showModal(modalDialog(
          title = "Delete a mutation rate",
          selectInput(session$ns("input_myu_mk_del"), label = "Select a locus", choices = rv_myu$mk, selected = rv_myu$mk[1]),
          footer = tagList(
            actionButton(session$ns("act_myu_del_save"), "Save"),
            modalButton("Cancel")
          ),
          size = "l"
        ))
      }, ignoreInit = TRUE)

      observeEvent(input$act_myu_del_save, {
        pos_del <- which(rv_myu$mk == input$input_myu_mk_del)
        rv_myu$mk <- rv_myu$mk[- pos_del]
        rv_myu$val <- rv_myu$val[- pos_del]

        new_dt_myu <- data.table(Marker = rv_myu$mk, Myu = rv_myu$val)
        write.csv(new_dt_myu, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)

        output$dt_myu <- renderDataTable({
          datatable(
            new_dt_myu,
            colnames = c("Locus", "Mutation rates"),
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        removeModal()
      }, ignoreInit = TRUE)

      ############################
      # Reset the mutation rates #
      ############################

      observeEvent(input$act_myu_reset, {
        showModal(modalDialog(
          title = "Reset the mutation rates",
          "Click Restore default to remove all changes.",
          footer = tagList(
            actionButton(session$ns("act_myu_reset_yes"), "Restore default"),
            modalButton("Cancel")
          ),
        ))
      }, ignoreInit = TRUE)

      observeEvent(input$act_myu_reset_yes, {
        new_dt_myu <- create_dt_myu(path_pack, FALSE)

        rv_myu$mk <- new_dt_myu[, Marker]
        rv_myu$val <- new_dt_myu[, Myu]

        output$dt_myu <- renderDataTable({
          datatable(
            new_dt_myu,
            colnames = c("Locus", "Mutation rates"),
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        removeModal()
      }, ignoreInit = TRUE)

      ######################################
      # Display the initial mutation rates #
      ######################################

      output$dt_myu <- renderDataTable({
        datatable(
          init_dt_myu,
          colnames = c("Locus", "Mutation rates"),
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE),
          rownames = FALSE
        )
      })

      return(rv_myu)
    }
  )
}
