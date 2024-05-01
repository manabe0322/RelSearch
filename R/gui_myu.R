#' tab_myu_ui
#'
#' @description The function to create the ui module for mutation rates
tab_myu_ui <- function(id){
  ns <- NS(id)

  tabPanel("Mutation rates",
           useShinyjs(),
           useShinyFeedback(),
           titlePanel("Mutation rates"),
           br(),
           sidebarLayout(
             sidebarPanel(width = 2,
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
             mainPanel(width = 10,
                       dataTableOutput(ns("dt_myu"))
             )
           )
  )
}

#' tab_myu_server
#'
#' @description The function to create the server module for mutation rates
#' @param path_pack Package path
tab_myu_server <- function(id, path_pack){
  moduleServer(
    id,
    function(input, output, session){

      ######################################
      # Define the initial reactive values #
      ######################################

      init_dt_myu <- create_dt_myu(path_pack)
      rv_myu <- reactiveValues()
      rv_myu$mk <- init_dt_myu[, Marker]
      rv_myu$paternal_m2 <- init_dt_myu[, Paternal_m2]
      rv_myu$paternal_m1 <- init_dt_myu[, Paternal_m1]
      rv_myu$paternal_0 <- init_dt_myu[, Paternal_0]
      rv_myu$paternal_p1 <- init_dt_myu[, Paternal_p1]
      rv_myu$paternal_p2 <- init_dt_myu[, Paternal_p2]
      rv_myu$maternal_m2 <- init_dt_myu[, Maternal_m2]
      rv_myu$maternal_m1 <- init_dt_myu[, Maternal_m1]
      rv_myu$maternal_0 <- init_dt_myu[, Maternal_0]
      rv_myu$maternal_p1 <- init_dt_myu[, Maternal_p1]
      rv_myu$maternal_p2 <- init_dt_myu[, Maternal_p2]

      rv_check_edit <- reactiveValues()
      rv_check_edit$paternal_m2 <- FALSE
      rv_check_edit$paternal_m1 <- FALSE
      rv_check_edit$paternal_p1 <- FALSE
      rv_check_edit$paternal_p2 <- FALSE
      rv_check_edit$maternal_m2 <- FALSE
      rv_check_edit$maternal_m1 <- FALSE
      rv_check_edit$maternal_p1 <- FALSE
      rv_check_edit$maternal_p2 <- FALSE

      rv_check_add <- reactiveValues()
      rv_check_add$mk <- FALSE
      rv_check_add$paternal_m2 <- FALSE
      rv_check_add$paternal_m1 <- FALSE
      rv_check_add$paternal_p1 <- FALSE
      rv_check_add$paternal_p2 <- FALSE
      rv_check_add$maternal_m2 <- FALSE
      rv_check_add$maternal_m1 <- FALSE
      rv_check_add$maternal_p1 <- FALSE
      rv_check_add$maternal_p2 <- FALSE

      ########################
      # Edit a mutation rate #
      ########################

      observeEvent(input$act_myu_edit, {
        showModal(modalDialog(
          title = "Edit a mutation rate",
          selectInput(session$ns("input_myu_mk_edit"), label = "Select a locus", choices = rv_myu$mk, selected = rv_myu$mk[1], selectize = FALSE),
          fluidRow(
            column(6,
                   uiOutput(session$ns("output_myu_paternal_m2_edit")),
                   uiOutput(session$ns("output_myu_paternal_m1_edit")),
                   uiOutput(session$ns("output_myu_paternal_p1_edit")),
                   uiOutput(session$ns("output_myu_paternal_p2_edit"))
            ),
            column(6,
                   uiOutput(session$ns("output_myu_maternal_m2_edit")),
                   uiOutput(session$ns("output_myu_maternal_m1_edit")),
                   uiOutput(session$ns("output_myu_maternal_p1_edit")),
                   uiOutput(session$ns("output_myu_maternal_p2_edit"))
            )
          ),
          footer = tagList(
            actionButton(session$ns("act_myu_edit_save"), "Save"),
            modalButton("Cancel")
          ),
          size = "l"
        ))
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_mk_edit, {
        myu_mk_all <- rv_myu$mk
        myu_mk_edit <- input$input_myu_mk_edit
        output$output_myu_paternal_m2_edit <- renderUI(numericInput(session$ns("input_myu_paternal_m2_edit"), label = "Paternal -2 step", value = rv_myu$paternal_m2[myu_mk_all == myu_mk_edit]))
        output$output_myu_paternal_m1_edit <- renderUI(numericInput(session$ns("input_myu_paternal_m1_edit"), label = "Paternal -1 step", value = rv_myu$paternal_m1[myu_mk_all == myu_mk_edit]))
        output$output_myu_paternal_p1_edit <- renderUI(numericInput(session$ns("input_myu_paternal_p1_edit"), label = "Paternal +1 step", value = rv_myu$paternal_p1[myu_mk_all == myu_mk_edit]))
        output$output_myu_paternal_p2_edit <- renderUI(numericInput(session$ns("input_myu_paternal_p2_edit"), label = "Paternal +2 step", value = rv_myu$paternal_p2[myu_mk_all == myu_mk_edit]))
        output$output_myu_maternal_m2_edit <- renderUI(numericInput(session$ns("input_myu_maternal_m2_edit"), label = "Maternal -2 step", value = rv_myu$maternal_m2[myu_mk_all == myu_mk_edit]))
        output$output_myu_maternal_m1_edit <- renderUI(numericInput(session$ns("input_myu_maternal_m1_edit"), label = "Maternal -1 step", value = rv_myu$maternal_m1[myu_mk_all == myu_mk_edit]))
        output$output_myu_maternal_p1_edit <- renderUI(numericInput(session$ns("input_myu_maternal_p1_edit"), label = "Maternal +1 step", value = rv_myu$maternal_p1[myu_mk_all == myu_mk_edit]))
        output$output_myu_maternal_p2_edit <- renderUI(numericInput(session$ns("input_myu_maternal_p2_edit"), label = "Maternal +2 step", value = rv_myu$maternal_p2[myu_mk_all == myu_mk_edit]))
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_paternal_m2_edit, {
        paternal_m2 <- input$input_myu_paternal_m2_edit
        paternal_m1 <- input$input_myu_paternal_m1_edit
        paternal_p1 <- input$input_myu_paternal_p1_edit
        paternal_p2 <- input$input_myu_paternal_p2_edit
        if(!is.numeric(paternal_m2)){
          hideFeedback("input_myu_paternal_m2_edit")
          rv_check_edit$paternal_m2 <- FALSE
        }else if(paternal_m2 < 0 || paternal_m2 > 1){
          showFeedbackDanger(inputId = "input_myu_paternal_m2_edit", text = "A positive number between 0 to 1 is allowed.")
          rv_check_edit$paternal_m2 <- FALSE
        }else{
          hideFeedback("input_myu_paternal_m2_edit")
          rv_check_edit$paternal_m2 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_paternal_m1_edit, {
        paternal_m2 <- input$input_myu_paternal_m2_edit
        paternal_m1 <- input$input_myu_paternal_m1_edit
        paternal_p1 <- input$input_myu_paternal_p1_edit
        paternal_p2 <- input$input_myu_paternal_p2_edit
        if(!is.numeric(paternal_m1)){
          hideFeedback("input_myu_paternal_m1_edit")
          rv_check_edit$paternal_m1 <- FALSE
        }else if(paternal_m1 < 0 || paternal_m1 > 1){
          showFeedbackDanger(inputId = "input_myu_paternal_m1_edit", text = "A positive number between 0 to 1 is allowed.")
          rv_check_edit$paternal_m1 <- FALSE
        }else{
          hideFeedback("input_myu_paternal_m1_edit")
          rv_check_edit$paternal_m1 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_paternal_p1_edit, {
        paternal_m2 <- input$input_myu_paternal_m2_edit
        paternal_m1 <- input$input_myu_paternal_m1_edit
        paternal_p1 <- input$input_myu_paternal_p1_edit
        paternal_p2 <- input$input_myu_paternal_p2_edit
        if(!is.numeric(paternal_p1)){
          hideFeedback("input_myu_paternal_p1_edit")
          rv_check_edit$paternal_p1 <- FALSE
        }else if(paternal_p1 < 0 || paternal_p1 > 1){
          showFeedbackDanger(inputId = "input_myu_paternal_p1_edit", text = "A positive number between 0 to 1 is allowed.")
          rv_check_edit$paternal_p1 <- FALSE
        }else{
          hideFeedback("input_myu_paternal_p1_edit")
          rv_check_edit$paternal_p1 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_paternal_p2_edit, {
        paternal_m2 <- input$input_myu_paternal_m2_edit
        paternal_m1 <- input$input_myu_paternal_m1_edit
        paternal_p1 <- input$input_myu_paternal_p1_edit
        paternal_p2 <- input$input_myu_paternal_p2_edit
        if(!is.numeric(paternal_p2)){
          hideFeedback("input_myu_paternal_p2_edit")
          rv_check_edit$paternal_p2 <- FALSE
        }else if(paternal_p2 < 0 || paternal_p2 > 1){
          showFeedbackDanger(inputId = "input_myu_paternal_p2_edit", text = "A positive number between 0 to 1 is allowed.")
          rv_check_edit$paternal_p2 <- FALSE
        }else{
          hideFeedback("input_myu_paternal_p2_edit")
          rv_check_edit$paternal_p2 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_maternal_m2_edit, {
        maternal_m2 <- input$input_myu_maternal_m2_edit
        maternal_m1 <- input$input_myu_maternal_m1_edit
        maternal_p1 <- input$input_myu_maternal_p1_edit
        maternal_p2 <- input$input_myu_maternal_p2_edit
        if(!is.numeric(maternal_m2)){
          hideFeedback("input_myu_maternal_m2_edit")
          rv_check_edit$maternal_m2 <- FALSE
        }else if(maternal_m2 < 0 || maternal_m2 > 1){
          showFeedbackDanger(inputId = "input_myu_maternal_m2_edit", text = "A positive number between 0 to 1 is allowed.")
          rv_check_edit$maternal_m2 <- FALSE
        }else{
          hideFeedback("input_myu_maternal_m2_edit")
          rv_check_edit$maternal_m2 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_maternal_m1_edit, {
        maternal_m2 <- input$input_myu_maternal_m2_edit
        maternal_m1 <- input$input_myu_maternal_m1_edit
        maternal_p1 <- input$input_myu_maternal_p1_edit
        maternal_p2 <- input$input_myu_maternal_p2_edit
        if(!is.numeric(maternal_m1)){
          hideFeedback("input_myu_maternal_m1_edit")
          rv_check_edit$maternal_m1 <- FALSE
        }else if(maternal_m1 < 0 || maternal_m1 > 1){
          showFeedbackDanger(inputId = "input_myu_maternal_m1_edit", text = "A positive number between 0 to 1 is allowed.")
          rv_check_edit$maternal_m1 <- FALSE
        }else{
          hideFeedback("input_myu_maternal_m1_edit")
          rv_check_edit$maternal_m1 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_maternal_p1_edit, {
        maternal_m2 <- input$input_myu_maternal_m2_edit
        maternal_m1 <- input$input_myu_maternal_m1_edit
        maternal_p1 <- input$input_myu_maternal_p1_edit
        maternal_p2 <- input$input_myu_maternal_p2_edit
        if(!is.numeric(maternal_p1)){
          hideFeedback("input_myu_maternal_p1_edit")
          rv_check_edit$maternal_p1 <- FALSE
        }else if(maternal_p1 < 0 || maternal_p1 > 1){
          showFeedbackDanger(inputId = "input_myu_maternal_p1_edit", text = "A positive number between 0 to 1 is allowed.")
          rv_check_edit$maternal_p1 <- FALSE
        }else{
          hideFeedback("input_myu_maternal_p1_edit")
          rv_check_edit$maternal_p1 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_maternal_p2_edit, {
        maternal_m2 <- input$input_myu_maternal_m2_edit
        maternal_m1 <- input$input_myu_maternal_m1_edit
        maternal_p1 <- input$input_myu_maternal_p1_edit
        maternal_p2 <- input$input_myu_maternal_p2_edit
        if(!is.numeric(maternal_p2)){
          hideFeedback("input_myu_maternal_p2_edit")
          rv_check_edit$maternal_p2 <- FALSE
        }else if(maternal_p2 < 0 || maternal_p2 > 1){
          showFeedbackDanger(inputId = "input_myu_maternal_p2_edit", text = "A positive number between 0 to 1 is allowed.")
          rv_check_edit$maternal_p2 <- FALSE
        }else{
          hideFeedback("input_myu_maternal_p2_edit")
          rv_check_edit$maternal_p2 <- TRUE
        }
      }, ignoreInit = TRUE)

      observe({
        req(rv_check_edit)
        bool_act_myu_edit_save <- TRUE
        if(all(rv_check_edit$paternal_m2, rv_check_edit$paternal_m1, rv_check_edit$paternal_p1, rv_check_edit$paternal_p2)){
          paternal_m2 <- input$input_myu_paternal_m2_edit
          paternal_m1 <- input$input_myu_paternal_m1_edit
          paternal_p1 <- input$input_myu_paternal_p1_edit
          paternal_p2 <- input$input_myu_paternal_p2_edit

          if(paternal_m2 + paternal_m1 + paternal_p1 + paternal_p2 > 1){
            showFeedbackDanger(inputId = "input_myu_paternal_m2_edit", text = "Sum of all paternal mutation rates should not be greater than 1.")
            showFeedbackDanger(inputId = "input_myu_paternal_m1_edit", text = "Sum of all paternal mutation rates should not be greater than 1.")
            showFeedbackDanger(inputId = "input_myu_paternal_p1_edit", text = "Sum of all paternal mutation rates should not be greater than 1.")
            showFeedbackDanger(inputId = "input_myu_paternal_p2_edit", text = "Sum of all paternal mutation rates should not be greater than 1.")
            bool_act_myu_edit_save <- FALSE
          }else{
            hideFeedback("input_myu_paternal_m2_edit")
            hideFeedback("input_myu_paternal_m1_edit")
            hideFeedback("input_myu_paternal_p1_edit")
            hideFeedback("input_myu_paternal_p2_edit")
          }
        }else{
          bool_act_myu_edit_save <- FALSE
        }

        if(all(rv_check_edit$maternal_m2, rv_check_edit$maternal_m1, rv_check_edit$maternal_p1, rv_check_edit$maternal_p2)){
          maternal_m2 <- input$input_myu_maternal_m2_edit
          maternal_m1 <- input$input_myu_maternal_m1_edit
          maternal_p1 <- input$input_myu_maternal_p1_edit
          maternal_p2 <- input$input_myu_maternal_p2_edit

          if(maternal_m2 + maternal_m1 + maternal_p1 + maternal_p2 > 1){
            showFeedbackDanger(inputId = "input_myu_maternal_m2_edit", text = "Sum of all maternal mutation rates should not be greater than 1.")
            showFeedbackDanger(inputId = "input_myu_maternal_m1_edit", text = "Sum of all maternal mutation rates should not be greater than 1.")
            showFeedbackDanger(inputId = "input_myu_maternal_p1_edit", text = "Sum of all maternal mutation rates should not be greater than 1.")
            showFeedbackDanger(inputId = "input_myu_maternal_p2_edit", text = "Sum of all maternal mutation rates should not be greater than 1.")
            bool_act_myu_edit_save <- FALSE
          }else{
            hideFeedback("input_myu_maternal_m2_edit")
            hideFeedback("input_myu_maternal_m1_edit")
            hideFeedback("input_myu_maternal_p1_edit")
            hideFeedback("input_myu_maternal_p2_edit")
          }
        }else{
          bool_act_myu_edit_save <- FALSE
        }

        if(bool_act_myu_edit_save){
          enable("act_myu_edit_save")
        }else{
          disable("act_myu_edit_save")
        }
      })

      observeEvent(input$act_myu_edit_save, {
        myu_mk_all <- rv_myu$mk
        myu_mk_edit <- input$input_myu_mk_edit
        paternal_m2 <- input$input_myu_paternal_m2_edit
        paternal_m1 <- input$input_myu_paternal_m1_edit
        paternal_p1 <- input$input_myu_paternal_p1_edit
        paternal_p2 <- input$input_myu_paternal_p2_edit
        paternal_0 <- round(1 - paternal_m2 - paternal_m1 - paternal_p1 - paternal_p2, 10)
        maternal_m2 <- input$input_myu_maternal_m2_edit
        maternal_m1 <- input$input_myu_maternal_m1_edit
        maternal_p1 <- input$input_myu_maternal_p1_edit
        maternal_p2 <- input$input_myu_maternal_p2_edit
        maternal_0 <- round(1 - maternal_m2 - maternal_m1 - maternal_p1 - maternal_p2, 10)

        rv_myu$paternal_m2[myu_mk_all == myu_mk_edit] <- paternal_m2
        rv_myu$paternal_m1[myu_mk_all == myu_mk_edit] <- paternal_m1
        rv_myu$paternal_0[myu_mk_all == myu_mk_edit] <- paternal_0
        rv_myu$paternal_p1[myu_mk_all == myu_mk_edit] <- paternal_p1
        rv_myu$paternal_p2[myu_mk_all == myu_mk_edit] <- paternal_p2
        rv_myu$maternal_m2[myu_mk_all == myu_mk_edit] <- maternal_m2
        rv_myu$maternal_m1[myu_mk_all == myu_mk_edit] <- maternal_m1
        rv_myu$maternal_0[myu_mk_all == myu_mk_edit] <- maternal_0
        rv_myu$maternal_p1[myu_mk_all == myu_mk_edit] <- maternal_p1
        rv_myu$maternal_p2[myu_mk_all == myu_mk_edit] <- maternal_p2

        new_dt_myu <- data.table(Marker = myu_mk_all,
                                 Paternal_m2 = rv_myu$paternal_m2,
                                 Paternal_m1 = rv_myu$paternal_m1,
                                 Paternal_0 = rv_myu$paternal_0,
                                 Paternal_p1 = rv_myu$paternal_p1,
                                 Paternal_p2 = rv_myu$paternal_p2,
                                 Maternal_m2 = rv_myu$maternal_m2,
                                 Maternal_m1 = rv_myu$maternal_m1,
                                 Maternal_0 = rv_myu$maternal_0,
                                 Maternal_p1 = rv_myu$maternal_p1,
                                 Maternal_p2 = rv_myu$maternal_p2)
        write.csv(new_dt_myu, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)

        output$dt_myu <- renderDataTable({
          datatable(
            new_dt_myu,
            colnames = c("Locus", "Paternal -2 step", "Paternal -1 step", "Paternal 0 step", "Paternal +1 step", "Paternal +2 step", "Maternal -2 step", "Maternal -1 step", "Maternal 0 step", "Maternal +1 step", "Maternal +2 step"),
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
          textInput(session$ns("input_myu_mk_add"), label = "Locus name", value = NULL),
          fluidRow(
            column(6,
                   numericInput(session$ns("input_myu_paternal_m2_add"), label = "Paternal -2 step", value = NULL),
                   numericInput(session$ns("input_myu_paternal_m1_add"), label = "Paternal -1 step", value = NULL),
                   numericInput(session$ns("input_myu_paternal_p1_add"), label = "Paternal +1 step", value = NULL),
                   numericInput(session$ns("input_myu_paternal_p2_add"), label = "Paternal +2 step", value = NULL)
            ),
            column(6,
                   numericInput(session$ns("input_myu_maternal_m2_add"), label = "Maternal -2 step", value = NULL),
                   numericInput(session$ns("input_myu_maternal_m1_add"), label = "Maternal -1 step", value = NULL),
                   numericInput(session$ns("input_myu_maternal_p1_add"), label = "Maternal +1 step", value = NULL),
                   numericInput(session$ns("input_myu_maternal_p2_add"), label = "Maternal +2 step", value = NULL)
            )
          ),
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
          rv_check_add$mk <- FALSE
        }else if(is.element(myu_mk_add, rv_myu$mk)){
          showFeedbackDanger(inputId = "input_myu_mk_add", text = "The name has been already registered.")
          rv_check_add$mk <- FALSE
        }else{
          hideFeedback("input_myu_mk_add")
          rv_check_add$mk <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_paternal_m2_add, {
        paternal_m2 <- input$input_myu_paternal_m2_add
        paternal_m1 <- input$input_myu_paternal_m1_add
        paternal_p1 <- input$input_myu_paternal_p1_add
        paternal_p2 <- input$input_myu_paternal_p2_add
        if(!is.numeric(paternal_m2)){
          hideFeedback("input_myu_paternal_m2_add")
          rv_check_add$paternal_m2 <- FALSE
        }else if(paternal_m2 < 0 || paternal_m2 > 1){
          showFeedbackDanger(inputId = "input_myu_paternal_m2_add", text = "A positive number between 0 to 1 is allowed.")
          rv_check_add$paternal_m2 <- FALSE
        }else{
          hideFeedback("input_myu_paternal_m2_add")
          rv_check_add$paternal_m2 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_paternal_m1_add, {
        paternal_m2 <- input$input_myu_paternal_m2_add
        paternal_m1 <- input$input_myu_paternal_m1_add
        paternal_p1 <- input$input_myu_paternal_p1_add
        paternal_p2 <- input$input_myu_paternal_p2_add
        if(!is.numeric(paternal_m1)){
          hideFeedback("input_myu_paternal_m1_add")
          rv_check_add$paternal_m1 <- FALSE
        }else if(paternal_m1 < 0 || paternal_m1 > 1){
          showFeedbackDanger(inputId = "input_myu_paternal_m1_add", text = "A positive number between 0 to 1 is allowed.")
          rv_check_add$paternal_m1 <- FALSE
        }else{
          hideFeedback("input_myu_paternal_m1_add")
          rv_check_add$paternal_m1 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_paternal_p1_add, {
        paternal_m2 <- input$input_myu_paternal_m2_add
        paternal_m1 <- input$input_myu_paternal_m1_add
        paternal_p1 <- input$input_myu_paternal_p1_add
        paternal_p2 <- input$input_myu_paternal_p2_add
        if(!is.numeric(paternal_p1)){
          hideFeedback("input_myu_paternal_p1_add")
          rv_check_add$paternal_p1 <- FALSE
        }else if(paternal_p1 < 0 || paternal_p1 > 1){
          showFeedbackDanger(inputId = "input_myu_paternal_p1_add", text = "A positive number between 0 to 1 is allowed.")
          rv_check_add$paternal_p1 <- FALSE
        }else{
          hideFeedback("input_myu_paternal_p1_add")
          rv_check_add$paternal_p1 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_paternal_p2_add, {
        paternal_m2 <- input$input_myu_paternal_m2_add
        paternal_m1 <- input$input_myu_paternal_m1_add
        paternal_p1 <- input$input_myu_paternal_p1_add
        paternal_p2 <- input$input_myu_paternal_p2_add
        if(!is.numeric(paternal_p2)){
          hideFeedback("input_myu_paternal_p2_add")
          rv_check_add$paternal_p2 <- FALSE
        }else if(paternal_p2 < 0 || paternal_p2 > 1){
          showFeedbackDanger(inputId = "input_myu_paternal_p2_add", text = "A positive number between 0 to 1 is allowed.")
          rv_check_add$paternal_p2 <- FALSE
        }else{
          hideFeedback("input_myu_paternal_p2_add")
          rv_check_add$paternal_p2 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_maternal_m2_add, {
        maternal_m2 <- input$input_myu_maternal_m2_add
        maternal_m1 <- input$input_myu_maternal_m1_add
        maternal_p1 <- input$input_myu_maternal_p1_add
        maternal_p2 <- input$input_myu_maternal_p2_add
        if(!is.numeric(maternal_m2)){
          hideFeedback("input_myu_maternal_m2_add")
          rv_check_add$maternal_m2 <- FALSE
        }else if(maternal_m2 < 0 || maternal_m2 > 1){
          showFeedbackDanger(inputId = "input_myu_maternal_m2_add", text = "A positive number between 0 to 1 is allowed.")
          rv_check_add$maternal_m2 <- FALSE
        }else{
          hideFeedback("input_myu_maternal_m2_add")
          rv_check_add$maternal_m2 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_maternal_m1_add, {
        maternal_m2 <- input$input_myu_maternal_m2_add
        maternal_m1 <- input$input_myu_maternal_m1_add
        maternal_p1 <- input$input_myu_maternal_p1_add
        maternal_p2 <- input$input_myu_maternal_p2_add
        if(!is.numeric(maternal_m1)){
          hideFeedback("input_myu_maternal_m1_add")
          rv_check_add$maternal_m1 <- FALSE
        }else if(maternal_m1 < 0 || maternal_m1 > 1){
          showFeedbackDanger(inputId = "input_myu_maternal_m1_add", text = "A positive number between 0 to 1 is allowed.")
          rv_check_add$maternal_m1 <- FALSE
        }else{
          hideFeedback("input_myu_maternal_m1_add")
          rv_check_add$maternal_m1 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_maternal_p1_add, {
        maternal_m2 <- input$input_myu_maternal_m2_add
        maternal_m1 <- input$input_myu_maternal_m1_add
        maternal_p1 <- input$input_myu_maternal_p1_add
        maternal_p2 <- input$input_myu_maternal_p2_add
        if(!is.numeric(maternal_p1)){
          hideFeedback("input_myu_maternal_p1_add")
          rv_check_add$maternal_p1 <- FALSE
        }else if(maternal_p1 < 0 || maternal_p1 > 1){
          showFeedbackDanger(inputId = "input_myu_maternal_p1_add", text = "A positive number between 0 to 1 is allowed.")
          rv_check_add$maternal_p1 <- FALSE
        }else{
          hideFeedback("input_myu_maternal_p1_add")
          rv_check_add$maternal_p1 <- TRUE
        }
      }, ignoreInit = TRUE)

      observeEvent(input$input_myu_maternal_p2_add, {
        maternal_m2 <- input$input_myu_maternal_m2_add
        maternal_m1 <- input$input_myu_maternal_m1_add
        maternal_p1 <- input$input_myu_maternal_p1_add
        maternal_p2 <- input$input_myu_maternal_p2_add
        if(!is.numeric(maternal_p2)){
          hideFeedback("input_myu_maternal_p2_add")
          rv_check_add$maternal_p2 <- FALSE
        }else if(maternal_p2 < 0 || maternal_p2 > 1){
          showFeedbackDanger(inputId = "input_myu_maternal_p2_add", text = "A positive number between 0 to 1 is allowed.")
          rv_check_add$maternal_p2 <- FALSE
        }else{
          hideFeedback("input_myu_maternal_p2_add")
          rv_check_add$maternal_p2 <- TRUE
        }
      }, ignoreInit = TRUE)

      observe({
        req(rv_check_add)
        bool_act_myu_add_save <- TRUE

        if(!rv_check_add$mk){
          bool_act_myu_add_save <- FALSE
        }

        if(all(rv_check_add$paternal_m2, rv_check_add$paternal_m1, rv_check_add$paternal_p1, rv_check_add$paternal_p2)){
          paternal_m2 <- input$input_myu_paternal_m2_add
          paternal_m1 <- input$input_myu_paternal_m1_add
          paternal_p1 <- input$input_myu_paternal_p1_add
          paternal_p2 <- input$input_myu_paternal_p2_add

          if(paternal_m2 + paternal_m1 + paternal_p1 + paternal_p2 > 1){
            showFeedbackDanger(inputId = "input_myu_paternal_m2_add", text = "Sum of all paternal mutation rates should not be greater than 1.")
            showFeedbackDanger(inputId = "input_myu_paternal_m1_add", text = "Sum of all paternal mutation rates should not be greater than 1.")
            showFeedbackDanger(inputId = "input_myu_paternal_p1_add", text = "Sum of all paternal mutation rates should not be greater than 1.")
            showFeedbackDanger(inputId = "input_myu_paternal_p2_add", text = "Sum of all paternal mutation rates should not be greater than 1.")
            bool_act_myu_add_save <- FALSE
          }else{
            hideFeedback("input_myu_paternal_m2_add")
            hideFeedback("input_myu_paternal_m1_add")
            hideFeedback("input_myu_paternal_p1_add")
            hideFeedback("input_myu_paternal_p2_add")
          }
        }else{
          bool_act_myu_add_save <- FALSE
        }

        if(all(rv_check_add$maternal_m2, rv_check_add$maternal_m1, rv_check_add$maternal_p1, rv_check_add$maternal_p2)){
          maternal_m2 <- input$input_myu_maternal_m2_add
          maternal_m1 <- input$input_myu_maternal_m1_add
          maternal_p1 <- input$input_myu_maternal_p1_add
          maternal_p2 <- input$input_myu_maternal_p2_add

          if(maternal_m2 + maternal_m1 + maternal_p1 + maternal_p2 > 1){
            showFeedbackDanger(inputId = "input_myu_maternal_m2_add", text = "Sum of all maternal mutation rates should not be greater than 1.")
            showFeedbackDanger(inputId = "input_myu_maternal_m1_add", text = "Sum of all maternal mutation rates should not be greater than 1.")
            showFeedbackDanger(inputId = "input_myu_maternal_p1_add", text = "Sum of all maternal mutation rates should not be greater than 1.")
            showFeedbackDanger(inputId = "input_myu_maternal_p2_add", text = "Sum of all maternal mutation rates should not be greater than 1.")
            bool_act_myu_add_save <- FALSE
          }else{
            hideFeedback("input_myu_maternal_m2_add")
            hideFeedback("input_myu_maternal_m1_add")
            hideFeedback("input_myu_maternal_p1_add")
            hideFeedback("input_myu_maternal_p2_add")
          }
        }else{
          bool_act_myu_add_save <- FALSE
        }

        if(bool_act_myu_add_save){
          enable("act_myu_add_save")
        }else{
          disable("act_myu_add_save")
        }
      })

      observeEvent(input$act_myu_add_save, {
        myu_mk_add <- input$input_myu_mk_add
        paternal_m2 <- input$input_myu_paternal_m2_add
        paternal_m1 <- input$input_myu_paternal_m1_add
        paternal_p1 <- input$input_myu_paternal_p1_add
        paternal_p2 <- input$input_myu_paternal_p2_add
        paternal_0 <- round(1 - paternal_m2 - paternal_m1 - paternal_p1 - paternal_p2, 10)
        maternal_m2 <- input$input_myu_maternal_m2_add
        maternal_m1 <- input$input_myu_maternal_m1_add
        maternal_p1 <- input$input_myu_maternal_p1_add
        maternal_p2 <- input$input_myu_maternal_p2_add
        maternal_0 <- round(1 - maternal_m2 - maternal_m1 - maternal_p1 - maternal_p2, 10)

        rv_myu$mk <- c(rv_myu$mk, myu_mk_add)
        rv_myu$paternal_m2 <- c(rv_myu$paternal_m2, paternal_m2)
        rv_myu$paternal_m1 <- c(rv_myu$paternal_m1, paternal_m1)
        rv_myu$paternal_0 <- c(rv_myu$paternal_0, paternal_0)
        rv_myu$paternal_p1 <- c(rv_myu$paternal_p1, paternal_p1)
        rv_myu$paternal_p2 <- c(rv_myu$paternal_p2, paternal_p2)
        rv_myu$maternal_m2 <- c(rv_myu$maternal_m2, maternal_m2)
        rv_myu$maternal_m1 <- c(rv_myu$maternal_m1, maternal_m1)
        rv_myu$maternal_0 <- c(rv_myu$maternal_0, maternal_0)
        rv_myu$maternal_p1 <- c(rv_myu$maternal_p1, maternal_p1)
        rv_myu$maternal_p2 <- c(rv_myu$maternal_p2, maternal_p2)

        new_dt_myu <- data.table(Marker = rv_myu$mk,
                                 Paternal_m2 = rv_myu$paternal_m2,
                                 Paternal_m1 = rv_myu$paternal_m1,
                                 Paternal_0 = rv_myu$paternal_0,
                                 Paternal_p1 = rv_myu$paternal_p1,
                                 Paternal_p2 = rv_myu$paternal_p2,
                                 Maternal_m2 = rv_myu$maternal_m2,
                                 Maternal_m1 = rv_myu$maternal_m1,
                                 Maternal_0 = rv_myu$maternal_0,
                                 Maternal_p1 = rv_myu$maternal_p1,
                                 Maternal_p2 = rv_myu$maternal_p2)
        write.csv(new_dt_myu, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)

        output$dt_myu <- renderDataTable({
          datatable(
            new_dt_myu,
            colnames = c("Locus", "Paternal -2 step", "Paternal -1 step", "Paternal 0 step", "Paternal +1 step", "Paternal +2 step", "Maternal -2 step", "Maternal -1 step", "Maternal 0 step", "Maternal +1 step", "Maternal +2 step"),
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
          selectInput(session$ns("input_myu_mk_del"), label = "Select a locus", choices = rv_myu$mk, selected = rv_myu$mk[1], selectize = FALSE),
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
        rv_myu$paternal_m2 <- rv_myu$paternal_m2[- pos_del]
        rv_myu$paternal_m1 <- rv_myu$paternal_m1[- pos_del]
        rv_myu$paternal_0 <- rv_myu$paternal_0[- pos_del]
        rv_myu$paternal_p1 <- rv_myu$paternal_p1[- pos_del]
        rv_myu$paternal_p2 <- rv_myu$paternal_p2[- pos_del]
        rv_myu$maternal_m2 <- rv_myu$maternal_m2[- pos_del]
        rv_myu$maternal_m1 <- rv_myu$maternal_m1[- pos_del]
        rv_myu$maternal_0 <- rv_myu$maternal_0[- pos_del]
        rv_myu$maternal_p1 <- rv_myu$maternal_p1[- pos_del]
        rv_myu$maternal_p2 <- rv_myu$maternal_p2[- pos_del]

        new_dt_myu <- data.table(Marker = rv_myu$mk,
                                 Paternal_m2 = rv_myu$paternal_m2,
                                 Paternal_m1 = rv_myu$paternal_m1,
                                 Paternal_0 = rv_myu$paternal_0,
                                 Paternal_p1 = rv_myu$paternal_p1,
                                 Paternal_p2 = rv_myu$paternal_p2,
                                 Maternal_m2 = rv_myu$maternal_m2,
                                 Maternal_m1 = rv_myu$maternal_m1,
                                 Maternal_0 = rv_myu$maternal_0,
                                 Maternal_p1 = rv_myu$maternal_p1,
                                 Maternal_p2 = rv_myu$maternal_p2)
        write.csv(new_dt_myu, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)

        output$dt_myu <- renderDataTable({
          datatable(
            new_dt_myu,
            colnames = c("Locus", "Paternal -2 step", "Paternal -1 step", "Paternal 0 step", "Paternal +1 step", "Paternal +2 step", "Maternal -2 step", "Maternal -1 step", "Maternal 0 step", "Maternal +1 step", "Maternal +2 step"),
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
        rv_myu$paternal_m2 <- new_dt_myu[, Paternal_m2]
        rv_myu$paternal_m1 <- new_dt_myu[, Paternal_m1]
        rv_myu$paternal_0 <- new_dt_myu[, Paternal_0]
        rv_myu$paternal_p1 <- new_dt_myu[, Paternal_p1]
        rv_myu$paternal_p2 <- new_dt_myu[, Paternal_p2]
        rv_myu$maternal_m2 <- new_dt_myu[, Maternal_m2]
        rv_myu$maternal_m1 <- new_dt_myu[, Maternal_m1]
        rv_myu$maternal_0 <- new_dt_myu[, Maternal_0]
        rv_myu$maternal_p1 <- new_dt_myu[, Maternal_p1]
        rv_myu$maternal_p2 <- new_dt_myu[, Maternal_p2]

        output$dt_myu <- renderDataTable({
          datatable(
            new_dt_myu,
            colnames = c("Locus", "Paternal -2 step", "Paternal -1 step", "Paternal 0 step", "Paternal +1 step", "Paternal +2 step", "Maternal -2 step", "Maternal -1 step", "Maternal 0 step", "Maternal +1 step", "Maternal +2 step"),
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
          colnames = c("Locus", "Paternal -2 step", "Paternal -1 step", "Paternal 0 step", "Paternal +1 step", "Paternal +2 step", "Maternal -2 step", "Maternal -1 step", "Maternal 0 step", "Maternal +1 step", "Maternal +2 step"),
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE),
          rownames = FALSE
        )
      })

      return(rv_myu)
    }
  )
}
