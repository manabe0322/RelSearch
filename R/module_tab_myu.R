###########################################################
# The function to create the ui module for mutation rates #
###########################################################

tab_myu_ui <- function(id){
  ns <- NS(id)

  tabPanel("Mutation rates",

           titlePanel("Mutation rates"),

           br(),

           sidebarLayout(

             sidebarPanel(
               h4("Edit"),
               uiOutput(ns("myu_mk_edit")),
               uiOutput(ns("myu_val_edit")),
               uiOutput(ns("act_myu_edit")),
               br(),
               h4("Add"),
               uiOutput(ns("myu_mk_add")),
               uiOutput(ns("myu_val_add")),
               uiOutput(ns("act_myu_add")),
               br(),
               h4("Delete"),
               uiOutput(ns("myu_mk_del")),
               uiOutput(ns("act_myu_del")),
               br(),
               h4("Reset"),
               uiOutput(ns("act_myu_reset")),
               br(),
               h4("Update default"),
               uiOutput(ns("act_myu_update"))
             ),

             mainPanel(
               dataTableOutput(ns("dt_myu"))
             )
           )
  )
}


###############################################################
# The function to create the server module for mutation rates #
###############################################################

tab_myu_server <- function(input, output, session, init_dt_myu, path_pack){

  # Define reactive values for setting mutation rates
  rv_myu <- reactiveValues()
  rv_myu$mk <- init_dt_myu[, Marker]
  rv_myu$val <- init_dt_myu[, Myu]

  ########################
  # Edit a mutation rate #
  ########################

  # Select a locus
  output$myu_mk_edit <- renderUI(selectInput(session$ns("myu_mk_edit"), label = "Select a locus", choices = rv_myu$mk, selected = rv_myu$mk[1]))


  # Enter a mutation rate
  observeEvent(input$myu_mk_edit, {
    output$myu_val_edit <- renderUI(numericInput(session$ns("myu_val_edit"), label = "Enter a mutation rate", value = rv_myu$val[rv_myu$mk == input$myu_mk_edit]))
  })

  # Button to edit
  output$act_myu_edit <- renderUI(actionButton(session$ns("act_myu_edit"), label = "Edit"))

  # Perform
  observeEvent(input$act_myu_edit, {

    myu_mk_edit <- input$myu_mk_edit
    myu_val_edit <- input$myu_val_edit

    if(isTruthy(myu_val_edit)){
      rv_myu$val[rv_myu$mk == myu_mk_edit] <- myu_val_edit

      output$dt_myu <- renderDataTable({
        datatable(
          data.table(Marker = rv_myu$mk, Myu = rv_myu$val),
          colnames = c("Locus", "Mutation rates"),
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE),
          rownames = FALSE
        )
      })
    }else{
      showModal(modalDialog(
        title = "Error",
        "Enter the mutation rate!",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })

  #######################
  # Add a mutation rate #
  #######################

  # Enter a locus name
  output$myu_mk_add <- renderUI(textInput(session$ns("myu_mk_add"), label = "Enter a locus name", value = NULL))

  # Enter a mutation rate
  output$myu_val_add <- renderUI(numericInput(session$ns("myu_val_add"), label = "Mutation rate", value = NULL))

  # Button to add
  output$act_myu_add <- renderUI(actionButton(session$ns("act_myu_add"), label = "Add"))

  # Perform
  observeEvent(input$act_myu_add, {
    myu_mk_add <- input$myu_mk_add
    myu_val_add <- input$myu_val_add

    if(isTruthy(myu_mk_add) && isTruthy(myu_val_add)){
      rv_myu$mk <- c(rv_myu$mk, myu_mk_add)
      rv_myu$val <- c(rv_myu$val, myu_val_add)

      output$dt_myu <- renderDataTable({
        datatable(
          data.table(Marker = rv_myu$mk, Myu = rv_myu$val),
          colnames = c("Locus", "Mutation rates"),
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE),
          rownames = FALSE
        )
      })
    }else{
      showModal(modalDialog(
        title = "Error",
        "Enter the locus name and the mutation rate!",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })

  ##########################
  # Delete a mutation rate #
  ##########################

  # Select a locus
  output$myu_mk_del <- renderUI(selectInput(session$ns("myu_mk_del"), label = "Select a locus", choices = rv_myu$mk, selected = rv_myu$mk[1]))

  # Button to delete
  output$act_myu_del <- renderUI(actionButton(session$ns("act_myu_del"), label = "Delete"))

  # Perform
  observeEvent(input$act_myu_del, {
    pos_del <- which(rv_myu$mk == input$myu_mk_del)
    rv_myu$mk <- rv_myu$mk[- pos_del]
    rv_myu$val <- rv_myu$val[- pos_del]

    output$dt_myu <- renderDataTable({
      datatable(
        data.table(Marker = rv_myu$mk, Myu = rv_myu$val),
        colnames = c("Locus", "Mutation rates"),
        selection = "none",
        options = list(iDisplayLength = 50, ordering = FALSE),
        rownames = FALSE
      )
    })
  })

  ############################
  # Reset the mutation rates #
  ############################

  # Create a button to reset mutation rates
  output$act_myu_reset <- renderUI(actionButton(session$ns("act_myu_reset"), label = "Reset"))

  # Define the action to reset the mutation rates
  observeEvent(input$act_myu_reset, {
    init_dt_myu <- create_dt_myu(path_pack)
    rv_myu$mk <- init_dt_myu[, Marker]
    rv_myu$val <- init_dt_myu[, Myu]

    output$dt_myu <- renderDataTable({
      datatable(
        data.table(Marker = rv_myu$mk, Myu = rv_myu$val),
        colnames = c("Locus", "Mutation rates"),
        selection = "none",
        options = list(iDisplayLength = 50, ordering = FALSE),
        rownames = FALSE
      )
    })
  })

  #####################################
  # Update the default mutation rates #
  #####################################

  # Create a button to update the default mutation rates
  output$act_myu_update <- renderUI(actionButton(session$ns("act_myu_update"), label = "Update default"))

  # Define the action to update the default mutation rates
  observeEvent(input$act_myu_update, {
    write.csv(data.table(Marker = rv_myu$mk, Myu = rv_myu$val), paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)

    showModal(modalDialog(title = "Information", "Default mutation rates have been updated.", easyClose = TRUE, footer = NULL))
  })

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

  ##################################
  # Return reactive mutation rates #
  ##################################

  return(rv_myu)
}
