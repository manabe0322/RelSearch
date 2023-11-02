#' tab_rel_ui
#'
#' @description The function to create the ui module for information on the relationship
tab_rel_ui <- function(id){
  ns <- NS(id)

  tabPanel("Relationships",

           titlePanel("Relationships"),

           br(),

           sidebarLayout(

             sidebarPanel(width = 2,
                          actionButton(ns("act_rel_edit"), "Edit"),
                          br(),
                          br(),
                          actionButton(ns("act_rel_add"), "Add"),
                          br(),
                          br(),
                          actionButton(ns("act_rel_delete"), "Delete"),
                          br(),
                          br(),
                          actionButton(ns("act_rel_reset"), "Reset"),
             ),

             mainPanel(width = 10,
                       dataTableOutput(ns("dt_rel"))
             )
           )
  )
}

#' tab_rel_server
#'
#' @description The function to create the server module for information on the relationship
#' @param init_dt_rel The initial data.table of information on relationship
#' @param path_pack Package path
tab_rel_server <- function(input, output, session, init_dt_rel, path_pack){

  ######################################
  # Define the initial reactive values #
  ######################################

  rv_rel <- reactiveValues()
  rv_rel$name <- init_dt_rel[, Relationship]
  rv_rel$victim <- init_dt_rel[, Victim]
  rv_rel$reference <- init_dt_rel[, Reference]
  rv_rel$pibd2 <- init_dt_rel[, Pr_IBD2]
  rv_rel$pibd1 <- init_dt_rel[, Pr_IBD1]
  rv_rel$pibd0 <- init_dt_rel[, Pr_IBD0]
  rv_rel$paternal <- init_dt_rel[, Paternal]
  rv_rel$maternal <- init_dt_rel[, Maternal]

  ###################################
  # Edit the name of a relationship #
  ###################################

  observeEvent(input$act_rel_edit, {
    showModal(modalDialog(
      title = "Edit the name of the relationship",
      selectInput(session$ns("rel_old_edit"), label = "Select a relationship", choices = rv_rel$name, selected = rv_rel$name[1]),
      textInput(session$ns("rel_new_edit"), label = "Enter a new name", value = NULL),
      footer = tagList(
        actionButton(session$ns("act_rel_edit_save"), "Save"),
        modalButton("Cancel")
      ),
      size = "l"
    ))
  })

  observeEvent(input$act_rel_edit_save, {
    rel_old_edit <- input$rel_old_edit
    rel_new_edit <- input$rel_new_edit

    if(isTruthy(rel_new_edit)){
      rv_rel$name[rv_rel$name == rel_old_edit] <- rel_new_edit

      new_dt_rel <- data.table(Relationship = rv_rel$name, Victim = rv_rel$victim, Reference = rv_rel$reference,
                               Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0,
                               Paternal = rv_rel$paternal, Maternal = rv_rel$maternal)

      write.csv(new_dt_rel, paste0(path_pack, "/extdata/parameters/rel.csv"), row.names = FALSE)

      output$dt_rel <- renderDataTable({
        datatable(
          new_dt_rel,
          colnames = c("Relationship", "Victim", "Reference", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)", "Paternal lineage", "Maternal lineage"),
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE),
          rownames = FALSE
        )
      })

    }else{
      showModal(modalDialog(
        title = "Error",
        "Enter the relationship name!",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })

  #####################################
  # Add information on a relationship #
  #####################################

  rv_famtree <- reactiveValues()
  rv_famtree$num_uk <- 0
  rv_famtree$uk <- character(0)
  rv_famtree$sex <- character(0)
  rv_famtree$father <- character(0)
  rv_famtree$mother <- character(0)
  rv_famtree$founder <- character(0)
  rv_famtree$error_famtree <- TRUE
  rv_famtree$tree <- NULL

  make_unk_selectbox <- function(num_uk, uks, sexes, fathers, mothers, founders){

    output$uks <- renderUI({
      lapply(seq_len(num_uk), function(x){
        disabled(selectInput(session$ns(paste0("uk_", x)), label = NULL, choices = uks[x], selected = uks[x])
        )
      })
    })

    output$sexes <- renderUI({
      lapply(seq_len(num_uk), function(x){
        selectInput(session$ns(paste0("sex_", x)), label = NULL, choices = c("M", "F"), selected = sexes[x])
      })
    })

    output$fathers <- renderUI({
      output_list <- list()
      for(x in seq_len(num_uk)){
        if(founders[x] == "Yes"){
          output_list[[x]] <- disabled(selectInput(session$ns(paste0("father_", x)), label = NULL, choices = c("Victim", "Ref", uks), selected = fathers[x]))
        }else{
          output_list[[x]] <- selectInput(session$ns(paste0("father_", x)), label = NULL, choices = c("Victim", "Ref", uks), selected = fathers[x])
        }
      }
      output_list
    })

    output$mothers <- renderUI({
      output_list <- list()
      for(x in seq_len(num_uk)){
        if(founders[x] == "Yes"){
          output_list[[x]] <- disabled(selectInput(session$ns(paste0("mother_", x)), label = NULL, choices = c("Victim", "Ref", uks), selected = mothers[x]))
        }else{
          output_list[[x]] <- selectInput(session$ns(paste0("mother_", x)), label = NULL, choices = c("Victim", "Ref", uks), selected = mothers[x])
        }
      }
      output_list
    })

    output$founders <- renderUI({
      lapply(seq_len(num_uk), function(x){
        selectInput(session$ns(paste0("founder_", x)), label = NULL, choices = c("Yes", "No"), selected = founders[x])
      })
    })
  }

  observeEvent(input$act_rel_add, {
    showModal(modalDialog(
      title = "Add information on a relationship",

      fluidRow(
        column(5,
               wellPanel(
                 textInput(session$ns("rel_add"), label = "Relationship", value = NULL),
                 textInput(session$ns("vic_add"), label = "Victim", value = NULL),
                 textInput(session$ns("ref_add"), label = "Reference", value = NULL)
               )
        ),
        column(7,
               wellPanel(
                 h5(div("Family tree", style = "color:#555555;font-weight:bold;")),
                 plotOutput(session$ns("famtree"))
               )
        )
      ),

      fluidRow(
        column(12,
               h5(div("Set a family tree", style = "color:#555555;font-weight:bold;")),
               column(4, actionButton(session$ns("act_famtree_add"), label = "Add a person")),
               column(4, actionButton(session$ns("act_famtree_del"), label = "Delete a person")),
               column(4, actionButton(session$ns("act_famtree_view"), label = "View family tree"))
        ),

        column(12,
               column(2, br(), h5("Person")),
               column(2, br(), h5("Sex")),
               column(3, br(), h5("Father")),
               column(3, br(), h5("Mother")),
               column(2, br(), h5("Founder"))
        ),

        column(12,
               column(2, disabled(selectInput(session$ns("v_person"), label = NULL, choices = "Victim", selected = "Victim"))),
               column(2, selectInput(session$ns("v_sex"), label = NULL, choices = c("M", "F"), selected = "M")),
               column(3, selectInput(session$ns("v_father"), label = NULL, choices = c("Victim", "Ref"), selected = "Victim")),
               column(3, selectInput(session$ns("v_mother"), label = NULL, choices = c("Victim", "Ref"), selected = "Victim")),
               column(2, selectInput(session$ns("v_founder"), label = NULL, choices = c("Yes", "No"), selected = "No"))
        ),

        column(12,
               column(2, disabled(selectInput(session$ns("r_person"), label = NULL, choices = "Ref", selected = "Ref"))),
               column(2, selectInput(session$ns("r_sex"), label = NULL, choices = c("M", "F"), selected = "M")),
               column(3, selectInput(session$ns("r_father"), label = NULL, choices = c("Victim", "Ref"), selected = "Victim")),
               column(3, selectInput(session$ns("r_mother"), label = NULL, choices = c("Victim", "Ref"), selected = "Victim")),
               column(2, selectInput(session$ns("r_founder"), label = NULL, choices = c("Yes", "No"), selected = "No"))
        ),

        column(12,
               column(2, uiOutput(session$ns("uks"))),
               column(2, uiOutput(session$ns("sexes"))),
               column(3, uiOutput(session$ns("fathers"))),
               column(3, uiOutput(session$ns("mothers"))),
               column(2, uiOutput(session$ns("founders")))
        )
      ),

      footer = tagList(
        actionButton(session$ns("act_rel_add_save"), "Save"),
        actionButton(session$ns("act_rel_add_cancel"), "Cancel")
      ),
      size = "l"
    ))
  })

  observeEvent(input$v_founder, {
    v_founder <- input$v_founder
    if(v_founder == "Yes"){
      disable("v_father")
      disable("v_mother")
    }else{
      enable("v_father")
      enable("v_mother")
    }
  })

  observeEvent(input$r_founder, {
    r_founder <- input$r_founder
    if(r_founder == "Yes"){
      disable("r_father")
      disable("r_mother")
    }else{
      enable("r_father")
      enable("r_mother")
    }
  })

  observeEvent(input$act_famtree_add, {
    num_uk <- rv_famtree$num_uk + 1
    uk_add <- c(rv_famtree$uk, paste0("UK", num_uk))
    sex_add <- c(rv_famtree$sex, "M")
    father_add <- c(rv_famtree$father, "Victim")
    mother_add <- c(rv_famtree$mother, "Victim")
    founder_add <- c(rv_famtree$founder, "No")

    updateSelectInput(session, "v_father", label = NULL, choices = c("Victim", "Ref", uk_add), selected = input$v_father)
    updateSelectInput(session, "v_mother", label = NULL, choices = c("Victim", "Ref", uk_add), selected = input$v_mother)
    updateSelectInput(session, "r_father", label = NULL, choices = c("Victim", "Ref", uk_add), selected = input$r_father)
    updateSelectInput(session, "r_mother", label = NULL, choices = c("Victim", "Ref", uk_add), selected = input$r_mother)

    make_unk_selectbox(num_uk, uk_add, sex_add, father_add, mother_add, founder_add)

    rv_famtree$uk <- uk_add
    rv_famtree$sex <- sex_add
    rv_famtree$father <- father_add
    rv_famtree$mother <- mother_add
    rv_famtree$founder <- founder_add
    rv_famtree$num_uk <- num_uk

    change_sex <- reactive({
      num_uk <- isolate(rv_famtree$num_uk)
      lapply(seq_len(num_uk), function(x){
        input[[paste0("sex_", x)]]
      })
    })

    change_father <- reactive({
      num_uk <- isolate(rv_famtree$num_uk)
      lapply(seq_len(num_uk), function(x){
        input[[paste0("father_", x)]]
      })
    })

    change_mother <- reactive({
      num_uk <- isolate(rv_famtree$num_uk)
      lapply(seq_len(num_uk), function(x){
        input[[paste0("mother_", x)]]
      })
    })

    change_founder <- reactive({
      num_uk <- isolate(rv_famtree$num_uk)
      lapply(seq_len(num_uk), function(x){
        input[[paste0("founder_", x)]]
      })
    })

    observeEvent(ignoreInit = TRUE, change_sex(), {
      input_sexes <- change_sex()
      rv_famtree$sex <- unlist(input_sexes)
    })

    observeEvent(ignoreInit = TRUE, change_father(), {
      input_fathers <- change_father()
      father_new <- unlist(input_fathers)
      num_uk <- rv_famtree$num_uk
      if(length(father_new) == num_uk){
        for(x in seq_len(num_uk)){
          if(rv_famtree$father[x] != father_new[x]){
            rv_famtree$father[x] <- father_new[x]
          }
        }
      }
    })

    observeEvent(ignoreInit = TRUE, change_mother(), {
      input_mothers <- change_mother()
      mother_new <- unlist(input_mothers)
      num_uk <- rv_famtree$num_uk
      if(length(mother_new) == num_uk){
        for(x in seq_len(num_uk)){
          if(rv_famtree$mother[x] != mother_new[x]){
            rv_famtree$mother[x] <- mother_new[x]
          }
        }
      }
    })

    observeEvent(ignoreInit = TRUE, change_founder(), {
      input_founders <- change_founder()
      founder_new <- unlist(input_founders)
      num_uk <- rv_famtree$num_uk
      if(length(founder_new) == num_uk){
        for(x in seq_len(num_uk)){
          if(rv_famtree$founder[x] != founder_new[x]){
            rv_famtree$founder[x] <- founder_new[x]
            if(rv_famtree$founder[x] == "Yes"){
              disable(paste0("father_", x))
              disable(paste0("mother_", x))
            }else{
              enable(paste0("father_", x))
              enable(paste0("mother_", x))
            }
          }
        }
      }
    })
  })

  observeEvent(input$act_famtree_del, {

    num_uk <- rv_famtree$num_uk

    if(num_uk > 0){

      uk_del <- rv_famtree$uk[- num_uk]
      sex_del <- rv_famtree$sex[- num_uk]
      father_del <- rv_famtree$father[- num_uk]
      mother_del <- rv_famtree$mother[- num_uk]
      founder_del <- rv_famtree$founder[- num_uk]
      num_uk <- rv_famtree$num_uk - 1

      updateSelectInput(session, "v_father", label = NULL, choices = c("Victim", "Ref", uk_del), selected = input$v_father)
      updateSelectInput(session, "v_mother", label = NULL, choices = c("Victim", "Ref", uk_del), selected = input$v_mother)
      updateSelectInput(session, "r_father", label = NULL, choices = c("Victim", "Ref", uk_del), selected = input$r_father)
      updateSelectInput(session, "r_mother", label = NULL, choices = c("Victim", "Ref", uk_del), selected = input$r_mother)

      make_unk_selectbox(num_uk, uk_del, sex_del, father_del, mother_del, founder_del)

      rv_famtree$uk <- uk_del
      rv_famtree$sex <- sex_del
      rv_famtree$father <- father_del
      rv_famtree$mother <- mother_del
      rv_famtree$founder <- founder_del
      rv_famtree$num_uk <- num_uk

      change_sex <- reactive({
        num_uk <- isolate(rv_famtree$num_uk)
        lapply(seq_len(num_uk), function(x){
          input[[paste0("sex_", x)]]
        })
      })

      change_father <- reactive({
        num_uk <- isolate(rv_famtree$num_uk)
        lapply(seq_len(num_uk), function(x){
          input[[paste0("father_", x)]]
        })
      })

      change_mother <- reactive({
        num_uk <- isolate(rv_famtree$num_uk)
        lapply(seq_len(num_uk), function(x){
          input[[paste0("mother_", x)]]
        })
      })

      change_founder <- reactive({
        num_uk <- isolate(rv_famtree$num_uk)
        lapply(seq_len(num_uk), function(x){
          input[[paste0("founder_", x)]]
        })
      })

      observeEvent(ignoreInit = TRUE, change_sex(), {
        input_sexes <- change_sex()
        rv_famtree$sex <- unlist(input_sexes)
      })

      observeEvent(ignoreInit = TRUE, change_father(), {
        input_fathers <- change_father()
        father_new <- unlist(input_fathers)
        num_uk <- rv_famtree$num_uk
        if(length(father_new) == num_uk){
          for(x in seq_len(num_uk)){
            if(rv_famtree$father[x] != father_new[x]){
              rv_famtree$father[x] <- father_new[x]
            }
          }
        }
      })

      observeEvent(ignoreInit = TRUE, change_mother(), {
        input_mothers <- change_mother()
        mother_new <- unlist(input_mothers)
        num_uk <- rv_famtree$num_uk
        if(length(mother_new) == num_uk){
          for(x in seq_len(num_uk)){
            if(rv_famtree$mother[x] != mother_new[x]){
              rv_famtree$mother[x] <- mother_new[x]
            }
          }
        }
      })

      observeEvent(ignoreInit = TRUE, change_founder(), {
        input_founders <- change_founder()
        founder_new <- unlist(input_founders)
        num_uk <- rv_famtree$num_uk
        if(length(founder_new) == num_uk){
          for(x in seq_len(num_uk)){
            if(rv_famtree$founder[x] != founder_new[x]){
              rv_famtree$founder[x] <- founder_new[x]
              if(rv_famtree$founder[x] == "Yes"){
                disable(paste0("father_", x))
                disable(paste0("mother_", x))
              }else{
                enable(paste0("father_", x))
                enable(paste0("mother_", x))
              }
            }
          }
        }
      })
    }
  })

  observeEvent(input$act_famtree_view, {
    persons <- c("Victim", "Ref", rv_famtree$uk)
    sex_all <- c(input$v_sex, input$r_sex, rv_famtree$sex)
    father_all <- c(input$v_father, input$r_father, rv_famtree$father)
    mother_all <- c(input$v_mother, input$r_mother, rv_famtree$mother)
    founder_all <- c(input$v_founder, input$r_founder, rv_famtree$founder)

    rv_famtree$tree <- tree <- check_tree(persons, sex_all, father_all, mother_all, founder_all)
    if(length(class(tree)) > 1){
      rv_famtree$error_famtree <- TRUE
    }else if(class(tree) == "try-error"){
      rv_famtree$error_famtree <- TRUE
    }else{
      rv_famtree$error_famtree <- FALSE
    }
  })

  output$famtree <- renderPlot({
    validate(
      need(!rv_famtree$error_famtree, "Incorrect setting of the family tree!")
    )
    plot(rv_famtree$tree, hatched = c("Victim", "Ref"))
  })

  observeEvent(input$act_rel_add_save, {



    rv_famtree$num_uk <- 0
    rv_famtree$uk <- character(0)
    rv_famtree$sex <- character(0)
    rv_famtree$father <- character(0)
    rv_famtree$mother <- character(0)
    rv_famtree$founder <- character(0)
    rv_famtree$error_famtree <- TRUE

    make_unk_selectbox(rv_famtree$num_uk, rv_famtree$uk, rv_famtree$sex, rv_famtree$father, rv_famtree$mother, rv_famtree$founder)

    removeModal()
  })

  observeEvent(input$act_rel_add_cancel, {
    rv_famtree$num_uk <- 0
    rv_famtree$uk <- character(0)
    rv_famtree$sex <- character(0)
    rv_famtree$father <- character(0)
    rv_famtree$mother <- character(0)
    rv_famtree$founder <- character(0)
    rv_famtree$error_famtree <- TRUE

    make_unk_selectbox(rv_famtree$num_uk, rv_famtree$uk, rv_famtree$sex, rv_famtree$father, rv_famtree$mother, rv_famtree$founder)

    removeModal()
  })

  ########################################
  # Delete information on a relationship #
  ########################################

  observeEvent(input$act_rel_delete, {
    showModal(modalDialog(
      title = "Delete information on a relationship",
      selectInput(session$ns("rel_del"), label = "Select a relationship", choices = rv_rel$name, selected = rv_rel$name[1]),
      footer = tagList(
        actionButton(session$ns("act_rel_delete_save"), "Save"),
        modalButton("Cancel")
      ),
      size = "l"
    ))
  })

  observeEvent(input$act_rel_delete_save, {
    pos_del <- which(rv_rel$name == input$rel_del)
    rv_rel$name <- rv_rel$name[- pos_del]
    rv_rel$victim <- rv_rel$victim[- pos_del]
    rv_rel$reference <- rv_rel$reference[- pos_del]
    rv_rel$pibd2 <- rv_rel$pibd2[- pos_del]
    rv_rel$pibd1 <- rv_rel$pibd1[- pos_del]
    rv_rel$pibd0 <- rv_rel$pibd0[- pos_del]
    rv_rel$paternal <- rv_rel$paternal[- pos_del]
    rv_rel$maternal <- rv_rel$maternal[- pos_del]

    new_dt_rel <- data.table(Relationship = rv_rel$name, Victim = rv_rel$victim, Reference = rv_rel$reference,
                             Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0,
                             Paternal = rv_rel$paternal, Maternal = rv_rel$maternal)

    write.csv(new_dt_rel, paste0(path_pack, "/extdata/parameters/rel.csv"), row.names = FALSE)

    output$dt_rel <- renderDataTable({
      datatable(
        new_dt_rel,
        colnames = c("Relationship", "Victim", "Reference", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)", "Paternal lineage", "Maternal lineage"),
        selection = "none",
        options = list(iDisplayLength = 50, ordering = FALSE),
        rownames = FALSE
      )
    })
  })

  #########################################
  # Reset information on the relationship #
  #########################################

  observeEvent(input$act_rel_reset, {
    showModal(modalDialog(
      title = "Reset information on the relationship",
      "Click Restore default to remove all changes.",
      footer = tagList(
        actionButton(session$ns("act_rel_reset_yes"), "Restore default"),
        modalButton("Cancel")
      ),
    ))
  })

  observeEvent(input$act_rel_reset_yes, {
    rv_rel$victim <- c("Father", "Father", "Mother", "Mother", "Son", "Son", "Daughter", "Daughter",
                       "Brother", "Brother", "Sister", "Sister",
                       "Paternal-uncle", "Paternal-uncle", "Maternal-uncle", "Maternal-uncle", "Paternal-aunt", "Paternal-aunt", "Maternal-aunt", "Maternal-aunt",
                       "nephew", "niece", "nephew", "niece", "nephew", "niece", "nephew", "niece",
                       "Paternal-grandfather", "Paternal-grandfather", "Maternal-grandfather", "Maternal-grandfather", "Paternal-grandmother", "Paternal-grandmother", "Maternal-grandmother", "Maternal-grandmother",
                       "grandson", "granddaughter", "grandson", "granddaughter", "grandson", "granddaughter", "grandson", "granddaughter")
    rv_rel$reference <- c("Son", "Daughter", "Son", "Daughter", "Father", "Mother", "Father", "Mother",
                          "Brother", "Sister", "Brother", "Sister",
                          "nephew", "niece", "nephew", "niece", "nephew", "niece", "nephew", "niece",
                          "Paternal-uncle", "Paternal-uncle", "Maternal-uncle", "Maternal-uncle", "Paternal-aunt", "Paternal-aunt", "Maternal-aunt", "Maternal-aunt",
                          "grandson", "granddaughter", "grandson", "granddaughter", "grandson", "granddaughter", "grandson", "granddaughter",
                          "Paternal-grandfather", "Paternal-grandfather", "Maternal-grandfather", "Maternal-grandfather", "Paternal-grandmother", "Paternal-grandmother", "Maternal-grandmother", "Maternal-grandmother")
    rv_rel$name <- mapply(paste, rv_rel$victim, rv_rel$reference, sep = "_")
    rv_rel$pibd2 <- c(0, 0, 0, 0, 0, 0, 0, 0,
                      0.25, 0.25, 0.25, 0.25,
                      0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0)
    rv_rel$pibd1 <- c(1, 1, 1, 1, 1, 1, 1, 1,
                      0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
    rv_rel$pibd0 <- c(0, 0, 0, 0, 0, 0, 0, 0,
                      0.25, 0.25, 0.25, 0.25,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
    rv_rel$paternal <- c("Yes", "No", "No", "No", "Yes", "No", "No", "No",
                         "Yes", "No", "No", "No",
                         "Yes", "No", "No", "No", "No", "No", "No", "No",
                         "Yes", "No", "No", "No", "No", "No", "No", "No",
                         "Yes", "No", "No", "No", "No", "No", "No", "No",
                         "Yes", "No", "No", "No", "No", "No", "No", "No")
    rv_rel$maternal <- c("No", "No", "Yes", "Yes", "No", "Yes", "No", "Yes",
                         "Yes", "Yes", "Yes", "Yes",
                         "No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes",
                         "No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes",
                         "No", "No", "No", "No", "No", "No", "Yes", "Yes",
                         "No", "No", "No", "No", "No", "No", "Yes", "Yes")

    new_dt_rel <- data.table(Relationship = rv_rel$name, Victim = rv_rel$victim, Reference = rv_rel$reference,
                             Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0,
                             Paternal = rv_rel$paternal, Maternal = rv_rel$maternal)

    write.csv(new_dt_rel, paste0(path_pack, "/extdata/parameters/rel.csv"), row.names = FALSE)

    output$dt_rel <- renderDataTable({
      datatable(
        new_dt_rel,
        colnames = c("Relationship", "Victim", "Reference", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)", "Paternal lineage", "Maternal lineage"),
        selection = "none",
        options = list(iDisplayLength = 50, ordering = FALSE),
        rownames = FALSE
      )
    })
  })

  #######################################################
  # Display the initial information on the relationship #
  #######################################################

  output$dt_rel <- renderDataTable({
    datatable(
      init_dt_rel,
      colnames = c("Relationship", "Victim", "Reference", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)", "Paternal lineage", "Maternal lineage"),
      selection = "none",
      options = list(iDisplayLength = 50, ordering = FALSE),
      rownames = FALSE
    )
  })

  return(rv_rel)
}
