#' tab_rel_ui
#'
#' @description The function to create the ui module for information on the relationship
tab_rel_ui <- function(id){
  ns <- NS(id)

  tabPanel("Relationships",

           titlePanel("Relationships"),

           dataTableOutput(ns("dt_rel")),

           br(),

           fluidRow(

             column(2,
                    wellPanel(
                      h4("Edit"),
                      uiOutput(ns("rel_old_edit")),
                      uiOutput(ns("rel_new_edit")),
                      uiOutput(ns("act_rel_edit")),
                      br(),
                      h4("Delete"),
                      uiOutput(ns("rel_del")),
                      uiOutput(ns("act_rel_del")),
                      br(),
                      h4("Reset"),
                      uiOutput(ns("act_rel_reset")),
                      br(),
                      h4("Update default"),
                      uiOutput(ns("act_rel_update"))
                    )
             ),

             column(7,
                    wellPanel(

                      fluidRow(
                        column(12,
                               h4("Define a new relationship"),
                               br(),
                               uiOutput(ns("rel_add")),
                               br(),
                               h5(div("Set a family tree", style = "color:#555555;font-weight:bold;")),
                               column(3, uiOutput(ns("act_famtree_add"))),
                               column(3, uiOutput(ns("act_famtree_del"))),
                               column(4, uiOutput(ns("act_famtree_view"))),
                               column(2, uiOutput(ns("act_famtree_set")))
                        ),

                        column(12,
                               column(2, br(), h5("Person")),
                               column(2, br(), h5("Sex")),
                               column(3, br(), h5("Father")),
                               column(3, br(), h5("Mother")),
                               column(2, br(), h5("Founder"))
                        ),

                        column(12,
                               column(2, disabled(selectInput(ns("v_person"), label = NULL, choices = "Victim", selected = "Victim"))),
                               column(2, selectInput(ns("v_sex"), label = NULL, choices = c("M", "F"), selected = "M")),
                               column(3, selectInput(ns("v_father"), label = NULL, choices = c("Victim", "Ref"), selected = "Victim")),
                               column(3, selectInput(ns("v_mother"), label = NULL, choices = c("Victim", "Ref"), selected = "Victim")),
                               column(2, selectInput(ns("v_founder"), label = NULL, choices = c("Yes", "No"), selected = "No"))
                        ),

                        column(12,
                               column(2, disabled(selectInput(ns("r_person"), label = NULL, choices = "Ref", selected = "Ref"))),
                               column(2, selectInput(ns("r_sex"), label = NULL, choices = c("M", "F"), selected = "M")),
                               column(3, selectInput(ns("r_father"), label = NULL, choices = c("Victim", "Ref"), selected = "Victim")),
                               column(3, selectInput(ns("r_mother"), label = NULL, choices = c("Victim", "Ref"), selected = "Victim")),
                               column(2, selectInput(ns("r_founder"), label = NULL, choices = c("Yes", "No"), selected = "No"))
                        ),

                        column(12,
                               column(2, uiOutput(ns("uks"))),
                               column(2, uiOutput(ns("sexes"))),
                               column(3, uiOutput(ns("fathers"))),
                               column(3, uiOutput(ns("mothers"))),
                               column(2, uiOutput(ns("founders")))
                        )
                      )
                    )
             ),

             column(3,
                    wellPanel(
                      h4("Family tree"),
                      plotOutput(ns("famtree"))
                    )
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
  rv_rel <- reactiveValues()
  rv_rel$name <- init_dt_rel[, Name_relationship]
  rv_rel$degree <- init_dt_rel[, Degree]
  rv_rel$pibd2 <- init_dt_rel[, Pr_IBD2]
  rv_rel$pibd1 <- init_dt_rel[, Pr_IBD1]
  rv_rel$pibd0 <- init_dt_rel[, Pr_IBD0]

  ########################################
  # Edit information on the relationship #
  ########################################

  output$rel_old_edit <- renderUI(selectInput(session$ns("rel_old_edit"), label = "Select a relationship", choices = rv_rel$name, selected = rv_rel$name[1]))

  output$rel_new_edit <- renderUI(textInput(session$ns("rel_new_edit"), label = "Enter a new name", value = NULL))

  output$act_rel_edit <- renderUI(actionButton(session$ns("act_rel_edit"), label = "Edit"))

  observeEvent(input$act_rel_edit, {

    rel_old_edit <- input$rel_old_edit
    rel_new_edit <- input$rel_new_edit

    if(isTruthy(rel_new_edit)){
      rv_rel$name[rv_rel$name == rel_old_edit] <- rel_new_edit

      output$dt_rel <- renderDataTable({
        datatable(
          data.table(Name_relationship = rv_rel$name, Degree = rv_rel$degree, Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0),
          colnames = c("Relationship name", "Degree", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)"),
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

  ##########################################
  # Delete information on the relationship #
  ##########################################

  output$rel_del <- renderUI(selectInput(session$ns("rel_del"), label = "Select a relationship", choices = rv_rel$name, selected = rv_rel$name[1]))

  output$act_rel_del <- renderUI(actionButton(session$ns("act_rel_del"), label = "Delete"))

  observeEvent(input$act_rel_del, {

    pos_del <- which(rv_rel$name == input$rel_del)
    rv_rel$name <- rv_rel$name[- pos_del]
    rv_rel$degree <- rv_rel$degree[- pos_del]
    rv_rel$pibd2 <- rv_rel$pibd2[- pos_del]
    rv_rel$pibd1 <- rv_rel$pibd1[- pos_del]
    rv_rel$pibd0 <- rv_rel$pibd0[- pos_del]

    output$dt_rel <- renderDataTable({
      datatable(
        data.table(Name_relationship = rv_rel$name, Degree = rv_rel$degree, Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0),
        colnames = c("Relationship name", "Degree", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)"),
        selection = "none",
        options = list(iDisplayLength = 10, ordering = FALSE),
        rownames = FALSE
      )
    })
  })

  #########################################
  # Reset information on the relationship #
  #########################################

  output$act_rel_reset <- renderUI(actionButton(session$ns("act_rel_reset"), label = "Reset"))

  observeEvent(input$act_rel_reset, {
    init_dt_rel <- create_dt_rel(path_pack)
    rv_rel$name <- init_dt_rel[, Name_relationship]
    rv_rel$degree <- init_dt_rel[, Degree]
    rv_rel$pibd2 <- init_dt_rel[, Pr_IBD2]
    rv_rel$pibd1 <- init_dt_rel[, Pr_IBD1]
    rv_rel$pibd0 <- init_dt_rel[, Pr_IBD0]

    output$dt_rel <- renderDataTable({
      datatable(
        data.table(Name_relationship = rv_rel$name, Degree = rv_rel$degree, Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0),
        colnames = c("Relationship name", "Degree", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)"),
        selection = "none",
        options = list(iDisplayLength = 10, ordering = FALSE),
        rownames = FALSE
      )
    })
  })

  ##################################################
  # Update default information on the relationship #
  ##################################################

  output$act_rel_update <- renderUI(actionButton(session$ns("act_rel_update"), label = "Update default"))

  observeEvent(input$act_rel_update, {
    write.csv(data.table(Name_relationship = rv_rel$name, Degree = rv_rel$degree, Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0),
              paste0(path_pack, "/extdata/parameters/rel.csv"), row.names = FALSE)

    showModal(modalDialog(title = "Information", "Default information on the relationship have been updated.", easyClose = TRUE, footer = NULL))
  })

  #######################################
  # Add information on the relationship #
  #######################################

  rv_famtree <- reactiveValues()
  rv_famtree$num_uk <- 0
  rv_famtree$uk <- character(0)
  rv_famtree$sex <- character(0)
  rv_famtree$father <- character(0)
  rv_famtree$mother <- character(0)
  rv_famtree$founder <- character(0)

  output$rel_add <- renderUI(textInput(session$ns("rel_add"), label = "Enter the name of a defined relationship", value = NULL))

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

  output$act_famtree_add <- renderUI(actionButton(session$ns("act_famtree_add"), label = "Add a person"))

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

    output$uks <- renderUI({
      lapply(seq_len(num_uk), function(x){
        disabled(selectInput(session$ns(paste0("uk_", x)), label = NULL, choices = uk_add[x], selected = uk_add[x])
        )
      })
    })

    output$sexes <- renderUI({
      lapply(seq_len(num_uk), function(x){
        selectInput(session$ns(paste0("sex_", x)), label = NULL, choices = c("M", "F"), selected = sex_add[x])
      })
    })

    output$fathers <- renderUI({
      output_list <- list()
      for(x in seq_len(num_uk)){
        if(founder_add[x] == "Yes"){
          output_list[[x]] <- disabled(selectInput(session$ns(paste0("father_", x)), label = NULL, choices = c("Victim", "Ref", uk_add), selected = father_add[x]))
        }else{
          output_list[[x]] <- selectInput(session$ns(paste0("father_", x)), label = NULL, choices = c("Victim", "Ref", uk_add), selected = father_add[x])
        }
      }
      output_list
    })

    output$mothers <- renderUI({
      output_list <- list()
      for(x in seq_len(num_uk)){
        if(founder_add[x] == "Yes"){
          output_list[[x]] <- disabled(selectInput(session$ns(paste0("mother_", x)), label = NULL, choices = c("Victim", "Ref", uk_add), selected = mother_add[x]))
        }else{
          output_list[[x]] <- selectInput(session$ns(paste0("mother_", x)), label = NULL, choices = c("Victim", "Ref", uk_add), selected = mother_add[x])
        }
      }
      output_list
    })

    output$founders <- renderUI({
      lapply(seq_len(num_uk), function(x){
        selectInput(session$ns(paste0("founder_", x)), label = NULL, choices = c("Yes", "No"), selected = founder_add[x])
      })
    })

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

  output$act_famtree_del <- renderUI(actionButton(session$ns("act_famtree_del"), label = "Delete a person"))

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

      output$uks <- renderUI({
        lapply(seq_len(num_uk), function(x){
          disabled(selectInput(session$ns(paste0("uk_", x)), label = NULL, choices = uk_del[x], selected = uk_del[x]))
        })
      })

      output$sexes <- renderUI({
        lapply(seq_len(num_uk), function(x){
          selectInput(session$ns(paste0("sex_", x)), label = NULL, choices = c("M", "F"), selected = sex_del[x])
        })
      })

      output$fathers <- renderUI({
        output_list <- list()
        for(x in seq_len(num_uk)){
          if(founder_del[x] == "Yes"){
            output_list[[x]] <- disabled(selectInput(session$ns(paste0("father_", x)), label = NULL, choices = c("Victim", "Ref", uk_del), selected = father_del[x]))
          }else{
            output_list[[x]] <- selectInput(session$ns(paste0("father_", x)), label = NULL, choices = c("Victim", "Ref", uk_del), selected = father_del[x])
          }
        }
        output_list
      })

      output$mothers <- renderUI({
        output_list <- list()
        for(x in seq_len(num_uk)){
          if(founder_del[x] == "Yes"){
            output_list[[x]] <- disabled(selectInput(session$ns(paste0("mother_", x)), label = NULL, choices = c("Victim", "Ref", uk_del), selected = mother_del[x]))
          }else{
            output_list[[x]] <- selectInput(session$ns(paste0("mother_", x)), label = NULL, choices = c("Victim", "Ref", uk_del), selected = mother_del[x])
          }
        }
        output_list
      })

      output$founders <- renderUI({
        lapply(seq_len(num_uk), function(x){
          selectInput(session$ns(paste0("founder_", x)), label = NULL, choices = c("Yes", "No"), selected = founder_del[x])
        })
      })

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

  output$act_famtree_view <- renderUI(actionButton(session$ns("act_famtree_view"), label = "View family tree"))

  observeEvent(input$act_famtree_view, {
    persons <- c("Victim", "Ref", rv_famtree$uk)
    sex_all <- c(input$v_sex, input$r_sex, rv_famtree$sex)
    father_all <- c(input$v_father, input$r_father, rv_famtree$father)
    mother_all <- c(input$v_mother, input$r_mother, rv_famtree$mother)
    founder_all <- c(input$v_founder, input$r_founder, rv_famtree$founder)

    tree <- check_tree(persons, sex_all, father_all, mother_all, founder_all)

    error_famtree <- FALSE
    if(length(class(tree)) > 1){
      error_famtree <- TRUE
    }else if(class(tree) == "try-error"){
      error_famtree <- TRUE
    }
    if(error_famtree){
      showModal(modalDialog(
        title = "Error",
        "Incorrect setting of the family tree!",
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      output$famtree <- renderPlot({
        plot(tree, hatched = c("Victim", "Ref"))
      })
    }
  })

  output$act_famtree_set <- renderUI(actionButton(session$ns("act_famtree_set"), label = "Set"))

  observeEvent(input$act_famtree_set, {
    if(isTruthy(input$rel_add)){
      rel_add <- input$rel_add

      persons <- c("Victim", "Ref", rv_famtree$uk)
      sex_all <- c(input$v_sex, input$r_sex, rv_famtree$sex)
      father_all <- c(input$v_father, input$r_father, rv_famtree$father)
      mother_all <- c(input$v_mother, input$r_mother, rv_famtree$mother)
      founder_all <- c(input$v_founder, input$r_founder, rv_famtree$founder)

      tree <- check_tree(persons, sex_all, father_all, mother_all, founder_all)

      error_famtree <- FALSE
      if(length(class(tree)) > 1){
        error_famtree <- TRUE
      }else if(class(tree) == "try-error"){
        error_famtree <- TRUE
      }
      if(error_famtree){
        showModal(modalDialog(
          title = "Error",
          "Incorrect setting of the family tree!",
          easyClose = TRUE,
          footer = NULL
        ))
      }else{
        coeff_tree <- coeffTable(tree)

        pos_row <- intersect(which(is.element(coeff_tree[, "id1"], c("Victim", "Ref")) == TRUE),
                             which(is.element(coeff_tree[, "id2"], c("Victim", "Ref")) == TRUE))
        pibd <- as.numeric(coeff_tree[pos_row, c("k2", "k1", "k0")])
        deg <- as.numeric(coeff_tree[pos_row, "deg"])

        # Check inbred relationship
        if(any(is.na(pibd))){
          showModal(modalDialog(
            title = "Error",
            "Victim and Ref are inbred individuals!",
            easyClose = TRUE,
            footer = NULL
          ))
        }else{
          deg_display <- make_deg_display(deg, pibd[2])

          rv_rel$name <- c(rv_rel$name, rel_add)
          rv_rel$degree <- c(rv_rel$degree, deg_display)
          rv_rel$pibd2 <- c(rv_rel$pibd2, pibd[1])
          rv_rel$pibd1 <- c(rv_rel$pibd1, pibd[2])
          rv_rel$pibd0 <- c(rv_rel$pibd0, pibd[3])

          output$dt_rel <- renderDataTable({
            datatable(
              data.table(Name_relationship = rv_rel$name, Degree = rv_rel$degree, Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0),
              colnames = c("Relationship name", "Degree", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)"),
              selection = "none",
              options = list(iDisplayLength = 10, ordering = FALSE),
              rownames = FALSE
            )
          })

          if(deg_display == "1st_pc"){
            deg_message <- "parent-child"
          }else if(deg_display == "1st_sib"){
            deg_message <- "sibling"
          }else{
            deg_message <- paste0(deg_display, " degree relationship")
          }

          showModal(modalDialog(
            title = "Information",
            paste0("The user-defined relationship '", rel_add, "' has been registerd as a ",  deg_message, "."),
            easyClose = TRUE,
            footer = NULL
          ))
        }
      }
    }else{
      showModal(modalDialog(
        title = "Error",
        "Enter the relationship name!",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })

  #######################################################
  # Display the initial information on the relationship #
  #######################################################

  output$dt_rel <- renderDataTable({
    datatable(
      init_dt_rel,
      colnames = c("Relationship name", "Degree", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)"),
      selection = "none",
      options = list(iDisplayLength = 10, ordering = FALSE),
      rownames = FALSE
    )
  })

  return(rv_rel)
}
