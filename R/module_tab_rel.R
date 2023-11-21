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
                          actionButton(ns("act_rel_del"), "Delete"),
                          br(),
                          br(),
                          actionButton(ns("act_rel_reset"), "Reset"),
                          br(),
                          br(),
                          actionButton(ns("act_rel_famtree"), "Family tree")
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
  rv_rel$tree_persons <- init_dt_rel[, Tree_persons]
  rv_rel$tree_sexes <- init_dt_rel[, Tree_sexes]
  rv_rel$tree_fathers <- init_dt_rel[, Tree_fathers]
  rv_rel$tree_mothers <- init_dt_rel[, Tree_mothers]
  rv_rel$tree_founders <- init_dt_rel[, Tree_founders]

  ###################################
  # Edit the name of a relationship #
  ###################################

  iv_rel_edit <- InputValidator$new()

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

    iv_rel_edit$disable()
  })

  observeEvent(input$act_rel_edit_save, {
    rel_old_edit <- input$rel_old_edit
    rel_new_edit <- input$rel_new_edit

    if(isTruthy(rel_new_edit)){
      rv_rel$name[rv_rel$name == rel_old_edit] <- rel_new_edit

      new_dt_rel <- data.table(Relationship = rv_rel$name, Victim = rv_rel$victim, Reference = rv_rel$reference,
                               Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0,
                               Paternal = rv_rel$paternal, Maternal = rv_rel$maternal,
                               Tree_persons = rv_rel$tree_persons, Tree_sexes = rv_rel$tree_sexes, Tree_fathers = rv_rel$tree_fathers, Tree_mothers = rv_rel$tree_mothers, Tree_founders = rv_rel$tree_founders)

      write.csv(new_dt_rel, paste0(path_pack, "/extdata/parameters/rel.csv"), row.names = FALSE)

      output$dt_rel <- renderDataTable({
        datatable(
          new_dt_rel,
          colnames = c("Relationship", "Victim", "Reference", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)", "Paternal lineage", "Maternal lineage",
                       "Tree_persons", "Tree_sexes", "Tree_fathers", "Tree_mothers", "Tree_founders"),
          selection = list(mode = "single", target = "row"),
          options = list(iDisplayLength = 10, ordering = FALSE,
                         columnDefs = list(list(targets = 8:12, visible = FALSE))
          ),
          rownames = FALSE
        )
      })

      removeModal()

    }else{
      iv_rel_edit$add_rule("rel_new_edit", sv_required())
      iv_rel_edit$enable()
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

  iv_rel_add <- InputValidator$new()

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
        disabled(actionButton(session$ns("act_rel_add_save"), "Save")),
        actionButton(session$ns("act_rel_add_cancel"), "Cancel")
      ),
      size = "l"
    ))

    iv_rel_add$disable()
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
      state_save_butt()
    }
  })

  output$famtree <- renderPlot({
    validate(
      need(!rv_famtree$error_famtree, "Incorrect setting of the family tree!")
    )
    plot(rv_famtree$tree, hatched = c("Victim", "Ref"))
  })

  state_save_butt <- function(){
    if(rv_famtree$error_famtree){
      disable("act_rel_add_save")
    }else{
      enable("act_rel_add_save")
    }
  }

  observeEvent(input$act_rel_add_save, {
    if(all(c(isTruthy(input$rel_add), isTruthy(input$vic_add), isTruthy(input$ref_add)))){
      tree <- rv_famtree$tree

      # Kinship coefficient
      coeff_tree <- coeffTable(tree)
      pos_row <- intersect(which(is.element(coeff_tree[, "id1"], c("Victim", "Ref")) == TRUE),
                           which(is.element(coeff_tree[, "id2"], c("Victim", "Ref")) == TRUE))
      pibd <- as.numeric(coeff_tree[pos_row, c("k2", "k1", "k0")])
      deg <- as.numeric(coeff_tree[pos_row, "deg"])

      # Extract tree data
      tree_list <- unclass(tree)
      id <- tree_list$ID
      fid <- tree_list$FIDX
      fid[fid != 0] <- id[fid[fid != 0]]
      mid <- tree_list$MIDX
      mid[mid != 0] <- id[mid[mid != 0]]
      sex <- tree_list$SEX

      # Judge paternal lineage
      rel_paternal <- judge_paternal("Victim", "Ref", id, fid, sex)
      if(is.element(rel_paternal, c("lineal", "collateral"))){
        paternal_add <- "Yes"
      }else{
        paternal_add <- "No"
      }

      # Judge maternal lineage
      rel_maternal <- judge_maternal("Victim", "Ref", id, mid)
      if(is.element(rel_maternal, c("lineal", "collateral"))){
        maternal_add <- "Yes"
      }else{
        maternal_add <- "No"
      }

      # Make additional tree data
      sex_info <- sex
      sex_info[which(sex == 1)] <- "M"
      sex_info[which(sex == 2)] <- "F"
      founder_info <- rep("No", length(id))
      founder_info[which(fid == "0")] <- "Yes"

      # Update information on the relationship
      rv_rel$name <- c(rv_rel$name, input$rel_add)
      rv_rel$victim <- c(rv_rel$victim, input$vic_add)
      rv_rel$reference <- c(rv_rel$reference, input$ref_add)
      rv_rel$pibd2 <- c(rv_rel$pibd2, pibd[1])
      rv_rel$pibd1 <- c(rv_rel$pibd1, pibd[2])
      rv_rel$pibd0 <- c(rv_rel$pibd0, pibd[3])
      rv_rel$paternal <- c(rv_rel$paternal, paternal_add)
      rv_rel$maternal <- c(rv_rel$maternal, maternal_add)
      rv_rel$tree_persons <- c(rv_rel$tree_persons, paste(id, collapse = ", "))
      rv_rel$tree_sexes <- c(rv_rel$tree_sexes, paste(sex_info, collapse = ", "))
      rv_rel$tree_fathers <- c(rv_rel$tree_fathers, paste(fid, collapse = ", "))
      rv_rel$tree_mothers <- c(rv_rel$tree_mothers, paste(mid, collapse = ", "))
      rv_rel$tree_founders <- c(rv_rel$tree_founders, paste(founder_info, collapse = ", "))

      new_dt_rel <- data.table(Relationship = rv_rel$name, Victim = rv_rel$victim, Reference = rv_rel$reference,
                               Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0,
                               Paternal = rv_rel$paternal, Maternal = rv_rel$maternal,
                               Tree_persons = rv_rel$tree_persons, Tree_sexes = rv_rel$tree_sexes, Tree_fathers = rv_rel$tree_fathers, Tree_mothers = rv_rel$tree_mothers, Tree_founders = rv_rel$tree_founders)

      write.csv(new_dt_rel, paste0(path_pack, "/extdata/parameters/rel.csv"), row.names = FALSE)

      output$dt_rel <- renderDataTable({
        datatable(
          new_dt_rel,
          colnames = c("Relationship", "Victim", "Reference", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)", "Paternal lineage", "Maternal lineage",
                       "Tree_persons", "Tree_sexes", "Tree_fathers", "Tree_mothers", "Tree_founders"),
          selection = list(mode = "single", target = "row"),
          options = list(iDisplayLength = 10, ordering = FALSE,
                         columnDefs = list(list(targets = 8:12, visible = FALSE))
          ),
          rownames = FALSE
        )
      })

      # Reset family tree
      rv_famtree$num_uk <- 0
      rv_famtree$uk <- character(0)
      rv_famtree$sex <- character(0)
      rv_famtree$father <- character(0)
      rv_famtree$mother <- character(0)
      rv_famtree$founder <- character(0)
      rv_famtree$error_famtree <- TRUE
      make_unk_selectbox(rv_famtree$num_uk, rv_famtree$uk, rv_famtree$sex, rv_famtree$father, rv_famtree$mother, rv_famtree$founder)
      state_save_butt()
      removeModal()
    }else{
      iv_rel_add$add_rule("rel_add", sv_required())
      iv_rel_add$add_rule("vic_add", sv_required())
      iv_rel_add$add_rule("ref_add", sv_required())
      iv_rel_add$enable()
    }
  })

  observeEvent(input$act_rel_add_cancel, {
    # Reset family tree
    rv_famtree$num_uk <- 0
    rv_famtree$uk <- character(0)
    rv_famtree$sex <- character(0)
    rv_famtree$father <- character(0)
    rv_famtree$mother <- character(0)
    rv_famtree$founder <- character(0)
    rv_famtree$error_famtree <- TRUE
    make_unk_selectbox(rv_famtree$num_uk, rv_famtree$uk, rv_famtree$sex, rv_famtree$father, rv_famtree$mother, rv_famtree$founder)
    state_save_butt()
    removeModal()
  })

  ########################################
  # Delete information on a relationship #
  ########################################

  observeEvent(input$act_rel_del, {
    showModal(modalDialog(
      title = "Delete information on a relationship",
      selectInput(session$ns("rel_del"), label = "Select a relationship", choices = rv_rel$name, selected = rv_rel$name[1]),
      footer = tagList(
        actionButton(session$ns("act_rel_del_save"), "Save"),
        modalButton("Cancel")
      ),
      size = "l"
    ))
  })

  observeEvent(input$act_rel_del_save, {
    pos_del <- which(rv_rel$name == input$rel_del)
    rv_rel$name <- rv_rel$name[- pos_del]
    rv_rel$victim <- rv_rel$victim[- pos_del]
    rv_rel$reference <- rv_rel$reference[- pos_del]
    rv_rel$pibd2 <- rv_rel$pibd2[- pos_del]
    rv_rel$pibd1 <- rv_rel$pibd1[- pos_del]
    rv_rel$pibd0 <- rv_rel$pibd0[- pos_del]
    rv_rel$paternal <- rv_rel$paternal[- pos_del]
    rv_rel$maternal <- rv_rel$maternal[- pos_del]
    rv_rel$tree_persons <- rv_rel$tree_persons[- pos_del]
    rv_rel$tree_sexes <- rv_rel$tree_sexes[- pos_del]
    rv_rel$tree_fathers <- rv_rel$tree_fathers[- pos_del]
    rv_rel$tree_mothers <- rv_rel$tree_mothers[- pos_del]
    rv_rel$tree_founders <- rv_rel$tree_founders[- pos_del]

    new_dt_rel <- data.table(Relationship = rv_rel$name, Victim = rv_rel$victim, Reference = rv_rel$reference,
                             Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0,
                             Paternal = rv_rel$paternal, Maternal = rv_rel$maternal,
                             Tree_persons = rv_rel$tree_persons, Tree_sexes = rv_rel$tree_sexes, Tree_fathers = rv_rel$tree_fathers, Tree_mothers = rv_rel$tree_mothers, Tree_founders = rv_rel$tree_founders)

    write.csv(new_dt_rel, paste0(path_pack, "/extdata/parameters/rel.csv"), row.names = FALSE)

    output$dt_rel <- renderDataTable({
      datatable(
        new_dt_rel,
        colnames = c("Relationship", "Victim", "Reference", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)", "Paternal lineage", "Maternal lineage",
                     "Tree_persons", "Tree_sexes", "Tree_fathers", "Tree_mothers", "Tree_founders"),
        selection = list(mode = "single", target = "row"),
        options = list(iDisplayLength = 10, ordering = FALSE,
                       columnDefs = list(list(targets = 8:12, visible = FALSE))
        ),
        rownames = FALSE
      )
    })

    removeModal()
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
    new_dt_rel <- create_dt_rel(path_pack, FALSE)

    rv_rel$name <- new_dt_rel[, Relationship]
    rv_rel$victim <- new_dt_rel[, Victim]
    rv_rel$reference <- new_dt_rel[, Reference]
    rv_rel$pibd2 <- new_dt_rel[, Pr_IBD2]
    rv_rel$pibd1 <- new_dt_rel[, Pr_IBD1]
    rv_rel$pibd0 <- new_dt_rel[, Pr_IBD0]
    rv_rel$paternal <- new_dt_rel[, Paternal]
    rv_rel$maternal <- new_dt_rel[, Maternal]
    rv_rel$tree_persons <- new_dt_rel[, Tree_persons]
    rv_rel$tree_sexes <- new_dt_rel[, Tree_sexes]
    rv_rel$tree_fathers <- new_dt_rel[, Tree_fathers]
    rv_rel$tree_mothers <- new_dt_rel[, Tree_mothers]
    rv_rel$tree_founders <- new_dt_rel[, Tree_founders]

    output$dt_rel <- renderDataTable({
      datatable(
        new_dt_rel,
        colnames = c("Relationship", "Victim", "Reference", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)", "Paternal lineage", "Maternal lineage",
                     "Tree_persons", "Tree_sexes", "Tree_fathers", "Tree_mothers", "Tree_founders"),
        selection = list(mode = "single", target = "row"),
        options = list(iDisplayLength = 10, ordering = FALSE,
                       columnDefs = list(list(targets = 8:12, visible = FALSE))
                       ),
        rownames = FALSE
      )
    })

    removeModal()
  })

  #########################
  # Check the family tree #
  #########################

  rv_tree_check <- reactiveValues()
  rv_tree_check$tree <- NULL

  observeEvent(input$act_rel_famtree, {
    pos_select <- input$dt_rel_rows_selected
    if(isTruthy(pos_select)){
      rel_name <- rv_rel$name[pos_select]
      persons <- strsplit(rv_rel$tree_persons[pos_select], ", ")[[1]]
      sex_all <- strsplit(rv_rel$tree_sexes[pos_select], ", ")[[1]]
      father_all <- strsplit(rv_rel$tree_fathers[pos_select], ", ")[[1]]
      mother_all <- strsplit(rv_rel$tree_mothers[pos_select], ", ")[[1]]
      founder_all <- strsplit(rv_rel$tree_founders[pos_select], ", ")[[1]]
      rv_tree_check$tree <- check_tree(persons, sex_all, father_all, mother_all, founder_all)

      showModal(modalDialog(
        title = paste0("Relationship: ", rel_name),
        plotOutput(session$ns("tree_check")),
        footer = tagList(
          modalButton("Close")
        )
      ))
    }else{
      showModal(modalDialog(title = "Error", "Select a relationship!", easyClose = TRUE, footer = NULL))
    }
  })

  output$tree_check <- renderPlot({
    plot(rv_tree_check$tree, hatched = c("Victim", "Ref"))
  })

  #######################################################
  # Display the initial information on the relationship #
  #######################################################

  output$dt_rel <- renderDataTable({
    datatable(
      init_dt_rel,
      colnames = c("Relationship", "Victim", "Reference", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)", "Paternal lineage", "Maternal lineage",
                   "Tree_persons", "Tree_sexes", "Tree_fathers", "Tree_mothers", "Tree_founders"),
      selection = list(mode = "single", target = "row"),
      options = list(iDisplayLength = 10, ordering = FALSE,
                     columnDefs = list(list(targets = 8:12, visible = FALSE))
      ),
      rownames = FALSE
    )
  })

  return(rv_rel)
}
