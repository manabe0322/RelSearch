#' load_ui
#'
#' @description The function to create the ui module for loading files
load_ui <- function(id){
  ns <- NS(id)

  tabPanel("Load",
           useWaiter(),

           fluidRow(
             column(4,
                    h2("STR"),
                    fileInput(ns("file_v_auto"), label = h4("Victim database"), accept = ".csv"),
                    fileInput(ns("file_r_auto"), label = h4("Reference database"), accept = ".csv"),
                    fileInput(ns("file_af"), label = h4("Allele frequencies"), accept = ".csv"),
             ),
             column(4,
                    h2("Y-STR"),
                    fileInput(ns("file_v_y"), label = h4("Victim database"), accept = ".csv"),
                    fileInput(ns("file_r_y"), label = h4("Reference database"), accept = ".csv")
             ),
             column(4,
                    h2("mtDNA"),
                    fileInput(ns("file_v_mt"), label = h4("Victim database"), accept = ".csv"),
                    fileInput(ns("file_r_mt"), label = h4("Reference database"), accept = ".csv")
             )
           ),
           br(),
           fluidRow(
             column(12,
                    actionButton(ns("act_analyze"), label = "Analyze", class = "btn btn-primary btn-lg")
             )
           )
  )
}

#' load_server
#'
#' @description The function to create the server module for loading files
load_server <- function(id, session_top, rv_criteria, rv_rel, rv_myu, rv_par_auto, keep_min_lr, max_data){
  moduleServer(
    id,
    function(input, output, session){
      rv_file <- reactiveValues()
      rv_file$dt_v_auto <- NULL
      rv_file$dt_r_auto <- NULL
      rv_file$dt_af <- NULL
      rv_file$dt_v_y <- NULL
      rv_file$dt_r_y <- NULL
      rv_file$dt_v_mt <- NULL
      rv_file$dt_r_mt <- NULL
      rv_file$dt_criteria <- NULL
      rv_file$dt_rel <- NULL
      rv_file$dt_myu <- NULL
      rv_file$dt_par_auto <- NULL
      rv_file$data_list <- NULL

      observe({
        req(rv_criteria)
        rv_file$dt_criteria <- data.table(Criteria = c("min_lr_auto", "max_mismatch_y", "max_ignore_y", "max_mustep_y", "max_mismatch_mt", "min_share_mt"),
                                          Value = c(rv_criteria$min_lr_auto, rv_criteria$max_mismatch_y, rv_criteria$max_ignore_y, rv_criteria$max_mustep_y, rv_criteria$max_mismatch_mt, rv_criteria$min_share_mt))
      })

      observe({
        req(rv_rel)
        rv_file$dt_rel <- data.table(Relationship = rv_rel$name, Victim = rv_rel$victim, Reference = rv_rel$reference,
                                     Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0,
                                     Paternal = rv_rel$paternal, Maternal = rv_rel$maternal,
                                     Tree_persons = rv_rel$tree_persons, Tree_sexes = rv_rel$tree_sexes, Tree_fathers = rv_rel$tree_fathers, Tree_mothers = rv_rel$tree_mothers, Tree_founders = rv_rel$tree_founders)
      })

      observe({
        req(rv_myu)
        rv_file$dt_myu <- data.table(Marker = rv_myu$mk, Myu = rv_myu$val)
      })

      observe({
        req(rv_par_auto)
        rv_file$dt_par_auto <- data.table(Parameter = c("maf"), Value = c(rv_par_auto$maf))
      })

      #################
      # Load database #
      #################

      observeEvent(input$file_v_auto, {
        file_input <- input$file_v_auto
        if(!is.null(file_input)){
          dt <- fread(file_input$datapath)
          dt <- change_data_type_auto(dt)
          rv_file$dt_v_auto <- dt
        }
      })

      observeEvent(input$file_r_auto, {
        file_input <- input$file_r_auto
        if(!is.null(file_input)){
          dt <- fread(file_input$datapath)
          dt <- change_data_type_auto(dt)
          rv_file$dt_r_auto <- dt
        }
      })

      observeEvent(input$file_af, {
        file_input <- input$file_af
        if(!is.null(file_input)){
          dt <- fread(file_input$datapath)
          dt <- change_data_type_af(dt)
          rv_file$dt_af <- dt
        }
      })

      observeEvent(input$file_v_y, {
        file_input <- input$file_v_y
        if(!is.null(file_input)){
          dt <- fread(file_input$datapath)
          dt <- change_data_type_y(dt)
          rv_file$dt_v_y <- dt
        }
      })

      observeEvent(input$file_r_y, {
        file_input <- input$file_r_y
        if(!is.null(file_input)){
          dt <- fread(file_input$datapath)
          dt <- change_data_type_y(dt)
          rv_file$dt_r_y <- dt
        }
      })

      observeEvent(input$file_v_mt, {
        file_input <- input$file_v_mt
        if(!is.null(file_input)){
          dt <- fread(file_input$datapath)
          dt <- change_data_type_mt(dt)
          rv_file$dt_v_mt <- dt
        }
      })

      observeEvent(input$file_r_mt, {
        file_input <- input$file_r_mt
        if(!is.null(file_input)){
          dt <- fread(file_input$datapath)
          dt <- change_data_type_mt(dt)
          rv_file$dt_r_mt <- dt
        }
      })

      ####################
      # Perform analysis #
      ####################

      observeEvent(input$act_analyze, {
        # Fix data.table
        dt_v_auto <- rv_file$dt_v_auto
        dt_r_auto <- rv_file$dt_r_auto
        dt_af <- rv_file$dt_af
        dt_v_y <- rv_file$dt_v_y
        dt_r_y <- rv_file$dt_r_y
        dt_v_mt <- rv_file$dt_v_mt
        dt_r_mt <- rv_file$dt_r_mt
        dt_criteria <- rv_file$dt_criteria
        dt_rel <- rv_file$dt_rel
        dt_myu <- rv_file$dt_myu
        dt_par_auto <- rv_file$dt_par_auto

        # Fix file names of each database
        fn_v_auto <- input$file_v_auto$name
        fn_r_auto <- input$file_r_auto$name
        fn_af <- input$file_af$name
        fn_v_y <- input$file_v_y$name
        fn_r_y <- input$file_r_y$name
        fn_v_mt <- input$file_v_mt$name
        fn_r_mt <- input$file_r_mt$name

        # Check data.table
        error_message <- check_error(dt_v_auto, dt_r_auto, dt_af, dt_v_y, dt_r_y, dt_v_mt, dt_r_mt, dt_rel, dt_myu)
        if(error_message != ""){
          showModal(modalDialog(title = "Error", error_message, easyClose = TRUE, footer = NULL))
        }else{
          start_time <- proc.time()
          waiter_show(html = spin_3k(), color = "white")

          ##############################
          # Analysis for autosomal STR #
          ##############################

          bool_check_auto <- all(!is.null(dt_v_auto), !is.null(dt_r_auto), !is.null(dt_af))

          if(bool_check_auto){
            tmp <- order_loci_auto(dt_v_auto, dt_r_auto, dt_af)
            dt_v_auto <- tmp[[1]]
            dt_r_auto <- tmp[[2]]
            dt_af <- tmp[[3]]

            dt_result_auto <- analyze_auto(dt_v_auto, dt_r_auto, dt_af, dt_rel, dt_myu, dt_par_auto, dt_criteria)
          }else{
            dt_result_auto <- NULL
          }

          ######################
          # Analysis for Y-STR #
          ######################

          bool_check_y <- all(!is.null(dt_v_y), !is.null(dt_r_y))

          if(bool_check_y){
            tmp <- order_loci_y(dt_v_y, dt_r_y)
            dt_v_y <- tmp[[1]]
            dt_r_y <- tmp[[2]]

            dt_result_y <- analyze_y(dt_v_y, dt_r_y, dt_criteria)
          }else{
            dt_result_y <- NULL
          }

          ######################
          # Analysis for mtDNA #
          ######################

          bool_check_mt <- all(!is.null(dt_v_mt), !is.null(dt_r_mt))

          if(bool_check_mt){
            dt_result_mt <- analyze_mt(dt_v_mt, dt_r_mt, dt_criteria)
          }else{
            dt_result_mt <- NULL
          }

          #############################
          # Clean the console message #
          #############################

          message('\r', paste0(rep(" ", 100), collapse = ""), appendLF = FALSE)

          ############################
          # Create the combined data #
          ############################

          dt_combined <- create_combined_data(dt_result_auto, dt_result_y, dt_result_mt, dt_rel, keep_min_lr)

          #############################
          # Create the displayed data #
          #############################

          min_lr_auto <- dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"]
          if(bool_check_auto){
            dt_display <- create_displayed_data(dt_combined, fltr_type = "with_auto", min_lr = min_lr_auto, max_data = max_data)
          }else{
            dt_display <- create_displayed_data(dt_combined, fltr_type = "without_auto", max_data = max_data)
          }

          ###############################
          # Assign objects to data_list #
          ###############################

          data_list <- vector(mode = "list", length = 23)
          names(data_list) <- c("bool_check_auto", "bool_check_y", "bool_check_mt",
                                "dt_combined", "dt_display",
                                "dt_v_auto", "dt_r_auto", "dt_af",
                                "dt_v_y", "dt_r_y",
                                "dt_v_mt", "dt_r_mt",
                                "dt_criteria", "dt_rel", "dt_myu", "dt_par_auto",
                                "fn_v_auto", "fn_r_auto", "fn_af", "fn_v_y", "fn_r_y", "fn_v_mt", "fn_r_mt")
          data_list$bool_check_auto <- bool_check_auto
          data_list$bool_check_y <- bool_check_y
          data_list$bool_check_mt <- bool_check_mt
          data_list$dt_combined <- dt_combined
          data_list$dt_display <- dt_display
          data_list$dt_v_auto <- dt_v_auto
          data_list$dt_r_auto <- dt_r_auto
          data_list$dt_af <- dt_af
          data_list$dt_v_y <- dt_v_y
          data_list$dt_r_y <- dt_r_y
          data_list$dt_v_mt <- dt_v_mt
          data_list$dt_r_mt <- dt_r_mt
          data_list$dt_criteria <- dt_criteria
          data_list$dt_rel <- dt_rel
          data_list$dt_myu <- dt_myu
          data_list$dt_par_auto <- dt_par_auto
          data_list$fn_v_auto <- fn_v_auto
          data_list$fn_r_auto <- fn_r_auto
          data_list$fn_af <- fn_af
          data_list$fn_v_y <- fn_v_y
          data_list$fn_r_y <- fn_r_y
          data_list$fn_v_mt <- fn_v_mt
          data_list$fn_r_mt <- fn_r_mt
          rv_file$data_list <- data_list

          ######################
          # Finish calculation #
          ######################

          run_time <- proc.time() - start_time
          cat(paste0("\n", "Calculation time : ", run_time[3], " sec", "\n"))

          #enable(selector = '.navbar-nav a[data-value = "Result"]')
          #disable(selector = '.navbar-nav a[data-value = "Settings"]')

          #updateNavbarPage(session = session_top, "navbar", selected = "Result")

          waiter_hide()

          # Show a message for displayed data
          if(nrow(dt_display) == max_data){
            showModal(modalDialog(title = "Information", paste0("Top ", max_data, " data is displayed."), easyClose = TRUE, footer = NULL))
          }else if(bool_check_auto){
            showModal(modalDialog(title = "Information", "Data that satisfies the criterion of the minimum LR is displayed.", easyClose = TRUE, footer = NULL))
          }else{
            showModal(modalDialog(title = "Information", "Data that satisfies the criteria for Y-STR or mtDNA is displayed.", easyClose = TRUE, footer = NULL))
          }
        }
      })

      return(rv_file)
    }
  )
}
