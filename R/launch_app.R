#' relsearch
#'
#' @description Main window
#' @usage relsearch()
#' @export
relsearch <- function(){

  ver_soft <- packageVersion("relsearch")
  path_pack <- path.package("relsearch", quiet = FALSE)
  max_data <- 10000
  options(shiny.maxRequestSize = 500 * 1024^2)

  ui <- fluidPage(useShinyjs(),
                  theme = shinytheme("cerulean"),
                  navbarPage(title = paste0("relsearch ver. ", ver_soft),
                             id = "navbar",
                             position = c("fixed-top"),
                             tags$style(type = "text/css", "body{padding-top: 70px;}"),

                             tabPanel("Load",

                                      useWaiter(),

                                      fluidRow(

                                        column(4,
                                               h2("STR"),
                                               fileInput("file_v_auto", label = h4("Victim database"), accept = ".csv"),
                                               fileInput("file_r_auto", label = h4("Reference database"), accept = ".csv"),
                                               fileInput("file_af", label = h4("Allele frequencies"), accept = ".csv"),
                                        ),

                                        column(4,
                                               h2("Y-STR"),
                                               fileInput("file_v_y", label = h4("Victim database"), accept = ".csv"),
                                               fileInput("file_r_y", label = h4("Reference database"), accept = ".csv")
                                        ),

                                        column(4,
                                               h2("mtDNA"),
                                               fileInput("file_v_mt", label = h4("Victim database"), accept = ".csv"),
                                               fileInput("file_r_mt", label = h4("Reference database"), accept = ".csv")
                                        )
                                      ),

                                      br(),

                                      fluidRow(

                                        column(12,
                                               actionButton("act_analyze", label = "Analyze", class = "btn btn-primary btn-lg")
                                        )
                                      )
                             ),

                             tabPanel("Result",
                                      titlePanel("Result"),

                                      br(),

                                      tabsetPanel(id = "tab_result",

                                                  tabPanel("Summary",
                                                           br(),

                                                           sidebarLayout(
                                                             sidebarPanel(
                                                               h4("Display setting"),
                                                               br(),
                                                               actionButton("act_default", label = "Default display", class = "btn btn-primary"),
                                                               br(),
                                                               br(),
                                                               actionButton("act_identified", label = "Identified pairs", class = "btn btn-success"),
                                                               br(),
                                                               br(),
                                                               actionButton("act_multiple", label = "Multiple candidates", class = "btn btn-warning"),
                                                               br(),
                                                               br(),
                                                               actionButton("act_alert", label = "Alert", class = "btn btn-danger"),
                                                               br(),
                                                               br(),
                                                               uiOutput("summary_min_lr"),
                                                               actionButton("act_fltr_lr", label = "Apply"),
                                                               width = 3
                                                             ),
                                                             mainPanel(
                                                               br(),
                                                               dataTableOutput("dt_display"),
                                                               width = 9
                                                             )
                                                           )
                                                  ),

                                                  tabPanel("Selected data in detail",
                                                           br(),

                                                           sidebarLayout(
                                                             sidebarPanel(
                                                               h4("Victim"),
                                                               textOutput("sn_v_select"),
                                                               br(),
                                                               h4("Reference"),
                                                               textOutput("sn_r_select"),
                                                               br(),
                                                               h4("Estimated relationship"),
                                                               textOutput("estimated_rel_select"),
                                                               br(),
                                                               h4("Paternal lineage"),
                                                               textOutput("paternal_select"),
                                                               br(),
                                                               h4("Maternal lineage"),
                                                               textOutput("maternal_select"),
                                                               width = 2
                                                             ),
                                                             mainPanel(
                                                               tabsetPanel(
                                                                 tabPanel("STR",
                                                                          br(),
                                                                          dataTableOutput("dt_detail_auto")
                                                                 ),
                                                                 tabPanel("Y-STR",
                                                                          br(),
                                                                          dataTableOutput("dt_detail_y")
                                                                 ),
                                                                 tabPanel("mtDNA",
                                                                          br(),
                                                                          fluidRow(
                                                                            column(4,
                                                                                   h4("Number of mismatches"),
                                                                                   textOutput("num_mismatch_select")
                                                                                   ),
                                                                            column(4,
                                                                                   h4("Shared range"),
                                                                                   textOutput("share_range_select")
                                                                                   ),
                                                                            column(4,
                                                                                   h4("Shared length (bp)"),
                                                                                   textOutput("share_len_select")
                                                                                   )
                                                                          ),
                                                                          br(),
                                                                          br(),
                                                                          dataTableOutput("dt_detail_mt")
                                                                 )
                                                               )
                                                             )
                                                           )
                                                  ),

                                                  tabPanel("Analysis conditions",
                                                           br(),

                                                           tabsetPanel(
                                                             tabPanel("Database",
                                                                      br(),
                                                                      fluidRow(
                                                                        column(4,
                                                                               h4("STR"),
                                                                               br(),
                                                                               h5(div("Victim database", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_fn_v_auto"),
                                                                               br(),
                                                                               h5(div("Reference database", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_fn_r_auto"),
                                                                               br(),
                                                                               h5(div("Allele frequencies", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_fn_af")
                                                                        ),
                                                                        column(4,
                                                                               h4("Y-STR"),
                                                                               br(),
                                                                               h5(div("Victim database", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_fn_v_y"),
                                                                               br(),
                                                                               h5(div("Reference database", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_fn_r_y")
                                                                        ),
                                                                        column(4,
                                                                               h4("mtDNA"),
                                                                               br(),
                                                                               h5(div("Victim database", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_fn_v_mt"),
                                                                               br(),
                                                                               h5(div("Reference database", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_fn_r_mt")
                                                                        )
                                                                      )
                                                             ),
                                                             tabPanel("Criteria",
                                                                      br(),
                                                                      fluidRow(
                                                                        column(4,
                                                                               h4("STR"),
                                                                               br(),
                                                                               h5(div("Minimum LR", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_min_lr_auto")
                                                                        ),
                                                                        column(4,
                                                                               h4("Y-STR"),
                                                                               br(),
                                                                               h5(div("Maximum number of mismatched loci", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_max_mismatch_y"),
                                                                               br(),
                                                                               h5(div("Maximum number of ignored loci", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_max_ignore_y"),
                                                                               br(),
                                                                               h5(div("Maximum total mutational steps", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_max_mustep_y")
                                                                        ),
                                                                        column(4,
                                                                               h4("mtDNA"),
                                                                               br(),
                                                                               h5(div("Maximum number of inconsistency", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_max_mismatch_mt"),
                                                                               br(),
                                                                               h5(div("Minimum shared length", style = "color:#555555;font-weight:bold;")),
                                                                               textOutput("result_min_share_mt")
                                                                        )
                                                                      )
                                                             ),
                                                             tabPanel("Assumed relationship",
                                                                      br(),
                                                                      dataTableOutput("result_assumed_rel")
                                                             ),
                                                             tabPanel("Mutation rate",
                                                                      br(),
                                                                      dataTableOutput("result_myu")
                                                             ),
                                                             tabPanel("Parameter",
                                                                      br(),
                                                                      h5(div("Minimum allele frequency", style = "color:#555555;font-weight:bold;")),
                                                                      textOutput("result_maf")
                                                             )
                                                           )
                                                  ),
                                      )
                             ),

                             navbarMenu("Project",
                                        tabPanel("New project",
                                                 h2("New project"),
                                                 br(),
                                                 actionButton("act_new_proj", label = "New project", class = "btn btn-primary btn-lg")
                                        ),
                                        tabPanel("Load project",
                                                 useWaiter(),
                                                 h2("Load project"),
                                                 br(),
                                                 fileInput("file_proj", label = "Select a project file", accept = ".RData"),
                                                 br(),
                                                 actionButton("act_load_proj", label = "Load project", class = "btn btn-primary btn-lg")
                                        ),
                                        tabPanel("Save project",
                                                 useWaiter(),
                                                 h2("Save project"),
                                                 br(),
                                                 disabled(textInput("name_proj", label = "Enter the project name", value = NULL)),
                                                 br(),
                                                 disabled(downloadButton("download_proj", label = "Save as", class = "btn btn-primary btn-lg"))
                                        )
                             ),

                             navbarMenu("Database",
                                        tabPanel("STR : Victim", dataTableOutput("view_dt_v_auto")),
                                        tabPanel("STR : Reference", dataTableOutput("view_dt_r_auto")),
                                        tabPanel("STR : Allele frequencies", dataTableOutput("view_dt_af")),
                                        tabPanel("Y-STR : Victim", dataTableOutput("view_dt_v_y")),
                                        tabPanel("Y-STR : Reference", dataTableOutput("view_dt_r_y")),
                                        tabPanel("mtDNA : Victim", dataTableOutput("view_dt_v_mt")),
                                        tabPanel("mtDNA : Reference", dataTableOutput("view_dt_r_mt")),
                             ),

                             navbarMenu("Settings",
                                        tabPanel("Criteria",
                                                 titlePanel("Criteria"),
                                                 br(),
                                                 fluidRow(
                                                   column(4,
                                                          h4("STR"),
                                                          numeric_ui("min_lr_auto")
                                                   ),
                                                   column(4,
                                                          h4("Y-STR"),
                                                          numeric_ui("max_mismatch_y"),
                                                          numeric_ui("max_ignore_y"),
                                                          numeric_ui("max_mustep_y")
                                                   ),
                                                   column(4,
                                                          h4("mtDNA"),
                                                          numeric_ui("max_mismatch_mt"),
                                                          numeric_ui("min_share_mt")
                                                   )
                                                 ),
                                                 br(),
                                                 h4("Update default"),
                                                 actionButton("act_criteria_update", label = "Update default")
                                        ),

                                        tab_rel_ui("tab_rel"),

                                        tab_myu_ui("tab_myu"),

                                        tabPanel("Parameter",
                                                 titlePanel("Parameter"),
                                                 numeric_ui("maf"),
                                                 br(),
                                                 h4("Update default"),
                                                 actionButton("act_par_auto_update", label = "Update default")
                                        )
                             ),

                             tabPanel("Example files",
                                      titlePanel("Example files"),
                                      downloadButton("download_v_auto", "STR victim database"),
                                      br(),
                                      br(),
                                      downloadButton("download_r_auto", "STR reference database"),
                                      br(),
                                      br(),
                                      downloadButton("download_af", "Allele frequencies"),
                                      br(),
                                      br(),
                                      downloadButton("download_v_y", "Y-STR victim database"),
                                      br(),
                                      br(),
                                      downloadButton("download_r_y", "Y-STR reference database"),
                                      br(),
                                      br(),
                                      downloadButton("download_v_mt", "mtDNA victim database"),
                                      br(),
                                      br(),
                                      downloadButton("download_r_mt", "mtDNA reference database")
                             ),

                             tabPanel("Manual",
                                      includeMarkdown(paste0(path_pack, "/extdata/manual/relsearch_manual.md"))
                             )
                  )
  )

  server <- function(input, output, session){

    disable(selector = '.navbar-nav a[data-value = "Result"]')

    ###########################
    # Load initial data.table #
    ###########################

    init_dt_criteria <- create_dt_criteria(path_pack)
    init_dt_rel <- create_dt_rel(path_pack)
    init_dt_myu <- create_dt_myu(path_pack)
    init_dt_par_auto <- create_dt_par_auto(path_pack)

    ################
    # Set criteria #
    ################

    rv_min_lr_auto <- callModule(numeric_server, "min_lr_auto", "Minimum LR", init_dt_criteria$Value[init_dt_criteria$Criteria == "min_lr_auto"])

    rv_max_mismatch_y <- callModule(numeric_server, "max_mismatch_y", "Maximum number of mismatched loci", init_dt_criteria$Value[init_dt_criteria$Criteria == "max_mismatch_y"])
    rv_max_ignore_y <- callModule(numeric_server, "max_ignore_y", "Maximum number of ignored loci", init_dt_criteria$Value[init_dt_criteria$Criteria == "max_ignore_y"])
    rv_max_mustep_y <- callModule(numeric_server, "max_mustep_y", "Maximum total mutational steps", init_dt_criteria$Value[init_dt_criteria$Criteria == "max_mustep_y"])

    rv_max_mismatch_mt <- callModule(numeric_server, "max_mismatch_mt", "Maximum number of inconsistency", init_dt_criteria$Value[init_dt_criteria$Criteria == "max_mismatch_mt"])
    rv_min_share_mt <- callModule(numeric_server, "min_share_mt", "Minimum shared length", init_dt_criteria$Value[init_dt_criteria$Criteria == "min_share_mt"])

    observeEvent(input$act_criteria_update, {
      dt_criteria <- data.table(Criteria = c("min_lr_auto", "max_mismatch_y", "max_ignore_y", "max_mustep_y", "max_mismatch_mt", "min_share_mt"),
                                Value = c(rv_min_lr_auto(), rv_max_mismatch_y(), rv_max_ignore_y(), rv_max_mustep_y(), rv_max_mismatch_mt(), rv_min_share_mt()))
      write.csv(dt_criteria, paste0(path_pack, "/extdata/parameters/criteria.csv"), row.names = FALSE)
      showModal(modalDialog(title = "Information", "Default criteria have been updated.", easyClose = TRUE, footer = NULL))
    })

    #######################################
    # Set information on the relationship #
    #######################################

    rv_rel <- callModule(tab_rel_server, "tab_rel", init_dt_rel, path_pack)

    ######################
    # Set mutation rates #
    ######################

    rv_myu <- callModule(tab_myu_server, "tab_myu", init_dt_myu, path_pack)

    ###################################
    # Set parameters of autosomal STR #
    ###################################

    rv_maf <- callModule(numeric_server, "maf", "Minimum allele frequency", init_dt_par_auto$Value[init_dt_par_auto$Parameter == "maf"])

    observeEvent(input$act_par_auto_update, {
      dt_par_auto <- data.table(Parameter = c("maf"), Value = c(rv_maf()))
      write.csv(dt_par_auto, paste0(path_pack, "/extdata/parameters/par_auto.csv"), row.names = FALSE)
      showModal(modalDialog(title = "Information", "The default parameter has been updated.", easyClose = TRUE, footer = NULL))
    })

    #################
    # Load database #
    #################

    load_v_auto <- reactive({
      file_input <- input$file_v_auto
      if(is.null(file_input)){
        return(NULL)
      }else{
        dt <- fread(file_input$datapath)
        col_init <- names(dt)

        col_tmp <- col_init
        pos_mk <- which(!is.element(col_init, c("SampleName", "Relationship")))
        col_tmp[pos_mk] <- paste0("Col", 1:length(pos_mk))
        names(dt) <- col_tmp

        col_numeric <- col_tmp[pos_mk]
        options(warn = -1)
        dt[, (col_numeric) := lapply(.SD, as.numeric), .SDcols = col_numeric]
        options(warn = 0)

        col_char <- col_tmp[which(is.element(col_init, c("SampleName", "Relationship")))]
        options(warn = -1)
        dt[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
        options(warn = 0)

        names(dt) <- col_init
        return(dt)
      }
    })

    load_r_auto <- reactive({
      file_input <- input$file_r_auto
      if(is.null(file_input)){
        return(NULL)
      }else{
        dt <- fread(file_input$datapath)
        col_init <- names(dt)

        col_tmp <- col_init
        pos_mk <- which(!is.element(col_init, c("SampleName", "Relationship")))
        col_tmp[pos_mk] <- paste0("Col", 1:length(pos_mk))
        names(dt) <- col_tmp

        col_numeric <- col_tmp[pos_mk]
        options(warn = -1)
        dt[, (col_numeric) := lapply(.SD, as.numeric), .SDcols = col_numeric]
        options(warn = 0)

        col_char <- col_tmp[which(is.element(col_init, c("SampleName", "Relationship")))]
        options(warn = -1)
        dt[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
        options(warn = 0)

        names(dt) <- col_init
        return(dt)
      }
    })

    load_af <- reactive({
      file_input <- input$file_af
      if(is.null(file_input)){
        return(NULL)
      }else{
        dt <- fread(file_input$datapath)

        col_numeric <- names(dt)
        options(warn = -1)
        dt[, (col_numeric) := lapply(.SD, as.numeric), .SDcols = col_numeric]
        options(warn = 0)

        return(dt)
      }
    })

    load_v_y <- reactive({
      file_input <- input$file_v_y
      if(is.null(file_input)){
        return(NULL)
      }else{
        dt <- fread(file_input$datapath)

        col_char <- names(dt)
        options(warn = -1)
        dt[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
        options(warn = 0)

        return(dt)
      }
    })

    load_r_y <- reactive({
      file_input <- input$file_r_y
      if(is.null(file_input)){
        return(NULL)
      }else{
        dt <- fread(file_input$datapath)

        col_char <- names(dt)
        options(warn = -1)
        dt[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
        options(warn = 0)

        return(dt)
      }
    })

    load_v_mt <- reactive({
      file_input <- input$file_v_mt
      if(is.null(file_input)){
        return(NULL)
      }else{
        dt <- fread(file_input$datapath)

        col_char <- names(dt)
        options(warn = -1)
        dt[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
        options(warn = 0)

        return(dt)
      }
    })

    load_r_mt <- reactive({
      file_input <- input$file_r_mt
      if(is.null(file_input)){
        return(NULL)
      }else{
        dt <- fread(file_input$datapath)

        col_char <- names(dt)
        options(warn = -1)
        dt[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
        options(warn = 0)

        return(dt)
      }
    })

    #################
    # View database #
    #################

    rv_fn <- reactiveValues()

    observeEvent(load_v_auto(), {
      rv_fn$fn_v_auto <- input$file_v_auto$name

      output$view_dt_v_auto <- renderDataTable({
        datatable(
          load_v_auto(),
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE),
          rownames = FALSE
        )
      })
    })

    observeEvent(load_r_auto(), {
      rv_fn$fn_r_auto <- input$file_r_auto$name

      output$view_dt_r_auto <- renderDataTable({
        datatable(
          load_r_auto(),
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE),
          rownames = FALSE
        )
      })
    })

    observeEvent(load_af(), {
      rv_fn$fn_af <- input$file_af$name

      output$view_dt_af <- renderDataTable({
        datatable(
          load_af(),
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE),
          rownames = FALSE
        )
      })
    })

    observeEvent(load_v_y(), {
      rv_fn$fn_v_y <- input$file_v_y$name

      output$view_dt_v_y <- renderDataTable({
        datatable(
          load_v_y(),
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE),
          rownames = FALSE
        )
      })
    })

    observeEvent(load_r_y(), {
      rv_fn$fn_r_y <- input$file_r_y$name

      output$view_dt_r_y <- renderDataTable({
        datatable(
          load_r_y(),
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE),
          rownames = FALSE
        )
      })
    })

    observeEvent(load_v_mt(), {
      rv_fn$fn_v_mt <- input$file_v_mt$name

      output$view_dt_v_mt <- renderDataTable({
        datatable(
          load_v_mt(),
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE),
          rownames = FALSE
        )
      })
    })

    observeEvent(load_r_mt(), {
      rv_fn$fn_r_mt <- input$file_r_mt$name

      output$view_dt_r_mt <- renderDataTable({
        datatable(
          load_r_mt(),
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE),
          rownames = FALSE
        )
      })
    })

    #################
    # Example files #
    #################

    output$download_v_auto <- downloadHandler(
      filename = "str_victim_example.csv",
      content = function(file){
        csvfile <- read.csv(paste0(path_pack, "/extdata/examples/str_victim_example.csv"))
        mk <- setdiff(colnames(csvfile), "SampleName")
        id_mk <- 1:length(mk)
        mk <- mk[id_mk[id_mk %% 2 == 1]]
        colnames(csvfile) <- c("SampleName", as.vector(sapply(mk, rep, 2)))
        csvfile[is.na(csvfile)] <- ""
        write.csv(csvfile, file, row.names = FALSE)
      }
    )

    output$download_r_auto <- downloadHandler(
      filename = "str_ref_example.csv",
      content = function(file){
        csvfile <- read.csv(paste0(path_pack, "/extdata/examples/str_ref_example.csv"))
        mk <- setdiff(colnames(csvfile), c("SampleName", "Relationship"))
        id_mk <- 1:length(mk)
        mk <- mk[id_mk[id_mk %% 2 == 1]]
        colnames(csvfile) <- c("SampleName", "Relationship", as.vector(sapply(mk, rep, 2)))
        csvfile[is.na(csvfile)] <- ""
        write.csv(csvfile, file, row.names = FALSE)
      }
    )

    output$download_af <- downloadHandler(
      filename = "str_af_example.csv",
      content = function(file){
        csvfile <- read.csv(paste0(path_pack, "/extdata/examples/str_af_example.csv"))
        csvfile[is.na(csvfile)] <- ""
        write.csv(csvfile, file, row.names = FALSE)
      }
    )

    output$download_v_y <- downloadHandler(
      filename = "y_victim_example.csv",
      content = function(file){
        csvfile <- read.csv(paste0(path_pack, "/extdata/examples/y_victim_example.csv"))
        csvfile[is.na(csvfile)] <- ""
        write.csv(csvfile, file, row.names = FALSE)
      }
    )

    output$download_r_y <- downloadHandler(
      filename = "y_ref_example.csv",
      content = function(file){
        csvfile <- read.csv(paste0(path_pack, "/extdata/examples/y_ref_example.csv"))
        csvfile[is.na(csvfile)] <- ""
        write.csv(csvfile, file, row.names = FALSE)
      }
    )

    output$download_v_mt <- downloadHandler(
      filename = "mt_victim_example.csv",
      content = function(file){
        csvfile <- read.csv(paste0(path_pack, "/extdata/examples/mt_victim_example.csv"))
        csvfile[is.na(csvfile)] <- ""
        write.csv(csvfile, file, row.names = FALSE)
      }
    )

    output$download_r_mt <- downloadHandler(
      filename = "mt_ref_example.csv",
      content = function(file){
        csvfile <- read.csv(paste0(path_pack, "/extdata/examples/mt_ref_example.csv"))
        csvfile[is.na(csvfile)] <- ""
        write.csv(csvfile, file, row.names = FALSE)
      }
    )

    ####################
    # Perform analysis #
    ####################

    dt_reactive <- reactiveValues()

    observeEvent(input$act_analyze, {

      ####################
      # Check parameters #
      ####################

      if(any(!isTruthy(rv_min_lr_auto()), !isTruthy(rv_max_mismatch_y()), !isTruthy(rv_max_ignore_y()), !isTruthy(rv_max_mustep_y()), !isTruthy(rv_max_mismatch_mt()), !isTruthy(rv_min_share_mt()))){
        showModal(modalDialog(title = "Error", "Set criteria!", easyClose = TRUE, footer = NULL))
      }else if(!isTruthy(rv_maf())){
        showModal(modalDialog(title = "Error", "Set the minimum allele frequency!", easyClose = TRUE, footer = NULL))
      }else{

        #####################
        # Define data.table #
        #####################

        dt_v_auto <- load_v_auto()
        dt_r_auto <- load_r_auto()
        dt_af <- load_af()
        dt_v_y <- load_v_y()
        dt_r_y <- load_r_y()
        dt_v_mt <- load_v_mt()
        dt_r_mt <- load_r_mt()
        dt_criteria <- data.table(Criteria = c("min_lr_auto", "max_mismatch_y", "max_ignore_y", "max_mustep_y", "max_mismatch_mt", "min_share_mt"),
                                  Value = c(rv_min_lr_auto(), rv_max_mismatch_y(), rv_max_ignore_y(), rv_max_mustep_y(), rv_max_mismatch_mt(), rv_min_share_mt()))
        dt_rel <- data.table(Relationship = rv_rel$name, Victim = rv_rel$victim, Reference = rv_rel$reference,
                             Pr_IBD2 = rv_rel$pibd2, Pr_IBD1 = rv_rel$pibd1, Pr_IBD0 = rv_rel$pibd0,
                             Paternal = rv_rel$paternal, Maternal = rv_rel$maternal,
                             Tree_persons = rv_rel$tree_persons, Tree_sexes = rv_rel$tree_sexes, Tree_fathers = rv_rel$tree_fathers, Tree_mothers = rv_rel$tree_mothers, Tree_founders = rv_rel$tree_founders)
        dt_myu <- data.table(Marker = rv_myu$mk, Myu = rv_myu$val)
        dt_par_auto <- data.table(Parameter = c("maf"), Value = c(rv_maf()))

        ###################################
        # Fix file names of each database #
        ###################################

        fn_v_auto <- rv_fn$fn_v_auto
        fn_r_auto <- rv_fn$fn_r_auto
        fn_af <- rv_fn$fn_af
        fn_v_y <- rv_fn$fn_v_y
        fn_r_y <- rv_fn$fn_r_y
        fn_v_mt <- rv_fn$fn_v_mt
        fn_r_mt <- rv_fn$fn_r_mt

        ####################
        # Check data.table #
        ####################

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

            output$view_dt_v_auto <- renderDataTable({
              datatable(
                dt_v_auto,
                selection = "none",
                options = list(iDisplayLength = 50, ordering = FALSE),
                rownames = FALSE
              )
            })

            output$view_dt_r_auto <- renderDataTable({
              datatable(
                dt_r_auto,
                selection = "none",
                options = list(iDisplayLength = 50, ordering = FALSE),
                rownames = FALSE
              )
            })

            output$view_dt_af <- renderDataTable({
              datatable(
                dt_af,
                selection = "none",
                options = list(iDisplayLength = 50, ordering = FALSE),
                rownames = FALSE
              )
            })

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

            output$view_dt_v_y <- renderDataTable({
              datatable(
                dt_v_y,
                selection = "none",
                options = list(iDisplayLength = 50, ordering = FALSE),
                rownames = FALSE
              )
            })

            output$view_dt_r_y <- renderDataTable({
              datatable(
                dt_r_y,
                selection = "none",
                options = list(iDisplayLength = 50, ordering = FALSE),
                rownames = FALSE
              )
            })

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

          dt_reactive$dt_combined <- create_combined_data(dt_result_auto, dt_result_y, dt_result_mt, dt_rel)
          dt_reactive$dt_display <- create_displayed_data(dt_reactive$dt_combined)

          #################################
          # Assign objects to dt_reactive #
          #################################

          dt_reactive$dt_result_auto <- dt_result_auto
          dt_reactive$dt_v_auto <- dt_v_auto
          dt_reactive$dt_r_auto <- dt_r_auto
          dt_reactive$dt_af <- dt_af
          dt_reactive$dt_result_y <- dt_result_y
          dt_reactive$dt_v_y <- dt_v_y
          dt_reactive$dt_r_y <- dt_r_y
          dt_reactive$dt_result_mt <- dt_result_mt
          dt_reactive$dt_v_mt <- dt_v_mt
          dt_reactive$dt_r_mt <- dt_r_mt
          dt_reactive$dt_criteria <- dt_criteria
          dt_reactive$dt_rel <- dt_rel
          dt_reactive$dt_myu <- dt_myu
          dt_reactive$dt_par_auto <- dt_par_auto

          dt_reactive$fn_v_auto <- fn_v_auto
          dt_reactive$fn_r_auto <- fn_r_auto
          dt_reactive$fn_af <- fn_af
          dt_reactive$fn_v_y <- fn_v_y
          dt_reactive$fn_r_y <- fn_r_y
          dt_reactive$fn_v_mt <- fn_v_mt
          dt_reactive$fn_r_mt <- fn_r_mt

          ######################
          # Finish calculation #
          ######################

          waiter_hide()
          run_time <- proc.time() - start_time
          cat(paste0("\n", "Calculation time : ", run_time[3], " sec", "\n"))

          enable("name_proj")
          enable("download_proj")
          enable(selector = '.navbar-nav a[data-value = "Result"]')
          updateNavbarPage(session, "navbar", selected = "Result")
          if(nrow(dt_reactive$dt_display) > max_data){
            showModal(modalDialog(title = "Information", paste0("Top ", max_data, " data is displayed."), easyClose = TRUE, footer = NULL))
          }
        }
      }
    })

    ########################
    # Display summary data #
    ########################

    output$summary_min_lr <- renderUI({numericInput("summary_min_lr", label = "Minimum LR displayed", value = rv_min_lr_auto())})

    observeEvent(input$act_default, {
      dt_reactive$dt_display <- create_displayed_data(dt_reactive$dt_combined)
      if(nrow(dt_reactive$dt_display) > max_data){
        showModal(modalDialog(title = "Information", paste0("Top ", max_data, " data is displayed."), easyClose = TRUE, footer = NULL))
      }
    })

    observeEvent(input$act_identified, {
      dt_reactive$dt_display <- create_displayed_data(dt_reactive$dt_combined, fltr_type = "identified")
    })

    observeEvent(input$act_multiple, {
      dt_reactive$dt_display <- create_displayed_data(dt_reactive$dt_combined, fltr_type = "multiple")
    })

    observeEvent(input$act_alert, {
      dt_reactive$dt_display <- create_displayed_data(dt_reactive$dt_combined, fltr_type = "alert")
    })

    observeEvent(input$act_fltr_lr, {
      summary_min_lr <- input$summary_min_lr
      if(isTruthy(summary_min_lr)){
        dt_reactive$dt_display <- create_displayed_data(dt_reactive$dt_combined, fltr_type = "min_lr", min_lr = summary_min_lr)
      }else{
        showModal(modalDialog(title = "Error", "Enter the minimum LR displayed!", easyClose = TRUE, footer = NULL))
      }
    })

    make_style_color_alert <- function(target_col){
      index_alert <- which(target_col == 2)
      color_display <- "red"
      if(length(index_alert) == 0){
        index_alert <- 1:length(target_col)
        color_display <- "black"
      }
      return(list(index_alert, color_display))
    }

    output$dt_display <- renderDataTable(server = FALSE, {
      dt_display <- dt_reactive$dt_display

      tmp <- make_style_color_alert(dt_display[, ColorY])
      index_alert_y <- tmp[[1]]
      color_display_y <- tmp[[2]]

      tmp <- make_style_color_alert(dt_display[, ColorMt])
      index_alert_mt <- tmp[[1]]
      color_display_mt <- tmp[[2]]

      datatable(
        dt_display,
        colnames = c("Victim", "Reference", "Assumed relationship", "LR", "Estimated relationship", "Paternal lineage", "Maternal lineage", "ColorBack", "ColorY", "ColorMt"),
        filter = "top",
        extensions = "Buttons",
        selection = list(mode = "single", target = "row"),
        options = list(iDisplayLength = 10, ordering = FALSE, autoWidth = TRUE,
                       dom = "Bfrtip",
                       buttons = list(list(extend = "csv",
                                           text = "Download",
                                           filename = paste0(gsub(" ", "_", format(as.POSIXct(Sys.time()), "%Y-%m-%d %H%M%S")), "_relsearch_result"),
                                           exportOptions = list(modifier = list(page = "all"),
                                                                columns = c(0:6))
                                           )
                                      ),
                       columnDefs = list(list(targets = 3, searchable = FALSE), list(targets = 7:9, visible = FALSE))
                       ),
        rownames = FALSE
      ) %>%
        formatStyle(columns = "ColorBack", target = "row", backgroundColor = styleEqual(c(0, 1, 2), c("#ffe0ef", "#e0ffe0", "#ffffe0"))) %>%
        formatStyle(columns = "Paternal", target = "cell", color = styleRow(index_alert_y, color_display_y)) %>%
        formatStyle(columns = "Maternal", target = "cell", color = styleRow(index_alert_mt, color_display_mt))
    })

    #########################
    # Display detailed data #
    #########################

    observeEvent(ignoreInit = TRUE, input$dt_display_rows_selected, {
      pos_select <- input$dt_display_rows_selected

      sn_v_select <- dt_reactive$dt_display[pos_select, Victim]
      sn_r_select <- dt_reactive$dt_display[pos_select, Reference]
      assumed_rel_select <- dt_reactive$dt_display[pos_select, AssumedRel]
      estimated_rel_select <- dt_reactive$dt_display[pos_select, EstimatedRel]
      if(is.na(estimated_rel_select)){
        estimated_rel_select <- "Not identified"
      }
      paternal_select <- dt_reactive$dt_display[pos_select, Paternal]
      if(is.na(paternal_select)){
        paternal_select <- "No data"
      }
      maternal_select <- dt_reactive$dt_display[pos_select, Maternal]
      if(is.na(maternal_select)){
        maternal_select <- "No data"
      }

      result_selected <- dt_reactive$dt_combined[.(sn_v_select, sn_r_select, assumed_rel_select)]

      if(!is.null(dt_reactive$dt_result_auto)){
        dt_detail_auto <- create_detailed_data_auto(dt_reactive$dt_v_auto, dt_reactive$dt_r_auto, sn_v_select, sn_r_select, assumed_rel_select, result_selected)
      }else{
        dt_detail_auto <- NULL
      }

      if(!is.null(dt_reactive$dt_result_y)){
        dt_detail_y <- create_detailed_data_y(dt_reactive$dt_v_y, dt_reactive$dt_r_y, sn_v_select, sn_r_select, assumed_rel_select, result_selected)
      }else{
        dt_detail_y <- NULL
      }

      if(!is.null(dt_reactive$dt_result_mt)){
        dt_detail_mt <- create_detailed_data_mt(dt_reactive$dt_v_mt, dt_reactive$dt_r_mt, sn_v_select, sn_r_select, assumed_rel_select, result_selected)

        output$num_mismatch_select <- renderText({paste0(result_selected[, MismatchMt])})
        output$share_range_select <- renderText({paste0(result_selected[, ShareRangeMt])})
        output$share_len_select <- renderText({paste0(result_selected[, ShareLengthMt])})
      }else{
        dt_detail_mt <- NULL
      }

      output$sn_v_select <- renderText({paste0(sn_v_select)})
      output$sn_r_select <- renderText({paste0(sn_r_select)})
      output$estimated_rel_select <- renderText({paste0(estimated_rel_select)})
      output$paternal_select <- renderText({paste0(paternal_select)})
      output$maternal_select <- renderText({paste0(maternal_select)})

      output$dt_detail_auto <- renderDataTable(server = FALSE, {
        datatable(
          dt_detail_auto,
          colnames = c("Locus", "Victim profile", "Reference profile", "Likelihood (related)", "Likelihood (unrelated)", "LR"),
          extensions = "Buttons",
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE,
                         dom = "Bfrtip",
                         buttons = list(list(extend = "csv",
                                             text = "Download",
                                             filename = paste0(gsub(" ", "_", format(as.POSIXct(Sys.time()), "%Y-%m-%d %H%M%S")), "_relsearch_detail_STR"),
                                             exportOptions = list(modifier = list(page = "all"))
                         )
                         )
          ),
          rownames = FALSE
        )
      })

      output$dt_detail_y <- renderDataTable(server = FALSE, {
        datatable(
          dt_detail_y,
          colnames = c("Locus", "Victim profile", "Reference profile", "Ignored locus", "Mismatched locus", "Mutational step"),
          extensions = "Buttons",
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE,
                         dom = "Bfrtip",
                         buttons = list(list(extend = "csv",
                                             text = "Download",
                                             filename = paste0(gsub(" ", "_", format(as.POSIXct(Sys.time()), "%Y-%m-%d %H%M%S")), "_relsearch_detail_Y-STR"),
                                             exportOptions = list(modifier = list(page = "all"))
                         )
                         )
          ),
          rownames = FALSE
        )
      })

      output$dt_detail_mt <- renderDataTable(server = FALSE, {
        datatable(
          dt_detail_mt,
          colnames = c("Victim profile", "Reference profile", "Out of shared range", "Mismatch"),
          extensions = "Buttons",
          selection = "none",
          options = list(iDisplayLength = 50, ordering = FALSE,
                         dom = "Bfrtip",
                         buttons = list(list(extend = "csv",
                                             text = "Download",
                                             filename = paste0(gsub(" ", "_", format(as.POSIXct(Sys.time()), "%Y-%m-%d %H%M%S")), "_relsearch_detail_mtDNA"),
                                             exportOptions = list(modifier = list(page = "all"))
                         )
                         )
          ),
          rownames = FALSE
        )
      })
    })

    ###############################
    # Display analysis conditions #
    ###############################

    output$result_fn_v_auto <- renderText({paste0(dt_reactive$fn_v_auto)})
    output$result_fn_r_auto <- renderText({paste0(dt_reactive$fn_r_auto)})
    output$result_fn_af <- renderText({paste0(dt_reactive$fn_af)})
    output$result_fn_v_y <- renderText({paste0(dt_reactive$fn_v_y)})
    output$result_fn_r_y <- renderText({paste0(dt_reactive$fn_r_y)})
    output$result_fn_v_mt <- renderText({paste0(dt_reactive$fn_v_mt)})
    output$result_fn_r_mt <- renderText({paste0(dt_reactive$fn_r_mt)})

    output$result_min_lr_auto <- renderText({paste0(dt_reactive$dt_criteria$Value[dt_reactive$dt_criteria$Criteria == "min_lr_auto"])})
    output$result_max_mismatch_y <- renderText({paste0(dt_reactive$dt_criteria$Value[dt_reactive$dt_criteria$Criteria == "max_mismatch_y"])})
    output$result_max_ignore_y <- renderText({paste0(dt_reactive$dt_criteria$Value[dt_reactive$dt_criteria$Criteria == "max_ignore_y"])})
    output$result_max_mustep_y <- renderText({paste0(dt_reactive$dt_criteria$Value[dt_reactive$dt_criteria$Criteria == "max_mustep_y"])})
    output$result_max_mismatch_mt <- renderText({paste0(dt_reactive$dt_criteria$Value[dt_reactive$dt_criteria$Criteria == "max_mismatch_mt"])})
    output$result_min_share_mt <- renderText({paste0(dt_reactive$dt_criteria$Value[dt_reactive$dt_criteria$Criteria == "min_share_mt"])})

    output$result_assumed_rel <- renderDataTable(server = FALSE, {
      datatable(
        dt_reactive$dt_rel,
        colnames = c("Relationship", "Victim", "Reference", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)", "Paternal lineage", "Maternal lineage",
                     "Tree_persons", "Tree_sexes", "Tree_fathers", "Tree_mothers", "Tree_founders"),
        selection = list(mode = "single", target = "row"),
        options = list(iDisplayLength = 10, ordering = FALSE,
                       columnDefs = list(list(targets = 8:12, visible = FALSE))
        ),
        rownames = FALSE
      )
    })

    output$result_myu <- renderDataTable(server = FALSE, {
      datatable(
        dt_reactive$dt_myu,
        colnames = c("Locus", "Mutation rates"),
        selection = "none",
        options = list(iDisplayLength = 50, ordering = FALSE),
        rownames = FALSE
      )
    })

    output$result_maf <- renderText({paste0(dt_reactive$dt_par_auto$Value[dt_reactive$dt_par_auto$Parameter == "maf"])})

    ###############
    # New project #
    ###############

    observeEvent(input$act_new_proj, {
      refresh()
    })

    ################
    # Load project #
    ################

    load_proj <- reactive({
      file_input <- input$file_proj
      if(is.null(file_input)){
        return(NULL)
      }else{
        return("OK")
      }
    })

    observeEvent(input$act_load_proj, {
      proj <- load_proj()
      if(is.null(proj)){
        showModal(modalDialog(title = "Error", "Select a project file!", easyClose = TRUE, footer = NULL))
      }else{
        waiter_show(html = spin_3k(), color = "white")

        load(input$file_proj$datapath)

        dt_reactive$dt_combined <- dt_combined
        dt_reactive$dt_display <- dt_display
        dt_reactive$dt_result_auto <- dt_result_auto
        dt_reactive$dt_v_auto <- dt_v_auto
        dt_reactive$dt_r_auto <- dt_r_auto
        dt_reactive$dt_af <- dt_af
        dt_reactive$dt_result_y <- dt_result_y
        dt_reactive$dt_v_y <- dt_v_y
        dt_reactive$dt_r_y <- dt_r_y
        dt_reactive$dt_result_mt <- dt_result_mt
        dt_reactive$dt_v_mt <- dt_v_mt
        dt_reactive$dt_r_mt <- dt_r_mt
        dt_reactive$dt_criteria <- dt_criteria
        dt_reactive$dt_rel <- dt_rel
        dt_reactive$dt_myu <- dt_myu
        dt_reactive$dt_par_auto <- dt_par_auto

        dt_reactive$fn_v_auto <- fn_v_auto
        dt_reactive$fn_r_auto <- fn_r_auto
        dt_reactive$fn_af <- fn_af
        dt_reactive$fn_v_y <- fn_v_y
        dt_reactive$fn_r_y <- fn_r_y
        dt_reactive$fn_v_mt <- fn_v_mt
        dt_reactive$fn_r_mt <- fn_r_mt

        output$view_dt_v_auto <- renderDataTable({
          datatable(
            dt_v_auto,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        output$view_dt_r_auto <- renderDataTable({
          datatable(
            dt_r_auto,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        output$view_dt_af <- renderDataTable({
          datatable(
            dt_af,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        output$view_dt_v_y <- renderDataTable({
          datatable(
            dt_v_y,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        output$view_dt_r_y <- renderDataTable({
          datatable(
            dt_r_y,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        output$view_dt_v_mt <- renderDataTable({
          datatable(
            dt_v_mt,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        output$view_dt_r_mt <- renderDataTable({
          datatable(
            dt_r_mt,
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        waiter_hide()

        enable("name_proj")
        enable("download_proj")
        enable(selector = '.navbar-nav a[data-value = "Result"]')
        updateNavbarPage(session, "navbar", selected = "Result")
        showModal(modalDialog(title = "Information", "Displayed data satisfies at least one of the criteria for STR, Y-STR, and mtDNA.", easyClose = TRUE, footer = NULL))
      }
    })

    ################
    # Save project #
    ################

    output$download_proj <- downloadHandler(
      filename = function(){
        if(isTruthy(input$name_proj)){
          paste0(input$name_proj, ".RData")
        }else{
          paste0(gsub(" ", "_", format(as.POSIXct(Sys.time()), "%Y-%m-%d %H%M%S")), "_relsearch_project.RData")
        }
      },

      content = function(file){
        waiter_show(html = spin_3k(), color = "white")

        dt_combined <- dt_reactive$dt_combined
        dt_display <- dt_reactive$dt_display
        dt_result_auto <- dt_reactive$dt_result_auto
        dt_v_auto <- dt_reactive$dt_v_auto
        dt_r_auto <- dt_reactive$dt_r_auto
        dt_af <- dt_reactive$dt_af
        dt_result_y <- dt_reactive$dt_result_y
        dt_v_y <- dt_reactive$dt_v_y
        dt_r_y <- dt_reactive$dt_r_y
        dt_result_mt <- dt_reactive$dt_result_mt
        dt_v_mt <- dt_reactive$dt_v_mt
        dt_r_mt <- dt_reactive$dt_r_mt
        dt_criteria <- dt_reactive$dt_criteria
        dt_rel <- dt_reactive$dt_rel
        dt_myu <- dt_reactive$dt_myu
        dt_par_auto <- dt_reactive$dt_par_auto

        fn_v_auto <- dt_reactive$fn_v_auto
        fn_r_auto <- dt_reactive$fn_r_auto
        fn_af <- dt_reactive$fn_af
        fn_v_y <- dt_reactive$fn_v_y
        fn_r_y <- dt_reactive$fn_r_y
        fn_v_mt <- dt_reactive$fn_v_mt
        fn_r_mt <- dt_reactive$fn_r_mt

        save(list = c("dt_combined", "dt_display",
                      "dt_result_auto", "dt_v_auto", "dt_r_auto", "dt_af",
                      "dt_result_y", "dt_v_y", "dt_r_y",
                      "dt_result_mt", "dt_v_mt", "dt_r_mt",
                      "dt_criteria", "dt_rel", "dt_myu", "dt_par_auto",
                      "fn_v_auto", "fn_r_auto", "fn_af",
                      "fn_v_y", "fn_r_y",
                      "fn_v_mt", "fn_r_mt"), file = file)

        waiter_hide()
      }
    )
  }

  shinyApp(ui, server)
}
