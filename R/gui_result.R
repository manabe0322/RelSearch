#' result_ui
#'
#' @description The function to create the ui module for displaying results
result_ui <- function(id){
  tabPanel("Result",
           titlePanel("Result"),
           br(),
           tabsetPanel(ns("tab_result"),
                       tabPanel("Summary",
                                br(),
                                sidebarLayout(
                                  sidebarPanel(
                                    h4("Display setting"),
                                    br(),
                                    actionButton(ns("act_default"), label = "Default display", class = "btn btn-primary"),
                                    br(),
                                    br(),
                                    actionButton(ns("act_identified"), label = "Identified pairs", class = "btn btn-success"),
                                    br(),
                                    br(),
                                    actionButton(ns("act_multiple"), label = "Multiple candidates", class = "btn btn-warning"),
                                    br(),
                                    br(),
                                    actionButton(ns("act_warning"), label = "Not support lineage", class = "btn btn-danger"),
                                    br(),
                                    br(),
                                    uiOutput(ns("summary_min_lr")),
                                    actionButton(ns("act_fltr_lr"), label = "Apply"),
                                    width = 3
                                  ),
                                  mainPanel(
                                    br(),
                                    dataTableOutput(ns("dt_display")),
                                    width = 9
                                  )
                                )
                       ),
                       tabPanel("Selected data in detail",
                                br(),
                                sidebarLayout(
                                  sidebarPanel(
                                    h4("Victim"),
                                    textOutput(ns("sn_v_select")),
                                    br(),
                                    h4("Reference"),
                                    textOutput(ns("sn_r_select")),
                                    br(),
                                    h4("Estimated relationship"),
                                    textOutput(ns("estimated_rel_select")),
                                    br(),
                                    h4("Paternal lineage"),
                                    textOutput(ns("paternal_select")),
                                    br(),
                                    h4("Maternal lineage"),
                                    textOutput(ns("maternal_select")),
                                    width = 2
                                  ),
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("STR",
                                               br(),
                                               dataTableOutput(ns("dt_detail_auto"))
                                      ),
                                      tabPanel("Y-STR",
                                               br(),
                                               dataTableOutput(ns("dt_detail_y"))
                                      ),
                                      tabPanel("mtDNA",
                                               br(),
                                               fluidRow(
                                                 column(4,
                                                        h4("Number of mismatches"),
                                                        textOutput(ns("num_mismatch_select"))
                                                 ),
                                                 column(4,
                                                        h4("Shared range"),
                                                        textOutput(ns("share_range_select"))
                                                 ),
                                                 column(4,
                                                        h4("Shared length (bp)"),
                                                        textOutput(ns("share_len_select"))
                                                 )
                                               ),
                                               br(),
                                               br(),
                                               dataTableOutput(ns("dt_detail_mt"))
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
                                                    textOutput(ns("result_fn_v_auto")),
                                                    br(),
                                                    h5(div("Reference database", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_fn_r_auto")),
                                                    br(),
                                                    h5(div("Allele frequencies", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_fn_af"))
                                             ),
                                             column(4,
                                                    h4("Y-STR"),
                                                    br(),
                                                    h5(div("Victim database", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_fn_v_y")),
                                                    br(),
                                                    h5(div("Reference database", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_fn_r_y"))
                                             ),
                                             column(4,
                                                    h4("mtDNA"),
                                                    br(),
                                                    h5(div("Victim database", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_fn_v_mt")),
                                                    br(),
                                                    h5(div("Reference database", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_fn_r_mt"))
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
                                                    textOutput(ns("result_min_lr_auto"))
                                             ),
                                             column(4,
                                                    h4("Y-STR"),
                                                    br(),
                                                    h5(div("Maximum number of mismatched loci", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_max_mismatch_y")),
                                                    br(),
                                                    h5(div("Maximum number of ignored loci", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_max_ignore_y")),
                                                    br(),
                                                    h5(div("Maximum total mutational steps", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_max_mustep_y"))
                                             ),
                                             column(4,
                                                    h4("mtDNA"),
                                                    br(),
                                                    h5(div("Maximum number of inconsistency", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_max_mismatch_mt")),
                                                    br(),
                                                    h5(div("Minimum shared length", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_min_share_mt"))
                                             )
                                           )
                                  ),
                                  tabPanel("Assumed relationship",
                                           br(),
                                           sidebarLayout(
                                             sidebarPanel(width = 2,
                                                          actionButton(ns("act_assumed_rel_famtree"), "Family tree")
                                             ),
                                             mainPanel(width = 10,
                                                       dataTableOutput(ns("result_assumed_rel"))
                                             )
                                           )
                                  ),
                                  tabPanel("Mutation rate",
                                           br(),
                                           dataTableOutput(ns("result_myu"))
                                  ),
                                  tabPanel("Parameter",
                                           br(),
                                           h5(div("Minimum allele frequency", style = "color:#555555;font-weight:bold;")),
                                           textOutput(ns("result_maf"))
                                  )
                                )
                       ),
           )
  )
}

#' result_server
#'
#' @description The function to create the server module for displaying results
result_server <- function(id, rv_file){
  moduleServer(
    id,
    function(input, output, session){
      data_list <- rv_file$data_list
      if(!is.null(data_list)){
        bool_check_auto <- data_list$bool_check_auto
        bool_check_y <- data_list$bool_check_y
        bool_check_mt <- data_list$bool_check_mt
        dt_combined <- data_list$dt_combined
        dt_display <- data_list$dt_display
        dt_v_auto <- data_list$dt_v_auto
        dt_r_auto <- data_list$dt_r_auto
        dt_af <- data_list$dt_af
        dt_v_y <- data_list$dt_v_y
        dt_r_y <- data_list$dt_r_y
        dt_v_mt <- data_list$dt_v_mt
        dt_r_mt <- data_list$dt_r_mt
        dt_criteria <- data_list$dt_criteria
        dt_rel <- data_list$dt_rel
        dt_myu <- data_list$dt_myu
        dt_par_auto <- data_list$dt_par_auto
        fn_v_auto <- data_list$fn_v_auto
        fn_r_auto <- data_list$fn_r_auto
        fn_af <- data_list$fn_af
        fn_v_y <- data_list$fn_v_y
        fn_r_y <- data_list$fn_r_y
        fn_v_mt <- data_list$fn_v_mt
        fn_r_mt <- data_list$fn_r_mt

        rv_result <- reactiveValues()
        rv_result$dt_display <- dt_display

        ########################
        # Display summary data #
        ########################

        output$summary_min_lr <- renderUI({
          numericInput(session$ns("summary_min_lr"), label = "Minimum LR displayed", value = dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"])
        })

        observeEvent(input$act_default, {
          if(bool_check_auto){
            dt_display <- create_displayed_data(dt_combined, fltr_type = "with_auto", min_lr = dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"], max_data = max_data)
          }else{
            dt_display <- create_displayed_data(dt_combined, fltr_type = "without_auto", max_data = max_data)
          }
          isolate(rv_result$dt_display <- dt_display)
          if(nrow(dt_display) == max_data){
            showModal(modalDialog(title = "Information", paste0("Top ", max_data, " data is displayed."), easyClose = TRUE, footer = NULL))
          }else if(bool_check_auto){
            showModal(modalDialog(title = "Information", "Data that satisfies the criterion of the minimum LR is displayed.", easyClose = TRUE, footer = NULL))
          }else{
            showModal(modalDialog(title = "Information", "Data that satisfies the criteria for Y-STR or mtDNA is displayed.", easyClose = TRUE, footer = NULL))
          }
        })

        observeEvent(input$act_identified, {
          dt_display <- create_displayed_data(dt_combined, fltr_type = "identified", max_data = max_data)
          isolate(rv_result$dt_display <- dt_display)
          if(nrow(dt_display) == max_data){
            showModal(modalDialog(title = "Information", paste0("Top ", max_data, " data is displayed."), easyClose = TRUE, footer = NULL))
          }
        })

        observeEvent(input$act_multiple, {
          dt_display <- create_displayed_data(dt_combined, fltr_type = "multiple", max_data = max_data)
          isolate(rv_result$dt_display <- dt_display)
          if(nrow(dt_display) == max_data){
            showModal(modalDialog(title = "Information", paste0("Top ", max_data, " data is displayed."), easyClose = TRUE, footer = NULL))
          }
        })

        observeEvent(input$act_warning, {
          dt_display <- create_displayed_data(dt_combined, fltr_type = "warning", max_data = max_data)
          isolate(rv_result$dt_display <- dt_display)
          if(nrow(dt_display) == max_data){
            showModal(modalDialog(title = "Information", paste0("Top ", max_data, " data is displayed."), easyClose = TRUE, footer = NULL))
          }
        })

        observeEvent(input$summary_min_lr, {
          summary_min_lr <- input$summary_min_lr
          if(!is.numeric(summary_min_lr) || summary_min_lr < keep_min_lr){
            showFeedbackDanger(inputId = "summary_min_lr", text = paste0("The minimum LR value should be greater than ", keep_min_lr, "."))
            disable("act_fltr_lr")
          }else{
            hideFeedback("summary_min_lr")
            enable("act_fltr_lr")
          }
        })

        observeEvent(input$act_fltr_lr, {
          summary_min_lr <- input$summary_min_lr
          if(isTruthy(summary_min_lr)){
            if(summary_min_lr >= keep_min_lr){
              dt_display <- create_displayed_data(dt_combined, fltr_type = "with_auto", min_lr = summary_min_lr, max_data = max_data)
              isolate(rv_result$dt_display <- dt_display)
              if(nrow(dt_display) == max_data){
                showModal(modalDialog(title = "Information", paste0("Top ", max_data, " data is displayed."), easyClose = TRUE, footer = NULL))
              }
            }
          }
        })

        output$dt_display <- renderDataTable(server = FALSE, {
          dt_display <- rv_result$dt_display

          # The function to change colors does not work when dealing with big data
          #index_warning_y <- which(dt_display[, ColorY] == 2)
          #color_display_y <- "red"
          #if(length(index_warning_y) == 0){
          #  index_warning_y <- 1
          #  color_display_y <- "#333333"
          #}

          #index_warning_mt <- which(dt_display[, ColorMt] == 2)
          #color_display_mt <- "red"
          #if(length(index_warning_mt) == 0){
          #  index_warning_mt <- 1
          #  color_display_mt <- "#333333"
          #}

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
            formatStyle(columns = "ColorBack", target = "row", backgroundColor = styleEqual(c(0, 1, 2), c("#ffe0ef", "#e0ffe0", "#ffffe0"))) #%>%
          #formatStyle(columns = "Paternal", target = "cell", color = styleRow(index_warning_y, color_display_y)) %>%
          #formatStyle(columns = "Maternal", target = "cell", color = styleRow(index_warning_mt, color_display_mt))
        })

        #########################
        # Display detailed data #
        #########################

        observeEvent(ignoreInit = TRUE, input$dt_display_rows_selected, {
          pos_select <- input$dt_display_rows_selected

          sn_v_select <- dt_display[pos_select, Victim]
          sn_r_select <- dt_display[pos_select, Reference]
          assumed_rel_select <- dt_display[pos_select, AssumedRel]
          estimated_rel_select <- dt_display[pos_select, EstimatedRel]
          if(is.na(estimated_rel_select)){
            estimated_rel_select <- "Not identified"
          }
          paternal_select <- dt_display[pos_select, Paternal]
          if(is.na(paternal_select)){
            paternal_select <- "No data"
          }
          maternal_select <- dt_display[pos_select, Maternal]
          if(is.na(maternal_select)){
            maternal_select <- "No data"
          }

          result_selected <- dt_combined[.(sn_v_select, sn_r_select, assumed_rel_select)]

          if(bool_check_auto){
            dt_detail_auto <- create_detailed_data_auto(dt_v_auto, dt_r_auto, sn_v_select, sn_r_select, assumed_rel_select, result_selected)
          }else{
            dt_detail_auto <- NULL
          }

          if(data_list$bool_check_y){
            dt_detail_y <- create_detailed_data_y(dt_v_y, dt_r_y, sn_v_select, sn_r_select, assumed_rel_select, result_selected)
          }else{
            dt_detail_y <- NULL
          }

          if(data_list$bool_check_mt){
            dt_detail_mt <- create_detailed_data_mt(dt_v_mt, dt_r_mt, sn_v_select, sn_r_select, assumed_rel_select, result_selected)

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

        output$result_fn_v_auto <- renderText({isolate(paste0(fn_v_auto))})
        output$result_fn_r_auto <- renderText({isolate(paste0(fn_r_auto))})
        output$result_fn_af <- renderText({isolate(paste0(fn_af))})
        output$result_fn_v_y <- renderText({isolate(paste0(fn_v_y))})
        output$result_fn_r_y <- renderText({isolate(paste0(fn_r_y))})
        output$result_fn_v_mt <- renderText({isolate(paste0(fn_v_mt))})
        output$result_fn_r_mt <- renderText({isolate(paste0(fn_r_mt))})

        output$result_min_lr_auto <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"])
        })
        output$result_max_mismatch_y <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_y"])
        })
        output$result_max_ignore_y <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "max_ignore_y"])
        })
        output$result_max_mustep_y <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "max_mustep_y"])
        })
        output$result_max_mismatch_mt <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_mt"])
        })
        output$result_min_share_mt <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "min_share_mt"])
        })

        output$result_assumed_rel <- renderDataTable(server = FALSE, {
          datatable(
            dt_rel,
            colnames = c("Relationship", "Victim", "Reference", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)", "Paternal lineage", "Maternal lineage",
                         "Tree_persons", "Tree_sexes", "Tree_fathers", "Tree_mothers", "Tree_founders"),
            selection = list(mode = "single", target = "row"),
            options = list(iDisplayLength = 10, ordering = FALSE,
                           columnDefs = list(list(targets = 8:12, visible = FALSE))
            ),
            rownames = FALSE
          )
        })

        observeEvent(input$act_assumed_rel_famtree, {
          pos_select <- input$result_assumed_rel_rows_selected
          if(isTruthy(pos_select)){
            names_all <- dt_rel[, Relationship]
            persons_all <- dt_rel[, Tree_persons]
            sexes_all <- dt_rel[, Tree_sexes]
            fathers_all <- dt_rel[, Tree_fathers]
            mothers_all <- dt_rel[, Tree_mothers]
            founders_all <- dt_rel[, Tree_founders]

            name_select <- names_all[pos_select]
            person_select <- strsplit(persons_all[pos_select], ", ")[[1]]
            sex_select <- strsplit(sexes_all[pos_select], ", ")[[1]]
            father_select <- strsplit(fathers_all[pos_select], ", ")[[1]]
            mother_select <- strsplit(mothers_all[pos_select], ", ")[[1]]
            founder_select <- strsplit(founders_all[pos_select], ", ")[[1]]
            tree <- check_tree(person_select, sex_select, father_select, mother_select, founder_select)

            showModal(modalDialog(
              title = paste0("Relationship: ", name_select),
              plotOutput("tree_check"),
              footer = tagList(
                modalButton("Close")
              )
            ))

            output$tree_check <- renderPlot({
              plot(tree, hatched = c("Victim", "Ref"))
            })
          }else{
            showModal(modalDialog(title = "Error", "Select a relationship!", easyClose = TRUE, footer = NULL))
          }
        })

        output$result_myu <- renderDataTable(server = FALSE, {
          datatable(
            dt_myu,
            colnames = c("Locus", "Mutation rates"),
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        output$result_maf <- renderText({paste0(dt_par_auto$Value[dt_par_auto$Parameter == "maf"])})
      }
    }
  )
}
