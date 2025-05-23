#' result_ui
#'
#' @description The function to create the ui module for displaying results
result_ui <- function(id){
  ns <- NS(id)

  tabPanel("Result",
           useShinyjs(),
           useShinyFeedback(),
           titlePanel("Result"),
           br(),
           tabsetPanel(id = ns("tab_result"),
                       type = "tabs",
                       tabPanel("Summary",
                                br(),
                                fluidRow(
                                  column(2,
                                         wellPanel(
                                           h4("Display setting"),
                                           br(),
                                           actionButton(ns("act_default"), label = "Default display", class = "btn btn-primary"),
                                           br(),
                                           br(),
                                           actionButton(ns("act_identified"), label = "Identified", class = "btn btn-success"),
                                           br(),
                                           br(),
                                           actionButton(ns("act_multiple"), label = "Multiple candidates", class = "btn btn-info"),
                                           br(),
                                           br(),
                                           actionButton(ns("act_inconclusive"), label = "Inconclusive", class = "btn-warning"),
                                           br(),
                                           br(),
                                           actionButton(ns("act_excluded"), label = "Excluded", class = "btn btn-danger"),
                                           br(),
                                           br(),
                                           uiOutput(ns("summary_min_lr")),
                                           actionButton(ns("act_fltr_lr"), label = "Apply")
                                         ),
                                         downloadButton(ns("download_summary"), "Download", class = "btn btn-primary btn-lg")
                                  ),
                                  column(10,
                                         br(),
                                         dataTableOutput(ns("dt_display"))
                                  )
                                )
                       ),
                       tabPanel("Selected data in detail",
                                br(),
                                fluidRow(
                                  column(3,
                                         wellPanel(
                                           h4("Selected victim"),
                                           textOutput(ns("sn_v_select_detail")),
                                           br(),
                                           h4("Selected reference"),
                                           textOutput(ns("sn_r_select_detail")),
                                           br(),
                                           h4("Estimated relationship"),
                                           textOutput(ns("estimated_rel_select")),
                                           br(),
                                           h4("Paternal lineage"),
                                           textOutput(ns("paternal_select")),
                                           br(),
                                           h4("Maternal lineage"),
                                           textOutput(ns("maternal_select")),
                                           br(),
                                           h4("All family members"),
                                           textOutput(ns("family_member"))
                                         ),
                                         disabled(downloadButton(ns("download_auto"), "Download (STR)")),
                                         br(),
                                         br(),
                                         disabled(downloadButton(ns("download_y"), "Download (Y-STR)")),
                                         br(),
                                         br(),
                                         disabled(downloadButton(ns("download_mt"), "Download (mtDNA)"))
                                  ),
                                  column(9,
                                         tabsetPanel(
                                           tabPanel("STR",
                                                    br(),
                                                    dataTableOutput(ns("dt_detail_auto")),
                                                    br()
                                           ),
                                           tabPanel("Y-STR",
                                                    br(),
                                                    dataTableOutput(ns("dt_detail_y")),
                                                    br()
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
                                                    dataTableOutput(ns("dt_detail_mt")),
                                                    br()
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
                                  tabPanel("Allele probability",
                                           br(),
                                           fluidRow(
                                             column(6,
                                                    h4("Unobserved alleles in the population database"),
                                                    dataTableOutput(ns("dt_unobs_al"))
                                             ),
                                             column(6,
                                                    h4("Allele probabilities used"),
                                                    downloadButton(ns("download_af_use"), "Download")
                                             )
                                           )
                                  ),
                                  tabPanel("Criteria",
                                           br(),
                                           fluidRow(
                                             column(4,
                                                    h4("LR Criteria"),
                                                    br(),
                                                    h5(div("Minimum LR to support the assumed relationship", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_min_lr_auto")),
                                                    br(),
                                                    h5(div("Maximum LR to exclude the assumed relationship", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_max_lr_auto"))
                                             ),
                                             column(4,
                                                    h4("Y-STR Usage Criteria"),
                                                    br(),
                                                    h5(div("Minimum number of loci with genotypes", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_min_detect_y")),
                                                    br(),
                                                    h4("Paternal Lineage Criteria"),
                                                    br(),
                                                    h5(div("Maximum number of mismatched loci", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_max_mismatch_y")),
                                                    br(),
                                                    h5(div("Upper limit of the total mutational steps", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_max_mustep_y"))
                                             ),
                                             column(4,
                                                    h4("mtDNA Usage Criteria"),
                                                    br(),
                                                    h5(div("Minimum read length (bp)", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_min_share_len_mt")),
                                                    br(),
                                                    h4("Maternal Lineage Criteria"),
                                                    br(),
                                                    h5(div("Maximum number of inconsistency", style = "color:#555555;font-weight:bold;")),
                                                    textOutput(ns("result_max_mismatch_mt"))
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
                                  tabPanel("Data storage conditions",
                                           br(),
                                           h5(div("Minimum LR to be stored", style = "color:#555555;font-weight:bold;")),
                                           textOutput(ns("result_keep_min_lr")),
                                           br(),
                                           h5(div("Maximum data displayed", style = "color:#555555;font-weight:bold;")),
                                           textOutput(ns("result_max_data_displayed"))
                                  )
                                )
                       ),
           )
  )
}

#' result_server
#'
#' @description The function to create the server module for displaying results
#' @param rv_file The reactive valus for the load data and the analysis results
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
        dt_af_use <- data_list$dt_af_use
        dt_unobs_al <- data_list$dt_unobs_al
        dt_v_y <- data_list$dt_v_y
        dt_r_y <- data_list$dt_r_y
        dt_v_mt <- data_list$dt_v_mt
        dt_r_mt <- data_list$dt_r_mt
        dt_criteria <- data_list$dt_criteria
        dt_rel <- data_list$dt_rel
        dt_myu <- data_list$dt_myu
        dt_data_manage <- data_list$dt_data_manage
        max_data_displayed <- dt_data_manage$Value[dt_data_manage$Parameter == "max_data_displayed"]
        fn_v_auto <- data_list$fn_v_auto
        fn_r_auto <- data_list$fn_r_auto
        fn_af <- data_list$fn_af
        fn_v_y <- data_list$fn_v_y
        fn_r_y <- data_list$fn_r_y
        fn_v_mt <- data_list$fn_v_mt
        fn_r_mt <- data_list$fn_r_mt

        rv_result <- reactiveValues()
        rv_result$dt_display <- dt_display
        rv_result$dt_detail_auto <- NULL
        rv_result$dt_detail_y <- NULL
        rv_result$dt_detail_mt <- NULL
        rv_result$sn_v_select_detail <- NULL
        rv_result$sn_r_select_detail <- NULL
        rv_result$family_member <- NULL
        rv_result$estimated_rel_select <- NULL
        rv_result$paternal_select <- NULL
        rv_result$maternal_select <- NULL
        rv_result$mismatch_mt <- NULL
        rv_result$share_range_mt <- NULL
        rv_result$share_length_mt <- NULL

        ########################
        # Display summary data #
        ########################

        output$summary_min_lr <- renderUI({
          numericInput(session$ns("input_summary_min_lr"), label = "Minimum LR displayed", value = dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"])
        })

        observeEvent(input$act_default, {
          if(bool_check_auto){
            dt_display <- create_displayed_data(dt_combined, fltr_type = "with_auto", min_lr = dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"], max_data_displayed = max_data_displayed)
          }else{
            dt_display <- create_displayed_data(dt_combined, fltr_type = "without_auto", max_data_displayed = max_data_displayed)
          }
          rv_result$dt_display <- dt_display
          if(nrow(dt_display) == max_data_displayed){
            showModal(modalDialog(title = "Information", paste0("Top ", max_data_displayed, " data is displayed."), easyClose = TRUE, footer = NULL))
          }else if(bool_check_auto){
            showModal(modalDialog(title = "Information", paste0("Data with LR greater than ", dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"], " is displayed."), easyClose = TRUE, footer = NULL))
          }else{
            showModal(modalDialog(title = "Information", "Data that is not excluded from paternal or maternal lineages is displayed.", easyClose = TRUE, footer = NULL))
          }
        }, ignoreInit = TRUE)

        observeEvent(input$act_identified, {
          dt_display <- create_displayed_data(dt_combined, fltr_type = "identified", max_data_displayed = max_data_displayed)
          rv_result$dt_display <- dt_display
          if(nrow(dt_display) == max_data_displayed){
            showModal(modalDialog(title = "Information", paste0("Top ", max_data_displayed, " data is displayed."), easyClose = TRUE, footer = NULL))
          }
        }, ignoreInit = TRUE)

        observeEvent(input$act_multiple, {
          dt_display <- create_displayed_data(dt_combined, fltr_type = "multiple", max_data_displayed = max_data_displayed)
          rv_result$dt_display <- dt_display
          if(nrow(dt_display) == max_data_displayed){
            showModal(modalDialog(title = "Information", paste0("Top ", max_data_displayed, " data is displayed."), easyClose = TRUE, footer = NULL))
          }
        }, ignoreInit = TRUE)

        observeEvent(input$act_inconclusive, {
          dt_display <- create_displayed_data(dt_combined, fltr_type = "inconclusive", max_data_displayed = max_data_displayed)
          rv_result$dt_display <- dt_display
          if(nrow(dt_display) == max_data_displayed){
            showModal(modalDialog(title = "Information", paste0("Top ", max_data_displayed, " data is displayed."), easyClose = TRUE, footer = NULL))
          }
        }, ignoreInit = TRUE)

        observeEvent(input$act_excluded, {
          dt_display <- create_displayed_data(dt_combined, fltr_type = "excluded", max_data_displayed = max_data_displayed)
          rv_result$dt_display <- dt_display
          if(nrow(dt_display) == max_data_displayed){
            showModal(modalDialog(title = "Information", paste0("Top ", max_data_displayed, " data is displayed."), easyClose = TRUE, footer = NULL))
          }
        }, ignoreInit = TRUE)

        observeEvent(input$input_summary_min_lr, {
          summary_min_lr <- input$input_summary_min_lr
          if(!is.numeric(summary_min_lr)){
            hideFeedback("input_summary_min_lr")
            disable("act_fltr_lr")
          }else if(summary_min_lr < 0){
            showFeedbackDanger(inputId = "input_summary_min_lr", text = "The negative number is not allowed.")
            disable("act_fltr_lr")
          }else{
            hideFeedback("input_summary_min_lr")
            enable("act_fltr_lr")
          }
        }, ignoreInit = TRUE)

        observeEvent(input$act_fltr_lr, {
          summary_min_lr <- input$input_summary_min_lr
          if(isTruthy(summary_min_lr)){
            if(summary_min_lr >= 0){
              dt_display <- create_displayed_data(dt_combined, fltr_type = "with_auto", min_lr = summary_min_lr, max_data_displayed = max_data_displayed)
              rv_result$dt_display <- dt_display
              if(nrow(dt_display) == max_data_displayed){
                showModal(modalDialog(title = "Information", paste0("Top ", max_data_displayed, " data is displayed."), easyClose = TRUE, footer = NULL))
              }
            }
          }
        }, ignoreInit = TRUE)

        output$dt_display <- renderDataTable(server = FALSE, {
          dt_display <- rv_result$dt_display

          datatable(
            dt_display,
            colnames = c("Victim", "Reference", "Family", "Assumed relationship", "Estimated relationship", "LR", "Paternal lineage", "Maternal lineage", "Group of candidates", "ColorBack"),
            filter = "top",
            selection = list(mode = "single", target = "row"),
            options = list(iDisplayLength = 10, autoWidth = TRUE,
                           columnDefs = list(list(targets = 5, searchable = FALSE), list(targets = 9, visible = FALSE))
            ),
            rownames = FALSE
          ) %>%
            formatSignif(columns = c("LR_Total"), digits = 3) %>%
            formatStyle(columns = "ColorBack", target = "row", backgroundColor = styleEqual(c(0, 1, 2, 3), c("#ffe0ef", "#ffffe0", "#e0efff", "#e0ffe0")))
        })

        output$download_summary <- downloadHandler(
          filename = paste0(gsub(" ", "_", format(as.POSIXct(Sys.time()), "%Y-%m-%d %H%M%S")), "_RelSearch_summary_data.csv"),
          content = function(file){
            dt_download <- copy(rv_result$dt_display)
            dt_download[, ColorBack:=NULL]
            colnames(dt_download) <- c("Victim", "Reference", "Family", "Assumed relationship", "Estimated relationship", "LR", "Paternal lineage", "Maternal lineage", "Group of candidates")
            write.csv(dt_download, file, row.names = FALSE)
          }
        )

        #########################
        # Display detailed data #
        #########################

        output$dt_detail_auto <- renderDataTable(server = FALSE, {
          dt_detail_auto <- rv_result$dt_detail_auto
          if(is.null(dt_detail_auto)){
            datatable(
              dt_detail_auto,
              colnames = c("Victim", "Reference", "Estimated relationship", "Locus", "Victim profile", "Reference profile", "Likelihood (related)", "Likelihood (unrelated)", "LR"),
              selection = "none",
              options = list(dom = "t", iDisplayLength = nrow(dt_detail_auto), ordering = FALSE, columnDefs = list(list(targets = 0:2, visible = FALSE))),
              rownames = FALSE
            )
          }else{
            datatable(
              dt_detail_auto,
              colnames = c("Victim", "Reference", "Estimated relationship", "Locus", "Victim profile", "Reference profile", "Likelihood (related)", "Likelihood (unrelated)", "LR"),
              selection = "none",
              options = list(dom = "t", iDisplayLength = nrow(dt_detail_auto), ordering = FALSE, columnDefs = list(list(targets = 0:2, visible = FALSE))),
              rownames = FALSE
            ) %>%
              formatSignif(columns = c("LikeH1", "LikeH2", "LR"), digits = 3)
          }
        })

        output$dt_detail_y <- renderDataTable(server = FALSE, {
          dt_detail_y <- rv_result$dt_detail_y
          datatable(
            dt_detail_y,
            colnames = c("Victim", "Reference", "Paternal lineage", "Locus", "Victim profile", "Reference profile", "Ignored locus", "Mismatched locus", "Mutational step"),
            selection = "none",
            options = list(dom = "t", iDisplayLength = nrow(dt_detail_y), ordering = FALSE, columnDefs = list(list(targets = 0:2, visible = FALSE))),
            rownames = FALSE
          )
        })

        output$dt_detail_mt <- renderDataTable(server = FALSE, {
          dt_detail_mt <- rv_result$dt_detail_mt
          datatable(
            dt_detail_mt,
            colnames = c("Victim", "Reference", "Maternal lineage", "Shared range", "Shared length", "Victim profile", "Reference profile", "Out of shared range", "Mismatch"),
            selection = "none",
            options = list(dom = "t", iDisplayLength = nrow(dt_detail_mt), ordering = FALSE, columnDefs = list(list(targets = 0:4, visible = FALSE))),
            rownames = FALSE
          )
        })

        output$download_auto <- downloadHandler(
          filename = paste0(gsub(" ", "_", format(as.POSIXct(Sys.time()), "%Y-%m-%d %H%M%S")), "_RelSearch_Detail_STR.csv"),
          content = function(file){
            dt_detail_auto <- rv_result$dt_detail_auto
            colnames(dt_detail_auto) <- c("Victim", "Reference", "Estimated relationship", "Locus", "Victim profile", "Reference profile", "Likelihood (related)", "Likelihood (unrelated)", "LR")
            write.csv(dt_detail_auto, file, row.names = FALSE)
          }
        )

        output$download_y <- downloadHandler(
          filename = paste0(gsub(" ", "_", format(as.POSIXct(Sys.time()), "%Y-%m-%d %H%M%S")), "_RelSearch_Detail_Y-STR.csv"),
          content = function(file){
            dt_detail_y <- rv_result$dt_detail_y
            colnames(dt_detail_y) <- c("Victim", "Reference", "Paternal lineage", "Locus", "Victim profile", "Reference profile", "Ignored locus", "Mismatched locus", "Mutational step")
            write.csv(dt_detail_y, file, row.names = FALSE)
          }
        )

        output$download_mt <- downloadHandler(
          filename = paste0(gsub(" ", "_", format(as.POSIXct(Sys.time()), "%Y-%m-%d %H%M%S")), "_RelSearch_Detail_mtDNA.csv"),
          content = function(file){
            dt_detail_mt <- rv_result$dt_detail_mt
            colnames(dt_detail_mt) <- c("Victim", "Reference", "Maternal lineage", "Shared range", "Shared length", "Victim profile", "Reference profile", "Out of shared range", "Mismatch")
            write.csv(dt_detail_mt, file, row.names = FALSE)
          }
        )

        output$sn_v_select_detail <- renderText({paste0(rv_result$sn_v_select)})
        output$sn_r_select_detail <- renderText({paste0(rv_result$sn_r_select)})
        output$family_member <- renderText({paste0(rv_result$family_member)})
        output$estimated_rel_select <- renderText({paste0(rv_result$estimated_rel_select)})
        output$paternal_select <- renderText({paste0(rv_result$paternal_select)})
        output$maternal_select <- renderText({paste0(rv_result$maternal_select)})

        output$num_mismatch_select <- renderText({paste0(rv_result$mismatch_mt)})
        output$share_range_select <- renderText({paste0(rv_result$share_range_mt)})
        output$share_len_select <- renderText({paste0(rv_result$share_length_mt)})

        observeEvent(input$dt_display_rows_selected, {
          pos_select <- input$dt_display_rows_selected

          if(is.null(pos_select)){
            disable("download_auto")
            disable("download_y")
            disable("download_mt")
            rv_result$dt_detail_auto <- NULL
            rv_result$dt_detail_y <- NULL
            rv_result$dt_detail_mt <- NULL
            rv_result$sn_v_select <- NULL
            rv_result$sn_r_select <- NULL
            rv_result$family_member <- NULL
            rv_result$estimated_rel_select <- NULL
            rv_result$paternal_select <- NULL
            rv_result$maternal_select <- NULL
            rv_result$mismatch_mt <- NULL
            rv_result$share_range_mt <- NULL
            rv_result$share_length_mt <- NULL
          }else{
            dt_display <- rv_result$dt_display
            sn_v_select <- dt_display[pos_select, Victim]
            sn_r_select <- dt_display[pos_select, Reference]
            family_select <- dt_display[pos_select, Family]
            assumed_rel_select <- dt_display[pos_select, AssumedRel]
            estimated_rel_select <- dt_display[pos_select, EstimatedRel]
            if(is.na(estimated_rel_select)){
              estimated_rel_select <- "No data"
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
              tmp_fam_auto <- dt_r_auto$SampleName[dt_r_auto$Family == family_select]
              rv_result$dt_detail_auto <- dt_detail_auto <- create_detailed_data_auto(dt_v_auto, dt_r_auto, sn_v_select, sn_r_select, assumed_rel_select, estimated_rel_select, result_selected)
              if(is.null(dt_detail_auto)){
                disable("download_auto")
              }else{
                enable("download_auto")
              }
            }else{
              tmp_fam_auto <- character(0)
            }

            if(bool_check_y){
              tmp_fam_y <- dt_r_y$SampleName[dt_r_y$Family == family_select]
              rv_result$dt_detail_y <- dt_detail_y <- create_detailed_data_y(dt_v_y, dt_r_y, sn_v_select, sn_r_select, assumed_rel_select, paternal_select, result_selected)
              if(is.null(dt_detail_y)){
                disable("download_y")
              }else{
                enable("download_y")
              }
            }else{
              tmp_fam_y <- character(0)
            }

            if(bool_check_mt){
              tmp_fam_mt <- dt_r_mt$SampleName[dt_r_mt$Family == family_select]
              rv_result$dt_detail_mt <- dt_detail_mt <- create_detailed_data_mt(dt_v_mt, dt_r_mt, sn_v_select, sn_r_select, assumed_rel_select, maternal_select, result_selected)
              if(is.null(dt_detail_mt)){
                mismatch_mt <- NULL
                share_range_mt <- NULL
                share_length_mt <- NULL
                disable("download_mt")
              }else{
                mismatch_mt <- result_selected[, MismatchMt]
                share_range_mt <- result_selected[, ShareRangeMt]
                share_length_mt <- result_selected[, ShareLengthMt]
                enable("download_mt")
              }
              rv_result$mismatch_mt <- mismatch_mt
              rv_result$share_range_mt <- share_range_mt
              rv_result$share_length_mt <- share_length_mt
            }else{
              tmp_fam_mt <- character(0)
            }

            family_member <- sort(unique(c(tmp_fam_auto, tmp_fam_y, tmp_fam_mt)))
            family_member <- paste0(family_member, collapse = ", ")

            rv_result$sn_v_select <- sn_v_select
            rv_result$sn_r_select <- sn_r_select
            rv_result$family_member <- family_member
            rv_result$estimated_rel_select <- estimated_rel_select
            rv_result$paternal_select <- paternal_select
            rv_result$maternal_select <- maternal_select
          }
        }, ignoreInit = TRUE)

        ###############################
        # Display analysis conditions #
        ###############################

        output$result_fn_v_auto <- renderText({fn_v_auto})
        output$result_fn_r_auto <- renderText({fn_r_auto})
        output$result_fn_af <- renderText({fn_af})
        output$result_fn_v_y <- renderText({fn_v_y})
        output$result_fn_r_y <- renderText({fn_r_y})
        output$result_fn_v_mt <- renderText({fn_v_mt})
        output$result_fn_r_mt <- renderText({fn_r_mt})

        output$dt_unobs_al <- renderDataTable(server = FALSE, {
          datatable(
            dt_unobs_al,
            colnames = c("Marker", "Allele"),
            selection = "none",
            options = list(dom = "t", iDisplayLength = nrow(dt_unobs_al), ordering = FALSE),
            rownames = FALSE
          )
        })

        output$download_af_use <- downloadHandler(
          filename = paste0(gsub(" ", "_", format(as.POSIXct(Sys.time()), "%Y-%m-%d %H%M%S")), "_RelSearch_Allele_Probability.csv"),
          content = function(file){
            write.csv(dt_af_use, file, row.names = FALSE)
          }
        )

        output$result_min_lr_auto <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"])
        })
        output$result_max_lr_auto <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "max_lr_auto"])
        })
        output$result_min_detect_y <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "min_detect_y"])
        })
        output$result_max_mismatch_y <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_y"])
        })
        output$result_max_mustep_y <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "max_mustep_y"])
        })
        output$result_min_share_len_mt <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "min_share_len_mt"])
        })
        output$result_max_mismatch_mt <- renderText({
          paste0(dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_mt"])
        })

        output$result_assumed_rel <- renderDataTable(server = FALSE, {
          datatable(
            dt_rel,
            colnames = c("Relationship", "Sex (Victim)", "Sex (Reference)", "Pr (IBD = 2)", "Pr (IBD = 1)", "Pr (IBD = 0)", "Paternal lineage", "Maternal lineage",
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
              plotOutput(session$ns("tree_check")),
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
        }, ignoreInit = TRUE)

        output$result_myu <- renderDataTable(server = FALSE, {
          datatable(
            dt_myu,
            colnames = c("Locus", "Paternal -2 step", "Paternal -1 step", "Paternal 0 step", "Paternal +1 step", "Paternal +2 step", "Maternal -2 step", "Maternal -1 step", "Maternal 0 step", "Maternal +1 step", "Maternal +2 step"),
            selection = "none",
            options = list(iDisplayLength = 50, ordering = FALSE),
            rownames = FALSE
          )
        })

        output$result_keep_min_lr <- renderText({
          paste0(dt_data_manage$Value[dt_data_manage$Parameter == "keep_min_lr"])
        })

        output$result_max_data_displayed <- renderText({
          paste0(dt_data_manage$Value[dt_data_manage$Parameter == "max_data_displayed"])
        })
      }
    }
  )
}
