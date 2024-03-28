#' relsearch
#'
#' @description Main window
#' @usage relsearch()
#' @export
relsearch <- function(){
  ver_soft <- packageVersion("relsearch")
  path_pack <- path.package("relsearch", quiet = FALSE)
  max_data <- 10000
  keep_min_lr <- 1
  options(shiny.maxRequestSize = 500 * 1024^2)

  ui <- fluidPage(useShinyjs(),
                  useShinyFeedback(),
                  theme = shinytheme("cerulean"),
                  tags$style(type = "text/css", "body{padding-top: 70px;}"),
                  navbarPage(title = paste0("relsearch ver. ", ver_soft),
                             id = "navbar",
                             position = c("fixed-top"),

                             load_ui("load"),

                             result_ui("result"),

                             navbarMenu("Project",
                                        tabPanel("New project",
                                                 h2("New project"),
                                                 br(),
                                                 actionButton("act_new_proj", label = "New project", class = "btn btn-primary btn-lg")
                                        ),
                                        load_proj_ui("load_proj"),
                                        save_proj_ui("save_proj")
                             ),

                             navbarMenu("Database",
                                        tab_view_dt_v_auto_ui("view_dt_v_auto"),
                                        tab_view_dt_r_auto_ui("view_dt_r_auto"),
                                        tab_view_dt_af_ui("view_dt_af"),
                                        tab_view_dt_v_y_ui("view_dt_v_y"),
                                        tab_view_dt_v_y_ui("view_dt_r_y"),
                                        tab_view_dt_v_mt_ui("view_dt_v_mt"),
                                        tab_view_dt_v_mt_ui("view_dt_r_mt"),
                             ),

                             navbarMenu("Settings",
                                        tab_criteria_ui("tab_criteria"),
                                        tab_rel_ui("tab_rel"),
                                        tab_myu_ui("tab_myu"),
                                        tab_par_auto_ui("tab_par_auto")
                             ),

                             example_ui("example")
                             #,
#                             tabPanel("Manual",
#                                      tabsetPanel(
#                                        tabPanel("Getting started",
#                                                 includeMarkdown(paste0(path_pack, "/extdata/manual/relsearch_manual_getting-started.md"))
#                                        ),
#                                        tabPanel("Overall flow",
#                                                 includeMarkdown(paste0(path_pack, "/extdata/manual/relsearch_manual_overall-flow.md"))
#                                        ),
#                                        tabPanel("File format",
#                                                 includeMarkdown(paste0(path_pack, "/extdata/manual/relsearch_manual_file-format.md"))
#                                        ),
#                                        tabPanel("Settings",
#                                                 includeMarkdown(paste0(path_pack, "/extdata/manual/relsearch_manual_settings.md"))
#                                        ),
#                                        tabPanel("Calculation principle",
#                                                 includeMarkdown(paste0(path_pack, "/extdata/manual/relsearch_manual_calculation-principle.md"))
#                                        )
#                                      )
#                             )
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
    # Setting tabs #
    ################

    rv_criteria <- tab_criteria_server("tab_criteria", init_dt_criteria, path_pack)
    rv_rel <- tab_rel_server("tab_rel", init_dt_rel, path_pack)
    rv_myu <- tab_myu_server("tab_myu", init_dt_myu, path_pack)
    rv_par_auto <- tab_par_auto_server("tab_par_auto", init_dt_par_auto, path_pack)

    example_server("example", path_pack)

    rv_file <- load_server("load", session, rv_criteria, rv_rel, rv_myu, rv_par_auto)

    rv_load_proj <- load_proj_server("load_proj")
    observe({
      req(rv_load_proj)
      if(!is.null(rv_load_proj$dt_combined)){
        rv_file$dt_v_auto <- rv_load_proj$dt_v_auto
        rv_file$dt_r_auto <- rv_load_proj$dt_r_auto
        rv_file$dt_af <- rv_load_proj$dt_af
        rv_file$dt_v_y <- rv_load_proj$dt_v_y
        rv_file$dt_v_y <- rv_load_proj$dt_v_y
        rv_file$dt_v_mt <- rv_load_proj$dt_v_mt
        rv_file$dt_r_mt <- rv_load_proj$dt_r_mt
        rv_file$dt_criteria <- rv_load_proj$dt_criteria
        rv_file$dt_rel <- rv_load_proj$dt_rel
        rv_file$dt_myu <- rv_load_proj$dt_myu
        rv_file$dt_par_auto <- rv_load_proj$dt_par_auto
        rv_file$data_list <- rv_load_proj$data_list
      }
    })

    observe({
      req(rv_file)
      tab_view_dt_v_auto_server("view_dt_v_auto", rv_file)
      tab_view_dt_r_auto_server("view_dt_r_auto", rv_file)
      tab_view_dt_af_server("view_dt_af", rv_file)
      tab_view_dt_v_y_server("view_dt_v_y", rv_file)
      tab_view_dt_r_y_server("view_dt_r_y", rv_file)
      tab_view_dt_v_mt_server("view_dt_v_mt", rv_file)
      tab_view_dt_r_mt_server("view_dt_r_mt", rv_file)
      result_server("result", rv_file)
      save_proj_server("save_proj", rv_file)
    })

    ###############
    # New project #
    ###############

    observeEvent(input$act_new_proj, {
      refresh()
    })
  }

  shinyApp(ui, server)
}
