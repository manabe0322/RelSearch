#' RelSearch
#'
#' @description Main window
#' @usage RelSearch()
#' @export
RelSearch <- function(){
  ver_soft <- packageVersion("RelSearch")
  path_pack <- path.package("RelSearch", quiet = FALSE)
  options(shiny.maxRequestSize = 500 * 1024^2)

  ui <- fluidPage(theme = shinytheme("cerulean"),
                  tags$style(type = "text/css", "body{padding-top: 70px;}"),
                  navbarPage(title = paste0("RelSearch ver. ", ver_soft),
                             id = "navbar",
                             selected = "Load",
                             position = c("fixed-top"),

                             load_ui("load"),

                             result_ui("result"),

                             navbarMenu("Project",
                                        tabPanel("New project",
                                                 useShinyjs(),
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
                                        tab_view_dt_r_y_ui("view_dt_r_y"),
                                        tab_view_dt_v_mt_ui("view_dt_v_mt"),
                                        tab_view_dt_r_mt_ui("view_dt_r_mt"),
                             ),

                             navbarMenu("Settings",
                                        tab_criteria_ui("tab_criteria"),
                                        tab_rel_ui("tab_rel"),
                                        tab_myu_ui("tab_myu"),
                                        tab_data_manage_ui("tab_data_manage")
                             ),

                             example_ui("example"),

                             manual_ui("manual")
                  )
  )

  server <- function(input, output, session){
    rv_criteria <- tab_criteria_server("tab_criteria", path_pack)
    rv_rel <- tab_rel_server("tab_rel", path_pack)
    rv_myu <- tab_myu_server("tab_myu", path_pack)
    rv_data_manage <- tab_data_manage_server("tab_data_manage", path_pack)
    example_server("example", path_pack)
    manual_server("manual", path_pack)
    rv_file <- load_server("load", session, rv_criteria, rv_rel, rv_myu, rv_data_manage)

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

    observeEvent(input$act_new_proj, {
      refresh()
    })

    rv_load_proj <- load_proj_server("load_proj", session, rv_data_manage)
    observe({
      req(rv_load_proj)
      data_list <- rv_load_proj$data_list
      if(!is.null(data_list)){
        rv_file$dt_v_auto <- data_list$dt_v_auto
        rv_file$dt_r_auto <- data_list$dt_r_auto
        rv_file$dt_af <- data_list$dt_af
        rv_file$dt_v_y <- data_list$dt_v_y
        rv_file$dt_r_y <- data_list$dt_r_y
        rv_file$dt_v_mt <- data_list$dt_v_mt
        rv_file$dt_r_mt <- data_list$dt_r_mt
        rv_file$dt_criteria <- data_list$dt_criteria
        rv_file$dt_rel <- data_list$dt_rel
        rv_file$dt_myu <- data_list$dt_myu
        rv_file$dt_data_manage <- data_list$dt_data_manage
        rv_file$data_list <- data_list
        rv_file$bool_load_v_auto <- FALSE
        rv_file$bool_load_r_auto <- FALSE
        rv_file$bool_load_af <- FALSE
        rv_file$bool_load_v_y <- FALSE
        rv_file$bool_load_r_y <- FALSE
        rv_file$bool_load_v_mt <- FALSE
        rv_file$bool_load_r_mt <- FALSE
      }
    })
  }

  shinyApp(ui, server)
}
