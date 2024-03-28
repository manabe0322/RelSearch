#' load_proj_ui
#'
#' @description The function to create the ui module for loading the project
load_proj_ui <- function(id){
  tabPanel("Load project",
           useWaiter(),
           h2("Load project"),
           br(),
           fileInput(ns("file_proj"), label = "Select a project file", accept = ".RData"),
           br(),
           actionButton(ns("act_load_proj"), label = "Load project", class = "btn btn-primary btn-lg")
  )
}

#' load_proj_server
#'
#' @description The function to create the server module for loading the project
load_proj_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      rv_load_proj <- reactiveValues()
      rv_load_proj$dt_combined <- NULL
      rv_load_proj$dt_display <- NULL
      rv_load_proj$bool_check_auto <- NULL
      rv_load_proj$bool_check_y <- NULL
      rv_load_proj$bool_check_mt <- NULL
      rv_load_proj$dt_v_auto <- NULL
      rv_load_proj$dt_r_auto <- NULL
      rv_load_proj$dt_af <- NULL
      rv_load_proj$dt_v_y <- NULL
      rv_load_proj$dt_r_y <- NULL
      rv_load_proj$dt_v_mt <- NULL
      rv_load_proj$dt_r_mt <- NULL
      rv_load_proj$dt_criteria <- NULL
      rv_load_proj$dt_rel <- NULL
      rv_load_proj$dt_myu <- NULL
      rv_load_proj$dt_par_auto <- NULL
      rv_load_proj$fn_v_auto <- NULL
      rv_load_proj$fn_r_auto <- NULL
      rv_load_proj$fn_af <- NULL
      rv_load_proj$fn_v_y <- NULL
      rv_load_proj$fn_r_y <- NULL
      rv_load_proj$fn_v_mt <- NULL
      rv_load_proj$fn_r_mt <- NULL

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

          isolate(rv_load_proj$dt_combined <- dt_combined)
          isolate(rv_load_proj$dt_display <- dt_display)

          isolate(rv_load_proj$bool_check_auto <- bool_check_auto)
          isolate(rv_load_proj$bool_check_y <- bool_check_y)
          isolate(rv_load_proj$bool_check_mt <- bool_check_mt)

          isolate(rv_load_proj$dt_v_auto <- dt_v_auto)
          isolate(rv_load_proj$dt_r_auto <- dt_r_auto)
          isolate(rv_load_proj$dt_af <- dt_af)

          isolate(rv_load_proj$dt_v_y <- dt_v_y)
          isolate(rv_load_proj$dt_r_y <- dt_r_y)

          isolate(rv_load_proj$dt_v_mt <- dt_v_mt)
          isolate(rv_load_proj$dt_r_mt <- dt_r_mt)

          isolate(rv_load_proj$dt_criteria <- dt_criteria)
          isolate(rv_load_proj$dt_rel <- dt_rel)
          isolate(rv_load_proj$dt_myu <- dt_myu)
          isolate(rv_load_proj$dt_par_auto <- dt_par_auto)

          isolate(rv_load_proj$fn_v_auto <- fn_v_auto)
          isolate(rv_load_proj$fn_r_auto <- fn_r_auto)
          isolate(rv_load_proj$fn_af <- fn_af)
          isolate(rv_load_proj$fn_v_y <- fn_v_y)
          isolate(rv_load_proj$fn_r_y <- fn_r_y)
          isolate(rv_load_proj$fn_v_mt <- fn_v_mt)
          isolate(rv_load_proj$fn_r_mt <- fn_r_mt)

          waiter_hide()

          enable("name_proj")
          enable("download_proj")
          enable(selector = '.navbar-nav a[data-value = "Result"]')
          disable(selector = '.navbar-nav a[data-value = "Settings"]')
          updateNavbarPage(session, "navbar", selected = "Result")
          showModal(modalDialog(title = "Information", "Displayed data satisfies at least one of the criteria for STR, Y-STR, and mtDNA.", easyClose = TRUE, footer = NULL))
        }
      })
    }
  )
}
