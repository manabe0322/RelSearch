#' load_proj_ui
#'
#' @description The function to create the ui module for loading the project
load_proj_ui <- function(id){
  ns <- NS(id)

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
load_proj_server <- function(id, session_top, rv_data_manage){
  moduleServer(
    id,
    function(input, output, session){
      rv_load_proj <- reactiveValues()
      rv_load_proj$data_list <- NULL
      rv_load_proj$max_data_displayed <- NULL

      observe({
        req(rv_data_manage)
        rv_load_proj$max_data_displayed <- rv_data_manage$max_data_displayed
      })

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
          rv_load_proj$data_list <- data_list
          updateNavbarPage(session = session_top, "navbar", selected = "Result")
          waiter_hide()

          # Show a message for displayed data
          max_data_displayed <- rv_load_proj$max_data_displayed
          if(!is.null(max_data_displayed)){
            if(nrow(data_list$dt_display) == max_data_displayed){
              showModal(modalDialog(title = "Information", paste0("Top ", max_data_displayed, " data is displayed."), easyClose = TRUE, footer = NULL))
            }else if(data_list$bool_check_auto){
              showModal(modalDialog(title = "Information", "Data that satisfies the criterion of the minimum LR is displayed.", easyClose = TRUE, footer = NULL))
            }else{
              showModal(modalDialog(title = "Information", "Data that satisfies the criteria for Y-STR or mtDNA is displayed.", easyClose = TRUE, footer = NULL))
            }
          }
        }
      }, ignoreInit = TRUE)

      return(rv_load_proj)
    }
  )
}
