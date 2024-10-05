#' save_proj_ui
#'
#' @description The function to create the ui module for saving the project
save_proj_ui <- function(id){
  ns <- NS(id)

  tabPanel("Save project",
           useShinyjs(),
           useWaiter(),
           h2("Save project"),
           br(),
           disabled(textInput(ns("name_proj"), label = "Enter the project name", value = NULL)),
           br(),
           disabled(downloadButton(ns("download_proj"), label = "Save as", class = "btn btn-primary btn-lg"))
  )
}

#' save_proj_server
#'
#' @description The function to create the server module for saving the project
#' @param rv_file The reactive valus for the load data and the analysis results
save_proj_server <- function(id, rv_file){
  moduleServer(
    id,
    function(input, output, session){
      data_list <- rv_file$data_list

      waiter_ui <- Waiter$new(html = tagList(spin_3k(),
                                             h3("Preparing...")),
                              color = transparent(.5))

      if(is.null(data_list)){
        disable("name_proj")
        disable("download_proj")
      }else{
        enable("name_proj")
        enable("download_proj")

        output$download_proj <- downloadHandler(
          filename = function(){
            if(isTruthy(input$name_proj)){
              paste0(input$name_proj, ".RData")
            }else{
              paste0(gsub(" ", "_", format(as.POSIXct(Sys.time()), "%Y-%m-%d %H%M%S")), "_relsearch_project.RData")
            }
          },

          content = function(file){
            waiter_ui$show()
            save(list = c("data_list"), file = file)
            waiter_ui$hide()
          }
        )
      }
    }
  )
}
