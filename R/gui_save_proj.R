#' save_proj_ui
#'
#' @description The function to create the ui module for saving the project
save_proj_ui <- function(id){
  tabPanel("Save project",
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
save_proj_server <- function(id, rv_file){
  moduleServer(
    id,
    function(input, output, session){
      data_list <- rv_file$data_list

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
            waiter_show(html = spin_3k(), color = "white")

            dt_combined <- data_list$dt_combined
            dt_display <- data_list$dt_display
            bool_check_auto <- data_list$bool_check_auto
            bool_check_y <- data_list$bool_check_y
            bool_check_mt <- data_list$bool_check_mt
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

            save(list = c("dt_combined", "dt_display",
                          "bool_check_auto", "bool_check_y", "bool_check_mt",
                          "dt_v_auto", "dt_r_auto", "dt_af",
                          "dt_v_y", "dt_r_y",
                          "dt_v_mt", "dt_r_mt",
                          "dt_criteria", "dt_rel", "dt_myu", "dt_par_auto",
                          "fn_v_auto", "fn_r_auto", "fn_af",
                          "fn_v_y", "fn_r_y",
                          "fn_v_mt", "fn_r_mt"), file = file)

            waiter_hide()
          }
        )
      }
    }
  )
}
