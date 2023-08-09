#######################################################
# The function to create the ui module for the manual #
#######################################################

tab_manual_ui <- function(id){
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      h2("Table of contents"),
      br(),
      p("Getting started"),
      p("Load database"),
      p("Check results")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Getting started",
                 br()
                 )
      )
    )
  )

}

###########################################################
# The function to create the server module for the manual #
###########################################################

tab_manual_server <- function(input, output, session){

}
