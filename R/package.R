#' RelSearch: Open-source software for searching relatives between victim and reference databases
#'
#' @importFrom data.table as.data.table data.table copy fread setDT setkey setorder
#' @importFrom dplyr full_join mutate_all
#' @importFrom DT datatable dataTableOutput formatSignif formatStyle styleEqual styleRow renderDataTable
#' @importFrom magrittr "%>%"
#' @importFrom pedtools ped
#' @importFrom Rcpp sourceCpp
#' @importFrom ribd coeffTable
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback useShinyFeedback
#' @importFrom shinyjs disable disabled enable refresh useShinyjs
#' @importFrom shinythemes shinytheme
#' @import waiter
"_PACKAGE"

#' @useDynLib RelSearch, .registration = TRUE
NULL
