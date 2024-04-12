#' relsearch: Open-source software for searching relatives between victim and reference databases
#'
#' @importFrom data.table data.table copy fread setDT setkey setorder
#' @importFrom dplyr full_join mutate_all
#' @importFrom DT datatable dataTableOutput formatSignif formatStyle styleEqual styleRow renderDataTable
#' @importFrom magrittr "%>%"
#' @importFrom pandoc pandoc_activate pandoc_install
#' @importFrom pedtools ped
#' @importFrom Rcpp sourceCpp
#' @importFrom ribd coeffTable
#' @import rmarkdown
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback useShinyFeedback
#' @importFrom shinyjs disable disabled enable refresh useShinyjs
#' @importFrom shinythemes shinytheme
#' @importFrom waiter useWaiter waiter_show waiter_hide spin_3k
"_PACKAGE"

#' @useDynLib relsearch, .registration = TRUE
NULL
