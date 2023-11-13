#' relsearch: Open-source software for searching relatives between victim and reference databases
#'
#' @importFrom Rcpp sourceCpp
#' @importFrom data.table data.table copy fread setDT setkey setorder
#' @importFrom dplyr full_join mutate_all
#' @importFrom DT datatable dataTableOutput formatStyle styleEqual styleRow renderDataTable
#' @importFrom magrittr "%>%"
#' @importFrom pedtools ped
#' @importFrom ribd coeffTable
#' @importFrom shinyjs disable disabled enable refresh useShinyjs
#' @importFrom shinythemes shinytheme
#' @importFrom shinyvalidate InputValidator sv_gte sv_numeric sv_required
#' @importFrom waiter useWaiter waiter_show waiter_hide spin_3k
"_PACKAGE"

#' @useDynLib relsearch, .registration = TRUE
NULL
