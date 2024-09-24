#' extract_other_cand
#'
#' @description The function to extract other candidates from dt_display
#' @param dt_display A data.table of the displayed data
#' @param sn_v_select The sample name of the selected victim
#' @param sn_r_select The sample name of the selected reference
extract_other_cand <- function(dt_display, sn_v_select, sn_r_select){
  cand_v <- sn_v_select
  cand_r <- sn_r_select
  repeat{
    index_v <- which(is.element(dt_display$Victim, cand_v))
    index_r <- which(is.element(dt_display$Reference, cand_r))
    index_cand <- union(index_v, index_r)
    dt_other_cand <- dt_display[index_cand, ]
    cand_v_new <- unique(dt_other_cand$Victim)
    cand_r_new <- unique(dt_other_cand$Reference)
    if(length(cand_v_new) > length(cand_v) || length(cand_r_new) > length(cand_r)){
      cand_v <- cand_v_new
      cand_r <- cand_r_new
    }else{
      break
    }
  }
  return(dt_other_cand)
}
