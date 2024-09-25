#' extract_all_cand
#'
#' @description The function to extract other candidates from dt_display
#' @param dt_display A data.table of the displayed data
#' @param sn_v_select The sample name of the selected victim
#' @param sn_r_select The sample name of the selected reference
extract_all_cand <- function(dt_display, sn_v_select, sn_r_select){
  cand_v <- sn_v_select
  cand_r <- sn_r_select
  index_v <- which(is.element(dt_display$Victim, cand_v))
  index_r <- which(is.element(dt_display$Reference, cand_r))
  dt_all_cand <- dt_display[intersect(index_v, index_r), ]
  index_inconclusive <- which(dt_display$ColorBack == 2)
  repeat{
    index_v <- which(is.element(dt_display$Victim, cand_v))
    index_r <- which(is.element(dt_display$Reference, cand_r))
    index_cand <- union(index_v, index_r)
    index_cand <- intersect(index_cand, index_inconclusive)
    if(length(index_cand) == 0){
      break
    }else{
      dt_all_cand <- dt_display[index_cand, ]
      cand_v_new <- unique(dt_all_cand$Victim)
      cand_r_new <- unique(dt_all_cand$Reference)
      if(length(cand_v_new) > length(cand_v) || length(cand_r_new) > length(cand_r)){
        cand_v <- cand_v_new
        cand_r <- cand_r_new
      }else{
        break
      }
    }
  }
  return(dt_all_cand)
}
