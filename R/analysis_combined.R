#' create_num_cand
#'
#' @description The function to create the sign of the number of candidates
#' @param dt_combined A data.table of the combined data
create_num_cand <- function(dt_combined){
  num_cand <- rep(0, nrow(dt_combined))

  pos_est_rel <- which(dt_combined[, EstimatedRel] != "")
  n_est_rel <- length(pos_est_rel)

  if(n_est_rel > 0){
    dt_extract <- dt_combined[pos_est_rel, ]
    vics <- dt_extract[, Victim]
    refs <- dt_extract[, Reference]

    for(i in seq_len(n_est_rel)){
      nc <- length(union(which(vics == vics[i]), which(refs == refs[i])))
      if(nc >= 2){
        num_cand[pos_est_rel[i]] <- 2
      }else{
        num_cand[pos_est_rel[i]] <- nc
      }
    }
  }

  return(num_cand)
}

#' create_combined_data
#'
#' @description The function to create combined data
#' @param dt_result_auto A data.table of the result for the autosomal STR
#' @param dt_result_y A data.table of the result for the Y-STR
#' @param dt_result_mt A data.table of the result for the mtDNA
#' @param dt_rel A data.table of information on relationships
create_combined_data <- function(dt_result_auto, dt_result_y, dt_result_mt, dt_rel){
  dt_combined <- NULL

  if(!is.null(dt_result_auto)){
    dt_combined <- copy(dt_result_auto)
  }

  if(!is.null(dt_result_y)){
    if(is.null(dt_combined)){
      dt_combined <- copy(dt_result_y)
    }else{
      dt_combined <- full_join(dt_combined, dt_result_y, by = c("Victim", "Reference", "AssumedRel"))
    }
  }

  if(!is.null(dt_result_mt)){
    if(is.null(dt_combined)){
      dt_combined <- copy(dt_result_mt)
    }else{
      dt_combined <- full_join(dt_combined, dt_result_mt, by = c("Victim", "Reference", "AssumedRel"))
    }
  }

  n_data <- nrow(dt_combined)
  options(warn = -1)
  if(is.null(dt_result_auto)){
    dt_combined[, LR_Total := rep(NA, n_data)]
    dt_combined[, EstimatedRel := rep(NA, nrow(dt_combined))]
  }
  if(is.null(dt_result_y)){
    dt_combined[, Paternal := rep(NA, n_data)]
  }
  if(is.null(dt_result_mt)){
    dt_combined[, Maternal := rep(NA, n_data)]
  }
  options(warn = 0)

  # Change data type
  sn_v <- sort(unique(dt_combined[, Victim]))
  dt_combined$Victim <- factor(dt_combined$Victim, levels = sn_v, labels = sn_v)
  sn_r <- sort(unique(dt_combined[, Reference]))
  dt_combined$Reference <- factor(dt_combined$Reference, levels = sn_r, labels = sn_r)
  name_rel <- dt_rel[, Name_relationship]
  dt_combined$AssumedRel <- factor(dt_combined$AssumedRel, levels = name_rel, labels = name_rel)
  dt_combined$EstimatedRel <- factor(dt_combined$EstimatedRel, levels = name_rel, labels = name_rel)
  dt_combined$Paternal <- factor(dt_combined$Paternal, levels = c("Support", "Not support"), labels = c("Support", "Not support"))
  dt_combined$Maternal <- factor(dt_combined$Maternal, levels = c("Support", "Not support"), labels = c("Support", "Not support"))

  # Keep data which satisfied criteria
  dt_combined <- dt_combined[EstimatedRel != "" | Paternal == "Support" | Maternal == "Support"]

  # Create the sign of the number of candidates
  num_cand <- create_num_cand(dt_combined)
  options(warn = -1)
  dt_combined[, NumCand := num_cand]
  options(warn = 0)

  return(dt_combined)
}

#' create_displayed_data
#'
#' @description The function to create the displayed data
#' @param dt_combined A data.table of the combined data
#' @param fltr_type The filtering method
#' @param min_lr The minimum LR displayed
create_displayed_data <- function(dt_combined, fltr_type = "default", min_lr = NULL){
  setkey(dt_combined, Victim, Reference, AssumedRel)

  dt_display <- dt_combined[, list(Victim, Reference, AssumedRel, LR_Total, EstimatedRel, Paternal, Maternal, NumCand)]

  if(fltr_type == "identified"){
    dt_display <- dt_display[NumCand == 1]
  }else if(fltr_type == "multiple"){
    dt_display <- dt_display[NumCand == 2]
  }else if(fltr_type == "min_lr"){
    dt_display <- dt_display[LR_Total >= min_lr]
  }else if(fltr_type == "paternal"){
    dt_display <- dt_display[Paternal == "Support"]
  }else if(fltr_type == "maternal"){
    dt_display <- dt_display[Maternal == "Support"]
  }

  setorder(dt_display, cols = - "LR_Total", na.last = TRUE)
  dt_display$LR_Total <- signif(dt_display$LR_Total, 3)

  return(dt_display)
}
