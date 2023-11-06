#' create_background_color
#'
#' @description The function to create the sign of the number of candidates
#' @param dt_combined A data.table of the combined data
#' @param index_alert Indices of not-supporting paternal or maternal lineage with LR >= 100
create_background_color <- function(dt_combined, index_alert){
  background_color <- rep(-1, nrow(dt_combined))

  pos_est_rel <- which(dt_combined[, EstimatedRel] != "")
  n_est_rel <- length(pos_est_rel)

  if(n_est_rel > 0){
    dt_extract <- dt_combined[pos_est_rel, ]
    vics <- dt_extract[, Victim]
    refs <- dt_extract[, Reference]

    for(i in seq_len(n_est_rel)){
      nc <- length(union(which(vics == vics[i]), which(refs == refs[i])))
      if(nc >= 2){
        background_color[pos_est_rel[i]] <- 2
      }else{
        background_color[pos_est_rel[i]] <- nc
      }
    }
  }

  background_color[index_alert] <- 0

  return(background_color)
}

#' create_combined_data
#'
#' @description The function to create combined data
#' @param dt_result_auto A data.table of the result for the autosomal STR
#' @param dt_result_y A data.table of the result for the Y-STR
#' @param dt_result_mt A data.table of the result for the mtDNA
#' @param dt_rel A data.table of information on relationships
create_combined_data <- function(dt_result_auto, dt_result_y, dt_result_mt, dt_rel){
  # Combine data.table
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

  # Add lacking columns
  n_data <- nrow(dt_combined)
  options(warn = -1)
  if(is.null(dt_result_auto)){
    dt_combined[, LR_Total := rep(NA, n_data)]
    dt_combined[, EstimatedRel := rep(NA, n_data)]
  }
  if(is.null(dt_result_y)){
    dt_combined[, Paternal := rep(NA, n_data)]
  }
  if(is.null(dt_result_mt)){
    dt_combined[, Maternal := rep(NA, n_data)]
  }
  options(warn = 0)

  # Investigate unexpected results of Y-STR and mtDNA
  index_unexpected_y <- index_unexpected_mt <- integer(0)
  est_rel_all <- dt_combined[, EstimatedRel]
  est_paternal_all <- dt_combined[, Paternal]
  est_maternal_all <- dt_combined[, Maternal]
  names_rel <- dt_rel[, Relationship]
  info_paternal <- dt_rel[, Paternal]
  info_maternal <- dt_rel[, Maternal]
  for(i in 1:length(names_rel)){
    index_target_rel <- which(est_rel_all == names_rel[i])
    if(length(index_target_rel) > 0){
      if(info_paternal[i] == "Yes"){
        est_paternal_target_rel <- est_paternal_all[index_target_rel]
        index_unexpected_y <- c(index_unexpected_y, index_target_rel[which(est_paternal_target_rel == "Not support")])
      }
      if(info_maternal[i] == "Yes"){
        est_maternal_target_rel <- est_maternal_all[index_target_rel]
        index_unexpected_mt <- c(index_unexpected_mt, index_target_rel[which(est_maternal_target_rel == "Not support")])
      }
    }
  }
  color_y <- color_mt <- rep(1, n_data)
  color_y[index_unexpected_y] <- 2
  color_mt[index_unexpected_mt] <- 2

  # Update estimated relationships
  index_satisfy_lr <- which(!is.na(est_rel_all))
  index_unexpected_y_mt <- sort(unique(c(index_unexpected_y, index_unexpected_mt)))
  index_alert <- intersect(index_satisfy_lr, index_unexpected_y_mt)
  est_rel_all[index_alert] <- NA
  dt_combined[, EstimatedRel := est_rel_all]

  # Change data type
  sn_v <- sort(unique(dt_combined[, Victim]))
  dt_combined$Victim <- factor(dt_combined$Victim, levels = sn_v, labels = sn_v)
  sn_r <- sort(unique(dt_combined[, Reference]))
  dt_combined$Reference <- factor(dt_combined$Reference, levels = sn_r, labels = sn_r)
  dt_combined$AssumedRel <- factor(dt_combined$AssumedRel, levels = names_rel, labels = names_rel)
  dt_combined$EstimatedRel <- factor(dt_combined$EstimatedRel, levels = names_rel, labels = names_rel)
  dt_combined$Paternal <- factor(dt_combined$Paternal, levels = c("Support", "Not support"), labels = c("Support", "Not support"))
  dt_combined$Maternal <- factor(dt_combined$Maternal, levels = c("Support", "Not support"), labels = c("Support", "Not support"))

  # Create the sign of the number of candidates
  background_color <- create_background_color(dt_combined, index_alert)

  # Additional columns
  options(warn = -1)
  dt_combined[, ColorBack := background_color]
  dt_combined[, ColorY := color_y]
  dt_combined[, ColorMt := color_mt]
  options(warn = 0)

  return(dt_combined)
}

#' create_displayed_data
#'
#' @description The function to create the displayed data
#' @param dt_combined A data.table of the combined data
#' @param fltr_type The filtering method
#' @param max_data The maximum data displayed
#' @param min_lr The minimum LR displayed
create_displayed_data <- function(dt_combined, fltr_type = "default", max_data = 10000, min_lr = NULL){
  setkey(dt_combined, Victim, Reference, AssumedRel)

  dt_display <- dt_combined[, list(Victim, Reference, AssumedRel, LR_Total, EstimatedRel, Paternal, Maternal, ColorBack, ColorY, ColorMt)]
  setorder(dt_display, - LR_Total, Paternal, Maternal, na.last = TRUE)
  dt_display$LR_Total <- signif(dt_display$LR_Total, 3)

  if(fltr_type == "identified"){
    dt_display <- dt_display[ColorBack == 1]
  }else if(fltr_type == "multiple"){
    dt_display <- dt_display[ColorBack == 2]
  }else if(fltr_type == "alert"){
    dt_display <- dt_display[ColorBack == 0]
  }else if(fltr_type == "min_lr"){
    dt_display <- dt_display[LR_Total >= min_lr]
  }

  if(nrow(dt_display) > max_data){
    dt_display <- dt_display[1:max_data, ]
  }

  return(dt_display)
}
