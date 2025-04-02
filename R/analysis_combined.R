#' create_background_color
#'
#' @description The function to create the sign of the number of candidates (0: Excluded, 1: Inconclusive, 2: Multiple candidates, 3: Identified)
#' @param dt_combined A data.table of the combined data
#' @param dt_criteria A data.table of criteria
#' @param dt_rel A data.table of information on relationships
create_background_color <- function(dt_combined, dt_criteria, dt_rel){
  # Extract required data from dt_combined
  lr_all <- dt_combined$LR_Total
  lr_all[is.na(lr_all)] <- 1 # The LR values for no STR data are regarded as 1
  assumed_rel_all <- dt_combined$AssumedRel
  est_paternal_all <- dt_combined$Paternal
  est_paternal_all[is.na(est_paternal_all)] <- ""
  est_maternal_all <- dt_combined$Maternal
  est_maternal_all[is.na(est_maternal_all)] <- ""

  # Extract required data from dt_criteria
  min_lr_auto <- dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"]
  max_lr_auto <- dt_criteria$Value[dt_criteria$Criteria == "max_lr_auto"]

  # Extract required data from dt_rel
  names_rel <- dt_rel$Relationship
  names_rel_paternal <- names_rel[dt_rel$Paternal == "Yes"]
  names_rel_maternal <- names_rel[dt_rel$Maternal == "Yes"]

  n_data <- nrow(dt_combined)
  background_color <- rep(0, n_data) # Default: Excluded (Red)

  for(i in 1:n_data){
    est_paternal <- est_paternal_all[i]
    if(est_paternal != "Sex mismatch"){ # Sex-mismatched data is classified as Red
      lr <- lr_all[i]
      if(lr > max_lr_auto){ # Data with LR <= max_lr_auto is classified as Red
        assumed_rel <- assumed_rel_all[i]
        est_maternal <- est_maternal_all[i]
        bool_lineage <- c(is.element(assumed_rel, names_rel_paternal), is.element(assumed_rel, names_rel_maternal))
        if(lr >= min_lr_auto){ # LR >= min_lr_auto
          background_color[i] <- 3
          if(any(bool_lineage)){
            if(bool_lineage[1]){
              if(est_paternal == "Excluded"){
                background_color[i] <- 0
              }else if(est_paternal == "Inconclusive"){
                background_color[i] <- 1
              }
            }
            if(background_color[i] != 0 && bool_lineage[2]){
              if(est_maternal == "Excluded"){
                background_color[i] <- 0
              }else if(est_maternal == "Inconclusive"){
                background_color[i] <- 1
              }
            }
          }
        }else{ # max_lr_auto < LR < min_lr_auto
          background_color[i] <- 1
          if(any(bool_lineage)){
            if(bool_lineage[1]){
              if(est_paternal == "Excluded"){
                background_color[i] <- 0
              }
            }
            if(background_color[i] != 0 && bool_lineage[2]){
              if(est_maternal == "Excluded"){
                background_color[i] <- 0
              }
            }
          }
        }
      }
    }
  }

  # Check multiple candidates
  index_identified <- which(background_color == 3)
  n_identified <- length(index_identified)
  if(n_identified > 0){
    dt_extract <- dt_combined[index_identified, ]
    vics <- dt_extract[, Victim]
    refs <- dt_extract[, Reference]

    for(i in seq_len(n_identified)){
      nc <- length(union(which(vics == vics[i]), which(refs == refs[i])))
      if(nc >= 2){
        background_color[index_identified[i]] <- 2
      }
    }
  }

  return(background_color)
}

#' create_group_cand
#'
#' @description The function to create the groups of multiple candidates
#' @param dt_combined A data.table of the combined data
#' @param background_color The background colors for the displayed table
create_group_cand <- function(dt_combined, background_color){
  group_cand <- rep(NA, nrow(dt_combined))
  index_multi_cand <- which(background_color == 2)
  if(length(index_multi_cand) != 0){
    dt_extract <- dt_combined[index_multi_cand, ]
    vics <- dt_extract[, Victim]
    refs <- dt_extract[, Reference]
    current_group <- 1
    for(i in 1:length(index_multi_cand)){
      if(is.na(group_cand[index_multi_cand[i]])){
        cand_v <- vics[i]
        cand_r <- refs[i]
        repeat{
          index_v <- which(is.element(vics, cand_v))
          index_r <- which(is.element(refs, cand_r))
          index_vr <- sort(unique(c(index_v, index_r)))
          cand_v_new <- unique(vics[index_vr])
          cand_r_new <- unique(refs[index_vr])
          if(length(cand_v_new) > length(cand_v) || length(cand_r_new) > length(cand_r)){
            cand_v <- cand_v_new
            cand_r <- cand_r_new
          }else{
            break
          }
        }
        group_cand[index_multi_cand[index_vr]] <- current_group
        current_group <- current_group + 1
      }
    }
  }
  return(group_cand)
}

#' create_combined_data
#'
#' @description The function to create combined data
#' @param dt_result_auto A data.table of the result for the autosomal STR
#' @param dt_result_y A data.table of the result for the Y-STR
#' @param sn_v_y_male A character vector of the victim names who are estimated to be male
#' @param sn_r_y_male A character vector of the reference names who are estimated to be male
#' @param dt_result_mt A data.table of the result for the mtDNA
#' @param dt_criteria A data.table of criteria
#' @param dt_rel A data.table of information on relationships
#' @param dt_data_manage A data.table for data management
create_combined_data <- function(dt_result_auto, dt_result_y, sn_v_y_male, sn_r_y_male, dt_result_mt, dt_criteria, dt_rel, dt_data_manage){
  # Combine data.table
  dt_combined <- NULL
  if(!is.null(dt_result_auto)){
    dt_combined <- copy(dt_result_auto)
  }
  if(!is.null(dt_result_y)){
    if(is.null(dt_combined)){
      dt_combined <- copy(dt_result_y)
    }else{
      dt_combined <- full_join(dt_combined, dt_result_y, by = c("Victim", "Reference", "Family", "AssumedRel"))
    }
  }
  if(!is.null(dt_result_mt)){
    if(is.null(dt_combined)){
      dt_combined <- copy(dt_result_mt)
    }else{
      dt_combined <- full_join(dt_combined, dt_result_mt, by = c("Victim", "Reference", "Family", "AssumedRel"))
    }
  }

  # Add empty columns
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

  # Investigate sex mismatches
  rel_female_v <- dt_rel$Relationship[dt_rel$Sex_Victim == "F"]
  rel_female_r <- dt_rel$Relationship[dt_rel$Sex_Reference == "F"]
  index_male_v <- which(is.element(dt_combined$Victim, sn_v_y_male))
  index_male_r <- which(is.element(dt_combined$Reference, sn_r_y_male))
  index_sex_mismatch_v <- intersect(which(is.element(dt_combined$AssumedRel, rel_female_v)), index_male_v)
  index_sex_mismatch_r <- intersect(which(is.element(dt_combined$AssumedRel, rel_female_r)), index_male_r)
  index_sex_mismatch <- sort(union(index_sex_mismatch_v, index_sex_mismatch_r))
  dt_combined$Paternal[index_sex_mismatch] <- "Sex mismatch"

  # Create objects for the estimated sex
  est_sex_v <- est_sex_r <- rep(NA, n_data)
  est_sex_v[index_male_v] <- "Male"
  est_sex_r[index_male_r] <- "Male"

  # Create the background colors for the displayed table
  background_color <- create_background_color(dt_combined, dt_criteria, dt_rel)

  # Create multiple candidate groups
  group_cand <- create_group_cand(dt_combined, background_color)

  # Additional columns
  options(warn = -1)
  dt_combined[, EstSexV := est_sex_v]
  dt_combined[, EstSexR := est_sex_r]
  dt_combined[, MultiCandGroup := group_cand]
  dt_combined[, ColorBack := background_color]
  options(warn = 0)

  # Change data type
  sn_v <- sort(unique(dt_combined[, Victim]))
  dt_combined$Victim <- factor(dt_combined$Victim, levels = sn_v, labels = sn_v)
  sn_r <- sort(unique(dt_combined[, Reference]))
  dt_combined$Reference <- factor(dt_combined$Reference, levels = sn_r, labels = sn_r)
  names_rel <- dt_rel$Relationship
  dt_combined$AssumedRel <- factor(dt_combined$AssumedRel, levels = names_rel, labels = names_rel)
  dt_combined$EstimatedRel <- factor(dt_combined$EstimatedRel, levels = c(names_rel), labels = c(names_rel))
  dt_combined$Paternal <- factor(dt_combined$Paternal, levels = c("Not excluded", "Inconclusive", "Excluded", "Sex mismatch"), labels = c("Not excluded", "Inconclusive", "Excluded", "Sex mismatch"))
  dt_combined$Maternal <- factor(dt_combined$Maternal, levels = c("Not excluded", "Inconclusive", "Excluded"), labels = c("Not excluded", "Inconclusive", "Excluded"))
  group_cand_labels <- as.character(sort(unique(group_cand[!is.na(group_cand)])))
  dt_combined$MultiCandGroup <- factor(dt_combined$MultiCandGroup, levels = group_cand_labels, labels = group_cand_labels)

  # Keep only important data
  if(!is.null(dt_result_auto)){
    keep_min_lr <- dt_data_manage$Value[dt_data_manage$Parameter == "keep_min_lr"]
    dt_combined <- dt_combined[LR_Total >= keep_min_lr]
  }else{
    names_rel_paternal <- names_rel[dt_rel$Paternal == "Yes"]
    names_rel_maternal <- names_rel[dt_rel$Maternal == "Yes"]
    bool_keep <- matrix(FALSE, nrow(dt_combined), 3)
    bool_keep[, 1] <- is.element(dt_combined$AssumedRel, unique(c(names_rel_paternal, names_rel_maternal)))
    bool_keep[, 2] <- background_color != 0

    bool_tmp_lineage <- matrix(FALSE, nrow(dt_combined), 2)
    tmp_paternal <- as.character(dt_combined$Paternal)
    tmp_paternal[is.na(tmp_paternal)] <- ""
    bool_tmp_lineage[, 1] <- tmp_paternal == "Not excluded"
    tmp_maternal <- as.character(dt_combined$Maternal)
    tmp_maternal[is.na(tmp_maternal)] <- ""
    bool_tmp_lineage[, 2] <- tmp_maternal == "Not excluded"
    bool_keep[, 3] <- apply(bool_tmp_lineage, 1, any)

    index_keep <- which(apply(bool_keep, 1, all))
    dt_combined <- dt_combined[index_keep, ]
  }

  return(dt_combined)
}

#' create_displayed_data
#'
#' @description The function to create the displayed data
#' @param dt_combined A data.table of the combined data
#' @param fltr_type The filtering method
#' @param min_lr The minimum LR displayed
#' @param max_data_displayed The maximum data displayed
create_displayed_data <- function(dt_combined, fltr_type = "with_auto", min_lr = 100, max_data_displayed = 10000){
  setkey(dt_combined, Victim, Reference, AssumedRel)

  dt_display <- dt_combined[, list(Victim, Reference, Family, AssumedRel, EstimatedRel, LR_Total, Paternal, Maternal, MultiCandGroup, ColorBack)]
  setorder(dt_display, - LR_Total, Paternal, Maternal, na.last = TRUE)

  if(fltr_type == "with_auto"){
    dt_display <- dt_display[LR_Total >= min_lr]
  }else if(fltr_type == "identified"){
    dt_display <- dt_display[ColorBack == 3]
  }else if(fltr_type == "multiple"){
    dt_display <- dt_display[ColorBack == 2]
  }else if(fltr_type == "inconclusive"){
    dt_display <- dt_display[ColorBack == 1]
  }else if(fltr_type == "excluded"){
    dt_display <- dt_display[ColorBack == 0]
  }

  if(nrow(dt_display) > max_data_displayed){
    dt_display <- dt_display[1:max_data_displayed, ]
  }

  return(dt_display)
}
