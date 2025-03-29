#' create_background_color
#'
#' @description The function to create the sign of the number of candidates (0: Excluded, 1: Inconclusive, 2: Multiple candidates, 3: Identified)
#' @param dt_combined A data.table of the combined data
#' @param index_change_yellow Indices of inconclusive paternal or maternal lineages with supportive LR
#' @param index_change_red Indices of excluded paternal or maternal lineages with supportive LR
create_background_color <- function(dt_combined, index_change_yellow, index_change_red){
  background_color <- rep(0, nrow(dt_combined))

  # Initially, backgound colors are determined based on the LR
  ## Extract data with supportive LR
  index_identified <- which(!is.element(dt_combined[, EstimatedRel], c("Inconclusive", "Unrelated")))
  background_color[index_identified] <- 3

  ## Extract data with inconclusive LR
  index_inconclusive_lr <- which(dt_combined[, EstimatedRel] == "Inconclusive")
  background_color[index_inconclusive_lr] <- 1

  # Next, background colors are updated based on the results for Y-STR and mtDNA
  background_color[index_change_yellow] <- 1
  background_color[index_change_red] <- 0

  # Finally, check multiple candidates
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
#' @param dt_rel A data.table of information on relationships
#' @param dt_data_manage A data.table for data management
create_combined_data <- function(dt_result_auto, dt_result_y, sn_v_y_male, sn_r_y_male, dt_result_mt, dt_rel, dt_data_manage){
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

  # Investigate indices of data that does not support lineage unexpectedly
  assumed_rel_all <- dt_combined[, AssumedRel]
  est_rel_all <- dt_combined[, EstimatedRel]
  est_paternal_all <- dt_combined[, Paternal]
  est_maternal_all <- dt_combined[, Maternal]
  names_rel <- dt_rel[, Relationship]
  info_paternal <- dt_rel[, Paternal]
  info_maternal <- dt_rel[, Maternal]
  index_not_exclude_lr <- which(!is.element(est_rel_all, "Unrelated"))
  index_unexpectedly_excluded_y <- index_unexpectedly_excluded_mt <- integer(0)
  index_unexpectedly_inconclusive_y <- index_unexpectedly_inconclusive_mt <- integer(0)
  for(i in 1:length(names_rel)){
    index_tmp <- which(assumed_rel_all == names_rel[i])
    index_target_rel <- sort(intersect(index_not_exclude_lr, index_tmp))
    if(length(index_target_rel) > 0){
      est_paternal_target_rel <- est_paternal_all[index_target_rel]
      index_unexpectedly_excluded_y <- c(index_unexpectedly_excluded_y, index_target_rel[which(is.element(est_paternal_target_rel, "Sex mismatch"))])
      if(info_paternal[i] == "Yes"){
        index_unexpectedly_excluded_y <- c(index_unexpectedly_excluded_y, index_target_rel[which(is.element(est_paternal_target_rel, "Excluded"))])
        index_unexpectedly_inconclusive_y <- c(index_unexpectedly_inconclusive_y, index_target_rel[which(est_paternal_target_rel == "Inconclusive")])
      }
      if(info_maternal[i] == "Yes"){
        est_maternal_target_rel <- est_maternal_all[index_target_rel]
        index_unexpectedly_excluded_mt <- c(index_unexpectedly_excluded_mt, index_target_rel[which(est_maternal_target_rel == "Excluded")])
        index_unexpectedly_inconclusive_mt <- c(index_unexpectedly_inconclusive_mt, index_target_rel[which(est_maternal_target_rel == "Inconclusive")])
      }
    }
  }

  # Update estimated relationships
  index_exclude_y_mt <- sort(unique(c(index_unexpectedly_excluded_y, index_unexpectedly_excluded_mt)))
  index_inconclusive_y_mt <- sort(unique(c(index_unexpectedly_inconclusive_y, index_unexpectedly_inconclusive_mt)))
  index_change_yellow <- intersect(index_not_exclude_lr, index_inconclusive_y_mt)
  index_change_red <- intersect(index_not_exclude_lr, index_exclude_y_mt)
  est_rel_all[index_change_yellow] <- "Inconclusive"
  est_rel_all[index_change_red] <- "Excluded" # Priotize Excluded to Inconclusive
  dt_combined[, EstimatedRel := est_rel_all]

  # Create the background colors for the displayed table
  background_color <- create_background_color(dt_combined, index_change_yellow, index_change_red)

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
  dt_combined$AssumedRel <- factor(dt_combined$AssumedRel, levels = names_rel, labels = names_rel)
  dt_combined$EstimatedRel <- factor(dt_combined$EstimatedRel, levels = c(names_rel, "Inconclusive", "Unrelated", "Excluded"), labels = c(names_rel, "Inconclusive", "Unrelated", "Excluded"))
  dt_combined$Paternal <- factor(dt_combined$Paternal, levels = c("Not excluded", "Inconclusive", "Excluded", "Sex mismatch"), labels = c("Not excluded", "Inconclusive", "Excluded", "Sex mismatch"))
  dt_combined$Maternal <- factor(dt_combined$Maternal, levels = c("Not excluded", "Inconclusive", "Excluded"), labels = c("Not excluded", "Inconclusive", "Excluded"))
  group_cand_labels <- as.character(sort(unique(group_cand[!is.na(group_cand)])))
  dt_combined$MultiCandGroup <- factor(dt_combined$MultiCandGroup, levels = group_cand_labels, labels = group_cand_labels)

  # Keep only important data
  if(!is.null(dt_result_auto)){
    keep_min_lr <- dt_data_manage$Value[dt_data_manage$Parameter == "keep_min_lr"]
    dt_combined <- dt_combined[LR_Total >= keep_min_lr]
  }else{
    dt_combined <- dt_combined[Paternal == "Not excluded" | Maternal == "Not excluded"]
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
