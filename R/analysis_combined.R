###############################################################
# The function to create the sign of the number of candidates #
###############################################################

create_num_cand <- function(dt_combined){

  # Define the initial number of candidates
  num_cand <- rep(0, nrow(dt_combined))

  # Extract indices with the estimated relationship
  pos_est_rel <- which(dt_combined[, EstimatedRel] != "")

  # The number of indices
  n_est_rel <- length(pos_est_rel)

  # If the number of indices > 0
  if(n_est_rel > 0){

    # Extract data with the estimated relationship
    dt_extract <- dt_combined[pos_est_rel, ]

    # Extract victims
    vics <- dt_extract[, Victim]

    # Extract references
    refs <- dt_extract[, Reference]

    # Count the number of candidates
    for(i in seq_len(n_est_rel)){
      num_cand[pos_est_rel[i]] <- length(union(which(vics == vics[i]), which(refs == refs[i])))
    }
  }

  # Return
  return(num_cand)
}


########################################################
# The function to judge relationships in combined data #
########################################################

judge_rel_combined_data <- function(dt_combined, dt_result_auto, dt_result_y, dt_result_mt, dt_criteria){

  # Extract criteria
  min_lr_auto <- dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"]
  max_mismatch_y <- dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_y"]
  max_ignore_y <- dt_criteria$Value[dt_criteria$Criteria == "max_ignore_y"]
  max_mustep_y <- dt_criteria$Value[dt_criteria$Criteria == "max_mustep_y"]
  max_mismatch_mt <- dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_mt"]
  min_share_mt <- dt_criteria$Value[dt_criteria$Criteria == "min_share_mt"]

  # The number of data
  n_data <- nrow(dt_combined)

  # Define initial objects
  est_rel_all <- rep("", n_data)
  paternal_all <- rep("", n_data)
  maternal_all <- rep("", n_data)

  # Record estimated relationships
  if(!is.null(dt_result_auto)){
    pos_meet_criteria_auto <- which(dt_combined[, "LR_Total"] >= min_lr_auto)
    est_rel_all[pos_meet_criteria_auto] <- dt_combined[pos_meet_criteria_auto, AssumedRel]
  }

  # Record paternal relationships
  if(!is.null(dt_result_y)){
    bool_meet_criteria_y <- matrix(FALSE, n_data, 4)
    bool_meet_criteria_y[, 1] <- dt_combined[, "Mismatch_Total"] <= max_mismatch_y
    bool_meet_criteria_y[, 2] <- dt_combined[, "Ignore_Total"] <= max_ignore_y
    bool_meet_criteria_y[, 3] <- dt_combined[, "MuStep_Total"] <= max_mustep_y
    bool_meet_criteria_y[, 4] <- dt_combined[, "MuStep_Total"] %% 1 == 0
    pos_meet_criteria_y <- which(apply(bool_meet_criteria_y, 1, all))
    paternal_all[pos_meet_criteria_y] <- "support"
  }

  # Record maternal relationships
  if(!is.null(dt_result_mt)){
    bool_meet_criteria_mt <- matrix(FALSE, n_data, 2)
    bool_meet_criteria_mt[, 1] <- dt_combined[, "MismatchMt"] <= max_mismatch_mt
    bool_meet_criteria_mt[, 2] <- dt_combined[, "ShareLengthMt"] >= min_share_mt
    pos_meet_criteria_mt <- which(apply(bool_meet_criteria_mt, 1, all))
    maternal_all[pos_meet_criteria_mt] <- "support"
  }

  # Add columns to the data.table
  options(warn = -1)
  if(is.null(dt_result_auto)){
    dt_combined[, AssumedRel := rep("", n_data)]
    dt_combined[, LR_Total := rep(NA, n_data)]
  }
  dt_combined[, EstimatedRel := est_rel_all]
  dt_combined[, Paternal := paternal_all]
  dt_combined[, Maternal := maternal_all]
  options(warn = 0)

  # Create the sign of the number of candidates
  num_cand <- create_num_cand(dt_combined)
  options(warn = -1)
  dt_combined[, NumCand := num_cand]
  options(warn = 0)

  # Return
  return(dt_combined)
}


########################################
# The function to create combined data #
########################################

create_combined_data <- function(dt_result_auto, dt_result_y, dt_result_mt, dt_criteria){

  ##################################
  # Create the combined data.table #
  ##################################

  # Define the initial combined data.table
  dt_combined <- NULL

  # If the analysis for autosomal STR is finished
  if(!is.null(dt_result_auto)){

    # Update the combined data.table
    dt_combined <- copy(dt_result_auto)
  }

  # If the analysis for Y-STR is finished
  if(!is.null(dt_result_y)){

    # Update the combined data.table
    if(is.null(dt_combined)){
      dt_combined <- copy(dt_result_y)
    }else{
      dt_combined <- full_join(dt_combined, dt_result_y, by = c("Victim", "Reference", "AssumedRel"))
    }
  }

  # If the analysis for mtDNA is finished
  if(!is.null(dt_result_mt)){

    # Update the combined data.table
    if(is.null(dt_combined)){
      dt_combined <- copy(dt_result_mt)
    }else{
      dt_combined <- full_join(dt_combined, dt_result_mt, by = c("Victim", "Reference", "AssumedRel"))
    }
  }

  ########################################
  # Judge relationships in combined data #
  ########################################

  # Run judge_rel_combined_data
  dt_combined <- judge_rel_combined_data(dt_combined, dt_result_auto, dt_result_y, dt_result_mt, dt_criteria)

  # Return
  return(dt_combined)
}


#############################################
# The function to create the displayed data #
#############################################

create_displayed_data <- function(dt_combined, min_lr = NULL, no_lr = FALSE){

  # Set key
  setkey(dt_combined, Victim, Reference, AssumedRel)

  # Extract required columns
  dt_display <- dt_combined[, list(Victim, Reference, AssumedRel, LR_Total, EstimatedRel, Paternal, Maternal, NumCand)]

  # Filtering
  if(no_lr){
    dt_display <- dt_display[is.na(LR_Total)]
  }else if(!is.null(min_lr)){
    dt_display <- dt_display[LR_Total >= min_lr]
  }

  # Descending order of LR
  setorder(dt_display, cols = - "LR_Total", na.last = TRUE)

  # Round LR
  dt_display$LR_Total <- signif(dt_display$LR_Total, 3)

  # Return
  return(dt_display)
}
