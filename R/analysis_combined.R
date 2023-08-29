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
      nc <- length(union(which(vics == vics[i]), which(refs == refs[i])))
      if(nc >= 2){
        num_cand[pos_est_rel[i]] <- 2
      }else{
        num_cand[pos_est_rel[i]] <- nc
      }
    }
  }

  # Return
  return(num_cand)
}


########################################
# The function to create combined data #
########################################

create_combined_data <- function(dt_result_auto, dt_result_y, dt_result_mt, dt_rel){

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

  # Add columns to the data.table
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

  # Change character to factor
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

  # Return
  return(dt_combined)
}


#############################################
# The function to create the displayed data #
#############################################

create_displayed_data <- function(dt_combined, fltr_type = "default", min_lr = NULL){

  # Set key
  setkey(dt_combined, Victim, Reference, AssumedRel)

  # Extract required columns
  dt_display <- dt_combined[, list(Victim, Reference, AssumedRel, LR_Total, EstimatedRel, Paternal, Maternal, NumCand)]

  # Filtering
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

  # Descending order of LR
  setorder(dt_display, cols = - "LR_Total", na.last = TRUE)

  # Round LR
  dt_display$LR_Total <- signif(dt_display$LR_Total, 3)

  # Return
  return(dt_display)
}
