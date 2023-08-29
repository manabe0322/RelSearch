#################################################################
# The function to rearrange in order of loci for Y-STR database #
#################################################################

order_loci_y <- function(dt_v_y, dt_r_y){

  # Extract loci
  locus_y <- setdiff(names(dt_v_y), c("SampleName", "Relationship"))

  # The number of loci
  n_l <- length(locus_y)

  # Define objects for the column position
  pos_v <- rep(0, n_l + 1)
  pos_r <- rep(0, n_l + 2)

  # Assign the index for the position of SampleName
  pos_v[1] <- which(is.element(names(dt_v_y), "SampleName"))
  pos_r[1] <- which(is.element(names(dt_r_y), "SampleName"))

  # Assign the index for the position of Relationship
  pos_r[2] <- which(is.element(names(dt_r_y), "Relationship"))

  # Assign indices for the position of each locus
  for(i in 1:n_l){
    pos_v[i + 1] <- which(is.element(names(dt_v_y), locus_y[i]))
    pos_r[i + 2] <- which(is.element(names(dt_r_y), locus_y[i]))
  }

  # Rearrange in order of loci
  dt_v_y <- dt_v_y[, pos_v, with = FALSE]
  dt_r_y <- dt_r_y[, pos_r, with = FALSE]

  # Return
  return(list(dt_v_y, dt_r_y))
}


##########################################
# The function to analyze data for Y-STR #
##########################################

analyze_y <- function(dt_v_y, dt_r_y, dt_criteria, show_progress = TRUE){

  #########################################
  # Prepare objects to analyze Y-STR data #
  #########################################

  # Extract loci
  locus_y <- setdiff(names(dt_v_y), c("SampleName", "Relationship"))

  # The number of loci
  n_l <- length(locus_y)

  # Extract required data from victim database
  sn_v_y <- dt_v_y[, SampleName]
  hap_v_y <- as.matrix(dt_v_y)
  hap_v_y <- hap_v_y[, which(is.element(names(dt_v_y), locus_y))]

  # Extract required data from reference database
  sn_r_y <- dt_r_y[, SampleName]
  hap_r_y <- as.matrix(dt_r_y)
  hap_r_y <- hap_r_y[, which(is.element(names(dt_r_y), locus_y))]

  # The NA in genotypes is replaced to "" to deal with the C++ program
  hap_v_y[which(is.na(hap_v_y) == TRUE, arr.ind = TRUE)] <- ""
  hap_r_y[which(is.na(hap_r_y) == TRUE, arr.ind = TRUE)] <- ""

  # Change matrix to list for haplotypes
  hap_v_y <- asplit(hap_v_y, 1)
  hap_r_y <- asplit(hap_r_y, 1)

  ##########################
  # Analyze data for Y-STR #
  ##########################

  if(show_progress){
    withProgress(
      withCallingHandlers(
        result_y <- match_y_all(hap_v_y, hap_r_y),
        message = function(m) if(grepl("Y-STR_Victim-Reference_ : ", m$message)){
          val <- as.numeric(gsub("Y-STR_Victim-Reference_ : ", "", m$message))
          setProgress(value = val)
        }
      ),
      message = "Analyzing Y-STR data...",
      max = length(sn_v_y) * length(sn_r_y),
      value = 0
    )
  }else{
    result_y <- match_y_all(hap_v_y, hap_r_y)
  }

  #######################
  # Arrange the results #
  #######################

  # Create a vector for the result of victim names
  result_sn_v_y <- rep(sn_v_y, length(sn_r_y))

  # Create a vector for the result of reference names
  result_sn_r_y <- as.vector(sapply(sn_r_y, rep, length(sn_v_y)))

  # Create a vector for the result of assumed relationships
  result_assumed_rel <- as.vector(sapply(dt_r_y[, Relationship], rep, length(sn_v_y)))

  # Create a part of the data.table
  dt_left <- data.table(Victim = result_sn_v_y, Reference = result_sn_r_y, AssumedRel = result_assumed_rel)

  # Create the data.table for the results of Y-STR
  result_y <- unlist(result_y)
  result_y <- matrix(result_y, nrow = length(sn_v_y) * length(sn_r_y), ncol = 3 * (n_l + 1), byrow = TRUE)
  dt_right <- as.data.frame(result_y)
  setDT(dt_right)
  names(dt_right) <- c(paste0("Mismatch_", c(locus_y, "Total")), paste0("Ignore_", c(locus_y, "Total")), paste0("MuStep_", c(locus_y, "Total")))
  dt_result_y <- cbind(dt_left, dt_right)

  ###############################
  # Estimate paternal relatives #
  ###############################

  # Extract criteria
  max_mismatch_y <- dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_y"]
  max_ignore_y <- dt_criteria$Value[dt_criteria$Criteria == "max_ignore_y"]
  max_mustep_y <- dt_criteria$Value[dt_criteria$Criteria == "max_mustep_y"]

  # Estimate paternal relatives using criteria
  n_data <- nrow(dt_result_y)
  paternal_all <- rep("Not support", n_data)
  bool_meet_criteria_y <- matrix(FALSE, n_data, 4)
  bool_meet_criteria_y[, 1] <- dt_result_y[, "Mismatch_Total"] <= max_mismatch_y
  bool_meet_criteria_y[, 2] <- dt_result_y[, "Ignore_Total"] <= max_ignore_y
  bool_meet_criteria_y[, 3] <- dt_result_y[, "MuStep_Total"] <= max_mustep_y
  bool_meet_criteria_y[, 4] <- dt_result_y[, "MuStep_Total"] %% 1 == 0
  pos_meet_criteria_y <- which(apply(bool_meet_criteria_y, 1, all))
  paternal_all[pos_meet_criteria_y] <- "Support"

  # Add the column "Paternal" to the data.table
  options(warn = -1)
  dt_result_y[, Paternal := paternal_all]
  options(warn = 0)

  # Return
  return(dt_result_y)
}
