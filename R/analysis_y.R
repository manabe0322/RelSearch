#' order_loci_y
#'
#' @description The function to rearrange in order of loci for Y-STR database
#' @param dt_v_y A data.table of victim profiles (Y-STR)
#' @param dt_r_y A data.table of reference profiles (Y-STR)
order_loci_y <- function(dt_v_y, dt_r_y){
  locus_y <- setdiff(names(dt_v_y), c("SampleName", "Relationship"))
  n_l <- length(locus_y)

  # Define objects for the column position
  pos_v <- rep(0, n_l + 1)
  pos_r <- rep(0, n_l + 2)

  pos_v[1] <- which(is.element(names(dt_v_y), "SampleName"))
  pos_r[1] <- which(is.element(names(dt_r_y), "SampleName"))
  pos_r[2] <- which(is.element(names(dt_r_y), "Relationship"))

  for(i in 1:n_l){
    pos_v[i + 1] <- which(is.element(names(dt_v_y), locus_y[i]))
    pos_r[i + 2] <- which(is.element(names(dt_r_y), locus_y[i]))
  }

  dt_v_y <- dt_v_y[, pos_v, with = FALSE]
  dt_r_y <- dt_r_y[, pos_r, with = FALSE]

  return(list(dt_v_y, dt_r_y))
}

#' analyze_y
#'
#' @description The function to analyze data for Y-STR
#' @param dt_v_y A data.table of victim profiles (Y-STR)
#' @param dt_r_y A data.table of reference profiles (Y-STR)
#' @param dt_criteria A data.table of criteria
#' @param show_progress Whether the progress is shown or not
analyze_y <- function(dt_v_y, dt_r_y, dt_criteria, show_progress = TRUE){

  #########################################
  # Prepare objects to analyze Y-STR data #
  #########################################

  locus_y <- setdiff(names(dt_v_y), c("SampleName", "Relationship"))
  n_l <- length(locus_y)

  sn_v_y <- dt_v_y[, SampleName]
  hap_v_y <- as.matrix(dt_v_y)
  hap_v_y <- hap_v_y[, which(is.element(names(dt_v_y), locus_y))]

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

  ##################################################
  # Create the data.table for the results of Y-STR #
  ##################################################

  result_sn_v_y <- rep(sn_v_y, length(sn_r_y))
  result_sn_r_y <- as.vector(sapply(sn_r_y, rep, length(sn_v_y)))
  result_assumed_rel <- as.vector(sapply(dt_r_y[, Relationship], rep, length(sn_v_y)))

  dt_left <- data.table(Victim = result_sn_v_y, Reference = result_sn_r_y, AssumedRel = result_assumed_rel)
  result_y <- unlist(result_y)
  result_y <- matrix(result_y, nrow = length(sn_v_y) * length(sn_r_y), ncol = 3 * (n_l + 1), byrow = TRUE)
  dt_right <- as.data.frame(result_y)
  setDT(dt_right)
  names(dt_right) <- c(paste0("Mismatch_", c(locus_y, "Total")), paste0("Ignore_", c(locus_y, "Total")), paste0("MuStep_", c(locus_y, "Total")))
  dt_result_y <- cbind(dt_left, dt_right)

  ###############################
  # Estimate paternal relatives #
  ###############################

  max_mismatch_y <- dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_y"]
  max_ignore_y <- dt_criteria$Value[dt_criteria$Criteria == "max_ignore_y"]
  max_mustep_y <- dt_criteria$Value[dt_criteria$Criteria == "max_mustep_y"]

  n_data <- nrow(dt_result_y)
  paternal_all <- rep("Not support", n_data)
  bool_meet_criteria_y <- matrix(FALSE, n_data, 4)
  bool_meet_criteria_y[, 1] <- dt_result_y[, "Mismatch_Total"] <= max_mismatch_y
  bool_meet_criteria_y[, 2] <- dt_result_y[, "Ignore_Total"] <= max_ignore_y
  bool_meet_criteria_y[, 3] <- dt_result_y[, "MuStep_Total"] <= max_mustep_y
  bool_meet_criteria_y[, 4] <- dt_result_y[, "MuStep_Total"] %% 1 == 0
  pos_meet_criteria_y <- which(apply(bool_meet_criteria_y, 1, all))
  paternal_all[pos_meet_criteria_y] <- "Support"

  options(warn = -1)
  dt_result_y[, Paternal := paternal_all]
  options(warn = 0)

  return(dt_result_y)
}
