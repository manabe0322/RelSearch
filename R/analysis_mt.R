#' analyze_mt
#'
#' @description The function to analyze data for mtDNA
#' @param dt_v_mt A data.table of victim profiles (mtDNA)
#' @param dt_r_mt A data.table of reference profiles (mtDNA)
#' @param dt_criteria A data.table of criteria
#' @param show_progress Whether the progress is shown or not
analyze_mt <- function(dt_v_mt, dt_r_mt, dt_criteria, show_progress = TRUE){

  #############################################
  # Prepare objects to analyze data for mtDNA #
  #############################################

  sn_v_mt <- dt_v_mt[, SampleName]
  range_v_mt <- dt_v_mt[, Range]
  hap_v_mt <- strsplit(dt_v_mt[, Haplotype], " ")
  hap_v_mt <- lapply(hap_v_mt, setdiff, "")

  sn_r_mt <- dt_r_mt[, SampleName]
  range_r_mt <- dt_r_mt[, Range]
  hap_r_mt <- strsplit(dt_r_mt[, Haplotype], " ")
  hap_r_mt <- lapply(hap_r_mt, setdiff, "")

  # Number of pairs
  n_pair <- length(sn_v_mt) * length(sn_r_mt)

  ##########################
  # Analyze data for mtDNA #
  ##########################

  if(show_progress){
    withProgress(
      withCallingHandlers(
        result_mt <- match_mt_all(hap_v_mt, hap_r_mt, range_v_mt, range_r_mt),
        message = function(m) if(grepl("mtDNA_Victim-Reference_ : ", m$message)){
          val <- as.numeric(gsub("mtDNA_Victim-Reference_ : ", "", m$message))
          setProgress(value = val, message = paste0(round(100 * val / n_pair, 0), "% done"))
        }
      ),
      message = "0% done",
      max = n_pair,
      value = 0
    )
  }else{
    result_mt <- match_mt_all(hap_v_mt, hap_r_mt, range_v_mt, range_r_mt)
  }

  ##################################################
  # Create the data.table for the results of mtDNA #
  ##################################################

  result_sn_v_mt <- rep(sn_v_mt, length(sn_r_mt))
  result_sn_r_mt <- as.vector(sapply(sn_r_mt, rep, length(sn_v_mt)))
  result_family <- as.vector(sapply(dt_r_mt[, Family], rep, length(sn_v_mt)))
  result_assumed_rel <- as.vector(sapply(dt_r_mt[, Relationship], rep, length(sn_v_mt)))

  dt_left <- data.table(Victim = result_sn_v_mt, Reference = result_sn_r_mt, Family = result_family, AssumedRel = result_assumed_rel)
  result_mt <- unlist(result_mt)
  result_mt <- matrix(result_mt, nrow = length(sn_v_mt) * length(sn_r_mt), ncol = 3, byrow = TRUE)
  dt_right <- as.data.frame(result_mt)
  setDT(dt_right)
  names(dt_right) <- c("MismatchMt", "ShareRangeMt", "ShareLengthMt")
  dt_result_mt <- cbind(dt_left, dt_right)

  ###############################
  # Estimate maternal relatives #
  ###############################

  max_mismatch_mt <- dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_mt"]
  min_share_mt <- dt_criteria$Value[dt_criteria$Criteria == "min_share_mt"]

  n_data <- nrow(dt_result_mt)
  maternal_all <- rep("Not support", n_data)
  bool_meet_criteria_mt <- matrix(FALSE, n_data, 2)
  bool_meet_criteria_mt[, 1] <- dt_result_mt[, "MismatchMt"] <= max_mismatch_mt
  bool_meet_criteria_mt[, 2] <- dt_result_mt[, "ShareLengthMt"] >= min_share_mt
  pos_meet_criteria_mt <- which(apply(bool_meet_criteria_mt, 1, all))
  maternal_all[pos_meet_criteria_mt] <- "Support"

  options(warn = -1)
  dt_result_mt[, Maternal := maternal_all]
  options(warn = 0)

  return(dt_result_mt)
}
