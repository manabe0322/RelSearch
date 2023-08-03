##########################################
# The function to analyze data for mtDNA #
##########################################

analyze_mt <- function(dt_v_mt, dt_r_mt){

  #############################################
  # Prepare objects to analyze data for mtDNA #
  #############################################

  # Extract required data from victim database
  sn_v_mt <- dt_v_mt[, SampleName]
  range_v_mt <- dt_v_mt[, Range]
  hap_v_mt <- strsplit(dt_v_mt[, Haplotype], " ")
  hap_v_mt <- lapply(hap_v_mt, setdiff, "")

  # Extract required data from reference database
  sn_r_mt <- dt_r_mt[, SampleName]
  range_r_mt <- dt_r_mt[, Range]
  hap_r_mt <- strsplit(dt_r_mt[, Haplotype], " ")
  hap_r_mt <- lapply(hap_r_mt, setdiff, "")

  ##########################
  # Analyze data for mtDNA #
  ##########################

  withProgress(
    withCallingHandlers(
      result_mt <- match_mt_all(hap_v_mt, hap_r_mt, range_v_mt, range_r_mt),
      message = function(m) if(grepl("mtDNA_Victim-Reference_ : ", m$message)){
        val <- as.numeric(gsub("mtDNA_Victim-Reference_ : ", "", m$message))
        setProgress(value = val)
      }
    ),
    message = "Analyzing data of mtDNA...",
    max = length(sn_v_mt) * length(sn_r_mt),
    value = 0
  )

  #######################
  # Arrange the results #
  #######################

  # Create a vector for the result of victim names
  result_sn_v_mt <- rep(sn_v_mt, length(sn_r_mt))

  # Create a vector for the result of reference names
  result_sn_r_mt <- as.vector(sapply(sn_r_mt, rep, length(sn_v_mt)))

  # Create a vector for the result of assumed relationships
  result_assumed_rel <- as.vector(sapply(dt_r_mt[, Relationship], rep, length(sn_v_mt)))

  # Create a part of the data.table
  dt_left <- data.table(Victim = result_sn_v_mt, Reference = result_sn_r_mt, AssumedRel = result_assumed_rel)

  # Create the data.table for the results of mtDNA
  result_mt <- unlist(result_mt)
  result_mt <- matrix(result_mt, nrow = length(sn_v_mt) * length(sn_r_mt), ncol = 3, byrow = TRUE)
  dt_right <- as.data.frame(result_mt)
  setDT(dt_right)
  names(dt_right) <- c("MismatchMt", "ShareRangeMt", "ShareLengthMt")
  dt_result_mt <- cbind(dt_left, dt_right)

  # Return
  return(dt_result_mt)
}
