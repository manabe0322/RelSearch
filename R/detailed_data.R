################################################################
# The function to make a vector of genotypes for detailed data #
################################################################

display_gt <- function(data_gt){

  # The number of loci
  n_l <- length(data_gt) / 2

  # Define a vector of genotypes for detailed data
  gt_vec <- rep("", n_l)

  # Repetitive execution for each locus
  for(i in 1:n_l){

    # Extract a genotype in one locus
    gt <- as.numeric(data_gt[c(2 * i - 1, 2 * i)])

    # Remove NA
    gt <- gt[!is.na(gt)]

    # If homozygote
    if(length(gt) == 1){
      gt_vec[i] <- gt

      # If heterozygote
    }else if(length(gt) == 2){
      gt_vec[i] <- paste(gt[1], ", ", gt[2], sep = "")
    }
  }

  # Return
  return(gt_vec)
}


##############################################################
# The function to create the detailed data for autosomal STR #
##############################################################

create_detailed_data_auto <- function(dt_v_auto, dt_r_auto, sn_v_select, sn_r_select, assumed_rel_select, result_selected){

  # Set key for each data.table
  setkey(dt_v_auto, SampleName)
  setkey(dt_r_auto, SampleName, Relationship)

  # Extract the column names of the selected data
  cn_result <- names(result_selected)

  # Extract genotypes for the selected data
  dt_v_auto_select <- dt_v_auto[.(sn_v_select), nomatch = NULL]
  dt_r_auto_select <- dt_r_auto[.(sn_r_select, assumed_rel_select), nomatch = NULL]

  # Create detailed data
  if(nrow(dt_v_auto_select) == 1 && nrow(dt_r_auto_select) == 1){

    # Extract profiles
    options(warn = -1)
    prof_v_select <- as.numeric(dt_v_auto_select[, -c("SampleName", "Relationship"), with = FALSE])
    prof_r_select <- as.numeric(dt_r_auto_select[, -c("SampleName", "Relationship"), with = FALSE])
    options(warn = 0)

    # Create displayed genotypes
    prof_v_display <- c(display_gt(prof_v_select), "")
    prof_r_display <- c(display_gt(prof_r_select), "")

    # Locus
    locus_display <- gsub("LikeH1_", "", cn_result[grep("LikeH1_", cn_result)])

    # Likelihood of H1
    like_h1_display <- signif(as.numeric(result_selected[, grep("LikeH1_", cn_result), with = FALSE]), 3)

    # Likelihood of H2
    like_h2_display <- signif(as.numeric(result_selected[, grep("LikeH2_", cn_result), with = FALSE]), 3)

    # LR
    lr_display <- signif(as.numeric(result_selected[, grep("LR_", cn_result), with = FALSE]), 3)

    # Create the data.table
    dt_detail_auto <- data.table(Locus = locus_display, Profile_V = prof_v_display, Profile_R = prof_r_display, LikeH1 = like_h1_display, LikeH2 = like_h2_display, LR = lr_display)

  }else{
    dt_detail_auto <- NULL
  }

  # Return
  return(dt_detail_auto)
}


######################################################
# The function to create the detailed data for Y-STR #
######################################################

create_detailed_data_y <- function(dt_v_y, dt_r_y, sn_v_select, sn_r_select, assumed_rel_select, result_selected){

  # Set key for each data.table
  setkey(dt_v_y, SampleName)
  setkey(dt_r_y, SampleName, Relationship)

  # Extract the column names of the selected data
  cn_result <- names(result_selected)

  # Extract haplotypes for the selected data
  dt_v_y_select <- dt_v_y[.(sn_v_select), nomatch = NULL]
  dt_r_y_select <- dt_r_y[.(sn_r_select, assumed_rel_select), nomatch = NULL]

  # Create detailed data
  if(nrow(dt_v_y_select) == 1 && nrow(dt_r_y_select) == 1){

    # Victim profile
    options(warn = -1)
    prof_v_display <- c(as.character(dt_v_y_select[, -c("SampleName", "Relationship"), with = FALSE]), "")
    prof_v_display[is.na(prof_v_display)] <- ""
    options(warn = 0)

    # Reference profile
    options(warn = -1)
    prof_r_display <- c(as.character(dt_r_y_select[, -c("SampleName", "Relationship"), with = FALSE]), "")
    prof_r_display[is.na(prof_r_display)] <- ""
    options(warn = 0)

    # Locus
    locus_display <- gsub("Mismatch_", "", cn_result[grep("Mismatch_", cn_result)])

    # Mismatched loci
    mismatch_y_display <- as.character(result_selected[, grep("Mismatch_", cn_result), with = FALSE])

    # Ignored loci
    ignore_y_display <- as.character(result_selected[, grep("Ignore_", cn_result), with = FALSE])

    # Mutational step
    mustep_y_display <- result_selected[, grep("MuStep_", cn_result), with = FALSE]
    pos99 <- which(mustep_y_display >= 99)
    mustep_y_display <- as.character(mustep_y_display)
    mustep_y_display[pos99] <- "Not integer"

    # Create the data.table
    dt_detail_y <- data.table(Locus = locus_display, Profile_V = prof_v_display, Profile_R = prof_r_display, Ignore = ignore_y_display, Mismatch = mismatch_y_display, MuStep = mustep_y_display)
  }else{
    dt_detail_y <- NULL
  }

  # Return
  return(dt_detail_y)
}


######################################################
# The function to create the detailed data for mtDNA #
######################################################

create_detailed_data_mt <- function(dt_v_mt, dt_r_mt, sn_v_select, sn_r_select, assumed_rel_select, result_selected){

  # Set key for each input data
  setkey(dt_v_mt, SampleName)
  setkey(dt_r_mt, SampleName, Relationship)

  # Extract haplotypes for the selected data
  dt_v_mt_select <- dt_v_mt[.(sn_v_select), nomatch = NULL]
  dt_r_mt_select <- dt_r_mt[.(sn_r_select, assumed_rel_select), nomatch = NULL]

  # Create detailed data
  if(nrow(dt_v_mt_select) == 1 && nrow(dt_r_mt_select) == 1){

    # Victim types
    options(warn = -1)
    type_v <- as.character(dt_v_mt_select[, -c("SampleName", "Relationship", "Range"), with = FALSE])
    options(warn = 0)
    type_v <- strsplit(type_v, " ")[[1]]
    type_v <- setdiff(type_v, "")
    type_v <- type_v[order(sapply(type_v, extract_integer))]

    # Reference types
    options(warn = -1)
    type_r <- as.character(dt_r_mt_select[, -c("SampleName", "Relationship", "Range"), with = FALSE])
    options(warn = 0)
    type_r <- strsplit(type_r, " ")[[1]]
    type_r <- setdiff(type_r, "")
    type_r <- type_r[order(sapply(type_r, extract_integer))]

    # Victim or Reference types
    type_vr <- union(type_v, type_r)
    type_vr <- type_vr[order(sapply(type_vr, extract_integer))]
    n_type_vr <- length(type_vr)

    # Common positions between victim and reference
    pos_mt_vr <- extract_pos_mt_vr(dt_v_mt_select[, Range], dt_r_mt_select[, Range])

    # Victim types (displayed)
    type_v_display <- rep("", n_type_vr)
    type_v_display[is.element(type_vr, type_v)] <- type_v

    # Reference types (displayed)
    type_r_display <- rep("", n_type_vr)
    type_r_display[is.element(type_vr, type_r)] <- type_r

    # Bool for common range
    pos_common <- is.element(sapply(type_vr, extract_integer), pos_mt_vr)

    # Out of range (displayed)
    out_range_display <- rep("", n_type_vr)
    out_range_display[!pos_common] <- "x"

    # Bool for mismatch
    pos_mismatch <- apply(rbind(!is.element(type_vr, type_v), !is.element(type_vr, type_r)), 2, any)

    # Mismatch (displayed)
    mismatch_mt_display <- rep("", n_type_vr)
    mismatch_mt_display[apply(rbind(pos_common, pos_mismatch), 2, all)] <- "x"

    # Create the data.table
    dt_detail_mt <- data.table(Type_Victim = type_v_display, Type_Reference = type_r_display, OutRange = out_range_display, Mismatch = mismatch_mt_display)
  }else{
    dt_detail_mt <- NULL
  }

  # Return
  return(dt_detail_mt)
}
