#' set_af
#'
#' @description The function to set allele frequencies
#' @param dt_v_auto A data.table of victim profiles (autosomal STR)
#' @param dt_r_auto A data.table of reference profiles (autosomal STR)
#' @param dt_af A data.table of allele frequencies (autosomal STR)
#' @param maf The minimum allele frequency
set_af <- function(dt_v_auto, dt_r_auto, dt_af, maf){
  n_l <- ncol(dt_af) - 1
  name_al <- dt_af[, Allele]
  name_l <- setdiff(names(dt_v_auto), c("SampleName", "Relationship"))
  af_list <- af_al_list <- list()

  for(i in 1:n_l){
    pos_v <- which(names(dt_v_auto) == name_l[i])
    pos_r <- which(names(dt_r_auto) == name_l[i])
    pos_af <- which(names(dt_af) == name_l[i])

    obsal <- unique(c(dt_v_auto[[pos_v[1]]], dt_v_auto[[pos_v[2]]], dt_r_auto[[pos_r[1]]], dt_r_auto[[pos_r[2]]]))
    obsal <- obsal[!is.na(obsal)]

    af <- dt_af[[pos_af]]
    pos_al <- !is.na(af)
    af <- af[pos_al]
    af_al <- name_al[pos_al]

    pos_unobs <- which(is.element(obsal, af_al) == FALSE)
    if(length(pos_unobs) > 0){
      af_al <- c(af_al, obsal[pos_unobs])
      af <- c(af, rep(maf, length(pos_unobs)))
    }
    af_list[[i]] <- af
    af_al_list[[i]] <- af_al
  }
  names(af_list) <- name_l
  names(af_al_list) <- name_l
  return(list(af_list, af_al_list))
}

#' order_loci_auto
#'
#' @description The function to rearrange in order of loci for autosomal STR database
#' @param dt_v_auto A data.table of victim profiles (autosomal STR)
#' @param dt_r_auto A data.table of reference profiles (autosomal STR)
#' @param dt_af A data.table of allele frequencies (autosomal STR)
order_loci_auto <- function(dt_v_auto, dt_r_auto, dt_af){
  locus_auto <- setdiff(names(dt_v_auto), c("SampleName", "Relationship"))
  n_l <- length(locus_auto)

  # Define objects for the column position
  pos_v <- rep(0, 2 * n_l + 1)
  pos_r <- rep(0, 2 * n_l + 2)
  pos_af <- rep(0, n_l + 1)

  pos_v[1] <- which(is.element(names(dt_v_auto), "SampleName"))
  pos_r[1] <- which(is.element(names(dt_r_auto), "SampleName"))
  pos_r[2] <- which(is.element(names(dt_r_auto), "Relationship"))
  pos_af[1] <- which(is.element(names(dt_af), "Allele"))

  for(i in 1:n_l){
    pos_v[c(2 * i, 2 * i + 1)] <- which(is.element(names(dt_v_auto), locus_auto[i]))
    pos_r[c(2 * i + 1, 2 * i + 2)] <- which(is.element(names(dt_r_auto), locus_auto[i]))
    pos_af[i + 1] <- which(is.element(names(dt_af), locus_auto[i]))
  }

  # Rearrange in order of loci
  dt_v_auto <- dt_v_auto[, pos_v, with = FALSE]
  dt_r_auto <- dt_r_auto[, pos_r, with = FALSE]
  dt_af <- dt_af[, pos_af, with = FALSE]

  return(list(dt_v_auto, dt_r_auto, dt_af))
}

#' analyze_auto
#'
#' @description The function to analyze data for autosomal STR
#' @param dt_v_auto A data.table of victim profiles (autosomal STR)
#' @param dt_r_auto A data.table of reference profiles (autosomal STR)
#' @param dt_af A data.table of allele frequencies (autosomal STR)
#' @param dt_rel A data.table of information on relationships
#' @param dt_myu A data.table of mutation rates
#' @param dt_par_auto A data.table of parameters for autosomal STR
#' @param dt_criteria A data.table of criteria
#' @param show_progress Whether the progress is shown or not
analyze_auto <- function(dt_v_auto, dt_r_auto, dt_af,
                         dt_rel, dt_myu, dt_par_auto, dt_criteria,
                         show_progress = TRUE){

  ##################################################
  # Prepare objects to calculate likelihood ratios #
  ##################################################

  locus_auto <- setdiff(names(dt_v_auto), c("SampleName", "Relationship"))

  n_l <- length(locus_auto)

  maf <- dt_par_auto$Value[dt_par_auto$Parameter == "maf"]

  sn_v_auto <- dt_v_auto[, SampleName]
  sn_r_auto <- dt_r_auto[, SampleName]

  options(warn = -1)
  gt_v_auto <- as.matrix(dt_v_auto[, -c("SampleName", "Relationship")])
  gt_r_auto <- as.matrix(dt_r_auto[, -c("SampleName", "Relationship")])
  options(warn = 0)

  # The NA in genotypes is replaced to -99 to deal with the C++ program
  gt_v_auto[which(is.na(gt_v_auto) == TRUE, arr.ind = TRUE)] <- -99
  gt_r_auto[which(is.na(gt_r_auto) == TRUE, arr.ind = TRUE)] <- -99

  # Change matrix to list for genotypes
  gt_v_auto <- asplit(gt_v_auto, 1)
  gt_r_auto <- asplit(gt_r_auto, 1)

  assumed_rel_all <- dt_r_auto[, Relationship]

  tmp <- set_af(dt_v_auto, dt_r_auto, dt_af, maf)
  af_list <- tmp[[1]]
  af_al_list <- tmp[[2]]

  names_rel <- dt_rel[, Name_relationship]
  degrees_rel <- dt_rel[, Degree]
  pibds_rel <- as.matrix(dt_rel[, list(Pr_IBD2, Pr_IBD1, Pr_IBD0)])
  pibds_rel <- asplit(pibds_rel, 1)

  locus_myu <- dt_myu[, Marker]
  myu_all <- dt_myu[, Myu]
  myus <- rep(0, n_l)
  for(i in 1:n_l){
    myus[i] <- myu_all[which(locus_myu == locus_auto[i])]
  }
  names(myus) <- locus_auto

  ###############################
  # Calculate likelihood ratios #
  ###############################

  if(show_progress){
    withProgress(
      withCallingHandlers(
        result_auto <- calc_kin_lr_all(gt_v_auto, gt_r_auto, assumed_rel_all, af_list, af_al_list, names_rel, degrees_rel, pibds_rel, myus),
        message = function(m) if(grepl("STR_Victim-Reference_ : ", m$message)){
          val <- as.numeric(gsub("STR_Victim-Reference_ : ", "", m$message))
          setProgress(value = val)
        }
      ),
      message = "Analyzing STR data...",
      max = length(sn_v_auto) * length(sn_r_auto),
      value = 0
    )
  }else{
    result_auto <- calc_kin_lr_all(gt_v_auto, gt_r_auto, assumed_rel_all, af_list, af_al_list, names_rel, degrees_rel, pibds_rel, myus)
  }

  ##########################################################
  # Create the data.table for the results of autosomal STR #
  ##########################################################

  result_sn_v_auto <- rep(sn_v_auto, length(sn_r_auto))
  result_sn_r_auto <- as.vector(sapply(sn_r_auto, rep, length(sn_v_auto)))
  result_assumed_rel <- as.vector(sapply(assumed_rel_all, rep, length(sn_v_auto)))

  dt_left <- data.table(Victim = result_sn_v_auto, Reference = result_sn_r_auto, AssumedRel = result_assumed_rel)
  result_auto <- unlist(result_auto)
  result_auto <- matrix(result_auto, nrow = length(sn_v_auto) * length(sn_r_auto), ncol = 3 * (n_l + 1), byrow = TRUE)
  dt_right <- as.data.frame(result_auto)
  setDT(dt_right)
  names(dt_right) <- c(paste0("LikeH1_", c(locus_auto, "Total")), paste0("LikeH2_", c(locus_auto, "Total")), paste0("LR_", c(locus_auto, "Total")))
  dt_result_auto <- cbind(dt_left, dt_right)

  ##########################
  # Estimate relationships #
  ##########################

  min_lr_auto <- dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"]

  est_rel_all <- rep(NA, nrow(dt_result_auto))
  pos_meet_criteria_auto <- which(dt_result_auto[, "LR_Total"] >= min_lr_auto)
  est_rel_all[pos_meet_criteria_auto] <- dt_result_auto[pos_meet_criteria_auto, AssumedRel]

  options(warn = -1)
  dt_result_auto[, EstimatedRel := est_rel_all]
  options(warn = 0)

  return(dt_result_auto)
}
