#' correct_af_dirichlet
#'
#' @description The function to correct allele frequencies based on the Dirichlet distribution
#' @param pop_al A vector of the alleles in the population database
#' @param pop_freq A vector of the allele frequencies in the population database
#' @param unobs_al A vector of the unobserved alleles in the population database
correct_af_dirichlet <- function(pop_al, pop_freq, unobs_al = numeric(0)){
  n_unobs <- length(unobs_al)
  if(n_unobs > 0){
    pop_al <- c(pop_al, unobs_al)
    pop_freq <- c(pop_freq, rep(0, n_unobs))
  }
  pop_prob <- (pop_freq + 1) / (sum(pop_freq) + length(pop_al))

  order_al <- order(pop_al)
  pop_al <- pop_al[order_al]
  pop_prob <- pop_prob[order_al]

  return(list("pop_al" = pop_al,
              "pop_prob" = pop_prob))
}

#' set_af
#'
#' @description The function to set allele frequencies
#' @param dt_v_auto A data.table of victim profiles (autosomal STR)
#' @param dt_r_auto A data.table of reference profiles (autosomal STR)
#' @param dt_af A data.table of allele frequencies (autosomal STR)
set_af <- function(dt_v_auto, dt_r_auto, dt_af){
  n_mk <- ncol(dt_af) - 1
  name_al <- dt_af[, Allele]
  name_mk <- setdiff(names(dt_v_auto), c("SampleName", "Relationship"))
  af_list <- af_al_list <- unobs_al_list <- list()

  for(i in 1:n_mk){
    pos_v <- which(names(dt_v_auto) == name_mk[i])
    pos_r <- which(names(dt_r_auto) == name_mk[i])
    pos_af <- which(names(dt_af) == name_mk[i])

    vs_als <- unique(c(dt_v_auto[[pos_v[1]]], dt_v_auto[[pos_v[2]]], dt_r_auto[[pos_r[1]]], dt_r_auto[[pos_r[2]]]))
    vs_als <- vs_als[!is.na(vs_als)]

    pop_freq <- dt_af[[pos_af]]
    pos_al <- !is.na(pop_freq)
    pop_freq <- pop_freq[pos_al]
    pop_al <- name_al[pos_al]

    pos_unobs <- which(is.element(vs_als, pop_al) == FALSE)
    unobs_al <- vs_als[pos_unobs]

    tmp <- correct_af_dirichlet(pop_al, pop_freq, unobs_al)
    af_al_list[[i]] <- tmp$pop_al
    af_list[[i]] <- tmp$pop_prob
    unobs_al_list[[i]] <- unobs_al
  }
  names(af_list) <- name_mk
  names(af_al_list) <- name_mk
  names(unobs_al_list) <- name_mk
  return(list("af_list" = af_list,
              "af_al_list" = af_al_list,
              "unobs_al_list" = unobs_al_list))
}

#' order_loci_auto
#'
#' @description The function to rearrange in order of loci for autosomal STR database
#' @param dt_v_auto A data.table of victim profiles (autosomal STR)
#' @param dt_r_auto A data.table of reference profiles (autosomal STR)
#' @param dt_af A data.table of allele frequencies (autosomal STR)
order_loci_auto <- function(dt_v_auto, dt_r_auto, dt_af){
  locus_auto <- setdiff(names(dt_v_auto), c("SampleName", "Relationship"))
  n_mk <- length(locus_auto)

  # Define objects for the column position
  pos_v <- rep(0, 2 * n_mk + 1)
  pos_r <- rep(0, 2 * n_mk + 2)
  pos_af <- rep(0, n_mk + 1)

  pos_v[1] <- which(is.element(names(dt_v_auto), "SampleName"))
  pos_r[1] <- which(is.element(names(dt_r_auto), "SampleName"))
  pos_r[2] <- which(is.element(names(dt_r_auto), "Relationship"))
  pos_af[1] <- which(is.element(names(dt_af), "Allele"))

  for(i in 1:n_mk){
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

#' make_info_myu
#'
#' @description The function to make information on considering mutations
#' @param dt_rel A data.table of information on relationships
make_info_myu <- function(dt_rel){
  bool_pc_all <- dt_rel[, Pr_IBD1] == 1

  tree_persons <- dt_rel[, Tree_persons]
  tree_persons_pc <- tree_persons[bool_pc_all]
  tree_persons_pc_split <- strsplit(tree_persons_pc, ", ")

  tree_fathers <- dt_rel[, Tree_fathers]
  tree_fathers_pc <- tree_fathers[bool_pc_all]
  tree_fathers_pc_split <- strsplit(tree_fathers_pc, ", ")

  tree_mothers <- dt_rel[, Tree_mothers]
  tree_mothers_pc <- tree_mothers[bool_pc_all]
  tree_mothers_pc_split <- strsplit(tree_mothers_pc, ", ")

  tree_sexes <- dt_rel[, Tree_sexes]
  tree_sexes_pc <- tree_sexes[bool_pc_all]
  tree_sexes_pc_split <- strsplit(tree_sexes_pc, ", ")

  index_pc <- which(bool_pc_all)
  n_pc <- length(index_pc)
  bool_parent_victim_all <- bool_parent_male_all <- rep(FALSE, nrow(dt_rel))
  if(n_pc != 0){
    for(i in 1:n_pc){
      index_victim <- which(tree_persons_pc_split[[i]] == "Victim")
      victim_father <- tree_fathers_pc_split[[i]][index_victim]
      victim_mother <- tree_mothers_pc_split[[i]][index_victim]
      if(victim_father != "Ref" && victim_mother != "Ref"){
        bool_parent_victim_all[index_pc[i]] <- TRUE
        bool_parent_male_all[index_pc[i]] <- tree_sexes_pc_split[[i]][index_victim] == "M"
      }else{
        index_ref <- which(tree_persons_pc_split[[i]] == "Ref")
        bool_parent_male_all[index_pc[i]] <- tree_sexes_pc_split[[i]][index_ref] == "M"
      }
    }
  }

  return(list("bool_pc_all" = bool_pc_all,
              "bool_parent_victim_all" = bool_parent_victim_all,
              "bool_parent_male_all" = bool_parent_male_all))
}

#' make_dt_af_use
#'
#' @description The function to make a data.table of the allele freuqencies used
#' @param af_list A list of the allele frequencies
#' @param af_al_list A list of the alleles
make_dt_af_use <- function(af_list, af_al_list){
  n_mk <- length(af_list)
  af_alleles_all <- sort(unique(unlist(af_al_list)))
  mat_af_use <- matrix("", length(af_alleles_all), n_mk + 1)
  mat_af_use[, 1] <- af_alleles_all

  for(i in 1:n_mk){
    af_mk <- af_list[[i]]
    af_al_mk <- af_al_list[[i]]
    mat_af_use[is.element(af_alleles_all, af_al_mk), i + 1] <- af_mk
  }

  dt_af_use <- as.data.table(mat_af_use)
  colnames(dt_af_use) <- c("Allele", names(af_list))

  return(dt_af_use)
}

#' make_dt_unobs_al
#'
#' @description The function to make a data.table of the unobserved alleles
#' @param unobs_al_list A list of the unobserved alleles
make_dt_unobs_al <- function(unobs_al_list){
  unobs_mk_all <- character(0)
  unobs_al_all <- numeric(0)
  name_mk <- names(unobs_al_list)
  for(i in 1:length(unobs_al_list)){
    unobs_al_mk <- unobs_al_list[[i]]
    n_unobs <- length(unobs_al_mk)
    if(n_unobs != 0){
      unobs_mk_all <- c(unobs_mk_all, rep(name_mk[i], n_unobs))
      unobs_al_all <- c(unobs_al_all, unobs_al_mk)
    }
  }

  dt_unobs_al <- data.table(Marker = unobs_mk_all,
                            Allele = unobs_al_all)
  return(dt_unobs_al)
}

#' analyze_auto
#'
#' @description The function to analyze data for autosomal STR
#' @param dt_v_auto A data.table of victim profiles (autosomal STR)
#' @param dt_r_auto A data.table of reference profiles (autosomal STR)
#' @param dt_af A data.table of allele frequencies (autosomal STR)
#' @param dt_rel A data.table of information on relationships
#' @param dt_myu A data.table of mutation rates
#' @param dt_criteria A data.table of criteria
#' @param show_progress Whether the progress is shown or not
analyze_auto <- function(dt_v_auto, dt_r_auto, dt_af,
                         dt_rel, dt_myu, dt_criteria,
                         show_progress = TRUE){

  ##################################################
  # Prepare objects to calculate likelihood ratios #
  ##################################################

  # Locus
  locus_auto <- setdiff(names(dt_v_auto), c("SampleName", "Relationship"))
  n_mk <- length(locus_auto)

  # Sample names
  sn_v_auto <- dt_v_auto[, SampleName]
  sn_r_auto <- dt_r_auto[, SampleName]

  # Genotypes
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

  # Assumed relationships
  assumed_rel_all <- dt_r_auto[, Relationship]

  # Allele frequencies
  tmp <- set_af(dt_v_auto, dt_r_auto, dt_af)
  af_list <- tmp$af_list
  af_al_list <- tmp$af_al_list
  unobs_al_list <- tmp$unobs_al_list

  # IBD probabilities
  names_rel <- dt_rel[, Relationship]
  pibds_rel <- as.matrix(dt_rel[, list(Pr_IBD2, Pr_IBD1, Pr_IBD0)])
  pibds_rel <- asplit(pibds_rel, 1)

  # Mutation rates
  locus_myu <- dt_myu[, Marker]
  myus_paternal_m2_all <- dt_myu[, Paternal_m2]
  myus_paternal_m1_all <- dt_myu[, Paternal_m1]
  myus_paternal_0_all <- dt_myu[, Paternal_0]
  myus_paternal_p1_all <- dt_myu[, Paternal_p1]
  myus_paternal_p2_all <- dt_myu[, Paternal_p2]
  myus_maternal_m2_all <- dt_myu[, Maternal_m2]
  myus_maternal_m1_all <- dt_myu[, Maternal_m1]
  myus_maternal_0_all <- dt_myu[, Maternal_0]
  myus_maternal_p1_all <- dt_myu[, Maternal_p1]
  myus_maternal_p2_all <- dt_myu[, Maternal_p2]
  myus_paternal_m2 <- myus_paternal_m1 <- myus_paternal_0 <- myus_paternal_p1 <- myus_paternal_p2 <- rep(0, n_mk)
  names(myus_paternal_m2) <- names(myus_paternal_m1) <- names(myus_paternal_0) <- names(myus_paternal_p1) <- names(myus_paternal_p2) <- locus_auto
  myus_maternal_m2 <- myus_maternal_m1 <- myus_maternal_0 <- myus_maternal_p1 <- myus_maternal_p2 <- rep(0, n_mk)
  names(myus_maternal_m2) <- names(myus_maternal_m1) <- names(myus_maternal_0) <- names(myus_maternal_p1) <- names(myus_maternal_p2) <- locus_auto
  for(i in 1:n_mk){
    index_mk <- which(locus_myu == locus_auto[i])
    myus_paternal_m2[i] <- myus_paternal_m2_all[index_mk]
    myus_paternal_m1[i] <- myus_paternal_m1_all[index_mk]
    myus_paternal_0[i] <- myus_paternal_0_all[index_mk]
    myus_paternal_p1[i] <- myus_paternal_p1_all[index_mk]
    myus_paternal_p2[i] <- myus_paternal_p2_all[index_mk]
    myus_maternal_m2[i] <- myus_maternal_m2_all[index_mk]
    myus_maternal_m1[i] <- myus_maternal_m1_all[index_mk]
    myus_maternal_0[i] <- myus_maternal_0_all[index_mk]
    myus_maternal_p1[i] <- myus_maternal_p1_all[index_mk]
    myus_maternal_p2[i] <- myus_maternal_p2_all[index_mk]
  }

  # Consideration of mutations
  tmp <- make_info_myu(dt_rel)
  bool_pc_all <- tmp$bool_pc_all
  bool_parent_victim_all <- tmp$bool_parent_victim_all
  bool_parent_male_all <- tmp$bool_parent_male_all

  ###############################
  # Calculate likelihood ratios #
  ###############################

  if(show_progress){
    withProgress(
      withCallingHandlers(
        result_auto <- calc_kin_lr_all(gt_v_auto, gt_r_auto, assumed_rel_all, af_list, af_al_list, names_rel, pibds_rel,
                                       myus_paternal_m2, myus_paternal_m1, myus_paternal_0, myus_paternal_p1, myus_paternal_p2,
                                       myus_maternal_m2, myus_maternal_m1, myus_maternal_0, myus_maternal_p1, myus_maternal_p2,
                                       bool_pc_all, bool_parent_victim_all, bool_parent_male_all),
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
    result_auto <- calc_kin_lr_all(gt_v_auto, gt_r_auto, assumed_rel_all, af_list, af_al_list, names_rel, pibds_rel, myus, cons_mutations, parent_victim)
  }

  ##########################################################
  # Create the data.table for the results of autosomal STR #
  ##########################################################

  result_sn_v_auto <- rep(sn_v_auto, length(sn_r_auto))
  result_sn_r_auto <- as.vector(sapply(sn_r_auto, rep, length(sn_v_auto)))
  result_assumed_rel <- as.vector(sapply(assumed_rel_all, rep, length(sn_v_auto)))

  dt_left <- data.table(Victim = result_sn_v_auto, Reference = result_sn_r_auto, AssumedRel = result_assumed_rel)
  result_auto <- unlist(result_auto)
  result_auto <- matrix(result_auto, nrow = length(sn_v_auto) * length(sn_r_auto), ncol = 3 * (n_mk + 1), byrow = TRUE)
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

  ###########################################################
  # Create the data.table for the allele probabilities used #
  ###########################################################

  dt_af_use <- make_dt_af_use(af_list, af_al_list)
  dt_unobs_al <- make_dt_unobs_al(unobs_al_list)

  return(list("dt_result_auto" = dt_result_auto,
              "dt_af_use" = dt_af_use,
              "dt_unobs_al" = dt_unobs_al))
}
