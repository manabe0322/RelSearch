#' display_gt
#'
#' @description The function to make a vector of genotypes for detailed data
#' @param data_gt Genotype data
display_gt <- function(data_gt){
  n_l <- length(data_gt) / 2
  gt_vec <- rep("", n_l)

  for(i in 1:n_l){
    gt <- as.numeric(data_gt[c(2 * i - 1, 2 * i)])
    gt <- gt[!is.na(gt)]

    if(length(gt) == 1){
      gt_vec[i] <- gt
    }else if(length(gt) == 2){
      gt_vec[i] <- paste(gt[1], ", ", gt[2], sep = "")
    }
  }

  return(gt_vec)
}

#' create_detailed_data_auto
#'
#' @description The function to create the detailed data for autosomal STR
#' @param dt_v_auto A data.table of victim profiles (autosomal STR)
#' @param dt_r_auto A data.table of reference profiles (autosomal STR)
#' @param sn_v_select The sample name of the selected victim
#' @param sn_r_select The sample name of the selected reference
#' @param assumed_rel_select The assumed relationship
#' @param result_selected The selected result
create_detailed_data_auto <- function(dt_v_auto, dt_r_auto, sn_v_select, sn_r_select, assumed_rel_select, result_selected){
  setkey(dt_v_auto, SampleName)
  setkey(dt_r_auto, SampleName, Relationship)

  cn_result <- names(result_selected)

  dt_v_auto_select <- dt_v_auto[.(sn_v_select), nomatch = NULL]
  dt_r_auto_select <- dt_r_auto[.(sn_r_select, assumed_rel_select), nomatch = NULL]

  if(nrow(dt_v_auto_select) == 1 && nrow(dt_r_auto_select) == 1){
    options(warn = -1)
    prof_v_select <- as.numeric(dt_v_auto_select[, -c("SampleName", "Relationship"), with = FALSE])
    prof_r_select <- as.numeric(dt_r_auto_select[, -c("SampleName", "Relationship"), with = FALSE])
    options(warn = 0)

    prof_v_display <- c(display_gt(prof_v_select), "")
    prof_r_display <- c(display_gt(prof_r_select), "")
    locus_display <- gsub("LikeH1_", "", cn_result[grep("LikeH1_", cn_result)])
    like_h1_display <- signif(as.numeric(result_selected[, grep("LikeH1_", cn_result), with = FALSE]), 3)
    like_h2_display <- signif(as.numeric(result_selected[, grep("LikeH2_", cn_result), with = FALSE]), 3)
    lr_display <- signif(as.numeric(result_selected[, grep("LR_", cn_result), with = FALSE]), 3)

    dt_detail_auto <- data.table(Locus = locus_display, Profile_V = prof_v_display, Profile_R = prof_r_display, LikeH1 = like_h1_display, LikeH2 = like_h2_display, LR = lr_display)
  }else{
    dt_detail_auto <- NULL
  }

  return(dt_detail_auto)
}

#' create_detailed_data_y
#'
#' @description The function to create the detailed data for Y-STR
#' @param dt_v_y A data.table of victim profiles (Y-STR)
#' @param dt_r_y A data.table of reference profiles (Y-STR)
#' @param sn_v_select The sample name of the selected victim
#' @param sn_r_select The sample name of the selected reference
#' @param assumed_rel_select The assumed relationship
#' @param result_selected The selected result
create_detailed_data_y <- function(dt_v_y, dt_r_y, sn_v_select, sn_r_select, assumed_rel_select, result_selected){
  setkey(dt_v_y, SampleName)
  setkey(dt_r_y, SampleName, Relationship)

  cn_result <- names(result_selected)

  dt_v_y_select <- dt_v_y[.(sn_v_select), nomatch = NULL]
  dt_r_y_select <- dt_r_y[.(sn_r_select, assumed_rel_select), nomatch = NULL]

  if(nrow(dt_v_y_select) == 1 && nrow(dt_r_y_select) == 1){
    options(warn = -1)
    prof_v_display <- c(as.character(dt_v_y_select[, -c("SampleName", "Relationship"), with = FALSE]), "")
    prof_v_display[is.na(prof_v_display)] <- ""
    options(warn = 0)

    options(warn = -1)
    prof_r_display <- c(as.character(dt_r_y_select[, -c("SampleName", "Relationship"), with = FALSE]), "")
    prof_r_display[is.na(prof_r_display)] <- ""
    options(warn = 0)

    locus_display <- gsub("Mismatch_", "", cn_result[grep("Mismatch_", cn_result)])
    mismatch_y_display <- as.character(result_selected[, grep("Mismatch_", cn_result), with = FALSE])
    ignore_y_display <- as.character(result_selected[, grep("Ignore_", cn_result), with = FALSE])
    mustep_y_display <- result_selected[, grep("MuStep_", cn_result), with = FALSE]
    pos99 <- which(mustep_y_display >= 99)
    mustep_y_display <- as.character(mustep_y_display)
    mustep_y_display[pos99] <- "Unable to calculate"

    dt_detail_y <- data.table(Locus = locus_display, Profile_V = prof_v_display, Profile_R = prof_r_display, Ignore = ignore_y_display, Mismatch = mismatch_y_display, MuStep = mustep_y_display)
  }else{
    dt_detail_y <- NULL
  }

  return(dt_detail_y)
}

#' create_detailed_data_mt
#'
#' @description The function to create the detailed data for mtDNA
#' @param dt_v_mt A data.table of victim profiles (mtDNA)
#' @param dt_r_mt A data.table of reference profiles (mtDNA)
#' @param sn_v_select The sample name of the selected victim
#' @param sn_r_select The sample name of the selected reference
#' @param assumed_rel_select The assumed relationship
#' @param result_selected The selected result
create_detailed_data_mt <- function(dt_v_mt, dt_r_mt, sn_v_select, sn_r_select, assumed_rel_select, result_selected){
  setkey(dt_v_mt, SampleName)
  setkey(dt_r_mt, SampleName, Relationship)

  dt_v_mt_select <- dt_v_mt[.(sn_v_select), nomatch = NULL]
  dt_r_mt_select <- dt_r_mt[.(sn_r_select, assumed_rel_select), nomatch = NULL]

  if(nrow(dt_v_mt_select) == 1 && nrow(dt_r_mt_select) == 1){
    options(warn = -1)
    type_v <- as.character(dt_v_mt_select[, -c("SampleName", "Relationship", "Range"), with = FALSE])
    options(warn = 0)
    type_v <- strsplit(type_v, " ")[[1]]
    type_v <- setdiff(type_v, "")
    type_v <- type_v[order(sapply(type_v, extract_integer))]

    options(warn = -1)
    type_r <- as.character(dt_r_mt_select[, -c("SampleName", "Relationship", "Range"), with = FALSE])
    options(warn = 0)
    type_r <- strsplit(type_r, " ")[[1]]
    type_r <- setdiff(type_r, "")
    type_r <- type_r[order(sapply(type_r, extract_integer))]

    type_vr <- union(type_v, type_r)
    type_vr <- type_vr[order(sapply(type_vr, extract_integer))]
    n_type_vr <- length(type_vr)

    pos_mt_vr <- extract_pos_mt_vr(dt_v_mt_select[, Range], dt_r_mt_select[, Range])

    type_v_display <- rep("", n_type_vr)
    type_v_display[is.element(type_vr, type_v)] <- type_v

    type_r_display <- rep("", n_type_vr)
    type_r_display[is.element(type_vr, type_r)] <- type_r

    pos_common <- is.element(sapply(type_vr, extract_integer), pos_mt_vr)

    out_range_display <- rep("", n_type_vr)
    out_range_display[!pos_common] <- "x"

    pos_mismatch <- apply(rbind(!is.element(type_vr, type_v), !is.element(type_vr, type_r)), 2, any)
    mismatch_mt_display <- rep("", n_type_vr)
    mismatch_mt_display[apply(rbind(pos_common, pos_mismatch), 2, all)] <- "x"

    dt_detail_mt <- data.table(Type_Victim = type_v_display, Type_Reference = type_r_display, OutRange = out_range_display, Mismatch = mismatch_mt_display)
  }else{
    dt_detail_mt <- NULL
  }

  return(dt_detail_mt)
}
