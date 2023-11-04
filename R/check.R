#' check_error
#'
#' @description The function to check loaded data
#' @param dt_v_auto A data.table of victim profiles (autosomal STR)
#' @param dt_r_auto A data.table of reference profiles (autosomal STR)
#' @param dt_af A data.table of allele frequencies (autosomal STR)
#' @param dt_v_y A data.table of victim profiles (Y-STR)
#' @param dt_r_y A data.table of reference profiles (Y-STR)
#' @param dt_v_mt A data.table of victim profiles (mtDNA)
#' @param dt_r_mt A data.table of reference profiles (mtDNA)
#' @param dt_rel A data.table of information on relationships
#' @param dt_myu A data.table of mutation rates
check_error <- function(dt_v_auto, dt_r_auto, dt_af,
                        dt_v_y, dt_r_y,
                        dt_v_mt, dt_r_mt,
                        dt_rel, dt_myu){
  error_message <- ""

  ####################################################
  # check whether all required data is loaded or not #
  ####################################################

  bool_check_auto <- all(!is.null(dt_v_auto), !is.null(dt_r_auto), !is.null(dt_af))
  bool_check_y <- all(!is.null(dt_v_y), !is.null(dt_r_y))
  bool_check_mt <- all(!is.null(dt_v_mt), !is.null(dt_r_mt))

  if(!any(c(bool_check_auto, bool_check_y, bool_check_mt))){
    error_message <- "Load required file(s)!"
  }else{

    ################################
    # Check data for autosomal STR #
    ################################

    if(bool_check_auto){
      locus_v_auto <- setdiff(names(dt_v_auto), c("SampleName", "Relationship"))
      locus_r_auto <- setdiff(names(dt_r_auto), c("SampleName", "Relationship"))
      locus_af <- setdiff(names(dt_af), "Allele")
      locus_myu <- dt_myu[, Marker]

      bool_locus_1 <- setequal(locus_v_auto, locus_r_auto)
      bool_locus_2 <- setequal(locus_v_auto, locus_af)
      bool_locus_3 <- all(is.element(locus_v_auto, locus_myu))
      bool_rel_1 <- all(is.element(unique(dt_r_auto[, Relationship]), dt_rel[, Relationship]))

      if(!bool_locus_1){
        error_message <- "Locus set is not the same between victim database and reference database!"
      }else if(!bool_locus_2){
        error_message <- "Locus set is not the same between victim database and allele frequencies!"
      }else if(!bool_locus_3){
        error_message <- "There are some loci without mutation rates!"
      }else if(!bool_rel_1){
        error_message <- "Some relationships in the reference database are not defined!"
      }
    }

    ########################
    # Check data for Y-STR #
    ########################

    if(error_message == "" && bool_check_y){
      locus_v_y <- setdiff(names(dt_v_y), c("SampleName", "Relationship"))
      locus_r_y <- setdiff(names(dt_r_y), c("SampleName", "Relationship"))
      bool_locus_1 <- setequal(locus_v_y, locus_r_y)

      if(!bool_locus_1){
        error_message <- "Locus set is not the same between victim database and reference database!"
      }
    }
  }
  return(error_message)
}
