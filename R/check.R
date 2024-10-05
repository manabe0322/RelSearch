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
#' @param bool_load_v_auto Whether autosomal STR database for victims has been loaded or not
#' @param bool_load_r_auto Whether autosomal STR database for references has been loaded or not
#' @param bool_load_af Whether allele frequency database has been loaded or not
#' @param bool_load_v_y Whether Y-STR database for victims has been loaded or not
#' @param bool_load_r_y Whether Y-STR database for references has been loaded or not
#' @param bool_load_v_mt Whether mtDNA database for victims has been loaded or not
#' @param bool_load_r_mt Whether mtDNA database for references has been loaded or not
check_error <- function(dt_v_auto, dt_r_auto, dt_af,
                        dt_v_y, dt_r_y,
                        dt_v_mt, dt_r_mt,
                        dt_rel, dt_myu,
                        bool_load_v_auto, bool_load_r_auto, bool_load_af,
                        bool_load_v_y, bool_load_r_y,
                        bool_load_v_mt, bool_load_r_mt){
  error_message <- ""
  rel_defined <- dt_rel$Relationship
  rel_undefined <- character(0)

  ####################################################
  # check whether all required data is loaded or not #
  ####################################################

  bool_check_auto <- all(bool_load_v_auto, bool_load_r_auto, bool_load_af)
  bool_check_y <- all(bool_load_v_y, bool_load_r_y)
  bool_check_mt <- all(bool_load_v_mt, bool_load_r_mt)

  if(!any(c(bool_check_auto, bool_check_y, bool_check_mt))){
    error_message <- "Load required file(s)!"
  }else{

    ################################
    # Check data for autosomal STR #
    ################################

    if(bool_check_auto){
      bool_column_auto <- TRUE
      if(!is.element("SampleName", names(dt_v_auto))){
        bool_column_auto <- FALSE
        error_message <- paste0(error_message, "- There is no column 'SampleName' in the victim STR database.<br>")
      }
      if(!is.element("SampleName", names(dt_r_auto))){
        bool_column_auto <- FALSE
        error_message <- paste0(error_message, "- There is no column 'SampleName' in the reference STR database.<br>")
      }
      if(!is.element("Family", names(dt_r_auto))){
        bool_column_auto <- FALSE
        error_message <- paste0(error_message, "- There is no column 'Family' in the reference STR database.<br>")
      }
      if(!is.element("Relationship", names(dt_r_auto))){
        bool_column_auto <- FALSE
        error_message <- paste0(error_message, "- There is no column 'Relationship' in the reference STR database.<br>")
      }
      if(!is.element("Allele", names(dt_af))){
        bool_column_auto <- FALSE
        error_message <- paste0(error_message, "- There is no column 'Allele' in the database of the allele frequencies.<br>")
      }

      if(bool_column_auto){
        locus_v_auto <- setdiff(names(dt_v_auto), c("SampleName", "Family", "Relationship"))
        locus_r_auto <- setdiff(names(dt_r_auto), c("SampleName", "Family", "Relationship"))
        locus_af <- setdiff(names(dt_af), "Allele")
        locus_myu <- dt_myu[, Marker]

        if(!setequal(locus_v_auto, locus_r_auto)){
          error_message <- paste0(error_message, "- Locus set of the victim STR database is not the same as that of the reference STR database.<br>")
        }
        if(!setequal(locus_v_auto, locus_af)){
          error_message <- paste0(error_message, "- Locus set of the victim STR database is not the same as that of the allele frequencies.<br>")
        }
        bool_mk_myu <- is.element(locus_v_auto, locus_myu)
        if(!all(bool_mk_myu)){
          error_message <- paste0(error_message, "- Mutation rates of the following loci are not defined: ", paste0(locus_v_auto[bool_mk_myu], collapse = ", "), ".<br>")
        }

        rel_undefined <- c(rel_undefined, setdiff(dt_r_auto$Relationship, rel_defined))
      }
    }

    ########################
    # Check data for Y-STR #
    ########################

    if(bool_check_y){
      bool_column_y <- TRUE
      if(!is.element("SampleName", names(dt_v_y))){
        bool_column_y <- FALSE
        error_message <- paste0(error_message, "- There is no column 'SampleName' in the victim Y-STR database.<br>")
      }
      if(!is.element("SampleName", names(dt_r_y))){
        bool_column_y <- FALSE
        error_message <- paste0(error_message, "- There is no column 'SampleName' in the reference Y-STR database.<br>")
      }
      if(!is.element("Family", names(dt_r_y))){
        bool_column_y <- FALSE
        error_message <- paste0(error_message, "- There is no column 'Family' in the reference Y-STR database.<br>")
      }
      if(!is.element("Relationship", names(dt_r_y))){
        bool_column_y <- FALSE
        error_message <- paste0(error_message, "- There is no column 'Relationship' in the reference Y-STR database.<br>")
      }

      if(bool_column_y){
        locus_v_y <- setdiff(names(dt_v_y), c("SampleName", "Family", "Relationship"))
        locus_r_y <- setdiff(names(dt_r_y), c("SampleName", "Family", "Relationship"))
        if(!setequal(locus_v_y, locus_r_y)){
          error_message <- paste0(error_message, "- Locus set of the victim Y-STR database is not the same as that of the reference Y-STR database!<br>")
        }

        rel_undefined <- c(rel_undefined, setdiff(dt_r_y$Relationship, rel_defined))
      }
    }

    if(bool_check_mt){
      bool_column_mt <- TRUE
      if(!is.element("SampleName", names(dt_v_mt))){
        bool_column_mt <- FALSE
        error_message <- paste0(error_message, "- There is no column 'SampleName' in the victim mtDNA database.<br>")
      }
      if(!is.element("Range", names(dt_v_mt))){
        bool_column_mt <- FALSE
        error_message <- paste0(error_message, "- There is no column 'Range' in the victim mtDNA database.<br>")
      }
      if(!is.element("Haplotype", names(dt_v_mt))){
        bool_column_mt <- FALSE
        error_message <- paste0(error_message, "- There is no column 'Haplotype' in the victim mtDNA database.<br>")
      }
      if(!is.element("SampleName", names(dt_r_mt))){
        bool_column_mt <- FALSE
        error_message <- paste0(error_message, "- There is no column 'SampleName' in the reference mtDNA database.<br>")
      }
      if(!is.element("Family", names(dt_r_mt))){
        bool_column_mt <- FALSE
        error_message <- paste0(error_message, "- There is no column 'Family' in the reference mtDNA database.<br>")
      }
      if(!is.element("Relationship", names(dt_r_mt))){
        bool_column_mt <- FALSE
        error_message <- paste0(error_message, "- There is no column 'Relationship' in the reference mtDNA database.<br>")
      }
      if(!is.element("Range", names(dt_r_mt))){
        bool_column_mt <- FALSE
        error_message <- paste0(error_message, "- There is no column 'Range' in the reference mtDNA database.<br>")
      }
      if(!is.element("Haplotype", names(dt_r_mt))){
        error_message <- paste0(error_message, "- There is no column 'Haplotype' in the reference mtDNA database.<br>")
      }

      if(bool_column_mt){
        rel_undefined <- c(rel_undefined, setdiff(dt_r_mt$Relationship, rel_defined))
      }
    }

    rel_undefined <- unique(rel_undefined)
    if(length(rel_undefined) != 0){
      error_message <- paste0(error_message, "- The following relationships are not defined: ", paste0(rel_undefined, collapse = ", "), ".<br>")
    }
  }
  return(error_message)
}
