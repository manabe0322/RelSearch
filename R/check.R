#####################################
# The function to check loaded data #
#####################################

check_error <- function(dt_v_auto, dt_r_auto, dt_af,
                        dt_v_y, dt_r_y,
                        dt_v_mt, dt_r_mt,
                        dt_rel, dt_myu){

  #############################################
  # Define an object to save an error message #
  #############################################

  error_message <- ""

  ####################################################
  # check whether all required data is loaded or not #
  ####################################################

  bool_check_auto <- all(!is.null(dt_v_auto), !is.null(dt_r_auto), !is.null(dt_af))
  bool_check_y <- all(!is.null(dt_v_y), !is.null(dt_r_y))
  bool_check_mt <- all(!is.null(dt_v_mt), !is.null(dt_r_mt))

  # All required data for all markers is not loaded
  if(!any(c(bool_check_auto, bool_check_y, bool_check_mt))){
    error_message <- "Load required file(s)!"

  # All required data for at least one marker type is loaded
  }else{

    ################################
    # Check data for autosomal STR #
    ################################

    # All required data for the autosomal STR is loaded
    if(bool_check_auto){

      # Extract loci from each data.table
      locus_v_auto <- setdiff(names(dt_v_auto), c("SampleName", "Relationship"))
      locus_r_auto <- setdiff(names(dt_r_auto), c("SampleName", "Relationship"))
      locus_af <- setdiff(names(dt_af), "Allele")
      locus_myu <- dt_myu[, Marker]

      # Whether the locus set of the victim database is the same as that of the reference database or not
      bool_locus_1 <- setequal(locus_v_auto, locus_r_auto)

      # Whether the locus set of the victim database is the same as that of the allele frequencies or not
      bool_locus_2 <- setequal(locus_v_auto, locus_af)

      # Whether all loci of the victim database is included in the locus set of the mutation rates or not
      bool_locus_3 <- all(is.element(locus_v_auto, locus_myu))

      # Whether all relationships of the reference database is included in the information on the relationships or not
      bool_rel_1 <- all(is.element(unique(dt_r_auto[, Relationship]), dt_rel[, Name_relationship]))

      # If the locus set of the victim database is not the same as that of the reference database
      if(!bool_locus_1){
        error_message <- "Locus set is not the same between victim database and reference database!"

      # If the locus set of the victim database is not the same as that of the allele frequencies
      }else if(!bool_locus_2){
        error_message <- "Locus set is not the same between victim database and allele frequencies!"

      # If some loci of the victim database is not included in the locus set of the mutation rates
      }else if(!bool_locus_3){
        error_message <- "There are some loci without mutation rates!"

      # If some relationships of the reference database is not included in the relationships of the IBD probabilities
      }else if(!bool_rel_1){
        error_message <- "Some relationships in the reference database are not defined!"
      }
    }

    ########################
    # Check data for Y-STR #
    ########################

    # All required data for the Y-STR is loaded
    if(error_message == "" && bool_check_y){

      # Extract loci from each database
      locus_v_y <- setdiff(names(dt_v_y), c("SampleName", "Relationship"))
      locus_r_y <- setdiff(names(dt_r_y), c("SampleName", "Relationship"))

      # Whether the locus set of the victim database is the same as that of the reference database or not
      bool_locus_1 <- setequal(locus_v_y, locus_r_y)

      # If the locus set of the victim database is not the same as that of the reference database
      if(!bool_locus_1){
        error_message <- "Locus set is not the same between victim database and reference database!"
      }
    }
  }
  return(error_message)
}
