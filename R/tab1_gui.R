#############################
# The function to make tab1 #
#############################

make_tab1 <- function(env_proj, env_gui){

  #######################################
  # The function to load required files #
  #######################################

  open_file <- function(mk, fp, fn, label_fn, name_db, name_fp, name_fn){

    # Get the end-sign of the analysis from the environment "env_proj"
    if(mk == "auto"){
      marker_type <- "Autosomal STR"
      fin <- get("fin_auto", pos = env_proj)
    }else if(mk == "y"){
      marker_type <- "Y-STR"
      fin <- get("fin_y", pos = env_proj)
    }else if(mk == "mt"){
      marker_type <- "mtDNA"
      fin <- get("fin_mt", pos = env_proj)
    }

    # Confirm that the user allows to delete the results
    if(fin){
      sign_ok <- tclvalue(tkmessageBox(message = paste0("The result of ", marker_type, " will be deleted. Do you want to continue?"), type = "okcancel", icon = "warning"))
    }else{
      sign_ok <- "ok"
    }
    if(sign_ok == "ok"){

      # Reset the environment variables
      if(mk == "auto"){
        set_env_proj_auto(env_proj, FALSE)
      }else if(mk == "y"){
        set_env_proj_y(env_proj, FALSE)
      }else if(mk == "mt"){
        set_env_proj_mt(env_proj, FALSE)
      }

      # Reset tab2
      make_tab2(env_proj, env_gui)

      # Define tcl variables for the file path and the file name
      fp_var <- tclVar(fp)
      fn_var <- tclVar(fn)

      # Open a window to select a file
      path_file <- tclvalue(tkgetOpenFile(initialdir = tclvalue(fp_var), multiple = "true", filetypes = "{{CSV Files} {.csv}}"))

      # If the user does not select a file
      if(!nchar(path_file)){
        tkmessageBox(message = "No file was selected!", icon = "error", type = "ok")

      # If the user selects a file
      }else{

        # Update tcl variables for the file path and the file name
        tmp <- sub("\\}", path_file, replacement = "")
        tmp2 <- sub("\\{", tmp, replacement = "")
        tclvalue(fp_var) <- tmp2
        foo3 <- strsplit(tmp2, "/")[[1]]
        tclvalue(fn_var) <- strsplit(foo3[length(foo3)], "\\.csv")[[1]][1]

        # Update the name of query database
        tkconfigure(label_fn, textvariable = fn_var)

        # Load database
        db <- fread(tclvalue(fp_var))

        # Assign objects to the environment "env_proj"
        assign(name_db, db, envir = env_proj)
        assign(name_fp, tclvalue(fp_var), envir = env_proj)
        assign(name_fn, tclvalue(fn_var), envir = env_proj)
      }
    }
  }

  ############################################################
  # Get file paths and names from the environment "env_proj" #
  ############################################################

  # Victim database
  fp_v_auto <- get("fp_v_auto", pos = env_proj)
  fn_v_auto <- get("fn_v_auto", pos = env_proj)
  fp_v_y <- get("fp_v_y", pos = env_proj)
  fn_v_y <- get("fn_v_y", pos = env_proj)
  fp_v_mt <- get("fp_v_mt", pos = env_proj)
  fn_v_mt <- get("fn_v_mt", pos = env_proj)

  # Reference database
  fp_r_auto <- get("fp_r_auto", pos = env_proj)
  fn_r_auto <- get("fn_r_auto", pos = env_proj)
  fp_r_y <- get("fp_r_y", pos = env_proj)
  fn_r_y <- get("fn_r_y", pos = env_proj)
  fp_r_mt <- get("fp_r_mt", pos = env_proj)
  fn_r_mt <- get("fn_r_mt", pos = env_proj)

  # Allele frequencies
  fp_af <- get("fp_af", pos = env_proj)
  fn_af <- get("fn_af", pos = env_proj)

  ########################
  # Define tcl variables #
  ########################

  # Victim database
  fn_v_auto_var <- tclVar(fn_v_auto)
  fn_v_y_var <- tclVar(fn_v_y)
  fn_v_mt_var <- tclVar(fn_v_mt)

  # Reference database
  fn_r_auto_var <- tclVar(fn_r_auto)
  fn_r_y_var <- tclVar(fn_r_y)
  fn_r_mt_var <- tclVar(fn_r_mt)

  # Allele frequencies
  fn_af_var <- tclVar(fn_af)

  ####################
  # Reset frame_tab1 #
  ####################

  tab1 <- get("tab1", pos = env_gui)
  frame_tab1 <- get("frame_tab1", pos = env_gui)
  tkdestroy(frame_tab1)
  frame_tab1 <- tkframe(tab1)

  ######################################
  # Define widgets for victim database #
  ######################################

  # Frames
  frame_v <- tkframe(frame_tab1, relief = "groove", borderwidth = 2)
  frame_v_title <- tkframe(frame_v)
  frame_v_file <- tkframe(frame_v)
  frame_v_auto <- tkframe(frame_v_file)
  frame_v_y <- tkframe(frame_v_file)
  frame_v_mt <- tkframe(frame_v_file)

  # Title
  label_v_title <- tklabel(frame_v_title, text = "Victim database", font = "Helvetica 10 bold")

  # Autosomal STR
  label_v_auto_mk <- tklabel(frame_v_auto, text = "Autosomal STR")
  butt_v_auto_load <- tkbutton(frame_v_auto, text = "    Load    ", cursor = "hand2", command = function() open_file("auto", fp_v_auto, fn_v_auto, label_v_auto_fn, "data_v_auto", "fp_v_auto", "fn_v_auto"))
  label_v_auto_fn <- tklabel(frame_v_auto, textvariable = fn_v_auto_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")

  # Y-STR
  label_v_y_mk <- tklabel(frame_v_y, text = "Y-STR")
  butt_v_y_load <- tkbutton(frame_v_y, text = "    Load    ", cursor = "hand2", command = function() open_file("y", fp_v_y, fn_v_y, label_v_y_fn, "data_v_y", "fp_v_y", "fn_v_y"))
  label_v_y_fn <- tklabel(frame_v_y, textvariable = fn_v_y_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")

  # mtDNA
  label_v_mt_mk <- tklabel(frame_v_mt, text = "mtDNA")
  butt_v_mt_load <- tkbutton(frame_v_mt, text = "    Load    ", cursor = "hand2", command = function() open_file("mt", fp_v_mt, fn_v_mt, label_v_mt_fn, "data_v_mt", "fp_v_mt", "fn_v_mt"))
  label_v_mt_fn <- tklabel(frame_v_mt, textvariable = fn_v_mt_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")

  #########################################
  # Define widgets for reference database #
  #########################################

  # Frames
  frame_r <- tkframe(frame_tab1, relief = "groove", borderwidth = 2)
  frame_r_title <- tkframe(frame_r)
  frame_r_file <- tkframe(frame_r)
  frame_r_auto <- tkframe(frame_r_file)
  frame_r_y <- tkframe(frame_r_file)
  frame_r_mt <- tkframe(frame_r_file)

  # Title
  label_r_title <- tklabel(frame_r_title, text = "Reference database", font = "Helvetica 10 bold")

  # Autosomal STR
  label_r_auto_mk <- tklabel(frame_r_auto, text = "Autosomal STR")
  butt_r_auto_load <- tkbutton(frame_r_auto, text = "    Load    ", cursor = "hand2", command = function() open_file("auto", fp_r_auto, fn_r_auto, label_r_auto_fn, "data_r_auto", "fp_r_auto", "fn_r_auto"))
  label_r_auto_fn <- tklabel(frame_r_auto, textvariable = fn_r_auto_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")

  # Y-STR
  label_r_y_mk <- tklabel(frame_r_y, text = "Y-STR")
  butt_r_y_load <- tkbutton(frame_r_y, text = "    Load    ", cursor = "hand2", command = function() open_file("y", fp_r_y, fn_r_y, label_r_y_fn, "data_r_y", "fp_r_y", "fn_r_y"))
  label_r_y_fn <- tklabel(frame_r_y, textvariable = fn_r_y_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")

  # mtDNA
  label_r_mt_mk <- tklabel(frame_r_mt, text = "mtDNA")
  butt_r_mt_load <- tkbutton(frame_r_mt, text = "    Load    ", cursor = "hand2", command = function() open_file("mt", fp_r_mt, fn_r_mt, label_r_mt_fn, "data_r_mt", "fp_r_mt", "fn_r_mt"))
  label_r_mt_fn <- tklabel(frame_r_mt, textvariable = fn_r_mt_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")

  #########################################
  # Define widgets for allele frequencies #
  #########################################

  # Frames
  frame_af <- tkframe(frame_tab1, relief = "groove", borderwidth = 2)
  frame_af_title <- tkframe(frame_af)
  frame_af_file <- tkframe(frame_af)

  # Title
  label_af_title <- tklabel(frame_af_title, text = "Allele frequencies for autosomal STR", font = "Helvetica 10 bold")

  # Allele frequencies
  butt_af_load <- tkbutton(frame_af_file, text = "    Load    ", cursor = "hand2", command = function() open_file("auto", fp_af, fn_af, label_af_fn, "data_af", "fp_af", "fn_af"))
  label_af_fn <- tklabel(frame_af_file, textvariable = fn_af_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")

  ###############################
  # Define widgets for analysis #
  ###############################

  # Frame
  frame_search <- tkframe(frame_tab1)

  # Butt
  butt_search <- tkbutton(frame_search, text = "    Search    ", cursor = "hand2", command = function() search_rel(env_proj, env_gui))

  ####################################
  # Grid widgets for victim database #
  ####################################

  # Title
  tkgrid(label_v_title)

  # Autosomal STR
  tkgrid(label_v_auto_mk, pady = 5)
  tkgrid(butt_v_auto_load, pady = 5)
  tkgrid(label_v_auto_fn, pady = 5)

  # Y-STR
  tkgrid(label_v_y_mk, pady = 5)
  tkgrid(butt_v_y_load, pady = 5)
  tkgrid(label_v_y_fn, pady = 5)

  # mtDNA
  tkgrid(label_v_mt_mk, pady = 5)
  tkgrid(butt_v_mt_load, pady = 5)
  tkgrid(label_v_mt_fn, pady = 5)

  # Frames
  tkgrid(frame_v_auto, frame_v_y, frame_v_mt, padx = 10)
  tkgrid(frame_v_title, sticky = "w")
  tkgrid(frame_v_file)
  tkgrid(frame_v, padx = 10, pady = 5, sticky = "w")

  #######################################
  # Grid widgets for reference database #
  #######################################

  # Title
  tkgrid(label_r_title)

  # Autosomal STR
  tkgrid(label_r_auto_mk, pady = 5)
  tkgrid(butt_r_auto_load, pady = 5)
  tkgrid(label_r_auto_fn, pady = 5)

  # Y-STR
  tkgrid(label_r_y_mk, pady = 5)
  tkgrid(butt_r_y_load, pady = 5)
  tkgrid(label_r_y_fn, pady = 5)

  # mtDNA
  tkgrid(label_r_mt_mk, pady = 5)
  tkgrid(butt_r_mt_load, pady = 5)
  tkgrid(label_r_mt_fn, pady = 5)

  # Frames
  tkgrid(frame_r_auto, frame_r_y, frame_r_mt, padx = 10)
  tkgrid(frame_r_title, sticky = "w")
  tkgrid(frame_r_file)
  tkgrid(frame_r, padx = 10, pady = 5, sticky = "w")

  #######################################
  # Grid widgets for allele frequencies #
  #######################################

  # Title
  tkgrid(label_af_title)

  # Allele frequencies
  tkgrid(butt_af_load, pady = 5, padx = 10)
  tkgrid(label_af_fn, pady = 5, padx = 10)

  # Frames
  tkgrid(frame_af_title, sticky = "w")
  tkgrid(frame_af_file)
  tkgrid(frame_af, padx = 10, pady = 5, sticky = "w")

  #############################
  # Grid widgets for analysis #
  #############################

  # Butt
  tkgrid(butt_search)

  # Frame
  tkgrid(frame_search, padx = 10, pady = 5, sticky = "w")

  ###################
  # Grid frame_tab1 #
  ###################

  tkgrid(frame_tab1)
}


#####################################
# The function to check loaded data #
#####################################

check_error <- function(env_proj, env_gui){

  # Define an object to save an error message
  error_message <- ""

  # Get input data for autosomal STR from the environment "env_proj"
  data_v_auto <- get("data_v_auto", pos = env_proj)
  data_r_auto <- get("data_r_auto", pos = env_proj)
  data_af <- get("data_af", pos = env_proj)

  # Get input data for Y-STR from the environment "env_proj"
  data_v_y <- get("data_v_y", pos = env_proj)
  data_r_y <- get("data_r_y", pos = env_proj)

  # Get input data for mtDNA from the environment "env_proj"
  data_v_mt <- get("data_v_mt", pos = env_proj)
  data_r_mt <- get("data_r_mt", pos = env_proj)

  # check whether all required data is loaded or not
  bool_check_auto <- all(c(length(data_v_auto) > 0, length(data_r_auto) > 0, length(data_af) > 0))
  bool_check_y <- all(c(length(data_v_y) > 0, length(data_r_y) > 0))
  bool_check_mt <- all(c(length(data_v_mt) > 0, length(data_r_mt) > 0))

  # All required data for all markers is not loaded
  if(!any(c(bool_check_auto, bool_check_y, bool_check_mt))){
    error_message <- "Load required file(s)!"

  # All required data for at least one marker type is loaded
  }else{

    # Get the package path
    path_pack <- get("path_pack", pos = env_gui)

    # Get file names in the folder "parameters"
    fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

    # If the file 'criteria.csv' is not found
    if(!is.element("criteria.csv", fn_par)){
      error_message <- "Set criteria via 'Tools > Set criteria.'"

    # If the file 'criteria.csv' is found
    }else{

      # Get the end-sign of the analysis from the environment "env_proj"
      fin_auto <- get("fin_auto", pos = env_proj)
      fin_y <- get("fin_y", pos = env_proj)
      fin_mt <- get("fin_mt", pos = env_proj)

      # All required data for the autosomal STR is loaded
      if(!fin_auto && bool_check_auto){

        # If all required csv files are located in 'extdata > parameters'.
        if(all(is.element(c("myu.csv", "rel.csv", "par_auto.csv"), fn_par))){

          # Extract loci from each database
          locus_v_auto <- setdiff(names(data_v_auto), c("SampleName", "Relationship"))
          locus_r_auto <- setdiff(names(data_r_auto), c("SampleName", "Relationship"))
          locus_af <- setdiff(names(data_af), "Allele")

          # Load mutation rates
          data_myu <- fread(paste0(path_pack, "/extdata/parameters/myu.csv"))
          locus_myu <- data_myu[, Marker]

          # Load information on relationship
          data_rel <- fread(paste0(path_pack, "/extdata/parameters/rel.csv"))
          names_rel <- data_rel[, Name_relationship]

          # Whether the locus set of the query database is the same as that of the reference database or not
          bool_locus_1 <- setequal(locus_v_auto, locus_r_auto)

          # Whether the locus set of the query database is the same as that of the allele frequencies or not
          bool_locus_2 <- setequal(locus_v_auto, locus_af)

          # Whether all loci of the query database is included in the locus set of the mutation rates or not
          bool_locus_3 <- all(is.element(locus_v_auto, locus_myu))

          # Whether all relationships of the reference database is included in the relationships of the IBD probabilities or not
          bool_rel_1 <- all(is.element(unique(data_r_auto[, Relationship]), names_rel))

          # If the locus set of the query database is not the same as that of the reference database
          if(!bool_locus_1){
            error_message <- "Locus set is not the same between query data and reference data!"

          # If the locus set of the query database is not the same as that of the allele frequencies
          }else if(!bool_locus_2){
            error_message <- "Locus set is not the same between query data and allele frequencies!"

          # If some loci of the query database is not included in the locus set of the mutation rates
          }else if(!bool_locus_3){
            error_message <- "There are some loci without mutation rates!"

          # If some relationships of the reference database is not included in the relationships of the IBD probabilities
          }else if(!bool_rel_1){
            error_message <- "There are some relationships without IBD probabilities!"
          }

        # If the file 'myu.csv' is not found
        }else if(!is.element("myu.csv", fn_par)){
          error_message <- "Set mutation rates via 'Tools > Set mutation rates for autosomal STR.'"

        # If the file 'rel.csv' is not found
        }else if(!is.element("rel.csv", fn_par)){
          error_message <- "Set relationships via 'Tools > Set relationships.'"

        # If the file 'par_auto.csv' is not found
        }else if(!is.element("par_auto.csv", fn_par)){
          error_message <- "Set analysis methods via 'Tools > Set analysis method for autosomal STR.'"
        }
      }

      # All required data for the Y-STR is loaded
      if(!fin_y && error_message == "" && bool_check_y){

        # Extract loci from each database
        locus_v_y <- setdiff(names(data_v_y), c("SampleName", "Relationship"))
        locus_r_y <- setdiff(names(data_r_y), c("SampleName", "Relationship"))

        # Whether the locus set of the query database is the same as that of the reference database or not
        bool_locus_1 <- setequal(locus_v_y, locus_r_y)

        # If the locus set of the query database is not the same as that of the reference database
        if(!bool_locus_1){
          error_message <- "Locus set is not the same between query data and reference data!"
        }
      }
    }
  }
  return(error_message)
}


##################################################
# The function to analyze data for autosomal STR #
##################################################

analyze_auto <- function(env_proj, env_gui){

  # Get input data from the environment "env_proj"
  data_v_auto <- get("data_v_auto", pos = env_proj)
  data_r_auto <- get("data_r_auto", pos = env_proj)
  data_af <- get("data_af", pos = env_proj)

  # Get the package path
  path_pack <- get("path_pack", pos = env_gui)

  # Load mutation rates
  data_myu <- fread(paste0(path_pack, "/extdata/parameters/myu.csv"))
  locus_myu <- data_myu[, Marker]
  myu_all <- data_myu[, Myu]

  # Load information on relationship
  data_rel <- fread(paste0(path_pack, "/extdata/parameters/rel.csv"))
  names_rel <- data_rel[, Name_relationship]
  degrees_rel <- data_rel[, Degree]
  pibds_rel <- as.matrix(data_rel[, list(Pr_IBD2, Pr_IBD1, Pr_IBD0)])
  pibds_rel <- asplit(pibds_rel, 1)

  # Load analysis methods
  data_par_auto <- fread(paste0(path_pack, "/extdata/parameters/par_auto.csv"))
  maf <- data_par_auto$Value[data_par_auto$Parameter == "maf"]
  meth_d <- data_par_auto$Value[data_par_auto$Parameter == "meth_d"]
  pd <- data_par_auto$Value[data_par_auto$Parameter == "pd"]

  # Extract loci
  locus_auto <- setdiff(names(data_v_auto), c("SampleName", "Relationship"))

  # The number of loci
  n_l <- length(locus_auto)

  # Order loci of each database
  pos_v <- rep(0, 2 * n_l + 1)
  pos_r <- rep(0, 2 * n_l + 2)
  pos_af <- rep(0, n_l + 1)
  pos_v[1] <- which(is.element(names(data_v_auto), "SampleName"))
  pos_r[1] <- which(is.element(names(data_r_auto), "SampleName"))
  pos_r[2] <- which(is.element(names(data_r_auto), "Relationship"))
  pos_af[1] <- which(is.element(names(data_af), "Allele"))
  for(i in 1:n_l){
    pos_v[c(2 * i, 2 * i + 1)] <- which(is.element(names(data_v_auto), locus_auto[i]))
    pos_r[c(2 * i + 1, 2 * i + 2)] <- which(is.element(names(data_r_auto), locus_auto[i]))
    pos_af[i + 1] <- which(is.element(names(data_af), locus_auto[i]))
  }
  data_v_auto <- data_v_auto[, pos_v, with = FALSE]
  data_r_auto <- data_r_auto[, pos_r, with = FALSE]
  data_af <- data_af[, pos_af, with = FALSE]

  # Extract required data from victim database
  sn_v_auto <- data_v_auto[, SampleName]
  gt_v_auto <- as.matrix(data_v_auto[, -"SampleName"])

  # Extract required data from reference database
  sn_r_auto <- data_r_auto[, SampleName]
  assumed_rel_all <- data_r_auto[, Relationship]
  gt_r_auto <- as.matrix(data_r_auto[, -c("SampleName", "Relationship")])

  # The NA in genotypes is replaced to -99 to deal with the C++ program
  gt_v_auto[which(is.na(gt_v_auto) == TRUE, arr.ind = TRUE)] <- -99
  gt_r_auto[which(is.na(gt_r_auto) == TRUE, arr.ind = TRUE)] <- -99

  # Change matrix to list for genotypes
  gt_v_auto <- asplit(gt_v_auto, 1)
  gt_r_auto <- asplit(gt_r_auto, 1)

  # Set allele frequencies
  tmp <- set_af(data_v_auto, data_r_auto, data_af, maf)
  af_list <- tmp[[1]]
  af_al_list <- tmp[[2]]

  # Extract mutation rates
  myus <- rep(0, n_l)
  for(i in 1:n_l){
    myus[i] <- myu_all[which(locus_myu == locus_auto[i])]
  }
  names(myus) <- locus_auto

  # Calculate average probabilities of exclusion
  apes <- rep(0, n_l)
  for(i in 1:n_l){
    apes[i] <- calc_ape(af_list[[i]])
  }
  names(apes) <- locus_auto

  # Calculate likelihood ratios
  result_auto <- calc_kin_lr_all(gt_v_auto, gt_r_auto, assumed_rel_all, af_list, af_al_list, names_rel, degrees_rel, pibds_rel, myus, apes, meth_d, pd)

  # Arrange result_auto
  result_auto <- unlist(result_auto)
  result_auto <- matrix(result_auto, nrow = length(sn_v_auto) * length(sn_r_auto), ncol = 3 * (n_l + 1), byrow = TRUE)
  rownames(result_auto) <- NULL
  colnames(result_auto) <- c(paste0("LikeH1_", c(locus_auto, "Total")), paste0("LikeH2_", c(locus_auto, "Total")), paste0("LR_", c(locus_auto, "Total")))

  # Assign result_auto
  assign("result_auto", result_auto, envir = env_proj)

  # Assign updated input data (ordered loci)
  assign("data_v_auto", data_v_auto, envir = env_proj)
  assign("data_r_auto", data_r_auto, envir = env_proj)
  assign("data_af", data_af, envir = env_proj)

  # Assign parameters to the environment "env_proj"
  assign("data_rel", data_rel, envir = env_proj)
  assign("data_par_auto", data_par_auto, envir = env_proj)
  assign("myus", myus, envir = env_proj)
  assign("apes", apes, envir = env_proj)

  # Update sample names in the environment "env_proj"
  set_env_proj_sn(env_proj, FALSE, sn_v_auto, sn_r_auto)

  # Assign the end sign to the environment "env_proj"
  assign("fin_auto", TRUE, envir = env_proj)
}


##########################################
# The function to analyze data for Y-STR #
##########################################

analyze_y <- function(env_proj){

  # Get input data from the environment "env_proj"
  data_v_y <- get("data_v_y", pos = env_proj)
  data_r_y <- get("data_r_y", pos = env_proj)

  # Extract loci
  locus_y <- setdiff(names(data_v_y), c("SampleName", "Relationship"))

  # The number of loci
  n_l <- length(locus_y)

  # Order loci of reference database
  data_r_y <- data_r_y[, match(names(data_v_y), names(data_r_y)), with = FALSE]

  # Change from numeric to character
  change_columns <- names(data_v_y)
  data_v_y[, (change_columns) := lapply(.SD, as.character), .SDcols = change_columns]
  change_columns <- names(data_r_y)
  data_r_y[, (change_columns) := lapply(.SD, as.character), .SDcols = change_columns]

  # Extract required data from victim database
  sn_v_y <- data_v_y[, SampleName]
  hap_v_y <- as.matrix(data_v_y[, -"SampleName"])

  # Extract required data from reference database
  sn_r_y <- data_r_y[, SampleName]
  hap_r_y <- as.matrix(data_r_y[, -"SampleName"])

  # The NA in genotypes is replaced to "" to deal with the C++ program
  hap_v_y[which(is.na(hap_v_y) == TRUE, arr.ind = TRUE)] <- ""
  hap_r_y[which(is.na(hap_r_y) == TRUE, arr.ind = TRUE)] <- ""

  # Change matrix to list for haplotypes
  hap_v_y <- asplit(hap_v_y, 1)
  hap_r_y <- asplit(hap_r_y, 1)

  # Analyze data for Y-STR
  result_y <- match_y_all(hap_v_y, hap_r_y)

  # Arrange result_y
  result_y <- unlist(result_y)
  result_y <- matrix(result_y, nrow = length(sn_v_y) * length(sn_r_y), ncol = 3 * (n_l + 1), byrow = TRUE)
  rownames(result_y) <- NULL
  colnames(result_y) <- c(paste0("Mismatch_", c(locus_y, "Total")), paste0("Ignore_", c(locus_y, "Total")), paste0("MuStep_", c(locus_y, "Total")))

  # Assign result_y
  assign("result_y", result_y, envir = env_proj)

  # Assign updated input data (ordered loci)
  assign("data_v_y", data_v_y, envir = env_proj)
  assign("data_r_y", data_r_y, envir = env_proj)

  # Update sample names in the environment "env_proj"
  set_env_proj_sn(env_proj, FALSE, sn_v_y, sn_r_y)

  # Assign the end sign
  assign("fin_y", TRUE, envir = env_proj)
}


##########################################
# The function to analyze data for mtDNA #
##########################################

analyze_mt <- function(env_proj){

  # Get input data from the environment "env_proj"
  data_v_mt <- get("data_v_mt", pos = env_proj)
  data_r_mt <- get("data_r_mt", pos = env_proj)

  # Extract required data from victim database
  sn_v_mt <- data_v_mt[, SampleName]
  range_v_mt <- data_v_mt[, Range]
  hap_v_mt <- strsplit(data_v_mt[, Haplotype], " ")
  hap_v_mt <- lapply(hap_v_mt, setdiff, "")

  # Extract required data from reference database
  sn_r_mt <- data_r_mt[, SampleName]
  range_r_mt <- data_r_mt[, Range]
  hap_r_mt <- strsplit(data_r_mt[, Haplotype], " ")
  hap_r_mt <- lapply(hap_r_mt, setdiff, "")

  # Analyze data for mtDNA
  result_mt <- match_mt_all(hap_v_mt, hap_r_mt, range_v_mt, range_r_mt)

  # Arrange result_mt
  result_mt <- unlist(result_mt)
  result_mt <- matrix(result_mt, nrow = length(sn_v_mt) * length(sn_r_mt), ncol = 3, byrow = TRUE)
  rownames(result_mt) <- NULL
  colnames(result_mt) <- c("MismatchMt", "ShareRangeMt", "ShareLengthMt")

  # Assign result_mt
  assign("result_mt", result_mt, envir = env_proj)

  # Update sample names in the environment "env_proj"
  set_env_proj_sn(env_proj, FALSE, sn_v_mt, sn_r_mt)

  # Assign the end sign
  assign("fin_mt", TRUE, envir = env_proj)
}


########################
# Create combined data #
########################

create_combined_data <- function(env_proj){

  # Get criteria
  criteria <- get("criteria", pos = env_proj)

  # Extract criteria
  min_lr_auto <- criteria$Value[criteria$Criteria == "min_lr_auto"]
  max_mismatch_y <- criteria$Value[criteria$Criteria == "max_mismatch_y"]
  max_ignore_y <- criteria$Value[criteria$Criteria == "max_ignore_y"]
  max_mustep_y <- criteria$Value[criteria$Criteria == "max_mustep_y"]
  min_share_mt <- criteria$Value[criteria$Criteria == "min_share_mt"]
  max_mismatch_mt <- criteria$Value[criteria$Criteria == "max_mismatch_mt"]

  # Get all sample names from the environment "env_proj"
  sn_v_all <- get("sn_v_all", pos = env_proj)
  sn_r_all <- get("sn_r_all", pos = env_proj)

  # The number of samples in all database
  n_v_all <- length(sn_v_all)
  n_r_all <- length(sn_r_all)

  # Get the end-sign of the analysis from the environment "env_proj"
  fin_auto <- get("fin_auto", pos = env_proj)
  fin_y <- get("fin_y", pos = env_proj)
  fin_mt <- get("fin_mt", pos = env_proj)

  # Extract required data for autosomal STR
  if(fin_auto){

    # Get input data for autosomal STR from the environment "env_proj"
    data_v_auto <- get("data_v_auto", pos = env_proj)
    data_r_auto <- get("data_r_auto", pos = env_proj)

    # Get results
    result_auto <- get("result_auto", pos = env_proj)

    # Extract locus names
    locus_auto <- setdiff(names(data_v_auto), c("SampleName", "Relationship"))
  }else{

    # Define locus names
    locus_auto <- character(0)
  }

  # Extract required data for Y-STR
  if(fin_y){

    # Get input data for Y-STR from the environment "env_proj"
    data_v_y <- get("data_v_y", pos = env_proj)
    data_r_y <- get("data_r_y", pos = env_proj)

    # Get results
    result_y <- get("result_y", pos = env_proj)

    # Extract locus names
    locus_y <- setdiff(names(data_v_y), c("SampleName", "Relationship"))
  }else{

    # Define locus names
    locus_y <- character(0)
  }

  # Extract required data for mtDNA
  if(fin_mt){

    # Get input data for mtDNA from the environment "env_proj"
    data_v_mt <- get("data_v_mt", pos = env_proj)
    data_r_mt <- get("data_r_mt", pos = env_proj)

    # Get results
    result_mt <- get("result_mt", pos = env_proj)
  }

  # Define a matrix for saving all results
  result_all <- matrix("", nrow = n_v_all * n_r_all, ncol = 3 * (length(locus_auto) + 1) + 3 * (length(locus_y) + 1) + 9)

  # Define column names for result_all
  cn_result_all <- c("Victim", "Reference", "AssumedRel",
                     paste0("LikeH1_", c(locus_auto, "Total")), paste0("LikeH2_", c(locus_auto, "Total")), paste0("LR_", c(locus_auto, "Total")),
                     paste0("Mismatch_", c(locus_y, "Total")), paste0("Ignore_", c(locus_y, "Total")), paste0("MuStep_", c(locus_y, "Total")),
                     "MismatchMt", "ShareRangeMt", "ShareLengthMt",
                     "EstimatedRel", "Paternal", "Maternal")
  colnames(result_all) <- cn_result_all

  # Record all sample names
  result_all[, 1] <- rep(sn_v_all, n_r_all)
  result_all[, 2] <- as.vector(sapply(sn_r_all, rep, n_v_all))

  # Record results for autosomal STR
  if(fin_auto){
    pos_sn_auto <- search_pos_sn_comb(result_all[, 1], result_all[, 2], data_v_auto[, SampleName], data_r_auto[, SampleName]) + 1
    result_all[pos_sn_auto, 3] <- as.character(sapply(data_r_auto[, Relationship], rep, nrow(data_v_auto)))
    result_all[pos_sn_auto, c(grep("LikeH1_", cn_result_all), grep("LikeH2_", cn_result_all), grep("LR_", cn_result_all))] <- result_auto
  }

  # Record results for Y-STR
  if(fin_y){
    pos_sn_y <- search_pos_sn_comb(result_all[, 1], result_all[, 2], data_v_y[, SampleName], data_r_y[, SampleName]) + 1
    result_all[pos_sn_y, c(grep("Mismatch_", cn_result_all), grep("Ignore_", cn_result_all), grep("MuStep_", cn_result_all))] <- result_y
  }

  # Record results for mtDNA
  if(fin_mt){
    pos_sn_mt <- search_pos_sn_comb(result_all[, 1], result_all[, 2], data_v_mt[, SampleName], data_r_mt[, SampleName]) + 1
    result_all[pos_sn_mt, is.element(cn_result_all, c("MismatchMt", "ShareRangeMt", "ShareLengthMt"))] <- result_mt
  }

  # Record estimated relationships
  pos_meet_criteria_auto <- which(as.numeric(result_all[, "LR_Total"]) >= min_lr_auto)
  result_all[pos_meet_criteria_auto, "EstimatedRel"] <- result_all[pos_meet_criteria_auto, "AssumedRel"]

  # Record paternal relationships
  bool_meet_criteria_y <- matrix(FALSE, nrow(result_all), 4)
  bool_meet_criteria_y[, 1] <- as.numeric(result_all[, "Mismatch_Total"]) <= max_mismatch_y
  bool_meet_criteria_y[, 2] <- as.numeric(result_all[, "Ignore_Total"]) <= max_ignore_y
  bool_meet_criteria_y[, 3] <- as.numeric(result_all[, "MuStep_Total"]) <= max_mustep_y
  bool_meet_criteria_y[, 4] <- as.numeric(result_all[, "MuStep_Total"]) %% 1 == 0
  pos_meet_criteria_y <- which(apply(bool_meet_criteria_y, 1, all))
  result_all[pos_meet_criteria_y, "Paternal"] <- "support"

  # Record maternal relationships
  bool_meet_criteria_mt <- matrix(FALSE, nrow(result_all), 2)
  bool_meet_criteria_mt[, 1] <- as.numeric(result_all[, "MismatchMt"]) <= max_mismatch_mt
  bool_meet_criteria_mt[, 2] <- as.numeric(result_all[, "ShareLengthMt"]) >= min_share_mt
  pos_meet_criteria_mt <- which(apply(bool_meet_criteria_mt, 1, all))
  result_all[pos_meet_criteria_mt, "Maternal"] <- "support"

  # Create data.table for all results
  dt_result <- as.data.frame(result_all)
  setDT(dt_result)
  names(dt_result) <- cn_result_all

  # Convert column classes
  col_change <- c(paste0("LikeH1_", c(locus_auto, "Total")), paste0("LikeH2_", c(locus_auto, "Total")), paste0("LR_", c(locus_auto, "Total")),
                  paste0("Mismatch_", c(locus_y, "Total")), paste0("Ignore_", c(locus_y, "Total")), paste0("MuStep_", c(locus_y, "Total")),
                  "MismatchMt", "ShareLengthMt")
  dt_result[, (col_change) := lapply(.SD, as.numeric), .SDcols = col_change]

  # Assign data table for all results
  assign("dt_result", dt_result, envir = env_proj)
}


####################################
# The function to search relatives #
####################################

search_rel <- function(env_proj, env_gui){

  # Check whether all required data is loaded or not
  error_message <- check_error(env_proj, env_gui)
  if(error_message != ""){
    tkmessageBox(message = error_message, icon = "error", type = "ok")
  }else{

    # Get the package path
    path_pack <- get("path_pack", pos = env_gui)

    # Load criteria
    criteria <- fread(paste0(path_pack, "/extdata/parameters/criteria.csv"))
    assign("criteria", criteria, envir = env_proj)

    # Get the end-sign of the analysis from the environment "env_proj"
    fin_auto <- get("fin_auto", pos = env_proj)
    fin_y <- get("fin_y", pos = env_proj)
    fin_mt <- get("fin_mt", pos = env_proj)

    # Analyze data for autosomal STR
    if(!fin_auto){
      cat("Analyzing data for autosomal STR", "\n")
      analyze_auto(env_proj, env_gui)
    }

    # Analyze data for Y-STR
    if(!fin_y){
      cat("Analyzing data for Y-STR", "\n")
      analyze_y(env_proj)
    }

    # Analyze data for mtDNA
    if(!fin_mt){
      cat("Analyzing data for mtDNA", "\n")
      analyze_mt(env_proj)
    }

    # Create combined data
    create_combined_data(env_proj)

    # Make tab2
    make_tab2(env_proj, env_gui)
  }
}
