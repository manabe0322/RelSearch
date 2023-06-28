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
  return(gt_vec)
}


#############################
# The function to make tab2 #
#############################

make_tab2 <- function(env_proj, env_gui){

  # Get the end-sign of the analysis from the environment "env_proj"
  fin_auto <- get("fin_auto", pos = env_proj)
  fin_y <- get("fin_y", pos = env_proj)
  fin_mt <- get("fin_mt", pos = env_proj)

  if(any(fin_auto, fin_y, fin_mt)){

    ######################################################
    # The function to create displayed data for criteria #
    ######################################################

    create_display_criteria <- function(sn_v_select, sn_r_select){

      # Extract all results for the selected data
      result_selected <- dt_result[.(sn_v_select, sn_r_select)]

      # Get criteria
      criteria <- get("criteria", pos = env_proj)

      # Extract criteria
      min_lr_auto <- criteria$Value[criteria$Criteria == "min_lr_auto"]
      max_mismatch_y <- criteria$Value[criteria$Criteria == "max_mismatch_y"]
      max_ignore_y <- criteria$Value[criteria$Criteria == "max_ignore_y"]
      max_mustep_y <- criteria$Value[criteria$Criteria == "max_mustep_y"]
      max_mismatch_mt <- criteria$Value[criteria$Criteria == "max_mismatch_mt"]
      min_share_mt <- criteria$Value[criteria$Criteria == "min_share_mt"]

      # Create displayed marker data
      marker_display <- c("Autosomal STR", "Y-STR", "", "", "mtDNA", "")

      # Number of rows for displayed data
      n_row <- length(marker_display)

      # Create displayed condition data
      condition_display <- c(paste0("LR ≥ ", min_lr_auto),
                             paste0("Number of mismatched loci ≤ ", max_mismatch_y),
                             paste0("Number of ignored loci ≤ ", max_ignore_y),
                             paste0("Mutational step ≤ ", max_mustep_y),
                             paste0("Number of mismatched types ≤ ", max_mismatch_mt),
                             paste0("Shared length ≥ ", min_share_mt))

      # Create displayed meet-criteria data
      meet_criteria_display <- rep("", n_row)
      if(is.na(result_selected[, LR_Total])){
        meet_criteria_display[1] <- "No data"
      }else if(result_selected[, LR_Total] >= min_lr_auto){
        meet_criteria_display[1] <- "Yes"
      }else{
        meet_criteria_display[1] <- "No"
      }
      if(is.na(result_selected[, Mismatch_Total])){
        meet_criteria_display[2] <- "No data"
      }else if(result_selected[, Mismatch_Total] <= max_mismatch_y){
        meet_criteria_display[2] <- "Yes"
      }else{
        meet_criteria_display[2] <- "No"
      }
      if(is.na(result_selected[, Ignore_Total])){
        meet_criteria_display[3] <- "No data"
      }else if(result_selected[, Ignore_Total] <= max_ignore_y){
        meet_criteria_display[3] <- "Yes"
      }else{
        meet_criteria_display[3] <- "No"
      }
      if(is.na(result_selected[, MuStep_Total])){
        meet_criteria_display[4] <- "No data"
      }else if(result_selected[, MuStep_Total] <= max_mustep_y){
        meet_criteria_display[4] <- "Yes"
      }else{
        meet_criteria_display[4] <- "No"
      }
      if(is.na(result_selected[, MismatchMt])){
        meet_criteria_display[5] <- "No data"
      }else if(result_selected[, MismatchMt] <= max_mismatch_mt){
        meet_criteria_display[5] <- "Yes"
      }else{
        meet_criteria_display[5] <- "No"
      }
      if(is.na(result_selected[, ShareLengthMt])){
        meet_criteria_display[6] <- "No data"
      }else if(result_selected[, ShareLengthMt] >= min_share_mt){
        meet_criteria_display[6] <- "Yes"
      }else{
        meet_criteria_display[6] <- "No"
      }

      # Create displayed data
      dt_criteria_display <- data.table(Marker = marker_display,
                                        Condition = condition_display,
                                        Meet_Criteria = meet_criteria_display)

      # Return displayed data for criteria
      return(dt_criteria_display)
    }

    ###########################################################
    # The function to create displayed data for autosomal STR #
    ###########################################################

    create_display_auto <- function(sn_v_select, sn_r_select){

      if(fin_auto){

        # Get input data for autosomal STR from the environment "env_proj"
        data_v_auto <- get("data_v_auto", pos = env_proj)
        data_r_auto <- get("data_r_auto", pos = env_proj)

        # Set key for each input data
        setkey(data_v_auto, SampleName)
        setkey(data_r_auto, SampleName)

        # Extract all results for the selected data
        result_selected <- dt_result[.(sn_v_select, sn_r_select)]
        cn_result <- names(result_selected)

        # Extract genotypes for the selected data
        data_v_auto_select <- data_v_auto[.(sn_v_select), nomatch = NULL]
        data_r_auto_select <- data_r_auto[.(sn_r_select), nomatch = NULL]

        # Create displayed data
        if(nrow(data_v_auto_select) == 1 && nrow(data_r_auto_select) == 1){

          # Extract profiles
          options(warn = -1)
          prof_v_select <- as.numeric(data_v_auto_select[, -c("SampleName", "Relationship"), with = FALSE])
          prof_r_select <- as.numeric(data_r_auto_select[, -c("SampleName", "Relationship"), with = FALSE])
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

          # Create data table
          dt_auto_display <- data.table(Locus = locus_display, Profile_V = prof_v_display, Profile_R = prof_r_display, LikeH1 = like_h1_display, LikeH2 = like_h2_display, LR = lr_display)

        }else{
          dt_auto_display <- NULL
        }
      }else{
        dt_auto_display <- NULL
      }

      # Return displayed data for autosomal STR
      return(dt_auto_display)
    }


    ###################################################
    # The function to create displayed data for Y-STR #
    ###################################################

    create_display_y <- function(sn_v_select, sn_r_select){

      if(fin_y){

        # Get input data for Y-STR from the environment "env_proj"
        data_v_y <- get("data_v_y", pos = env_proj)
        data_r_y <- get("data_r_y", pos = env_proj)

        # Set key for each input data
        setkey(data_v_y, SampleName)
        setkey(data_r_y, SampleName)

        # Extract all results for the selected data
        result_selected <- dt_result[.(sn_v_select, sn_r_select)]
        cn_result <- names(result_selected)

        # Extract haplotypes for the selected data
        data_v_y_select <- data_v_y[.(sn_v_select), nomatch = NULL]
        data_r_y_select <- data_r_y[.(sn_r_select), nomatch = NULL]

        # Create displayed data
        if(nrow(data_v_y_select) == 1 && nrow(data_r_y_select) == 1){

          # Victim profile
          prof_v_display <- c(as.character(data_v_y_select[, -"SampleName", with = FALSE]), "")
          prof_v_display[is.na(prof_v_display)] <- ""

          # Reference profile
          prof_r_display <- c(as.character(data_r_y_select[, -"SampleName", with = FALSE]), "")
          prof_r_display[is.na(prof_r_display)] <- ""

          # Locus
          locus_display <- gsub("Mismatch_", "", cn_result[grep("Mismatch_", cn_result)])

          # Mismatched loci
          mismatch_y_display <- as.character(result_selected[, grep("Mismatch_", cn_result), with = FALSE])
          mismatch_y_display[mismatch_y_display == 0] <- ""
          if(mismatch_y_display[length(mismatch_y_display)] == ""){
            mismatch_y_display[length(mismatch_y_display)] <- "0"
          }

          # Ignored loci
          ignore_y_display <- as.character(result_selected[, grep("Ignore_", cn_result), with = FALSE])
          ignore_y_display[ignore_y_display == 0] <- ""
          if(ignore_y_display[length(ignore_y_display)] == ""){
            ignore_y_display[length(ignore_y_display)] <- "0"
          }

          # Mutational step
          mustep_y_display <- as.character(result_selected[, grep("MuStep_", cn_result), with = FALSE])

          # Create data table
          dt_y_display <- data.table(Locus = locus_display, Profile_V = prof_v_display, Profile_R = prof_r_display, Ignore = ignore_y_display, Mismatch = mismatch_y_display, MuStep = mustep_y_display)

        }else{
          dt_y_display <- NULL
        }
      }else{
        dt_y_display <- NULL
      }

      # Return displayed data for Y-STR
      return(dt_y_display)
    }


    ###################################################
    # The function to create displayed data for mtDNA #
    ###################################################

    create_display_mt <- function(sn_v_select, sn_r_select){

      if(fin_mt){

        # Get input data for mtDNA from the environment "env_proj"
        data_v_mt <- get("data_v_mt", pos = env_proj)
        data_r_mt <- get("data_r_mt", pos = env_proj)

        # Set key for each input data
        setkey(data_v_mt, SampleName)
        setkey(data_r_mt, SampleName)

        # Extract haplotypes for the selected data
        data_v_mt_select <- data_v_mt[.(sn_v_select), nomatch = NULL]
        data_r_mt_select <- data_r_mt[.(sn_r_select), nomatch = NULL]

        # Create displayed data
        if(nrow(data_v_mt_select) == 1 && nrow(data_r_mt_select) == 1){

          # Victim types
          type_v <- as.character(data_v_mt_select[, -c("SampleName", "Range"), with = FALSE])
          type_v <- strsplit(type_v, " ")[[1]]
          type_v <- setdiff(type_v, "")
          type_v <- type_v[order(sapply(type_v, extract_integer))]

          # Reference types
          type_r <- as.character(data_r_mt_select[, -c("SampleName", "Range"), with = FALSE])
          type_r <- strsplit(type_r, " ")[[1]]
          type_r <- setdiff(type_r, "")
          type_r <- type_r[order(sapply(type_r, extract_integer))]

          # Victim or Reference types
          type_vr <- union(type_v, type_r)
          type_vr <- type_vr[order(sapply(type_vr, extract_integer))]
          n_type_vr <- length(type_vr)

          # Common positions between victim and reference
          pos_mt_vr <- extract_pos_mt_vr(data_v_mt_select[, Range], data_r_mt_select[, Range])

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

          # Create data table
          dt_mt_display <- data.table(Type_Victim = type_v_display, Type_Reference = type_r_display, OutRange = out_range_display, Mismatch = mismatch_mt_display)
        }else{
          dt_mt_display <- NULL
        }
      }else{
        dt_mt_display <- NULL
      }

      # Return displayed data for mtDNA
      return(dt_mt_display)
    }


    #######################################
    # The function to show data in detail #
    #######################################

    show_detail <- function(){

      # Get the multi-list box for displayed data from the environment "env_result"
      mlb_result <- get("mlb_result", pos = env_result)

      # If the user does not select one line
      if(tclvalue(tkcurselection(mlb_result)) == ""){
        tkmessageBox(message = "Select one result!", icon = "error", type = "ok")

      # If the user selects one line
      }else{

        # Index of the selected data
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_result))) + 1

        # Get displayed data from the environment "env_result"
        dt_display <- get("dt_display", pos = env_result)

        # Extract sample names of the selected data
        sn_v_select <- dt_display[pos_select, Victim]
        sn_r_select <- dt_display[pos_select, Reference]

        # Extract results of selected samples
        result_selected <- dt_result[.(sn_v_select, sn_r_select)]

        # Define displayed estimated-relationship
        est_rel_display <- result_selected[, EstimatedRel]
        if(est_rel_display == ""){
          est_rel_display <- "Nothing"
        }

        # Define displayed paternal
        paternal_display <- result_selected[, Paternal]
        if(paternal_display == ""){
          paternal_display <- "Not support"
        }

        # Define displayed maternal
        maternal_display <- result_selected[, Maternal]
        if(maternal_display == ""){
          maternal_display <- "Not support"
        }

        # Create data for criteria
        dt_criteria_display <- create_display_criteria(sn_v_select, sn_r_select)

        # Create data for autosomal STR
        dt_auto_display <- create_display_auto(sn_v_select, sn_r_select)

        # Create data for Y-STR
        dt_y_display <- create_display_y(sn_v_select, sn_r_select)

        # Create data for mtDNA
        dt_mt_display <- create_display_mt(sn_v_select, sn_r_select)

        # Define displayed shared range for mtDNA
        share_range_display <- ""
        if(nrow(dt_mt_display) > 0){
          share_range_display <- result_selected[, ShareRangeMt]
        }

        # Create data.table for other candidates
        dt_other_candidate <- dt_display[Victim == sn_v_select | Reference == sn_r_select]
        dt_other_candidate <- dt_other_candidate[Victim != sn_v_select | Reference != sn_r_select]
        dt_other_candidate$LR_Total <- signif(dt_other_candidate$LR_Total, 3)

        # Make a top frame
        tf_detail <- tktoplevel()
        tkwm.title(tf_detail, "Results in detail")

        # Define frames
        frame_row1 <- tkframe(tf_detail)
        frame_row2 <- tkframe(tf_detail)
        frame_row3 <- tkframe(tf_detail)
        frame_sn <- tkframe(frame_row1, relief = "groove", borderwidth = 2)
        frame_summary <- tkframe(frame_row1, relief = "groove", borderwidth = 2)
        frame_criteria <- tkframe(frame_row1, relief = "groove", borderwidth = 2)
        frame_auto <- tkframe(frame_row2, relief = "groove", borderwidth = 2)
        frame_y <- tkframe(frame_row2, relief = "groove", borderwidth = 2)
        frame_mt <- tkframe(frame_row2, relief = "groove", borderwidth = 2)
        frame_other <- tkframe(frame_row3, relief = "groove", borderwidth = 2)

        # Define and grid widgets for sample names
        tkgrid(tklabel(frame_sn, text = "Sample names", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
        tkgrid(tklabel(frame_sn, text = "    Victim"), tklabel(frame_sn, text = sn_v_select), padx = 10, pady = 5, sticky = "w")
        tkgrid(tklabel(frame_sn, text = "    Reference"), tklabel(frame_sn, text = sn_r_select), padx = 10, pady = 5, sticky = "w")

        # Define and grid widgets for summary
        tkgrid(tklabel(frame_summary, text = "Summary", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
        tkgrid(tklabel(frame_summary, text = "    Estimated relationship"), tklabel(frame_summary, text = est_rel_display), padx = 10, pady = 5, sticky = "w")
        tkgrid(tklabel(frame_summary, text = "    Paternal"), tklabel(frame_summary, text = paternal_display), padx = 10, pady = 5, sticky = "w")
        tkgrid(tklabel(frame_summary, text = "    Maternal"), tklabel(frame_summary, text = maternal_display), padx = 10, pady = 5, sticky = "w")

        # Define and grid widgets for criteria
        tkgrid(tklabel(frame_criteria, text = "Criteria", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
        mlb_criteria <- tk2mclistbox(frame_criteria, width = 75, height = 6, resizablecolumns = TRUE, selectmode = "single")
        tk2column(mlb_criteria, "add", label = "Marker", width = 15)
        tk2column(mlb_criteria, "add", label = "Condition", width = 45)
        tk2column(mlb_criteria, "add", label = "Meet criteria", width = 15)
        tk2insert.multi(mlb_criteria, "end", dt_criteria_display)
        tkgrid(mlb_criteria)

        # Define and grid widgets for autosomal STR
        tkgrid(tklabel(frame_auto, text = "Autosomal STR", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
        if(length(dt_auto_display) == 0){

          # Grid a label for no-data
          tkgrid(tklabel(frame_auto, text = "    No data"), padx = 10, pady = 5, sticky = "w")
        }else{

          # Define a scrollbar for a multi-list box (mlb_auto)
          scr_auto <- tkscrollbar(frame_auto, repeatinterval = 5, command = function(...) tkyview(mlb_auto, ...))

          # Define a multi-list box (mlb_auto)
          mlb_auto <- tk2mclistbox(frame_auto, width = 60, height = 26, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr_auto, ...))
          tk2column(mlb_auto, "add", label = "Locus", width = 10)
          tk2column(mlb_auto, "add", label = "Victim", width = 10)
          tk2column(mlb_auto, "add", label = "Reference", width = 10)
          tk2column(mlb_auto, "add", label = "L1", width = 10)
          tk2column(mlb_auto, "add", label = "L2", width = 10)
          tk2column(mlb_auto, "add", label = "LR (L1/L2)", width = 10)
          tk2insert.multi(mlb_auto, "end", dt_auto_display)

          # Grid widgets
          tkgrid(mlb_auto, scr_auto)
          tkgrid.configure(scr_auto, rowspan = 26, sticky = "nsw")
        }

        # Define and grid widgets for Y-STR
        tkgrid(tklabel(frame_y, text = "Y-STR", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
        if(length(dt_y_display) == 0){

          # Grid a label for no-data
          tkgrid(tklabel(frame_y, text = "    No data"), padx = 10, pady = 5, sticky = "w")
        }else{

          # Define a scrollbar for a multi-list box (mlb_y)
          scr_y <- tkscrollbar(frame_y, repeatinterval = 5, command = function(...) tkyview(mlb_y, ...))

          # Define a multi-list box (mlb_y)
          mlb_y <- tk2mclistbox(frame_y, width = 75, height = 26, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr_y, ...))
          tk2column(mlb_y, "add", label = "Locus", width = 10)
          tk2column(mlb_y, "add", label = "Victim", width = 10)
          tk2column(mlb_y, "add", label = "Reference", width = 10)
          tk2column(mlb_y, "add", label = "Ignored loci", width = 15)
          tk2column(mlb_y, "add", label = "Mismatched loci", width = 15)
          tk2column(mlb_y, "add", label = "Mutational step", width = 15)
          tk2insert.multi(mlb_y, "end", dt_y_display)

          # Grid widgets
          tkgrid(mlb_y, scr_y)
          tkgrid.configure(scr_y, rowspan = 26, sticky = "nsw")
        }

        # Define and grid widgets for mtDNA
        tkgrid(tklabel(frame_mt, text = "mtDNA", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
        if(length(dt_mt_display) == 0){

          # Grid a label for no-data
          tkgrid(tklabel(frame_mt, text = "    No data"), padx = 10, pady = 5, sticky = "w")
        }else{

          # Grid a label for shared range
          tkgrid(tklabel(frame_mt, text = paste0("    Shared range : ", share_range_display)), padx = 10, pady = 5, sticky = "w")

          # Define a scrollbar for a multi-list box (mlb_mt)
          scr_mt <- tkscrollbar(frame_mt, repeatinterval = 5, command = function(...) tkyview(mlb_mt, ...))

          # Define a multi-list box (mlb_mt)
          mlb_mt <- tk2mclistbox(frame_mt, width = 50, height = 24, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr_mt, ...))
          tk2column(mlb_mt, "add", label = "Victim", width = 10)
          tk2column(mlb_mt, "add", label = "Reference", width = 10)
          tk2column(mlb_mt, "add", label = "Out of shared range", width = 20)
          tk2column(mlb_mt, "add", label = "Mismatch", width = 10)
          tk2insert.multi(mlb_mt, "end", dt_mt_display)

          # Grid widgets
          tkgrid(mlb_mt, scr_mt)
          tkgrid.configure(scr_mt, rowspan = 24, sticky = "nsw")
        }

        # Define and grid widgets for other candidates
        tkgrid(tklabel(frame_other, text = "Other candidates", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
        if(nrow(dt_other_candidate) == 0){

          # Grid a label for no-candidate
          tkgrid(tklabel(frame_other, text = "No candidate"))
        }else{

          # Define a scrollbar for a multi-list box (mlb_other)
          scr_other <- tkscrollbar(frame_other, repeatinterval = 5, command = function(...) tkyview(mlb_other, ...))

          # Define a multi-list box (mlb_mt)
          mlb_other <- tk2mclistbox(frame_other, width = 100, height = 5, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr_other, ...))
          tk2column(mlb_other, "add", label = "Victim", width = 10)
          tk2column(mlb_other, "add", label = "Reference", width = 10)
          tk2column(mlb_other, "add", label = "Assumed relationship", width = 20)
          tk2column(mlb_other, "add", label = "LR", width = 10)
          tk2column(mlb_other, "add", label = "Estimated relationship", width = 20)
          tk2column(mlb_other, "add", label = "Paternal lineage", width = 15)
          tk2column(mlb_other, "add", label = "Maternal lineage", width = 15)
          tk2insert.multi(mlb_other, "end", dt_other_candidate)

          # Grid widgets
          tkgrid(mlb_other, scr_other)
          tkgrid.configure(scr_other, rowspan = 5, sticky = "nsw")
        }

        # Grid frames
        tkgrid(frame_sn, frame_summary, frame_criteria, padx = 10, pady = 5, sticky = "nw")
        tkgrid(frame_auto, frame_y, frame_mt, padx = 10, pady = 5, sticky = "nw")
        tkgrid(frame_other, padx = 10, pady = 5, sticky = "w")
        tkgrid(frame_row1, padx = 10, pady = 5, sticky = "w")
        tkgrid(frame_row2, padx = 10, pady = 5, sticky = "w")
        tkgrid(frame_row3, padx = 10, pady = 5, sticky = "w")
      }
    }

    # Define an environment variable (env_result)
    env_result <- new.env(parent = globalenv())

    # Get the data table for all results
    dt_result <- get("dt_result", pos = env_proj)
    setkey(dt_result, Victim, Reference)

    dt_display <- dt_result[, list(Victim, Reference, AssumedRel, LR_Total, EstimatedRel, Paternal, Maternal)]
    dt_display <- dt_display[EstimatedRel != "" | Paternal == "support" | Maternal == "support"]
    setorder(dt_display, cols = - "LR_Total", na.last = TRUE)
    dt_display$LR_Total <- signif(dt_display$LR_Total, 3)

    # Reset frame_tab2
    tabs <- get("tabs", pos = env_gui)
    tab2 <- get("tab2", pos = env_gui)
    frame_tab2 <- get("frame_tab2", pos = env_gui)
    tkdestroy(frame_tab2)
    frame_tab2 <- tkframe(tab2)

    # Define frames
    frame_result_1 <- tkframe(frame_tab2)
    frame_result_2 <- tkframe(frame_tab2)

    # Define a scrollbar for a multi-list box (mlb_result)
    scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))

    # Define a multi-list box (mlb_result)
    mlb_result <- tk2mclistbox(frame_result_1, width = 100, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(mlb_result, "add", label = "Victim", width = 10)
    tk2column(mlb_result, "add", label = "Reference", width = 10)
    tk2column(mlb_result, "add", label = "Assumed relationship", width = 20)
    tk2column(mlb_result, "add", label = "LR", width = 10)
    tk2column(mlb_result, "add", label = "Estimated relationship", width = 20)
    tk2column(mlb_result, "add", label = "Paternal lineage", width = 15)
    tk2column(mlb_result, "add", label = "Maternal lineage", width = 15)
    tk2insert.multi(mlb_result, "end", dt_display)

    # Define widgets in frame_result_2
    butt_detail <- tkbutton(frame_result_2, text = "    Show detail    ", cursor = "hand2", command = function() show_detail())

    # Grid a scrollbar and a multi-list box
    tkgrid(mlb_result, scr1)
    tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")

    # Grid widgets in frame_result_2
    tkgrid(butt_detail, padx = 10, pady = 5)

    # Grid frames
    tkgrid(frame_result_1, padx = 10, pady = 5)
    tkgrid(frame_result_2)
    tkgrid(frame_tab2)

    # Assign data to the environment variable (env_result)
    assign("mlb_result", mlb_result, envir = env_result)
    assign("scr1", scr1, envir = env_result)
    assign("dt_display", dt_display, envir = env_result)

    # Assign data to the environment variable (env_gui)
    assign("frame_tab2", frame_tab2, envir = env_gui)

    # Select tab2
    tk2notetab.select(tabs, "Results")
  }
}
