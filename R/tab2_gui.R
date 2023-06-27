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

          # Victim profile
          prof_v_select <- as.numeric(data_v_auto_select[, -"SampleName", with = FALSE])
          prof_v_display <- c(display_gt(prof_v_select), "")

          # Reference profile
          prof_r_select <- as.numeric(data_r_auto_select[, -"SampleName", with = FALSE])
          prof_r_display <- c(display_gt(prof_r_select), "")

          # Locus
          locus_display <- gsub("LikeH1_", "", cn_result[grep("LikeH1_", cn_result)])

          # Likelihood of H1
          like_h1_display <- as.numeric(result_selected[, grep("LikeH1_", cn_result), with = FALSE])

          # Likelihood of H2
          like_h2_display <- as.numeric(result_selected[, grep("LikeH2_", cn_result), with = FALSE])

          # LR
          lr_display <- as.numeric(result_selected[, grep("LR_", cn_result), with = FALSE])

          # Create data table
          dt_auto_display <- data.table(Locus = locus_display, Profile_V = prof_v_display, Profile_R = prof_r_display, LikeH1 = like_h1_display, LikeH2 = like_h2_display, LR = lr_display)

        }else{
          dt_auto_display <- NULL
        }
      }else{
        dt_auto_display <- NULL
      }
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

          # Reference profile
          prof_r_display <- c(as.character(data_r_y_select[, -"SampleName", with = FALSE]), "")

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
          dt_y_display <- data.table(Locus = locus_y_display, Profile_V = prof_v_display, Profile_R = prof_r_display, Mismatch = mismatch_y_display, Ignore = ignore_y_display, MuStep = mustep_y_display)

        }else{
          dt_y_display <- NULL
        }
      }else{
        dt_y_display <- NULL
      }
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
          pos_common <- is.element(extract_integer(type_vr), pos_mt_vr)

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

        # Create data for autosomal STR
        dt_auto_display <- create_display_auto(sn_v_select, sn_r_select)

        # Create data for Y-STR
        dt_y_display <- create_display_y(sn_v_select, sn_r_select)

        # Create data for mtDNA
        dt_mt_display <- create_display_mt(sn_v_select, sn_r_select)

        # Make a top frame
        tf_detail <- tktoplevel()
        tkwm.title(tf_detail, "Results in detail")
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
    mlb_result <- tk2mclistbox(frame_result_1, width = 140, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(mlb_result, "add", label = "Victim", width = 10)
    tk2column(mlb_result, "add", label = "Reference", width = 10)
    tk2column(mlb_result, "add", label = "Assumed relationship", width = 30)
    tk2column(mlb_result, "add", label = "LR", width = 20)
    tk2column(mlb_result, "add", label = "Estimated relationship", width = 30)
    tk2column(mlb_result, "add", label = "Paternal lineage", width = 20)
    tk2column(mlb_result, "add", label = "Maternal lineage", width = 20)
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
