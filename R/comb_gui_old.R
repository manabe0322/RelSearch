# The function to make data_comb
make_data_comb <- function(env_proj){

  # Get finish signs
  fin_auto <- get("fin_auto", pos = env_proj)
  fin_y <- get("fin_y", pos = env_proj)
  fin_mt <- get("fin_mt", pos = env_proj)

  # Get sample names from the environment "env_proj"
  sn_q_all <- get("sn_q_all", pos = env_proj)
  sn_r_all <- get("sn_r_all", pos = env_proj)

  # The number of samples in all database
  n_q <- length(sn_q_all)
  n_r <- length(sn_r_all)

  # Define data_comb
  data_comb <- matrix("", n_q * n_r, 10)
  colnames(data_comb) <- c("Query", "Reference", "Assumed relationship", "Estimated relationship", "Paternal lineage", "Maternal lineage", "All H1", "All LR", "Data Y", "Data mt")
  data_comb[, 1] <- as.vector(sapply(sn_q_all, rep, n_r))
  data_comb[, 2] <- rep(sn_r_all, n_q)

  if(fin_auto){
    min_lr_auto <- get("min_lr_auto", pos = env_proj)
    sn_auto_q <- get("sn_auto_q", pos = env_proj)
    sn_auto_r_new <- get("sn_auto_r_new", pos = env_proj)
    rel_auto_r_new <- get("rel_auto_r_new", pos = env_proj)
    lr_all <- get("lr_all", pos = env_proj)
    clr_all_mat <- lr_all[, , length(lr_all[1, 1, ])]
    for(i in 1:n_q){
      pos_q <- which(sn_auto_q == sn_q_all[i])
      if(length(pos_q) == 1){
        for(j in 1:n_r){
          pos_r <- which(sn_auto_r_new == sn_r_all[j])
          if(length(pos_r) > 0){
            rel_1 <- rel_auto_r_new[pos_r]
            clr_1 <- clr_all_mat[pos_q, pos_r]
            pos_meet <- which(as.numeric(clr_1) >= min_lr_auto)
            if(length(rel_1) == 1){
              data_comb[n_r * (i - 1) + j, 3] <- rel_1
              data_comb[n_r * (i - 1) + j, 7] <- rel_1
              data_comb[n_r * (i - 1) + j, 8] <- clr_1
            }else{
              data_comb[n_r * (i - 1) + j, 3] <- "No assumption"
              data_comb[n_r * (i - 1) + j, 7] <- paste(rel_1, collapse = ", ")
              data_comb[n_r * (i - 1) + j, 8] <- paste(clr_1, collapse = ", ")
            }
            if(length(pos_meet) > 1){
              data_comb[n_r * (i - 1) + j, 4] <- "Multiple candidates"
            }else if(length(pos_meet) == 1){
              data_comb[n_r * (i - 1) + j, 4] <- rel_1[pos_meet]
            }else{
              data_comb[n_r * (i - 1) + j, 4] <- "No candidate"
            }
          }else{
            data_comb[n_r * (i - 1) + j, 3] <- "-"
            data_comb[n_r * (i - 1) + j, 4] <- "-"
          }
        }
      }else{
        data_comb[n_r * (i - 1) + j, 3] <- "-"
        data_comb[n_r * (i - 1) + j, 4] <- "-"
      }
    }
  }else{
    data_comb[, 3] <- "-"
    data_comb[, 4] <- "-"
  }

  if(fin_y){

    # Get criteria from the environment "env_proj"
    max_mismatch_y <- get("max_mismatch_y", pos = env_proj)
    max_ignore_y <- get("max_ignore_y", pos = env_proj)
    max_mustep_y <- get("max_mustep_y", pos = env_proj)

    # Get results from the environment "env_proj"
    sn_y_q <- get("sn_y_q", pos = env_proj)
    sn_y_r <- get("sn_y_r", pos = env_proj)
    mismatch_y <- get("mismatch_y", pos = env_proj)
    ignore_y <- get("ignore_y", pos = env_proj)
    mustep_y <- get("mustep_y", pos = env_proj)

    # Get data of all loci
    pos_total <- length(mismatch_y[1, 1, ])
    total_mismatch <- mismatch_y[, , pos_total]
    total_ignore <- ignore_y[, , pos_total]
    total_mustep <- mustep_y[, , pos_total]

    for(i in 1:n_q){
      pos_q <- which(sn_y_q == sn_q_all[i])
      if(length(pos_q) == 1){
        for(j in 1:n_r){
          pos_r <- which(sn_y_r == sn_r_all[j])
          if(length(pos_r) == 1){

            # Extract data in one case
            total_mismatch_y_one <- as.numeric(total_mismatch[pos_q, pos_r])
            total_ignore_y_one <- as.numeric(total_ignore[pos_q, pos_r])
            total_mustep_y_one <- as.numeric(total_mustep[pos_q, pos_r])

            # Save data
            data_comb[n_r * (i - 1) + j, 9] <- paste(c(total_mismatch_y_one, total_ignore_y_one, total_mustep_y_one), collapse = ", ")

            # Whether data is satisfied with criteria or not
            bool_total_mismatch <- total_mismatch_y_one <= max_mismatch_y
            bool_total_ignore <- total_ignore_y_one <= max_ignore_y
            bool_total_mustep <- total_mustep_y_one <= max_mustep_y && total_mustep_y_one %% 1 == 0
            if(all(c(bool_total_mismatch, bool_total_ignore, bool_total_mustep))){
              data_comb[n_r * (i - 1) + j, 5] <- "Support"
            }else{
              data_comb[n_r * (i - 1) + j, 5] <- "Not support"
            }
          }else{
            data_comb[n_r * (i - 1) + j, 5] <- "-"
          }
        }
      }else{
        data_comb[n_r * (i - 1) + j, 5] <- "-"
      }
    }
  }else{
    data_comb[, 5] <- "-"
  }

  if(fin_mt){

    # Get criteria from the environment "env_proj"
    min_share_mt <- get("min_share_mt", envir = env_proj)
    max_mismatch_mt <- get("max_mismatch_mt", envir = env_proj)

    # Get results from the environment "env_proj"
    sn_mt_q <- get("sn_mt_q", envir = env_proj)
    sn_mt_r <- get("sn_mt_r", envir = env_proj)
    share_len_mt <- get("share_len_mt", envir = env_proj)
    mismatch_mt <- get("mismatch_mt", envir = env_proj)

    for(i in 1:n_q){
      pos_q <- which(sn_mt_q == sn_q_all[i])
      if(length(pos_q) == 1){
        for(j in 1:n_r){
          pos_r <- which(sn_mt_r == sn_r_all[j])
          if(length(pos_r) == 1){

            # Extract data in one case
            share_len_mt_one <- as.numeric(share_len_mt[pos_q, pos_r])
            mismatch_mt_one <- as.numeric(mismatch_mt[pos_q, pos_r])

            # Save data
            data_comb[n_r * (i - 1) + j, 10] <- paste(c(share_len_mt_one, mismatch_mt_one), collapse = ", ")

            # Whether data is satisfied with criteria or not
            bool_share <- share_len_mt_one >= min_share_mt
            bool_mismatch <- mismatch_mt_one <= max_mismatch_mt
            if(all(c(bool_share, bool_mismatch))){
              data_comb[n_r * (i - 1) + j, 6] <- "Support"
            }else{
              data_comb[n_r * (i - 1) + j, 6] <- "Not support"
            }
          }else{
            data_comb[n_r * (i - 1) + j, 6] <- "-"
          }
        }
      }else{
        data_comb[n_r * (i - 1) + j, 6] <- "-"
      }
    }
  }else{
    data_comb[, 6] <- "-"
  }

  return(data_comb)
}

make_tab7 <- function(env_proj, env_gui){
  fin_auto <- get("fin_auto", pos = env_proj)
  fin_y <- get("fin_y", pos = env_proj)
  fin_mt <- get("fin_mt", pos = env_proj)

  if(any(fin_auto, fin_y, fin_mt)){

    # The function to show data in detail
    show_detail <- function(){

      # Get the multi-list box for displayed data from the environment "env_comb_result"
      mlb_result <- get("mlb_result", pos = env_comb_result)

      # If the user does not select one line
      if(tclvalue(tkcurselection(mlb_result)) == ""){
        tkmessageBox(message = "Select one result!", icon = "error", type = "ok")

      # If the user selects one line
      }else{

        # Get displayed data from the environment "env_comb_result"
        data_display <- get("data_display", pos = env_comb_result)

        # Extract selected data
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_result))) + 1
        sn_q_select <- data_display[pos_select, "Query"]
        sn_r_select <- data_display[pos_select, "Reference"]
        rel_estimated_select <- data_display[pos_select, "Estimated relationship"]
        if(rel_estimated_select == "-"){
          rel_estimated_select <- "No data"
        }

        # Extract the result of paternal lineage
        paternal_select <- data_display[pos_select, "Paternal lineage"]
        if(paternal_select == "-"){
          paternal_select <- "No data"
          y_satisfy <- "-"
        }else if(paternal_select == "Support"){
          y_satisfy <- "Yes"
        }else{
          y_satisfy <- "No"
        }

        # Extract the result of maternal lineage
        maternal_select <- data_display[pos_select, "Maternal lineage"]
        if(maternal_select == "-"){
          maternal_select <- "No data"
          mt_satisfy <- "-"
        }else if(maternal_select == "Support"){
          mt_satisfy <- "Yes"
        }else{
          mt_satisfy <- "No"
        }

        # Make a top frame
        tf_detail <- tktoplevel()
        tkwm.title(tf_detail, "Combined results in detail")

        # Define frames
        frame_detail_sn <- tkframe(tf_detail)
        frame_detail_summary <- tkframe(tf_detail)
        frame_detail_auto <- tkframe(tf_detail)
        frame_detail_y <- tkframe(tf_detail)
        frame_detail_mt <- tkframe(tf_detail)

        # Define sub frames
        frame_detail_auto_sub <- tkframe(frame_detail_auto)
        frame_detail_y_sub <- tkframe(frame_detail_y)
        frame_detail_mt_sub <- tkframe(frame_detail_mt)

        # Define widgets for sample names
        label_title_sn <- tklabel(frame_detail_sn, text = "Sample name", font = "Helvetica 10 bold")
        label_sn_q_1 <- tklabel(frame_detail_sn, text = "    Query")
        label_sn_q_2 <- tklabel(frame_detail_sn, text = paste0(": ", sn_q_select))
        label_sn_r_1 <- tklabel(frame_detail_sn, text = "    Reference ")
        label_sn_r_2 <- tklabel(frame_detail_sn, text = paste0(": ", sn_r_select))

        # Define widgets for summary
        label_title_summary <- tklabel(frame_detail_summary, text = "Summary", font = "Helvetica 10 bold")
        label_summary_rel_1 <- tklabel(frame_detail_summary, text = "    Estimated relationship")
        label_summary_rel_2 <- tklabel(frame_detail_summary, text = paste0(": ", rel_estimated_select))
        label_summary_pat_1 <- tklabel(frame_detail_summary, text = "    Paternal lineage")
        label_summary_pat_2 <- tklabel(frame_detail_summary, text = paste0(": ", paternal_select))
        label_summary_mat_1 <- tklabel(frame_detail_summary, text = "    Maternal lineage")
        label_summary_mat_2 <- tklabel(frame_detail_summary, text = paste0(": ", maternal_select))

        # Define widgets for the result of autosomal STRs
        label_title_auto <- tklabel(frame_detail_auto, text = "STR", font = "Helvetica 10 bold")

        # No result of the autosomal STRs
        if(data_display[pos_select, 4] == "-"){

          # Define a label that indicates "No data"
          label_result_auto <- tklabel(frame_detail_auto_sub, text = "No data")

          # Grid the label
          tkgrid(label_result_auto, padx = 10, pady = 5)

        # With result of the autosomal STRs
        }else{

          # Extract selected data
          h1_select <- data_display[pos_select, "All H1"]
          h1_select <- strsplit(h1_select, ", ")[[1]]
          lr_select <- data_display[pos_select, "All LR"]
          lr_select <- strsplit(lr_select, ", ")[[1]]
          lr_select <- as.numeric(lr_select)

          # Get objects from the environment "env_proj"
          min_lr_auto <- get("min_lr_auto", pos = env_proj)

          # Define a matrix for displayed LRs
          lr_display <- matrix("", length(h1_select), 4)
          lr_display[, 1] <- h1_select
          lr_display[, 2] <- "Unrelated"
          lr_display[, 3] <- sprintf('%.2e', lr_select)
          lr_display[, 4] <- "No"
          lr_display[which(lr_select >= min_lr_auto), 4] <- "Yes"

          # Define a scrollbar for the multi-list box for LRs
          scr_lr <- tkscrollbar(frame_detail_auto_sub, repeatinterval = 5, command = function(...) tkyview(mlb_lr, ...))

          # Define a multi-list box for LRs
          mlb_lr <- tk2mclistbox(frame_detail_auto_sub, width = 110, height = 5, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr_lr, ...))
          tk2column(mlb_lr, "add", label = "Numerator hypothesis", width = 30)
          tk2column(mlb_lr, "add", label = "Denominator hypothesis", width = 30)
          tk2column(mlb_lr, "add", label = "LR", width = 30)
          tk2column(mlb_lr, "add", label = "Satisfy criteria", width = 20)
          tk2insert.multi(mlb_lr, "end", lr_display)

          # Grid widgets
          tkgrid(mlb_lr, scr_lr)
          tkgrid.configure(scr_lr, rowspan = 5, sticky = "nsw")
        }

        # Define widgets for the result of the Y-STRs
        label_title_y <- tklabel(frame_detail_y, text = "Y-STR", font = "Helvetica 10 bold")

        # No result of the Y-STRs
        if(data_display[pos_select, 5] == "-"){

          # Define a label that indicates "No data"
          label_result_y <- tklabel(frame_detail_y_sub, text = "No data")

          # Grid the label
          tkgrid(label_result_y)

        # With result of the Y-STRs
        }else{

          # Get objects from the environment "env_proj"
          max_mismatch_y <- get("max_mismatch_y", pos = env_proj)
          max_ignore_y <- get("max_ignore_y", pos = env_proj)
          max_mustep_y <- get("max_mustep_y", pos = env_proj)

          # Extract selected data
          y_data_select <- data_display[pos_select, "Data Y"]
          y_data_select <- strsplit(y_data_select, ", ")[[1]]
          y_data_select <- as.numeric(y_data_select)

          # Define a matrix for displayed Y-STR results
          y_display <- matrix("", 1, 4)
          y_display[1, 1] <- y_data_select[1]
          y_display[1, 2] <- y_data_select[2]
          y_display[1, 3] <- y_data_select[3]
          y_display[1, 4] <- y_satisfy

          # Define a multi-list box for Y-STR results
          mlb_y <- tk2mclistbox(frame_detail_y_sub, width = 110, height = 1, resizablecolumns = TRUE, selectmode = "single")
          tk2column(mlb_y, "add", label = "Number of mismatched loci", width = 30)
          tk2column(mlb_y, "add", label = "Number of ignored loci", width = 30)
          tk2column(mlb_y, "add", label = "Maximum mutational step", width = 30)
          tk2column(mlb_y, "add", label = "Satisfy criteria", width = 20)
          tk2insert.multi(mlb_y, "end", y_display)

          # Grid widgets
          tkgrid(mlb_y)
        }

        # Define widgets for the result of the mtDNA
        label_title_mt <- tklabel(frame_detail_mt, text = "mtDNA", font = "Helvetica 10 bold")

        # No result of the mtDNA
        if(data_display[pos_select, 6] == "-"){

          # Define a label that indicates "No data"
          label_result_mt <- tklabel(frame_detail_mt_sub, text = "No data")

          # Grid the label
          tkgrid(label_result_mt)

        # With result of the Y-STRs
        }else{

          # Get objects from the environment "env_proj"
          min_share_mt <- get("min_share_mt", envir = env_proj)
          max_mismatch_mt <- get("max_mismatch_mt", envir = env_proj)

          # Extract selected data
          mt_data_select <- data_display[pos_select, "Data mt"]
          mt_data_select <- strsplit(mt_data_select, ", ")[[1]]
          mt_data_select <- as.numeric(mt_data_select)

          # Define a matrix for displayed mtDNA results
          mt_display <- matrix("", 1, 3)
          mt_display[1, 1] <- mt_data_select[1]
          mt_display[1, 2] <- mt_data_select[2]
          mt_display[1, 3] <- mt_satisfy

          # Define a multi-list box for Y-STR results
          mlb_mt <- tk2mclistbox(frame_detail_mt_sub, width = 110, height = 1, resizablecolumns = TRUE, selectmode = "single")
          tk2column(mlb_mt, "add", label = "Shared length", width = 40)
          tk2column(mlb_mt, "add", label = "Number of mismatch", width = 40)
          tk2column(mlb_mt, "add", label = "Satisfy criteria", width = 30)
          tk2insert.multi(mlb_mt, "end", mt_display)

          # Grid widgets
          tkgrid(mlb_mt)
        }

        # Define widgets for the warning message
        #label_title_warning <- tklabel(frame_detail_, text = "Warning")

        # Grid widgets for sample names
        tkgrid(label_title_sn, padx = 10, pady = 5, sticky = "w")
        tkgrid(label_sn_q_1, label_sn_q_2, padx = 10, pady = 5, sticky = "w")
        tkgrid(label_sn_r_1, label_sn_r_2, padx = 10, pady = 5, sticky = "w")

        # Grid widgets for summary
        tkgrid(label_title_summary, padx = 10, pady = 5, sticky = "w")
        tkgrid(label_summary_rel_1, label_summary_rel_2, padx = 10, pady = 5, sticky = "w")
        tkgrid(label_summary_pat_1, label_summary_pat_2, padx = 10, pady = 5, sticky = "w")
        tkgrid(label_summary_mat_1, label_summary_mat_2, padx = 10, pady = 5, sticky = "w")

        # Grid widgets for the autosomal STR
        tkgrid(label_title_auto, padx = 10, pady = 5, sticky = "w")

        # Grid widgets for the Y-STR
        tkgrid(label_title_y, padx = 10, pady = 5, sticky = "w")

        # Grid widgets for the mtDNA
        tkgrid(label_title_mt, padx = 10, pady = 5, sticky = "w")

        # tkgrid(label_title_warning, padx = 10, pady = 5, sticky = "w")

        # Grid sub frames
        tkgrid(frame_detail_auto_sub, padx = 20, pady = 5, sticky = "w")
        tkgrid(frame_detail_y_sub, padx = 20, pady = 5, sticky = "w")
        tkgrid(frame_detail_mt_sub, padx = 20, pady = 5, sticky = "w")

        # Grid frames
        tkgrid(frame_detail_sn, padx = 10, pady = 5, sticky = "w")
        tkgrid(frame_detail_summary, padx = 10, pady = 5, sticky = "w")
        tkgrid(frame_detail_auto, padx = 10, pady = 5, sticky = "w")
        tkgrid(frame_detail_y, padx = 10, pady = 5, sticky = "w")
        tkgrid(frame_detail_mt, padx = 10, pady = 5, sticky = "w")
      }

    }

    # Define an environment variable (env_comb_result)
    env_comb_result <- new.env(parent = globalenv())

    # Make combined data
    data_comb <- make_data_comb(env_proj)

    # Extract displayed data
    data_display <- data_comb[sort(unique(c(which(is.element(data_comb[, 4], c("-", "No candidate")) == FALSE),
                                            which(is.element(data_comb[, 5], c("-", "Not support")) == FALSE),
                                            which(is.element(data_comb[, 6], c("-", "Not support")) == FALSE)
                                            )
                                          )
                                   )
                              , , drop = FALSE]

    # Reset frame_tab7
    tabs <- get("tabs", pos = env_gui)
    tab7 <- get("tab7", pos = env_gui)
    frame_tab7 <- get("frame_tab7", pos = env_gui)
    tkdestroy(frame_tab7)
    frame_tab7 <- tkframe(tab7)

    # Define frames
    frame_result_1 <- tkframe(frame_tab7)
    frame_result_2 <- tkframe(frame_tab7)

    # Define a scrollbar for a multi-list box (mlb_result)
    scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))

    # Define a multi-list box (mlb_result)
    mlb_result <- tk2mclistbox(frame_result_1, width = 120, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(mlb_result, "add", label = "Query", width = 10)
    tk2column(mlb_result, "add", label = "Reference", width = 10)
    tk2column(mlb_result, "add", label = "Assumed relationship", width = 30)
    tk2column(mlb_result, "add", label = "Estimated relationship", width = 30)
    tk2column(mlb_result, "add", label = "Paternal lineage", width = 20)
    tk2column(mlb_result, "add", label = "Maternal lineage", width = 20)
    tk2insert.multi(mlb_result, "end", data_display[, 1:6])

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
    tkgrid(frame_tab7)

    # Assign data to the environment variable (env_comb_result)
    assign("mlb_result", mlb_result, envir = env_comb_result)
    assign("scr1", scr1, envir = env_comb_result)
    assign("data_display", data_display, envir = env_comb_result)

    # Assign data to the environment variable (env_gui)
    assign("frame_tab7", frame_tab7, envir = env_gui)
  }
}
