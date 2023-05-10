# The function to make data_comb
make_data_comb <- function(env_proj){

  # Get finish signs
  fin_auto <- get("fin_auto", pos = env_proj)
  fin_y <- get("fin_y", pos = env_proj)
  fin_mt <- get("fin_mt", pos = env_proj)

  # Get sample names from the environment "env_proj"
  sn_q_all <- get("sn_q_all", pos = env_proj)
  sn_r_all <- get("sn_r_all", pos = env_proj)

  # The number of samples in each database
  n_q <- length(sn_q_all)
  n_r <- length(sn_r_all)

  # Define data_comb
  data_comb <- matrix("", n_q * n_r, 8)
  colnames(data_comb) <- c("Query", "Reference", "Assumed relationship", "Estimated relationship", "Paternal lineage", "Maternal lineage", "All Hp", "All LR")
  data_comb[, 1] <- rep(sn_q_all, n_r)
  data_comb[, 2] <- as.vector(sapply(sn_r_all, rep, n_q))

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
              data_comb[n_r * (i - 1) + j, 3] <- "N/A"
              data_comb[n_r * (i - 1) + j, 7] <- paste(rel_1, collapse = ", ")
              data_comb[n_r * (i - 1) + j, 8] <- paste(clr_1, collapse = ", ")
            }
            if(length(pos_meet) > 0){
              data_comb[n_r * (i - 1) + j, 4] <- "Multiple candidates"
            }
          }
        }
      }
    }
  }else{
    data_comb[, 3] <- "N/A"
    data_comb[, 4] <- "N/A"
  }

  if(fin_y){
    max_diff <- get("max_diff", pos = env_proj)
    max_ignore_y <- get("max_ignore_y", pos = env_proj)
    max_mustep_y <- get("max_mustep_y", pos = env_proj)
    sn_y_q <- get("sn_y_q", pos = env_proj)
    sn_y_r <- get("sn_y_r", pos = env_proj)
    mismatch_y <- get("mismatch_y", pos = env_proj)
    pos_overall <- length(mismatch_y[1, 1, ])
    n_mm <- mismatch_y[, , pos_overall]
    drop_q_y <- get("drop_q_y", pos = env_proj)
    n_q_drop <- drop_q_y[, , pos_overall]
    mu_step_y <- get("mu_step_y", pos = env_proj)
    max_mu_step <- mu_step_y[, , pos_overall]
    for(i in 1:n_q){
      pos_q <- which(sn_y_q == sn_q_all[i])
      if(length(pos_q) == 1){
        for(j in 1:n_r){
          pos_r <- which(sn_y_r == sn_r_all[j])
          if(length(pos_r) == 1){
            bool_n_mm <- as.numeric(n_mm[pos_q, pos_r]) <= max_diff
            bool_n_q_drop <- as.numeric(n_q_drop[pos_q, pos_r]) <= max_ignore_y
            bool_max_mu_step <- as.numeric(max_mu_step[pos_q, pos_r]) <= max_mustep_y && max_mu_step[pos_q, pos_r] %% 1 == 0
            if(all(c(bool_n_mm, bool_n_q_drop, bool_max_mu_step))){
              data_comb[n_r * (i - 1) + j, 5] <- "\U2713"
            }else{
              data_comb[n_r * (i - 1) + j, 5] <- ""
            }
          }
        }
      }
    }
  }else{
    data_comb[, 5] <- "No data"
  }

  if(fin_mt){
    min_share_mt <- get("min_share_mt", envir = env_proj)
    max_diff_mt <- get("max_diff_mt", envir = env_proj)
    sn_mt_q <- get("sn_mt_q", envir = env_proj)
    sn_mt_r <- get("sn_mt_r", envir = env_proj)
    mismatch_mt <- get("mismatch_mt", envir = env_proj)
    share_len_mt <- get("share_len_mt", envir = env_proj)
    for(i in 1:n_q){
      pos_q <- which(sn_mt_q == sn_q_all[i])
      if(length(pos_q) == 1){
        for(j in 1:n_r){
          pos_r <- which(sn_mt_r == sn_r_all[j])
          if(length(pos_r) == 1){
            bool_n_mm <- as.numeric(mismatch_mt[pos_q, pos_r]) <= max_diff_mt
            bool_share <- as.numeric(share_len_mt[pos_q, pos_r]) >= min_share_mt
            if(all(c(bool_n_mm, bool_share))){
              data_comb[n_r * (i - 1) + j, 6] <- "\U2713"
            }else{
              data_comb[n_r * (i - 1) + j, 6] <- ""
            }
          }
        }
      }
    }
  }else{
    data_comb[, 6] <- "No data"
  }

  return(data_comb)
}

make_tab7 <- function(env_proj, env_gui){
  fin_auto <- get("fin_auto", pos = env_proj)
  fin_y <- get("fin_y", pos = env_proj)
  fin_mt <- get("fin_mt", pos = env_proj)

  if(any(fin_auto, fin_y, fin_mt)){
    show_detail <- function(){
      mlb_result <- get("mlb_result", pos = env_comb_result)
      if(tclvalue(tkcurselection(mlb_result)) == ""){
        tkmessageBox(message = "Select one result!", icon = "error", type = "ok")
      }else{
        data_display <- get("data_display", pos = env_comb_result)
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_result))) + 1
        sn_q_select <- data_display[pos_select, 1]
        sn_r_select <- data_display[pos_select, 2]

        # Extract the estimated relationship
        rel_estimated_select <- data_display[pos_select, 4]

        # Extract the result of Y-STRs
        paternal_lineage_select <- data_display[pos_select, 5]
        if(paternal_lineage_select == "\U2713"){
          paternal_lineage_select <- "Paternal lineage"
        }else if(paternal_lineage_select == ""){
          paternal_lineage_select <- "Not paternal lineage"
        }

        # Make a top frame
        tf_detail <- tktoplevel()
        tkwm.title(tf_detail, "Combined results in detail")

        # Define frames
        frame_detail_1 <- tkframe(tf_detail)
        frame_detail_1_1 <- tkframe(frame_detail_1)
        frame_detail_2 <- tkframe(tf_detail)

        # Define widgets for sample names
        label_title_sn <- tklabel(frame_detail_1, text = "Sample name")
        label_sn_q <- tklabel(frame_detail_1, text = paste0("    Query     : ", sn_q_select))
        label_sn_r <- tklabel(frame_detail_1, text = paste0("    Reference : ", sn_r_select))

        # Define widgets for the estimated relationship
        label_title_rel <- tklabel(frame_detail_1, text = "Estimated relationship")
        label_result_rel <- tklabel(frame_detail_1, text = rel_estimated_select)

        # Define widgets for the result of autosomal STRs
        label_title_auto <- tklabel(frame_detail_1, text = "STR")
        label_assumed_rel <- tklabel(frame_detail_1, text = "")

        # Define widgets for the result of Y-STRs
        label_title_y <- tklabel(frame_detail_1, text = "Y-STR")
        label_result_y <- tklabel(frame_detail_1, text = "")

        # Define widgets for the result of mtDNA
        label_title_mt <- tklabel(frame_detail_1, text = "mtDNA")

        # Define widgets for the warning message
        label_title_warning <- tklabel(frame_detail_1, text = "Warning")
      }

    }

    # Define an environment variable (env_comb_result)
    env_comb_result <- new.env(parent = globalenv())

    # Make combined data
    data_comb <- make_data_comb(env_proj)

    # Extract displayed data
    data_display <- data_comb[sort(unique(c(which(data_comb[, 4] != ""), which(data_comb[, 5] != ""), which(data_comb[, 6] != "")))), , drop = FALSE]

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
