make_comb_data <- function(env_proj){
  fin_auto <- get("fin_auto", pos = env_proj)
  fin_y <- get("fin_y", pos = env_proj)
  fin_mt <- get("fin_mt", pos = env_proj)

  if(any(fin_auto, fin_y, fin_mt)){
    # Get sample names from the environment variable "env_proj"
    sn_q_all <- get("sn_q_all", pos = env_proj)
    sn_r_all <- get("sn_r_all", pos = env_proj)

    # The number of samples in all database
    n_q <- length(sn_q_all)
    n_r <- length(sn_r_all)

    # Define comb_data
    comb_data <- matrix("", n_q * n_r, 3)
    colnames(comb_data) <- c("Query", "Reference", "Estimated relationship")
    comb_data[, 1] <- rep(sn_q_all, n_r)
    comb_data[, 2] <- as.vector(sapply(sn_r_all, rep, n_q))

    alleged_rels <- rep("", n_q * n_r)
    if(fin_auto){
      t_min_lr_auto <- get("t_min_lr_auto", pos = env_proj)
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
              pos_meet <- which(as.numeric(clr_1) >= t_min_lr_auto)
              if(length(pos_meet) > 0){
                alleged_rels[n_r * (i - 1) + j] <- paste0(rel_1[pos_meet], collapse = ", ")
              }else{
                alleged_rels[n_r * (i - 1) + j] <- paste0("LR < ", t_min_lr_auto)
              }
            }
          }
        }
      }
    }

    if(fin_y){
      t_max_diff <- get("t_max_diff", pos = env_proj)
      t_max_drop_y <- get("t_max_drop_y", pos = env_proj)
      t_max_mustep_y <- get("t_max_mustep_y", pos = env_proj)
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
              bool_n_mm <- as.numeric(n_mm[pos_q, pos_r]) <= t_max_diff
              bool_n_q_drop <- as.numeric(n_q_drop[pos_q, pos_r]) <= t_max_drop_y
              bool_max_mu_step <- as.numeric(max_mu_step[pos_q, pos_r]) <= t_max_mustep_y && max_mu_step[pos_q, pos_r] %% 1 == 0
              if(all(c(bool_n_mm, bool_n_q_drop, bool_max_mu_step))){
                alleged_rels[n_r * (i - 1) + j] <- paste0(alleged_rels[n_r * (i - 1) + j], ", Paternal lineage")
              }
            }
          }
        }
      }
    }

    if(fin_mt){
      t_min_share_mt <- get("t_min_share_mt", envir = env_proj)
      t_max_diff_mt <- get("t_max_diff_mt", envir = env_proj)
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
              bool_n_mm <- as.numeric(mismatch_mt[pos_q, pos_r]) <= t_max_diff_mt
              bool_share <- as.numeric(share_len_mt[pos_q, pos_r]) >= t_min_share_mt
              if(all(c(bool_n_mm, bool_share))){
                alleged_rels[n_r * (i - 1) + j] <- paste0(alleged_rels[n_r * (i - 1) + j], ", Maternal lineage", collapse = ", ")
              }
            }
          }
        }
      }
    }
    comb_data[, 3] <- alleged_rels
  }else{
    comb_data <- NULL
  }
  return(comb_data)
}

make_tab7 <- function(env_proj, env_gui){
  fin_auto <- get("fin_auto", pos = env_proj)
  fin_y <- get("fin_y", pos = env_proj)
  fin_mt <- get("fin_mt", pos = env_proj)

  if(any(fin_auto, fin_y, fin_mt)){
    # Define an environment variable (env_comb_result)
    env_comb_result <- new.env(parent = globalenv())

    # Make combined data
    comb_data <- make_comb_data(env_proj)

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
    mlb_result <- tk2mclistbox(frame_result_1, width = 90, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(mlb_result, "add", label = "Query", width = 10)
    tk2column(mlb_result, "add", label = "Reference", width = 10)
    tk2column(mlb_result, "add", label = "Estimated relationship", width = 70)
    tk2insert.multi(mlb_result, "end", comb_data)

    # Grid widgets
    tkgrid(mlb_result, scr1)
    tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")
    tkgrid(frame_result_1, padx = 10, pady = 5)
    tkgrid(frame_result_2)
    tkgrid(frame_tab7)

    assign("frame_tab7", frame_tab7, envir = env_gui)
  }
}
