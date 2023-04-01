make_comb_data <- function(env_proj){
  fin_auto <- get("fin_auto", pos = env_proj)
  fin_y <- get("fin_y", pos = env_proj)
  fin_mt <- get("fin_mt", pos = env_proj)

  if(any(fin_auto, fin_y, fin_mt)){
    # Get sample names from the environment variable "env_proj"
    sn_q_all <- get("sn_q_all", pos = env_proj)
    sn_r_all <- get("sn_r_all", pos = env_proj)

    n_q <- length(sn_q_all)
    n_r <- length(sn_r_all)

    # Define comb_data
    comb_data <- matrix("", n_q * n_r, 3)
    colnames(comb_data) <- c("Query", "Reference", "Estimated relationship")
    comb_data[, 1] <- rep(sn_q_all, n_r)
    comb_data[, 2] <- as.vector(sapply(sn_r_all, rep, n_q))

    alleged_rels <- rep("", n_q * n_r)
    if(fin_auto){

      sn_auto_q <- get("sn_auto_q", pos = env_proj)
      sn_auto_r_new <- get("sn_auto_r_new", pos = env_proj)
      rel_auto_r_new <- get("rel_auto_r_new", pos = env_proj)
      lr_all <- get("lr_all", pos = env_proj)
      clr_all_mat <- lr_all[, , n_l + 1]
      for(i in 1:n_q){
        pos_q <- which(sn_auto_q == sn_q_all[i])
        if(length(pos_q) > 0){
          for(j in 1:n_r){
            pos_r <- which(sn_auto_r_new == sn_r_all[j])
            if(length(pos_r) > 0){
              rel_1 <- rel_auto_r_new[pos_r]
              clr_1 <- clr_all_mat[pos_q, pos_r]
            }
          }
        }
      }
    }

    if(fin_y){

    }

    if(fin_mt){

    }


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

    data_display <- matrix("", 3, 7)
    data_display[1, ] <- c("\U1F7E2", "Q1", "R1", "10000", "Pass", "Pass", "Parent-child")
    data_display[2, ] <- c("\U1F7E3", "Q1", "R1", "10000", "Pass", "Pass", "Parent-child")
    data_display[3, ] <- c("\U1F7E4", "Q1", "R1", "10000", "Pass", "Pass", "Parent-child")

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
    tk2column(mlb_result, "add", label = "LR (STR)", width = 10)
    tk2column(mlb_result, "add", label = "Criteria for Y-STR", width = 20)
    tk2column(mlb_result, "add", label = "Criteria for mtDNA", width = 20)
    tk2column(mlb_result, "add", label = "Estimated relationship", width = 20)
    tk2insert.multi(mlb_result, "end", data_display)

    # Grid widgets
    tkgrid(mlb_result, scr1)
    tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")
    tkgrid(frame_result_1, padx = 10, pady = 5)
    tkgrid(frame_result_2)
    tkgrid(frame_tab7)
  }
}
