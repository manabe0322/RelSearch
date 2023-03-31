make_tab3 <- function(env_proj, env_gui){
  open_file <- function(type){
    sign_input <- "ok"
    fin_y <- get("fin_y", pos = env_proj)
    if(fin_y){
      sign_input <- tclvalue(tkmessageBox(message = "Y-STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }

    if(sign_input == "ok"){
      set_env_proj_y(env_proj, FALSE)
      make_tab4(env_proj, env_gui)

      if(type == "query"){
        fp <- get("fp_y_q", pos = env_proj)
        fn <- get("fn_y_q", pos = env_proj)
      }else if(type == "ref"){
        fp <- get("fp_y_r", pos = env_proj)
        fn <- get("fn_y_r", pos = env_proj)
      }
      fp_var <- tclVar(fp)
      fn_var <- tclVar(fn)

      path_file <- tclvalue(tkgetOpenFile(initialdir = tclvalue(fp_var), multiple = "true", filetypes = "{{CSV Files} {.csv}}"))
      if(!nchar(path_file)){
        tkmessageBox(message = "No file was selected!", icon = "error", type = "ok")
      }else{
        tmp <- sub("\\}", path_file, replacement = "")
        tmp2 <- sub("\\{", tmp, replacement = "")
        tclvalue(fp_var) <- tmp2
        foo3 <- strsplit(tmp2, "/")[[1]]
        tclvalue(fn_var) <- strsplit(foo3[length(foo3)], "\\.csv")[[1]][1]
        if(type == "query"){
          tkconfigure(label_q_name, textvariable = fn_var)
          data_y_q <- read.csv(tclvalue(fp_var), header = TRUE)
          data_y_q <- as.matrix(data_y_q)
          data_y_q[is.na(data_y_q)] <- ""
          assign("data_y_q", data_y_q, envir = env_proj)
          assign("fp_y_q", tclvalue(fp_var), envir = env_proj)
          assign("fn_y_q", tclvalue(fn_var), envir = env_proj)
        }else if(type == "ref"){
          tkconfigure(label_r_name, textvariable = fn_var)
          data_y_r <- read.csv(tclvalue(fp_var), header = TRUE)
          data_y_r <- as.matrix(data_y_r)
          data_y_r[is.na(data_y_r)] <- ""
          assign("data_y_r", data_y_r, envir = env_proj)
          assign("fp_y_r", tclvalue(fp_var), envir = env_proj)
          assign("fn_y_r", tclvalue(fn_var), envir = env_proj)
        }
      }
    }
  }

  # Define tclvalue
  fn_y_q <- get("fn_y_q", pos = env_proj)
  fn_y_q_var <- tclVar(fn_y_q)
  fn_y_r <- get("fn_y_r", pos = env_proj)
  fn_y_r_var <- tclVar(fn_y_r)

  tab3 <- get("tab3", pos = env_gui)
  frame_tab3 <- get("frame_tab3", pos = env_gui)
  tkdestroy(frame_tab3)
  frame_tab3 <- tkframe(tab3)
  assign("frame_tab3", frame_tab3, envir = env_gui)

  # Define frames
  frame_3_1 <- tkframe(frame_tab3, relief = "groove", borderwidth = 2)
  frame_3_2 <- tkframe(frame_tab3)

  # Define widgets in frame_3_1
  label_q_title <- tklabel(frame_3_1, text = "Query database")
  label_q_name <- tklabel(frame_3_1, textvariable = fn_y_q_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  butt_q <- tkbutton(frame_3_1, text = "    Load    ", cursor = "hand2", command = function() open_file("query"))
  label_r_title <- tklabel(frame_3_1, text = "Reference database")
  label_r_name <- tklabel(frame_3_1, textvariable = fn_y_r_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  butt_r <- tkbutton(frame_3_1, text = "    Load    ", cursor = "hand2", command = function() open_file("ref"))

  # Define widgets in frame_3_2
  butt_search <- tkbutton(frame_3_2, text = "    Screening    ", cursor = "hand2", command = function() search_y(env_proj, env_gui))

  # Grid widgets
  tkgrid(tklabel(frame_3_1, text = "Input files", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
  tkgrid(label_q_title, label_q_name, butt_q, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_r_title, label_r_name, butt_r, padx = 10, pady = 5, sticky = "w")
  tkgrid(butt_search, pady = 10)
  tkgrid(frame_3_1, padx = 10, pady = 5, sticky = "w")
  tkgrid(frame_3_2, padx = 10, pady = 5)
  tkgrid(frame_tab3)
}

search_y <- function(env_proj, env_gui){
  data_y_q <- get("data_y_q", pos = env_proj)
  data_y_r <- get("data_y_r", pos = env_proj)
  if(any(c(length(data_y_q) == 0, length(data_y_r) == 0))){
    tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
  }else{
    pos_sn_q <- intersect(grep("Sample", colnames(data_y_q)), grep("Name", colnames(data_y_q)))
    sn_y_q <- data_y_q[, pos_sn_q]
    hap_y_q <- data_y_q[, -pos_sn_q, drop = FALSE]
    locus_q <- colnames(hap_y_q)

    pos_sn_r <- intersect(grep("Sample", colnames(data_y_r)), grep("Name", colnames(data_y_r)))
    sn_y_r <- data_y_r[, pos_sn_r]
    hap_y_r <- data_y_r[, -pos_sn_r, drop = FALSE]
    locus_r <- colnames(hap_y_r)

    bool_locus_1 <- all(mapply(setequal, locus_q, locus_r))
    if(bool_locus_1){
      pb <- tkProgressBar("Searching", "0% done", 0, 100, 0)
      hap_y_r <- hap_y_r[, match(locus_q, locus_r)]
      n_q <- nrow(hap_y_q)
      n_r <- nrow(hap_y_r)
      n_l <- length(locus_q)
      mismatch_y <- drop_q_y <- mu_step_y <- array(0, dim = c(n_q, n_r, n_l + 1))
      for(i in 1:n_r){
        ref <- hap_y_r[i, ]
        for(j in 1:n_q){
          query <- hap_y_q[j, ]
          tmp <- match_y(query, ref)
          mismatch_y[j, i, ] <- tmp[[1]]
          drop_q_y[j, i, ] <- tmp[[2]]
          mu_step_y[j, i, ] <- tmp[[3]]
          info <- sprintf("%d%% done", round((n_q * (i - 1) + j) * 100 / (n_q * n_r)))
          setTkProgressBar(pb, (n_q * (i - 1) + j) * 100 / (n_q * n_r), sprintf("Searching"), info)
        }
      }
      assign("hap_y_q", hap_y_q, envir = env_proj)
      assign("hap_y_r", hap_y_r, envir = env_proj)
      assign("sn_y_q", sn_y_q, envir = env_proj)
      assign("sn_y_r", sn_y_r, envir = env_proj)
      assign("mismatch_y", mismatch_y, envir = env_proj)
      assign("drop_q_y", drop_q_y, envir = env_proj)
      assign("mu_step_y", mu_step_y, envir = env_proj)
      assign("fin_y", TRUE, envir = env_proj)
      make_tab4(env_proj, env_gui)
      close(pb)
    }else{
      tkmessageBox(message = "Locus set is not the same between query data and reference data!", icon = "error", type = "ok")
    }
  }
}

make_tab4 <- function(env_proj, env_gui){
  fin_y <- get("fin_y", pos = env_proj)
  if(fin_y){
    set_display_1 <- function(){
      # Define tclvalue
      cand_q <- c("All", sn_y_q)
      select_q_var <- tclVar("All")
      cand_r <- c("All", sn_y_r)
      select_r_var <- tclVar("All")
      select_n_mm_var <- tclVar(1)
      select_n_q_drop_var <- tclVar(1)
      select_max_mu_step_var <- tclVar(2)

      # Make a top frame
      tf <- tktoplevel()
      tkwm.title(tf, "Set display")

      # Define frames
      frame_display_1 <- tkframe(tf)
      frame_display_2 <- tkframe(tf)

      # Define widgets in frame_display_1
      label_title_1 <- tklabel(frame_display_1, text = "Query")
      label_title_2 <- tklabel(frame_display_1, text = "Reference")
      label_title_3 <- tklabel(frame_display_1, text = "Maximum number of inconsistent loci")
      label_title_4 <- tklabel(frame_display_1, text = "Maximum number of ignored loci")
      label_title_5 <- tklabel(frame_display_1, text = "Maximum mutational step")
      combo_q <- ttkcombobox(frame_display_1, values = cand_q, textvariable = select_q_var, state = "readonly")
      combo_r <- ttkcombobox(frame_display_1, values = cand_r, textvariable = select_r_var, state = "readonly")
      entry_n_mm <- tkentry(frame_display_1, textvariable = select_n_mm_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      entry_n_q_drop <- tkentry(frame_display_1, textvariable = select_n_q_drop_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      entry_max_mu_step <- tkentry(frame_display_1, textvariable = select_max_mu_step_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      # Define widgets in frame_display_2
      butt_set <- tkbutton(frame_display_2, text = "    Set    ", cursor = "hand2",
                           command = function() set_display_2(tf, tclvalue(select_q_var), tclvalue(select_r_var),
                                                              as.numeric(tclvalue(select_n_mm_var)), as.numeric(tclvalue(select_n_q_drop_var)), as.numeric(tclvalue(select_max_mu_step_var))))

      # Grid widgets
      tkgrid(label_title_1, label_title_2, label_title_3, label_title_4, label_title_5, padx = 10, pady = 5)
      tkgrid(combo_q, combo_r, entry_n_mm, entry_n_q_drop, entry_max_mu_step, padx = 10, pady = 5)
      tkgrid(butt_set, padx = 10, pady = 5)
      tkgrid(frame_display_1)
      tkgrid(frame_display_2)
    }

    set_display_2 <- function(tf, select_q, select_r, select_n_mm, select_n_q_drop, select_max_mu_step){
      select_mat <- matrix(TRUE, n_data, 5)
      if(select_q != "All"){
        select_mat[, 1] <- sn_y_q_display == select_q
      }
      if(select_r != "All"){
        select_mat[, 2] <- sn_y_r_display == select_r
      }
      select_mat[, 3] <- n_mm_vec <= select_n_mm
      select_mat[, 4] <- n_q_drop_vec <= select_n_q_drop
      select_mat[, 5] <- max_mu_step_vec <= select_max_mu_step
      pos_extract <- which(apply(select_mat, 1, all) == TRUE)

      if(length(pos_extract) != 0){
        mlb_result <- get("mlb_result", pos = env_y_result)
        tkdestroy(mlb_result)
        scr1 <- get("scr1", pos = env_y_result)
        tkdestroy(scr1)
        scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))
        mlb_result <- tk2mclistbox(frame_result_1, width = 120, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(mlb_result, "add", label = "Query", width = 15)
        tk2column(mlb_result, "add", label = "Reference", width = 15)
        tk2column(mlb_result, "add", label = "Number of inconsistent loci", width = 30)
        tk2column(mlb_result, "add", label = "Number of ignored loci", width = 30)
        tk2column(mlb_result, "add", label = "Maximum mutational step", width = 30)
        tkgrid(mlb_result, scr1)
        data_display <- default_display[pos_extract, , drop = FALSE]
        data_display <- data_display[order(as.numeric(data_display[, 5])), , drop = FALSE]
        data_display <- data_display[order(as.numeric(data_display[, 4])), , drop = FALSE]
        data_display <- data_display[order(as.numeric(data_display[, 3])), , drop = FALSE]
        data_display[which(as.numeric(data_display[, 5]) == 99), 5] <- "Not integer"
        tk2insert.multi(mlb_result, "end", data_display)
        assign("mlb_result", mlb_result, envir = env_y_result)
        assign("scr1", scr1, envir = env_y_result)
        assign("data_display", data_display, envir = env_y_result)
        tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")
        tkdestroy(tf)
      }else{
        tkmessageBox(message = "There is no data that meet the condition!", icon = "error", type = "ok")
      }
    }

    show_detail <- function(){
      mlb_result <- get("mlb_result", pos = env_y_result)
      if(tclvalue(tkcurselection(mlb_result)) == ""){
        tkmessageBox(message = "Select one line!", icon = "error", type = "ok")
      }else{
        data_display <- get("data_display", pos = env_y_result)
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_result))) + 1
        select_q_name <- data_display[pos_select, 1]
        pos_select_q <- which(sn_y_q == select_q_name)
        select_r_name <- data_display[pos_select, 2]
        pos_select_r <- which(sn_y_r == select_r_name)
        data_detail <- matrix("", n_l + 1, 6)
        data_detail[, 1] <- c(colnames(hap_y_q), "overall")
        colnames(data_detail) <- c("Locus",
                                  paste0("Query haplotype (", select_q_name, ")"),
                                  paste0("Reference haplotype (", select_r_name, ")"),
                                  "Number of inconsistent loci",
                                  "Number of ignored loci",
                                  "Mutational step")
        data_detail[1:n_l, 2] <- hap_y_q[pos_select_q, ]
        data_detail[1:n_l, 3] <- hap_y_r[pos_select_r, ]
        mismatch_y_ext <- mismatch_y[pos_select_q, pos_select_r, ]
        mismatch_y_ext2 <- mismatch_y_ext[1:n_l]
        mismatch_y_ext2[which(mismatch_y_ext2 == 0)] <- ""
        data_detail[, 4] <- c(mismatch_y_ext2, mismatch_y_ext[n_l + 1])
        drop_q_y_ext <- drop_q_y[pos_select_q, pos_select_r, ]
        drop_q_y_ext2 <- drop_q_y_ext[1:n_l]
        drop_q_y_ext2[which(drop_q_y_ext2 == 0)] <- ""
        data_detail[, 5] <- c(drop_q_y_ext2, drop_q_y_ext[n_l + 1])
        mu_step_y_ext <- mu_step_y[pos_select_q, pos_select_r, ]
        mu_step_y_ext[which(mu_step_y_ext == 0)] <- ""
        mu_step_y_ext[which(mu_step_y_ext == 99)] <- "Not integer"
        data_detail[, 6] <- mu_step_y_ext

        # Make a top frame
        tf_detail <- tktoplevel()
        tkwm.title(tf_detail, "Y result in detail")

        # Define frames
        frame_detail_1 <- tkframe(tf_detail)
        frame_detail_2 <- tkframe(tf_detail)

        # Define a scrollbar for a multi-list box (mlb_detail)
        scr2 <- tkscrollbar(frame_detail_1, repeatinterval = 5, command = function(...) tkyview(mlb_detail, ...))

        # Define a multi-list box (mlb_detail)
        mlb_detail <- tk2mclistbox(frame_detail_1, width = 160, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr2, ...))
        tk2column(mlb_detail, "add", label = "locus", width = 20)
        tk2column(mlb_detail, "add", label = paste0("Query haplotype (", select_q_name, ")"), width = 30)
        tk2column(mlb_detail, "add", label = paste0("Reference haplotype (", select_r_name, ")"), width = 30)
        tk2column(mlb_detail, "add", label = "Number of inconsistent loci", width = 30)
        tk2column(mlb_detail, "add", label = "Number of ignored loci", width = 30)
        tk2column(mlb_detail, "add", label = "Mutational step", width = 20)
        tk2insert.multi(mlb_detail, "end", data_detail)

        # Define widgets in frame_detail_2
        butt_export <- tkbutton(frame_detail_2, text = "    Export    ", cursor = "hand2", command = function() export_data(data_detail, FALSE))

        # Grid widgets
        tkgrid(mlb_detail, scr2)
        tkgrid.configure(scr2, rowspan = 20, sticky = "nsw")
        tkgrid(butt_export)
        tkgrid(frame_detail_1, padx = 10, pady = 5)
        tkgrid(frame_detail_2, padx = 10, pady = 5)
      }
    }

    env_y_result <- new.env(parent = globalenv())

    hap_y_q <- get("hap_y_q", pos = env_proj)
    hap_y_r <- get("hap_y_r", pos = env_proj)
    sn_y_q <- get("sn_y_q", pos = env_proj)
    sn_y_r <- get("sn_y_r", pos = env_proj)
    mismatch_y <- get("mismatch_y", pos = env_proj)
    drop_q_y <- get("drop_q_y", pos = env_proj)
    mu_step_y <- get("mu_step_y", pos = env_proj)

    n_l <- ncol(hap_y_q)
    n_q <- length(sn_y_q)
    n_r <- length(sn_y_r)

    n_mm <- mismatch_y[, , n_l + 1]
    rownames(n_mm) <- sn_y_q
    colnames(n_mm) <- sn_y_r
    n_q_drop <- drop_q_y[, , n_l + 1]
    rownames(n_q_drop) <- sn_y_q
    colnames(n_q_drop) <- sn_y_r
    max_mu_step <- mu_step_y[, , n_l + 1]
    rownames(max_mu_step) <- sn_y_q
    colnames(max_mu_step) <- sn_y_r

    n_data <- n_q * n_r
    default_display <- matrix(0, n_data, 5)
    colnames(default_display) <- c("Query", "Reference", "Number of inconsistent loci", "Number of ignored loci", "Maximum mutational step")
    sn_y_q_display <- rep(sn_y_q, n_r)
    default_display[, 1] <- sn_y_q_display
    sn_y_r_display <- as.vector(sapply(sn_y_r, rep, n_q))
    default_display[, 2] <- sn_y_r_display
    n_mm_vec <- as.vector(n_mm)
    default_display[, 3] <- n_mm_vec
    n_q_drop_vec <- as.vector(n_q_drop)
    default_display[, 4] <- n_q_drop_vec
    max_mu_step_vec <- as.vector(max_mu_step)
    default_display[, 5] <- max_mu_step_vec
    data_display <- default_display[which(as.numeric(default_display[, 3]) <= 1), , drop = FALSE]
    data_display <- data_display[which(as.numeric(data_display[, 4]) <= 1), , drop = FALSE]
    data_display <- data_display[which(as.numeric(data_display[, 5]) <= 2), , drop = FALSE]
    data_display <- data_display[order(as.numeric(data_display[, 4])), , drop = FALSE]
    data_display <- data_display[order(as.numeric(data_display[, 3])), , drop = FALSE]
    assign("data_display", data_display, envir = env_y_result)

    tabs <- get("tabs", pos = env_gui)
    tab4 <- get("tab4", pos = env_gui)
    frame_tab4 <- get("frame_tab4", pos = env_gui)
    tkdestroy(frame_tab4)
    frame_tab4 <- tkframe(tab4)

    # Define frames
    frame_result_1 <- tkframe(frame_tab4)
    frame_result_2 <- tkframe(frame_tab4)

    # Define a scrollbar for a multi-list box (mlb_result)
    scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))

    # Define a multi-list box (mlb_result)
    mlb_result <- tk2mclistbox(frame_result_1, width = 120, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(mlb_result, "add", label = "Query", width = 15)
    tk2column(mlb_result, "add", label = "Reference", width = 15)
    tk2column(mlb_result, "add", label = "Number of inconsistent loci", width = 30)
    tk2column(mlb_result, "add", label = "Number of ignored loci", width = 30)
    tk2column(mlb_result, "add", label = "Maximum mutational step", width = 30)
    tk2insert.multi(mlb_result, "end", data_display)

    # Define widgets in frame_result_2
    butt_display <- tkbutton(frame_result_2, text = "    Set display    ", cursor = "hand2", command = function() set_display_1())
    butt_detail <- tkbutton(frame_result_2, text = "    Show detail    ", cursor = "hand2", command = function() show_detail())
    butt_export <- tkbutton(frame_result_2, text = "    Export displayed data    ", cursor = "hand2", command = function() export_data(get("data_display", pos = env_y_result), FALSE))

    # Grid widgets
    tkgrid(mlb_result, scr1)
    tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")
    tkgrid(butt_display, butt_detail, butt_export, padx = 10, pady = 5)
    tkgrid(frame_result_1, padx = 10, pady = 5)
    tkgrid(frame_result_2)
    tkgrid(frame_tab4)

    # Assign data to environment variable (env_y_result)
    assign("mlb_result", mlb_result, envir = env_y_result)
    assign("scr1", scr1, envir = env_y_result)

    # Assign data to environment variable (env_gui)
    assign("frame_tab4", frame_tab4, envir = env_gui)

    # Select tab4
    tk2notetab.select(tabs, "Y results")
  }else{
    tab4 <- get("tab4", pos = env_gui)
    frame_tab4 <- get("frame_tab4", pos = env_gui)
    tkdestroy(frame_tab4)
    frame_tab4 <- tkframe(tab4)
    assign("frame_tab4", frame_tab4, envir = env_gui)
  }
}
