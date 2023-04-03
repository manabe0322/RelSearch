make_tab1 <- function(env_proj, env_gui){
  open_file <- function(type){
    sign_ok <- "ok"
    fin_auto <- get("fin_auto", pos = env_proj)
    if(fin_auto){
      sign_ok <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(sign_ok == "ok"){
      # Set environment variable (env_proj)
      set_env_proj_auto(env_proj, FALSE)

      # Make tab2
      make_tab2(env_proj, env_gui)

      # Get a file path and a file name from environment variable (env_proj)
      if(type == "query"){
        fp <- get("fp_auto_q", pos = env_proj)
        fn <- get("fn_auto_q", pos = env_proj)
      }else if(type == "ref"){
        fp <- get("fp_auto_r", pos = env_proj)
        fn <- get("fn_auto_r", pos = env_proj)
      }else if(type == "af"){
        fp <- get("fp_auto_af", pos = env_proj)
        fn <- get("fn_auto_af", pos = env_proj)
      }

      # Define tclvalue
      fp_var <- tclVar(fp)
      fn_var <- tclVar(fn)

      # Select an input file
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
          data_auto_q <- read.csv(tclvalue(fp_var), header = TRUE)
          data_auto_q <- as.matrix(data_auto_q)
          data_auto_q[is.na(data_auto_q)] <- ""
          assign("data_auto_q", data_auto_q, envir = env_proj)
          assign("fp_auto_q", tclvalue(fp_var), envir = env_proj)
          assign("fn_auto_q", tclvalue(fn_var), envir = env_proj)
        }else if(type == "ref"){
          tkconfigure(label_r_name, textvariable = fn_var)
          data_auto_r <- read.csv(tclvalue(fp_var), header = TRUE)
          data_auto_r <- as.matrix(data_auto_r)
          data_auto_r[is.na(data_auto_r)] <- ""
          assign("data_auto_r", data_auto_r, envir = env_proj)
          assign("fp_auto_r", tclvalue(fp_var), envir = env_proj)
          assign("fn_auto_r", tclvalue(fn_var), envir = env_proj)
        }else if(type == "af"){
          tkconfigure(label_af_name, textvariable = fn_var)
          data_auto_af <- read.csv(tclvalue(fp_var), header = TRUE)
          data_auto_af <- as.matrix(data_auto_af)
          assign("data_auto_af", data_auto_af, envir = env_proj)
          assign("fp_auto_af", tclvalue(fp_var), envir = env_proj)
          assign("fn_auto_af", tclvalue(fn_var), envir = env_proj)
        }
      }
    }
  }

  # Get file names from environment variable (env_proj)
  fn_auto_q <- get("fn_auto_q", pos = env_proj)
  fn_auto_r <- get("fn_auto_r", pos = env_proj)
  fn_auto_af <- get("fn_auto_af", pos = env_proj)

  # Define tclvalue
  fp_auto_q_var <- tclVar(fn_auto_q)
  fn_auto_r_var <- tclVar(fn_auto_r)
  fn_auto_afVar <- tclVar(fn_auto_af)

  tab1 <- get("tab1", pos = env_gui)
  frame_tab1 <- get("frame_tab1", pos = env_gui)
  tkdestroy(frame_tab1)
  frame_tab1 <- tkframe(tab1)
  assign("frame_tab1", frame_tab1, envir = env_gui)

  # Define frames
  frame_1_1 <- tkframe(frame_tab1, relief = "groove", borderwidth = 2)
  frame_1_2 <- tkframe(frame_tab1)

  # Define widgets in frame_1_1
  label_q_title <- tklabel(frame_1_1, text = "Query database")
  label_q_name <- tklabel(frame_1_1, textvariable = fp_auto_q_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  butt_q <- tkbutton(frame_1_1, text = "    Load    ", cursor = "hand2", command = function() open_file("query"))
  label_r_title <- tklabel(frame_1_1, text = "Reference database")
  label_r_name <- tklabel(frame_1_1, textvariable = fn_auto_r_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  butt_r <- tkbutton(frame_1_1, text = "    Load    ", cursor = "hand2", command = function() open_file("ref"))
  label_af_title <- tklabel(frame_1_1, text = "Allele frequencies")
  label_af_name <- tklabel(frame_1_1, textvariable = fn_auto_afVar, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  butt_af <- tkbutton(frame_1_1, text = "    Load    ", cursor = "hand2", command = function() open_file("af"))

  # Define widgets in frame_1_2
  butt_search <- tkbutton(frame_1_2, text = "    Search    ", cursor = "hand2", command = function() search_auto(env_proj, env_gui))

  # Grid widgets
  tkgrid(tklabel(frame_1_1, text = "Input files", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
  tkgrid(label_q_title, label_q_name, butt_q, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_r_title, label_r_name, butt_r, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_af_title, label_af_name, butt_af, padx = 10, pady = 5, sticky = "w")
  tkgrid(butt_search, pady = 10)
  tkgrid(frame_1_1, padx = 10, pady = 5, sticky = "w")
  tkgrid(frame_1_2, padx = 10, pady = 5)
  tkgrid(frame_tab1)
}

search_auto <- function(env_proj, env_gui){
  # Get input data from environment variable (env_proj)
  data_auto_q <- get("data_auto_q", pos = env_proj)
  data_auto_r <- get("data_auto_r", pos = env_proj)
  data_auto_af <- get("data_auto_af", pos = env_proj)

  # Check whether all data is loaded or not
  if(any(c(length(data_auto_q) == 0, length(data_auto_r) == 0, length(data_auto_af) == 0))){
    tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
  }else{
    # Get package path from environment variable (env_gui)
    path_pack <- get("path_pack", pos = env_gui)

    # Load mutation rates
    myu_all <- read.csv(paste0(path_pack, "/extdata/parameters/myu.csv"), header = TRUE)
    myu_all <- as.matrix(myu_all)
    locus_myu <- myu_all[, colnames(myu_all) == "Marker"]
    myu_all <- as.numeric(myu_all[, colnames(myu_all) == "Myu"])
    names(myu_all) <- locus_myu

    # Assign mutation rates
    assign("myu_all", myu_all, envir = env_proj)

    # Load IBD probabilities
    pibd_all <- read.csv(paste0(path_pack, "/extdata/parameters/pibd.csv"), header = TRUE, row.names = 1)
    pibd_all <- as.matrix(pibd_all)
    n_pibd_rel <- nrow(pibd_all)
    bool_cons_mu_all <- rep(FALSE, n_pibd_rel)
    bool_cons_mu_all[rownames(pibd_all) == "parent-child"] <- TRUE

    # Assign IBD probabilities
    assign("pibd_all", pibd_all, envir = env_proj)

    pos_sn_q <- intersect(grep("Sample", colnames(data_auto_q)), grep("Name", colnames(data_auto_q)))
    sn_auto_q <- data_auto_q[, pos_sn_q]
    gt_auto_q <- data_auto_q[, -pos_sn_q, drop = FALSE]
    locus_q <- colnames(gt_auto_q)[which(1:ncol(gt_auto_q) %% 2 == 1)]

    pos_sn_r <- intersect(grep("Sample", colnames(data_auto_r)), grep("Name", colnames(data_auto_r)))
    sn_auto_r <- data_auto_r[, pos_sn_r]
    pos_rel_r <- grep("Relationship", colnames(data_auto_r))
    rel_auto_r <- data_auto_r[, pos_rel_r]
    n_emp_rel <- length(which(rel_auto_r == ""))
    gt_auto_r <- data_auto_r[, -c(pos_sn_r, pos_rel_r), drop = FALSE]
    locus_r <- colnames(gt_auto_r)[which(1:ncol(gt_auto_r) %% 2 == 1)]

    locus_af <- colnames(data_auto_af)[-1]

    bool_locus_1 <- all(mapply(setequal, locus_q, locus_r))
    bool_locus_2 <- all(mapply(setequal, locus_q, locus_af))
    bool_locus_3 <- all(is.element(locus_q, locus_myu))
    bool_rel_1 <- all(is.element(setdiff(rel_auto_r, ""), rownames(pibd_all)))
    if(all(c(bool_locus_1, bool_locus_2, bool_locus_3, bool_rel_1))){
      # Define a progress bar
      pb <- tkProgressBar("Searching", "0% done", 0, 100, 0)

      # Update sample names
      set_env_proj_sn(env_proj, FALSE, sn_auto_q, sn_auto_r)

      # Load criteria
      criteria <- read.csv(paste0(path_pack, "/extdata/parameters/criteria.csv"), header = TRUE)
      min_lr_auto <- criteria$Value[criteria$Criteria == "Minimum LR"]

      # Assign criteria
      assign("min_lr_auto", min_lr_auto, envir = env_proj)

      # Load parameters
      par_auto <- read.csv(paste0(path_pack, "/extdata/parameters/par_auto.csv"), header = TRUE)
      maf <- par_auto$Value[par_auto$Parameter == "Minimum allele frequency"]
      meth_d <- par_auto$Value[par_auto$Parameter == "Drop-out of query alleles"]
      pd <- par_auto$Value[par_auto$Parameter == "Probability of drop-out"]

      # Assign parameters
      assign("maf", maf, envir = env_proj)
      assign("meth_d", meth_d, envir = env_proj)
      assign("pd", pd, envir = env_proj)

      # Set allele frequencies
      tmp <- set_af(gt_auto_q, gt_auto_r, data_auto_af, maf)
      af_list <- tmp[[1]]
      af_al_list <- tmp[[2]]

      # The numbers of samples
      n_q <- nrow(gt_auto_q)
      n_r <- nrow(gt_auto_r)

      # The number of loci
      n_l <- length(locus_q)

      # Extract mutation rates
      myus <- rep(0, n_l)
      for(i in 1:n_l){
        myus[i] <- myu_all[which(locus_myu == locus_q[i])]
      }
      names(myus) <- locus_q

      # Assign myus
      assign("myus", myus, envir = env_proj)

      # Calculate average probabilities of exclusion
      apes <- rep(0, n_l)
      for(i in 1:n_l){
        apes[i] <- calc_ape(af_list[[i]])
      }
      names(apes) <- locus_q

      # Assign apes
      assign("apes", apes, envir = env_proj)

      # Define objects for results
      like_h1_all <- like_h2_all <- lr_all <- array(0, dim = c(n_q, n_r + (n_pibd_rel - 1) * n_emp_rel, n_l + 1))
      gt_auto_r_new <- matrix(0, n_r + (n_pibd_rel - 1) * n_emp_rel, ncol(gt_auto_r))
      sn_auto_r_new <- rel_auto_r_new <- rep(0, n_r + (n_pibd_rel - 1) * n_emp_rel)

      # Set the number of counts for references
      count <- 1

      # Start searching
      for(i in 1:n_r){
        ref <- as.numeric(gt_auto_r[i, ])
        ref[is.na(ref)] <- -99
        if(rel_auto_r[i] == ""){
          pibds <- pibd_all
          sn_auto_r_new[count:(count + n_pibd_rel - 1)] <- sn_auto_r[i]
          rel_auto_r_new[count:(count + n_pibd_rel - 1)] <- rownames(pibd_all)
          bool_cons_mu <- bool_cons_mu_all
        }else{
          pibds <- pibd_all[rownames(pibd_all) == rel_auto_r[i], , drop = FALSE]
          sn_auto_r_new[count] <- sn_auto_r[i]
          rel_auto_r_new[count] <- rel_auto_r[i]
          if(rel_auto_r[i] == "parent-child"){
            bool_cons_mu <- TRUE
          }else{
            bool_cons_mu <- FALSE
          }
        }
        for(j in 1:nrow(pibds)){
          gt_auto_r_new[count, ] <- ref
          for(k in 1:n_q){
            query <- as.numeric(gt_auto_q[k, ])
            query[is.na(query)] <- -99
            tmp <- calc_kin_lr(query, ref, af_list, af_al_list, pibds[j, ], bool_cons_mu[j], myus, apes, meth_d, pd)
            like_h1_all[k, count, ] <- tmp[[1]]
            like_h2_all[k, count, ] <- tmp[[2]]
            lr_all[k, count, ] <- tmp[[3]]
            info <- sprintf("%d%% done", round((n_q * (count - 1) + k) * 100 / (n_q * (n_r + (n_pibd_rel - 1) * n_emp_rel))))
            setTkProgressBar(pb, (n_q * (count - 1) + k) * 100 / (n_q * (n_r + (n_pibd_rel - 1) * n_emp_rel)), sprintf("Searching"), info)
          }
          count <- count + 1
        }
      }

      # Assign results of autosomal STR
      assign("gt_auto_q", gt_auto_q, envir = env_proj)
      assign("gt_auto_r_new", gt_auto_r_new, envir = env_proj)
      assign("sn_auto_q", sn_auto_q, envir = env_proj)
      assign("sn_auto_r_new", sn_auto_r_new, envir = env_proj)
      assign("rel_auto_r_new", rel_auto_r_new, envir = env_proj)
      assign("like_h1_all", like_h1_all, envir = env_proj)
      assign("like_h2_all", like_h2_all, envir = env_proj)
      assign("lr_all", lr_all, envir = env_proj)
      assign("fin_auto", TRUE, envir = env_proj)

      # Make tab2
      make_tab2(env_proj, env_gui)

      # Close a progress bar
      close(pb)
    }else if(!bool_locus_1){
      tkmessageBox(message = "Locus set is not the same between query data and reference data!", icon = "error", type = "ok")
    }else if(!bool_locus_2){
      tkmessageBox(message = "Locus set is not the same between query data and allele frequencies!", icon = "error", type = "ok")
    }else if(!bool_locus_3){
      tkmessageBox(message = "There are some loci without mutation rates!", icon = "error", type = "ok")
    }else if(!bool_rel_1){
      tkmessageBox(message = "There are some relationships without IBD probabilities!", icon = "error", type = "ok")
    }
  }
}

display_gt <- function(data_gt){
  n_l <- length(data_gt) / 2
  data_display <- rep("", n_l)
  for(i in 1:n_l){
    gt <- as.numeric(data_gt[c(2 * i - 1, 2 * i)])
    gt <- gt[!is.na(gt)]
    if(length(gt) == 1){
      data_display[i] <- gt
    }else if(length(gt) == 2){
      data_display[i] <- paste(gt[1], ", ", gt[2], sep = "")
    }
  }
  return(data_display)
}

make_tab2 <- function(env_proj, env_gui){
  fin_auto <- get("fin_auto", pos = env_proj)
  if(fin_auto){
    set_display_1 <- function(){
      # Define tclvalue
      cand_q <- c("All", sn_auto_q)
      select_q_var <- tclVar("All")
      cand_r <- c("All", unique(sn_auto_r_new))
      select_r_var <- tclVar("All")
      cand_rel <- c("All", unique(rel_auto_r_new))
      select_rel_var <- tclVar("All")
      select_min_lr_var <- tclVar("1")

      # Make a top frame
      tf <- tktoplevel()
      tkwm.title(tf, "Set display")

      # Define frames
      frame_display_1 <- tkframe(tf)
      frame_display_2 <- tkframe(tf)

      # Define widgets in frame_display_1
      label_title_1 <- tklabel(frame_display_1, text = "Query")
      label_title_2 <- tklabel(frame_display_1, text = "Reference")
      label_title_3 <- tklabel(frame_display_1, text = "Relationship")
      label_title_4 <- tklabel(frame_display_1, text = "Minimum LR")
      combo_q <- ttkcombobox(frame_display_1, values = cand_q, textvariable = select_q_var, state = "readonly")
      combo_r <- ttkcombobox(frame_display_1, values = cand_r, textvariable = select_r_var, state = "readonly")
      combo_rel <- ttkcombobox(frame_display_1, values = cand_rel, textvariable = select_rel_var, state = "readonly")
      entry_min_lr <- tkentry(frame_display_1, textvariable = select_min_lr_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      # Define widgets in frame_display_2
      butt_set <- tkbutton(frame_display_2, text = "    Set    ", cursor = "hand2",
                           command = function() set_display_2(tf, tclvalue(select_q_var), tclvalue(select_r_var), tclvalue(select_rel_var), as.numeric(tclvalue(select_min_lr_var))))

      # Grid widgets
      tkgrid(label_title_1, label_title_2, label_title_3, label_title_4, padx = 10, pady = 5)
      tkgrid(combo_q, combo_r, combo_rel, entry_min_lr, padx = 10, pady = 5)
      tkgrid(butt_set, padx = 10, pady = 5)
      tkgrid(frame_display_1)
      tkgrid(frame_display_2)
    }

    set_display_2 <- function(tf, select_q, select_r, select_rel, select_min_lr){
      if(select_q == "All"){
        pos_q <- 1:n_data
      }else{
        pos_q <- which(sn_auto_q_display == select_q)
      }
      if(select_r == "All"){
        pos_r <- 1:n_data
      }else{
        pos_r <- which(sn_auto_r_display == select_r)
      }
      if(select_rel == "All"){
        pos_rel <- 1:n_data
      }else{
        pos_rel <- which(rel_auto_display == select_rel)
      }
      pos_lr <- which(clr_all > select_min_lr)

      pos_extract <- intersect(intersect(intersect(pos_q, pos_r), pos_rel), pos_lr)

      if(length(pos_extract) != 0){
        mlb_result <- get("mlb_result", pos = env_auto_result)
        tkdestroy(mlb_result)
        scr1 <- get("scr1", pos = env_auto_result)
        tkdestroy(scr1)
        scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))
        mlb_result <- tk2mclistbox(frame_result_1, width = 60, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(mlb_result, "add", label = "Query", width = 15)
        tk2column(mlb_result, "add", label = "Reference", width = 15)
        tk2column(mlb_result, "add", label = "Relationship", width = 15)
        tk2column(mlb_result, "add", label = "LR", width = 15)
        tkgrid(mlb_result, scr1)
        data_display <- default_display[pos_extract, , drop = FALSE]
        data_display <- data_display[order(as.numeric(data_display[, 4]), decreasing = TRUE), , drop = FALSE]
        tk2insert.multi(mlb_result, "end", data_display)
        assign("mlb_result", mlb_result, envir = env_auto_result)
        assign("scr1", scr1, envir = env_auto_result)
        assign("data_display", data_display, envir = env_auto_result)
        tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")
        tkdestroy(tf)
      }else{
        tkmessageBox(message = "There is no data that meet the condition!", icon = "error", type = "ok")
      }
    }

    show_detail <- function(){
      mlb_result <- get("mlb_result", pos = env_auto_result)
      if(tclvalue(tkcurselection(mlb_result)) == ""){
        tkmessageBox(message = "Select one result!", icon = "error", type = "ok")
      }else{
        data_display <- get("data_display", pos = env_auto_result)
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_result))) + 1
        sn_q_select <- data_display[pos_select, 1]
        pos_select_q <- which(sn_auto_q == sn_q_select)
        sn_r_select <- data_display[pos_select, 2]
        pos_select_r <- which(sn_auto_r_new == sn_r_select)
        rel_select <- data_display[pos_select, 3]
        pos_select_rel <- which(rel_auto_r_new == rel_select)
        pos_select_r <- intersect(pos_select_r, pos_select_rel)
        data_detail <- matrix("", n_l + 1, 6)
        data_detail[, 1] <- c(names(myus), "overall")
        colnames(data_detail) <- c("Locus",
                                  paste("Query (", sn_q_select, ")", sep = ""),
                                  paste("Reference (", sn_r_select, ")", sep = ""),
                                  "Likelihood (related)",
                                  "Likelihood (unrelated)",
                                  "LR")
        data_detail[1:n_l, 2] <- display_gt(gt_auto_q[pos_select_q, ])
        data_detail[1:n_l, 3] <- display_gt(gt_auto_r_new[pos_select_r, ])
        data_detail[, 4] <- signif(like_h1_all[pos_select_q, pos_select_r, ], digits = 4)
        data_detail[, 5] <- signif(like_h2_all[pos_select_q, pos_select_r, ], digits = 4)
        data_detail[, 6] <- signif(lr_all[pos_select_q, pos_select_r, ], digits = 4)

        # Make a top frame
        tf_detail <- tktoplevel()
        tkwm.title(tf_detail, "STR result in detail")

        # Define frames
        frame_detail_1 <- tkframe(tf_detail)
        frame_detail_2 <- tkframe(tf_detail)
        frame_detail_3 <- tkframe(tf_detail)

        # Define widgets in frame_detail_1
        label_title <- tklabel(frame_detail_1, text = paste("Relationship : ", rel_select, sep = ""))

        # Define a scrollbar for a multi-list box (mlb_detail)
        scr2 <- tkscrollbar(frame_detail_2, repeatinterval = 5, command = function(...) tkyview(mlb_detail, ...))

        # Define a multi-list box (mlb_detail)
        mlb_detail <- tk2mclistbox(frame_detail_2, width = 120, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr2, ...))
        tk2column(mlb_detail, "add", label = "locus", width = 20)
        tk2column(mlb_detail, "add", label = paste("Query (", sn_q_select, ")", sep = ""), width = 20)
        tk2column(mlb_detail, "add", label = paste("Reference (", sn_r_select, ")", sep = ""), width = 20)
        tk2column(mlb_detail, "add", label = "likelihood (related)", width = 20)
        tk2column(mlb_detail, "add", label = "likelihood (unrelated)", width = 20)
        tk2column(mlb_detail, "add", label = "LR", width = 20)
        tk2insert.multi(mlb_detail, "end", data_detail)

        # Define widgets in frame_detail_3
        butt_export <- tkbutton(frame_detail_3, text = "    Export    ", cursor = "hand2", command = function() export_data(data_detail, FALSE))

        # Grid widgets
        tkgrid(label_title)
        tkgrid(mlb_detail, scr2)
        tkgrid.configure(scr2, rowspan = 30, sticky = "nsw")
        tkgrid(butt_export)
        tkgrid(frame_detail_1, padx = 10, pady = 5)
        tkgrid(frame_detail_2, padx = 10, pady = 5)
        tkgrid(frame_detail_3, padx = 10, pady = 5)
      }
    }

    env_auto_result <- new.env(parent = globalenv())

    gt_auto_q <- get("gt_auto_q", pos = env_proj)
    gt_auto_r_new <- get("gt_auto_r_new", pos = env_proj)
    sn_auto_q <- get("sn_auto_q", pos = env_proj)
    sn_auto_r_new <- get("sn_auto_r_new", pos = env_proj)
    rel_auto_r_new <- get("rel_auto_r_new", pos = env_proj)
    like_h1_all <- get("like_h1_all", pos = env_proj)
    like_h2_all <- get("like_h2_all", pos = env_proj)
    lr_all <- get("lr_all", pos = env_proj)
#    pibd_all <- get("pibd_all", pos = env_proj)
    myus <- get("myus", pos = env_proj)
#    maf <- get("maf", pos = env_proj)
#    meth_d <- get("meth_d", pos = env_proj)
#    pd <- get("pd", pos = env_proj)

    n_l <- length(myus)
    n_q <- length(sn_auto_q)
    n_r <- length(sn_auto_r_new)

    clr_all_mat <- lr_all[, , n_l + 1]
    clr_all <- as.vector(clr_all_mat)
    clr_all_mat <- rbind(rel_auto_r_new, clr_all_mat)
    rownames(clr_all_mat) <- c("relationship", sn_auto_q)
    colnames(clr_all_mat) <- sn_auto_r_new

    n_data <- n_q * n_r
    default_display <- matrix(0, n_data, 4)
    colnames(default_display) <- c("Query", "Reference", "Relationship", "LR")
    sn_auto_q_display <- rep(sn_auto_q, n_r)
    default_display[, 1] <- sn_auto_q_display
    sn_auto_r_display <- as.vector(sapply(sn_auto_r_new, rep, n_q))
    default_display[, 2] <- sn_auto_r_display
    rel_auto_display <- as.vector(sapply(rel_auto_r_new, rep, n_q))
    default_display[, 3] <- rel_auto_display
    default_display[, 4] <- sprintf('%.2e', clr_all)
    data_display <- default_display[which(clr_all >= 1), , drop = FALSE]
    data_display <- data_display[order(as.numeric(data_display[, 4]), decreasing = TRUE), , drop = FALSE]
    assign("data_display", data_display, envir = env_auto_result)

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
    mlb_result <- tk2mclistbox(frame_result_1, width = 60, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(mlb_result, "add", label = "Query", width = 15)
    tk2column(mlb_result, "add", label = "Reference", width = 15)
    tk2column(mlb_result, "add", label = "Relationship", width = 15)
    tk2column(mlb_result, "add", label = "LR", width = 15)
    tk2insert.multi(mlb_result, "end", data_display)

    # Define widgets in frame_result_2
    butt_display <- tkbutton(frame_result_2, text = "    Set display    ", cursor = "hand2", command = function() set_display_1())
    butt_detail <- tkbutton(frame_result_2, text = "    Show detail    ", cursor = "hand2", command = function() show_detail())
    butt_export1 <- tkbutton(frame_result_2, text = "    Export displayed data    ", cursor = "hand2", command = function() export_data(get("data_display", pos = env_auto_result), FALSE))
    butt_export2 <- tkbutton(frame_result_2, text = "    Export All LRs    ", cursor = "hand2", command = function() export_data(clr_all_mat, TRUE))

    # Grid widgets
    tkgrid(mlb_result, scr1)
    tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")
    tkgrid(butt_display, butt_detail, butt_export1, butt_export2, padx = 10, pady = 5)
    tkgrid(frame_result_1, padx = 10, pady = 5)
    tkgrid(frame_result_2)
    tkgrid(frame_tab2)

    # Assign data to environment variable (env_auto_result)
    assign("mlb_result", mlb_result, envir = env_auto_result)
    assign("scr1", scr1, envir = env_auto_result)

    # Assign data to environment variable (env_gui)
    assign("frame_tab2", frame_tab2, envir = env_gui)

    # Select tab2
    tk2notetab.select(tabs, "STR results")
  }else{
    tab2 <- get("tab2", pos = env_gui)
    frame_tab2 <- get("frame_tab2", pos = env_gui)
    tkdestroy(frame_tab2)
    frame_tab2 <- tkframe(tab2)
    assign("frame_tab2", frame_tab2, envir = env_gui)
  }
}
