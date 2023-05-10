# The function to make tab5
make_tab5 <- function(env_proj, env_gui){

  # The function to load required files for the mtDNA analysis
  open_file <- function(type){

    # Get the end sign of the mtDNA analysis from the environment "env_proj"
    fin_mt <- get("fin_mt", pos = env_proj)

    # Confirm that the user allows to delete the mtDNA results
    if(fin_mt){
      sign_ok <- tclvalue(tkmessageBox(message = "mtDNA results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }else{
      sign_ok <- "ok"
    }
    if(sign_ok == "ok"){

      # Reset the environmental variables for the mtDNA
      set_env_proj_mt(env_proj, FALSE)

      # Reset tab6
      make_tab6(env_proj, env_gui)

      # Get a file path and a file name from the environment "env_proj"
      if(type == "query"){
        fp <- get("fp_mt_q", pos = env_proj)
        fn <- get("fn_mt_q", pos = env_proj)
      }else if(type == "ref"){
        fp <- get("fp_mt_r", pos = env_proj)
        fn <- get("fn_mt_r", pos = env_proj)
      }

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

        # If the user clicks the "Load" button for query database
        if(type == "query"){

          # Update the name of query database
          tkconfigure(label_q_name, textvariable = fn_var)

          # Load query database
          data_mt_q <- read.csv(tclvalue(fp_var), header = TRUE)
          data_mt_q <- as.matrix(data_mt_q)
          data_mt_q[is.na(data_mt_q)] <- ""

          # Assign objects to the environment "env_proj"
          assign("data_mt_q", data_mt_q, envir = env_proj)
          assign("fp_mt_q", tclvalue(fp_var), envir = env_proj)
          assign("fn_mt_q", tclvalue(fn_var), envir = env_proj)

        # If the user clicks the "Load" button for reference database
        }else if(type == "ref"){

          # Update the name of reference database
          tkconfigure(label_r_name, textvariable = fn_var)

          # Load reference database
          data_mt_r <- read.csv(tclvalue(fp_var), header = TRUE)
          data_mt_r <- as.matrix(data_mt_r)
          data_mt_r[is.na(data_mt_r)] <- ""

          # Assign objects to the environment "env_proj"
          assign("data_mt_r", data_mt_r, envir = env_proj)
          assign("fp_mt_r", tclvalue(fp_var), envir = env_proj)
          assign("fn_mt_r", tclvalue(fn_var), envir = env_proj)
        }
      }
    }
  }

  # Get file names from the environment "env_proj"
  fn_mt_q <- get("fn_mt_q", pos = env_proj)
  fn_mt_r <- get("fn_mt_r", pos = env_proj)

  # Define tcl variables
  fn_mt_q_var <- tclVar(fn_mt_q)
  fn_mt_r_var <- tclVar(fn_mt_r)

  # Reset frame_tab5
  tab5 <- get("tab5", pos = env_gui)
  frame_tab5 <- get("frame_tab5", pos = env_gui)
  tkdestroy(frame_tab5)
  frame_tab5 <- tkframe(tab5)

  # Define frames
  frame_5_1 <- tkframe(frame_tab5, relief = "groove", borderwidth = 2)
  frame_5_2 <- tkframe(frame_tab5)

  # Define widgets in frame_5_1
  label_q_title <- tklabel(frame_5_1, text = "Query database")
  label_q_name <- tklabel(frame_5_1, textvariable = fn_mt_q_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  butt_q <- tkbutton(frame_5_1, text = "    Load    ", cursor = "hand2", command = function() open_file("query"))
  label_r_title <- tklabel(frame_5_1, text = "Reference database")
  label_r_name <- tklabel(frame_5_1, textvariable = fn_mt_r_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  butt_r <- tkbutton(frame_5_1, text = "    Load    ", cursor = "hand2", command = function() open_file("ref"))

  # Define widgets in frame_5_2
  butt_search <- tkbutton(frame_5_2, text = "    Screening    ", cursor = "hand2", command = function() search_mt(env_proj, env_gui))

  # Grid widgets
  tkgrid(tklabel(frame_5_1, text = "Input files", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
  tkgrid(label_q_title, label_q_name, butt_q, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_r_title, label_r_name, butt_r, padx = 10, pady = 5, sticky = "w")
  tkgrid(butt_search, pady = 10)

  # Grid frames
  tkgrid(frame_5_1, padx = 10, pady = 5, sticky = "w")
  tkgrid(frame_5_2, padx = 10, pady = 5)
  tkgrid(frame_tab5)

  # Assign frame_tab5
  assign("frame_tab5", frame_tab5, envir = env_gui)
}

# The function to perform screening relatives using the mtDNA
search_mt <- function(env_proj, env_gui){

  # Get database from the environment "env_proj"
  data_mt_q <- get("data_mt_q", pos = env_proj)
  data_mt_r <- get("data_mt_r", pos = env_proj)

  # Confirm that all database is loaded
  if(any(c(length(data_mt_q) == 0, length(data_mt_r) == 0))){
    tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
  }else{

    # Get package path from the environment "env_gui"
    path_pack <- get("path_pack", pos = env_gui)

    # Define a progress bar
    pb <- tkProgressBar("Searching", "0% done", 0, 100, 0)

    # Load criteria
    criteria <- read.csv(paste0(path_pack, "/extdata/parameters/criteria.csv"), header = TRUE)
    min_share_mt <- criteria$Value[criteria$Criteria == "min_share_mt"]
    max_mismatch_mt <- criteria$Value[criteria$Criteria == "max_mismatch_mt"]

    # Extract sample names, ranges, and haplotypes from the query database
    pos_sn_q <- intersect(grep("Sample", colnames(data_mt_q)), grep("Name", colnames(data_mt_q)))
    sn_mt_q <- data_mt_q[, pos_sn_q]
    range_mt_q <- data_mt_q[, "Range"]
    hap_mt_q <- data_mt_q[, "Haplotype"]

    # Extract sample names, ranges, and haplotypes from the reference database
    pos_sn_r <- intersect(grep("Sample", colnames(data_mt_r)), grep("Name", colnames(data_mt_r)))
    sn_mt_r <- data_mt_r[, pos_sn_r]
    range_mt_r <- data_mt_r[, "Range"]
    hap_mt_r <- data_mt_r[, "Haplotype"]

    # Update sample names
    set_env_proj_sn(env_proj, FALSE, sn_mt_q, sn_mt_r)

    # The number of samples in each database
    n_q <- length(hap_mt_q)
    n_r <- length(hap_mt_r)

    # Define a matrix to save information on the number of mismatch between query and reference haplotypes
    mismatch_mt <- matrix("", nrow = n_q, ncol = n_r)
    rownames(mismatch_mt) <- sn_mt_q
    colnames(mismatch_mt) <- sn_mt_r

    # Define a matrix to save information on the shared range between query and reference haplotypes
    share_range_mt <- mismatch_mt
    rownames(share_range_mt) <- sn_mt_q
    colnames(share_range_mt) <- sn_mt_r

    # Define a matrix to save information on the shared length between query and reference haplotypes
    share_len_mt <- mismatch_mt
    rownames(share_len_mt) <- sn_mt_q
    colnames(share_len_mt) <- sn_mt_r

    # Repetitive execution for each reference haplotype
    for(i in 1:n_r){

      # Extract a reference haplotype
      ref <- hap_mt_r[i]

      # Extract a range information of the reference
      ran_r <- range_mt_r[i]

      # Repetitive execution for each query haplotype
      for(j in 1:n_q){

        # Extract a query haplotype
        query <- hap_mt_q[j]

        # Extract a range information of the query
        ran_q <- range_mt_q[j]

        # Compare a query haplotype with a reference haplotype
        tmp <- match_mt(query, ran_q, ref, ran_r)
        mismatch_mt[j, i] <- tmp[1]
        share_range_mt[j, i] <- tmp[2]
        share_len_mt[j, i] <- tmp[3]

        # Update the progress bar
        info <- sprintf("%d%% done", round((n_q * (i - 1) + j) * 100 / (n_q * n_r)))
        setTkProgressBar(pb, (n_q * (i - 1) + j) * 100 / (n_q * n_r), sprintf("Searching"), info)
      }
    }

    # Assign results of the mtDNA to the environment "env_proj"
    assign("sn_mt_q", sn_mt_q, envir = env_proj)
    assign("range_mt_q", range_mt_q, envir = env_proj)
    assign("hap_mt_q", hap_mt_q, envir = env_proj)
    assign("sn_mt_r", sn_mt_r, envir = env_proj)
    assign("range_mt_r", range_mt_r, envir = env_proj)
    assign("hap_mt_r", hap_mt_r, envir = env_proj)
    assign("mismatch_mt", mismatch_mt, envir = env_proj)
    assign("share_range_mt", share_range_mt, envir = env_proj)
    assign("share_len_mt", share_len_mt, envir = env_proj)

    # Assign criteria to the environment "env_proj"
    assign("min_share_mt", min_share_mt, envir = env_proj)
    assign("max_mismatch_mt", max_mismatch_mt, envir = env_proj)

    # Assign the end sign to the environment "env_proj"
    assign("fin_mt", TRUE, envir = env_proj)

    # Make tabs
    make_tab6(env_proj, env_gui)
    make_tab7(env_proj, env_gui)

    # Close the progress bar
    close(pb)
  }
}

# The function to make tab6
make_tab6 <- function(env_proj, env_gui){

  # Get the end sign from the environment "env_proj"
  fin_mt <- get("fin_mt", pos = env_proj)

  # If the mtDNA analysis has been already finished
  if(fin_mt){

    # The function to make a window for setting displayed data
    set_display_1 <- function(){

      # Define tcl variables
      cand_q <- c("All", sn_mt_q)
      select_q_var <- tclVar("All")
      cand_r <- c("All", sn_mt_r)
      select_r_var <- tclVar("All")
      select_len_share_var <- tclVar(min_share_mt)
      select_n_mm_var <- tclVar(max_mismatch_mt)

      # Make a top frame
      tf <- tktoplevel()
      tkwm.title(tf, "Set display")

      # Define frames
      frame_display_1 <- tkframe(tf)
      frame_display_2 <- tkframe(tf)

      # Define widgets in frame_display_1
      label_title_1 <- tklabel(frame_display_1, text = "Query")
      label_title_2 <- tklabel(frame_display_1, text = "Reference")
      label_title_3 <- tklabel(frame_display_1, text = "Minimum shared length")
      label_title_4 <- tklabel(frame_display_1, text = "Maximum number of inconsistency")
      combo_q <- ttkcombobox(frame_display_1, values = cand_q, textvariable = select_q_var, state = "readonly")
      combo_r <- ttkcombobox(frame_display_1, values = cand_r, textvariable = select_r_var, state = "readonly")
      entry_len_share <- tkentry(frame_display_1, textvariable = select_len_share_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      entry_n_mm <- tkentry(frame_display_1, textvariable = select_n_mm_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      # Define widgets in frame_display_2
      butt_set <- tkbutton(frame_display_2, text = "    Set    ", cursor = "hand2",
                           command = function() set_display_2(tf, tclvalue(select_q_var), tclvalue(select_r_var),
                                                              as.numeric(tclvalue(select_len_share_var)), as.numeric(tclvalue(select_n_mm_var))))

      # Grid widgets
      tkgrid(label_title_1, label_title_2, label_title_3, label_title_4, padx = 10, pady = 5)
      tkgrid(combo_q, combo_r, entry_len_share, entry_n_mm, padx = 10, pady = 5)
      tkgrid(butt_set, padx = 10, pady = 5)

      # Grid frames
      tkgrid(frame_display_1)
      tkgrid(frame_display_2)
    }

    # The function to overwrite displayed data
    set_display_2 <- function(tf, select_q, select_r, select_len_share, select_n_mm){
      if(select_q == "All"){
        pos_q <- 1:n_data
      }else{
        pos_q <- which(sn_mt_q_vec == select_q)
      }
      if(select_r == "All"){
        pos_r <- 1:n_data
      }else{
        pos_r <- which(sn_mt_r_vec == select_r)
      }
      pos_len_share <- which(share_len_mt_vec >= select_len_share)
      pos_n_mm <- which(mismatch_mt_vec <= select_n_mm)

      pos_extract <- intersect(intersect(intersect(pos_q, pos_r), pos_len_share), pos_n_mm)

      if(length(pos_extract) != 0){
        # Set display data
        data_display <- data_all[pos_extract, , drop = FALSE]
        data_display <- data_display[order(as.numeric(data_display[, 4]), decreasing = TRUE), , drop = FALSE]
        data_display <- data_display[order(as.numeric(data_display[, 5])), , drop = FALSE]

        mlb_result <- get("mlb_result", pos = env_mt_result)
        tkdestroy(mlb_result)
        scr1 <- get("scr1", pos = env_mt_result)
        tkdestroy(scr1)

        # Define a scrollbar for a multi-list box (mlb_result)
        scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))

        # Define a multi-list box (mlb_result)
        mlb_result <- tk2mclistbox(frame_result_1, width = 130, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(mlb_result, "add", label = "Query", width = 15)
        tk2column(mlb_result, "add", label = "Reference", width = 15)
        tk2column(mlb_result, "add", label = "Shared range", width = 50)
        tk2column(mlb_result, "add", label = "Shared length", width = 20)
        tk2column(mlb_result, "add", label = "Number of inconsistency", width = 30)
        tk2insert.multi(mlb_result, "end", data_display)

        # Grid widgets
        tkgrid(mlb_result)
        tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")

        # Assign widgets to environment variable (env_mt_result)
        assign("mlb_result", mlb_result, envir = env_mt_result)
        assign("scr1", scr1, envir = env_mt_result)
        assign("data_display", data_display, envir = env_mt_result)

        # Destroy the top frame
        tkdestroy(tf)
      }else{
        tkmessageBox(message = "There is no data that meet the condition!", icon = "error", type = "ok")
      }
    }

    # The function to show data in detail
    show_detail <- function(){
      mlb_result <- get("mlb_result", pos = env_mt_result)
      if(tclvalue(tkcurselection(mlb_result)) == ""){
        tkmessageBox(message = "Select one line!", icon = "error", type = "ok")
      }else{
        data_display <- get("data_display", pos = env_mt_result)
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_result))) + 1

        select_q_name <- data_display[pos_select, 1]
        pos_select_q <- which(sn_mt_q == select_q_name)
        ran_q <- range_mt_q[pos_select_q]
        query <- hap_mt_q[pos_select_q]
        q_type <- strsplit(query, " ")[[1]]
        q_type <- setdiff(q_type, "")
        q_type <- q_type[order(parse_number(q_type))]

        select_r_name <- data_display[pos_select, 2]
        pos_select_r <- which(sn_mt_r == select_r_name)
        ran_r <- range_mt_r[pos_select_r]
        ref <- hap_mt_r[pos_select_r]
        r_type <- strsplit(ref, " ")[[1]]
        r_type <- setdiff(r_type, "")
        r_type <- r_type[order(parse_number(r_type))]

        qr_type <- union(q_type, r_type)
        qr_type <- qr_type[order(parse_number(qr_type))]
        n_type <- length(qr_type)

        pos_mt_qr <- extract_pos_mt_qr(ran_q, ran_r)

        data_detail <- matrix("", n_type, 4)
        colnames(data_detail) <- c(paste0("Query (", select_q_name, ")"),
                                  paste0("Reference (", select_r_name, ")"),
                                  "Out of shared range",
                                  "Inconsistency")
        data_detail[is.element(qr_type, q_type), 1] <- q_type
        data_detail[is.element(qr_type, r_type), 2] <- r_type
        pos_common <- is.element(round(parse_number(qr_type), 0), pos_mt_qr)
        data_detail[!pos_common, 3] <- "X"
        pos_inconsistent <- apply(rbind(!is.element(qr_type, q_type), !is.element(qr_type, r_type)), 2, any)
        data_detail[apply(rbind(pos_common, pos_inconsistent), 2, all), 4] <- "X"

        # Make a top frame
        tf_detail <- tktoplevel()
        tkwm.title(tf_detail, "mtDNA result in detail")

        # Define frames
        frame_detail_1 <- tkframe(tf_detail)
        frame_detail_2 <- tkframe(tf_detail)

        # Define widgets in tf_detail
        label_share_range <- tklabel(tf_detail, text = paste0("Shared range : ", share_range_mt[pos_select_q, pos_select_r]))

        # Define a scrollbar for a multi-list box (mlb_detail)
        scr2 <- tkscrollbar(frame_detail_1, repeatinterval = 5, command = function(...) tkyview(mlb_detail, ...))

        # Define a multi-list box (mlb_detail)
        mlb_detail <- tk2mclistbox(frame_detail_1, width = 85, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr2, ...))
        tk2column(mlb_detail, "add", label = paste0("Query (", select_q_name, ")"), width = 20)
        tk2column(mlb_detail, "add", label = paste0("Reference (", select_r_name, ")"), width = 20)
        tk2column(mlb_detail, "add", label = "Out of shared range", width = 25)
        tk2column(mlb_detail, "add", label = "Inconsistency", width = 20)
        tk2insert.multi(mlb_detail, "end", data_detail)

        # Define widgets in frame_detail_2
        butt_export <- tkbutton(frame_detail_2, text = "    Export    ", cursor = "hand2", command = function() export_data(data_detail, FALSE))

        # Grid widgets
        tkgrid(label_share_range, padx = 10, pady = 5)
        tkgrid(mlb_detail, scr2)
        tkgrid.configure(scr2, rowspan = 30, sticky = "nsw")
        tkgrid(butt_export)
        tkgrid(frame_detail_1, padx = 10, pady = 5)
        tkgrid(frame_detail_2, padx = 10, pady = 5)
      }
    }

    # Define the environment "env_mt_result"
    env_mt_result <- new.env(parent = globalenv())

    # Get results from the environment "env_proj"
    hap_mt_q <- get("hap_mt_q", pos = env_proj)
    hap_mt_r <- get("hap_mt_r", pos = env_proj)
    sn_mt_q <- get("sn_mt_q", pos = env_proj)
    sn_mt_r <- get("sn_mt_r", pos = env_proj)
    range_mt_q <- get("range_mt_q", pos = env_proj)
    range_mt_r <- get("range_mt_r", pos = env_proj)
    mismatch_mt <- get("mismatch_mt", pos = env_proj)
    share_range_mt <- get("share_range_mt", pos = env_proj)
    share_len_mt <- get("share_len_mt", pos = env_proj)

    # Get criteria from the environment "env_proj"
    min_share_mt <- get("min_share_mt", pos = env_proj)
    max_mismatch_mt <- get("max_mismatch_mt", pos = env_proj)

    # The number of samples in each database
    n_q <- length(sn_mt_q)
    n_r <- length(sn_mt_r)

    # Define vectors for making displayed data
    sn_mt_q_vec <- rep(sn_mt_q, n_r)
    sn_mt_r_vec <- as.vector(sapply(sn_mt_r, rep, n_q))
    share_range_mt_vec <- as.vector(share_range_mt)
    share_len_mt_vec <- as.numeric(as.vector(share_len_mt))
    mismatch_mt_vec <- as.numeric(as.vector(mismatch_mt))

    # The number of comparisons between query and reference haplotypes
    n_data <- n_q * n_r

    # Define a matrix for all data
    data_all <- matrix(0, n_data, 5)
    colnames(data_all) <- c("Query", "Reference", "Shared range", "Shared length", "Number of inconsistency")
    data_all[, 1] <- sn_mt_q_vec
    data_all[, 2] <- sn_mt_r_vec
    data_all[, 3] <- share_range_mt_vec
    data_all[, 4] <- share_len_mt_vec
    data_all[, 5] <- mismatch_mt_vec

    # Define a matrix for the displayed data
    data_display <- data_all[which(as.numeric(data_all[, 4]) >= min_share_mt), , drop = FALSE]
    data_display <- data_display[which(as.numeric(data_display[, 5]) <= max_mismatch_mt), , drop = FALSE]
    data_display <- data_display[order(as.numeric(data_display[, 4]), decreasing = TRUE), , drop = FALSE]
    data_display <- data_display[order(as.numeric(data_display[, 5])), , drop = FALSE]

    # Reset frame_tab6
    tabs <- get("tabs", pos = env_gui)
    tab6 <- get("tab6", pos = env_gui)
    frame_tab6 <- get("frame_tab6", pos = env_gui)
    tkdestroy(frame_tab6)
    frame_tab6 <- tkframe(tab6)

    # Define frames
    frame_result_1 <- tkframe(frame_tab6)
    frame_result_2 <- tkframe(frame_tab6)

    # Define a scrollbar for the multi-list box for displayed data
    scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))

    # Define a multi-list box for displayed data
    mlb_result <- tk2mclistbox(frame_result_1, width = 130, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(mlb_result, "add", label = "Query", width = 15)
    tk2column(mlb_result, "add", label = "Reference", width = 15)
    tk2column(mlb_result, "add", label = "Shared range", width = 50)
    tk2column(mlb_result, "add", label = "Shared length", width = 20)
    tk2column(mlb_result, "add", label = "Number of inconsistency", width = 30)
    tk2insert.multi(mlb_result, "end", data_display)

    # Define widgets in frame_result_2
    butt_display <- tkbutton(frame_result_2, text = "    Set display    ", cursor = "hand2", command = function() set_display_1())
    butt_detail <- tkbutton(frame_result_2, text = "    Show detail    ", cursor = "hand2", command = function() show_detail())
    butt_export <- tkbutton(frame_result_2, text = "    Export displayed data    ", cursor = "hand2", command = function() export_data(get("data_display", pos = env_mt_result), FALSE))

    # Grid widgets
    tkgrid(mlb_result, scr1)
    tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")
    tkgrid(butt_display, butt_detail, butt_export, padx = 10, pady = 5)

    # Grid frames
    tkgrid(frame_result_1, padx = 10, pady = 5)
    tkgrid(frame_result_2)
    tkgrid(frame_tab6)

    # Assign objects to the environment "env_mt_result"
    assign("mlb_result", mlb_result, envir = env_mt_result)
    assign("scr1", scr1, envir = env_mt_result)
    assign("data_display", data_display, envir = env_mt_result)

    # Assign frame_tab6 to the environment "env_mt_result"
    assign("frame_tab6", frame_tab6, envir = env_gui)

    # Select tab6
    tk2notetab.select(tabs, "mtDNA results")

  # If the mtDNA analysis has not been finished
  }else{

    # Reset frame_tab6
    tab6 <- get("tab6", pos = env_gui)
    frame_tab6 <- get("frame_tab6", pos = env_gui)
    tkdestroy(frame_tab6)
    frame_tab6 <- tkframe(tab6)

    # Assign frame_tab6 to the environment "env_gui"
    assign("frame_tab6", frame_tab6, envir = env_gui)
  }
}
