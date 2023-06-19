# The function to make tab3
make_tab3 <- function(env_proj, env_gui){

  # The function to load required files for the Y-STR analysis
  open_file <- function(type){

    # Get the end sign of the Y-STR analysis from the environment "env_proj"
    fin_y <- get("fin_y", pos = env_proj)

    # Confirm that the user allows to delete the Y-STR results
    if(fin_y){
      sign_ok <- tclvalue(tkmessageBox(message = "Y-STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }else{
      sign_ok <- "ok"
    }
    if(sign_ok == "ok"){

      # Reset the environmental variables for the Y-STR
      set_env_proj_y(env_proj, FALSE)

      # Reset tab4
      make_tab4(env_proj, env_gui)

      # Get a file path and a file name from the environment "env_proj"
      if(type == "query"){
        fp <- get("fp_y_q", pos = env_proj)
        fn <- get("fn_y_q", pos = env_proj)
      }else if(type == "ref"){
        fp <- get("fp_y_r", pos = env_proj)
        fn <- get("fn_y_r", pos = env_proj)
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
          data_y_q <- read.csv(tclvalue(fp_var), header = TRUE)
          data_y_q <- as.matrix(data_y_q)
          data_y_q[is.na(data_y_q)] <- ""

          # Assign objects to the environment "env_proj"
          assign("data_y_q", data_y_q, envir = env_proj)
          assign("fp_y_q", tclvalue(fp_var), envir = env_proj)
          assign("fn_y_q", tclvalue(fn_var), envir = env_proj)

        # If the user clicks the "Load" button for reference database
        }else if(type == "ref"){

          # Update the name of reference database
          tkconfigure(label_r_name, textvariable = fn_var)

          # Load reference database
          data_y_r <- read.csv(tclvalue(fp_var), header = TRUE)
          data_y_r <- as.matrix(data_y_r)
          data_y_r[is.na(data_y_r)] <- ""

          # Assign objects to the environment "env_proj"
          assign("data_y_r", data_y_r, envir = env_proj)
          assign("fp_y_r", tclvalue(fp_var), envir = env_proj)
          assign("fn_y_r", tclvalue(fn_var), envir = env_proj)
        }
      }
    }
  }

  # Get file names from the environment "env_proj"
  fn_y_q <- get("fn_y_q", pos = env_proj)
  fn_y_r <- get("fn_y_r", pos = env_proj)

  # Define tcl variables
  fn_y_q_var <- tclVar(fn_y_q)
  fn_y_r_var <- tclVar(fn_y_r)

  # Reset frame_tab3
  tab3 <- get("tab3", pos = env_gui)
  frame_tab3 <- get("frame_tab3", pos = env_gui)
  tkdestroy(frame_tab3)
  frame_tab3 <- tkframe(tab3)

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

  # Grid frames
  tkgrid(frame_3_1, padx = 10, pady = 5, sticky = "w")
  tkgrid(frame_3_2, padx = 10, pady = 5)
  tkgrid(frame_tab3)

  # Assign frame_tab3
  assign("frame_tab3", frame_tab3, envir = env_gui)
}

# The function to perform screening relatives using the Y-STR
search_y <- function(env_proj, env_gui){

  # Get database from the environment "env_proj"
  data_y_q <- get("data_y_q", pos = env_proj)
  data_y_r <- get("data_y_r", pos = env_proj)

  # Confirm that all database is loaded
  if(any(c(length(data_y_q) == 0, length(data_y_r) == 0))){
    tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
  }else{

    # Get package path from the environment "env_gui"
    path_pack <- get("path_pack", pos = env_gui)

    # Extract sample names, haplotypes, and loci from the query database
    pos_sn_q <- intersect(grep("Sample", colnames(data_y_q)), grep("Name", colnames(data_y_q)))
    sn_y_q <- data_y_q[, pos_sn_q]
    hap_y_q <- data_y_q[, -pos_sn_q, drop = FALSE]
    locus_q <- colnames(hap_y_q)

    # Extract sample names, haplotypes, and loci from the reference database
    pos_sn_r <- intersect(grep("Sample", colnames(data_y_r)), grep("Name", colnames(data_y_r)))
    sn_y_r <- data_y_r[, pos_sn_r]
    hap_y_r <- data_y_r[, -pos_sn_r, drop = FALSE]
    locus_r <- colnames(hap_y_r)

    # Whether the locus set of the query database is the same as that of the reference database or not
    bool_locus_1 <- all(mapply(setequal, locus_q, locus_r))

    # If the locus set of the query database is the same as that of the reference database
    if(bool_locus_1){

      # Check whether the file 'criteria.csv' is found or not
      fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))
      if(is.element("criteria.csv", fn_par)){

        # Load criteria
        criteria <- read.csv(paste0(path_pack, "/extdata/parameters/criteria.csv"), header = TRUE)
        max_mismatch_y <- criteria$Value[criteria$Criteria == "max_mismatch_y"]
        max_ignore_y <- criteria$Value[criteria$Criteria == "max_ignore_y"]
        max_mustep_y <- criteria$Value[criteria$Criteria == "max_mustep_y"]

        # Define a progress bar
        pb <- tkProgressBar("Searching", "0% done", 0, 100, 0)

        # Put the loci of the reference haplotypes in the same order as that of the query haplotypes
        hap_y_r <- hap_y_r[, match(locus_q, locus_r)]

        # The number of samples in each database
        n_q <- nrow(hap_y_q)
        n_r <- nrow(hap_y_r)

        # The number of loci
        n_l <- length(locus_q)

        # Define an array to save information on the mismatched loci between query and reference haplotypes
        mismatch_y <- array(0, dim = c(n_q, n_r, n_l + 1))

        # Define an array to save information on the ignored loci
        ignore_y <- mismatch_y

        # Define an array to save information on the mutation step
        mustep_y <- mismatch_y

        # Repetitive execution for each reference haplotype
        for(i in 1:n_r){

          # Extract a reference haplotype
          ref <- hap_y_r[i, ]

          # Repetitive execution for each query haplotype
          for(j in 1:n_q){

            # Extract a query haplotype
            query <- hap_y_q[j, ]

            # Compare a query haplotype with a reference haplotype
            tmp <- match_y(query, ref)
            mismatch_y[j, i, ] <- tmp[[1]]
            ignore_y[j, i, ] <- tmp[[2]]
            mustep_y[j, i, ] <- tmp[[3]]

            # Update the progress bar
            info <- sprintf("%d%% done", round((n_q * (i - 1) + j) * 100 / (n_q * n_r)))
            setTkProgressBar(pb, (n_q * (i - 1) + j) * 100 / (n_q * n_r), sprintf("Searching"), info)
          }
        }

        # Update sample names in the environment "env_proj"
        set_env_proj_sn(env_proj, FALSE, sn_y_q, sn_y_r)

        # Assign results of the Y-STR to the environment "env_proj"
        assign("hap_y_q", hap_y_q, envir = env_proj)
        assign("hap_y_r", hap_y_r, envir = env_proj)
        assign("sn_y_q", sn_y_q, envir = env_proj)
        assign("sn_y_r", sn_y_r, envir = env_proj)
        assign("mismatch_y", mismatch_y, envir = env_proj)
        assign("ignore_y", ignore_y, envir = env_proj)
        assign("mustep_y", mustep_y, envir = env_proj)

        # Assign criteria to the environment "env_proj"
        assign("max_mismatch_y", max_mismatch_y, envir = env_proj)
        assign("max_ignore_y", max_ignore_y, envir = env_proj)
        assign("max_mustep_y", max_mustep_y, envir = env_proj)

        # Assign the end sign
        assign("fin_y", TRUE, envir = env_proj)

        # Make tabs
        make_tab4(env_proj, env_gui)
        make_tab7(env_proj, env_gui)

        # Close the progress bar
        close(pb)

      # If the file 'criteria.csv' is not found
      }else{
        tkmessageBox(message = paste0("The file 'criteria.csv' is not found in '", path_pack, "/extdata/parameters'. Set criteria via 'Tools > Set criteria.'"), icon = "error", type = "ok")
      }

    # If the locus set of the query database is not the same as that of the reference database
    }else{
      tkmessageBox(message = "Locus set is not the same between query data and reference data!", icon = "error", type = "ok")
    }
  }
}

# The function to make tab4
make_tab4 <- function(env_proj, env_gui){

  # Get the end sign from the environment "env_proj"
  fin_y <- get("fin_y", pos = env_proj)

  # If the Y-sTR analysis has been already finished
  if(fin_y){

    # The function to make a window for setting displayed data
    set_display_1 <- function(){

      # Define tcl variables
      cand_q <- c("All", sn_y_q)
      select_q_var <- tclVar("All")
      cand_r <- c("All", sn_y_r)
      select_r_var <- tclVar("All")
      select_total_mismatch_var <- tclVar(max_mismatch_y)
      select_total_ignore_var <- tclVar(max_ignore_y)
      select_total_mustep_var <- tclVar(max_mustep_y)

      # Make a top frame
      tf <- tktoplevel()
      tkwm.title(tf, "Set display")

      # Define frames
      frame_display_1 <- tkframe(tf)
      frame_display_2 <- tkframe(tf)

      # Define widgets in frame_display_1
      label_title_1 <- tklabel(frame_display_1, text = "Query")
      label_title_2 <- tklabel(frame_display_1, text = "Reference")
      label_title_3 <- tklabel(frame_display_1, text = "Number of ignored loci")
      label_title_4 <- tklabel(frame_display_1, text = "Number of mismatched loci")
      label_title_5 <- tklabel(frame_display_1, text = "Maximum mutational step")
      combo_q <- ttkcombobox(frame_display_1, values = cand_q, textvariable = select_q_var, state = "readonly")
      combo_r <- ttkcombobox(frame_display_1, values = cand_r, textvariable = select_r_var, state = "readonly")
      entry_total_ignore <- tkentry(frame_display_1, textvariable = select_total_ignore_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      entry_total_mismatch <- tkentry(frame_display_1, textvariable = select_total_mismatch_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      entry_total_mustep <- tkentry(frame_display_1, textvariable = select_total_mustep_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      # Define widgets in frame_display_2
      butt_set <- tkbutton(frame_display_2, text = "    Set    ", cursor = "hand2",
                           command = function() set_display_2(tf, tclvalue(select_q_var), tclvalue(select_r_var),
                                                              as.numeric(tclvalue(select_total_mismatch_var)), as.numeric(tclvalue(select_total_ignore_var)), as.numeric(tclvalue(select_total_mustep_var))))

      # Grid widgets
      tkgrid(label_title_1, label_title_2, label_title_3, label_title_4, label_title_5, padx = 10, pady = 5)
      tkgrid(combo_q, combo_r, entry_total_ignore, entry_total_mismatch, entry_total_mustep, padx = 10, pady = 5)
      tkgrid(butt_set, padx = 10, pady = 5)

      # Grid frames
      tkgrid(frame_display_1)
      tkgrid(frame_display_2)
    }

    # The function to overwrite displayed data
    set_display_2 <- function(tf, select_q, select_r, select_total_mismatch, select_total_ignore, select_total_mustep){

      # Investigate row indices to extract displayed data
      select_mat <- matrix(TRUE, n_data, 5)
      if(select_q != "All"){
        select_mat[, 1] <- sn_y_q_vec == select_q
      }
      if(select_r != "All"){
        select_mat[, 2] <- sn_y_r_vec == select_r
      }
      select_mat[, 3] <- total_ignore_vec <= select_total_ignore
      select_mat[, 4] <- total_mismatch_vec <= select_total_mismatch
      select_mat[, 5] <- total_mustep_vec <= select_total_mustep
      pos_extract <- which(apply(select_mat, 1, all) == TRUE)

      # If there is at least one row index to extract displayed data
      if(length(pos_extract) != 0){

        # Destroy the scroll bar for displayed data
        scr1 <- get("scr1", pos = env_y_result)
        tkdestroy(scr1)

        # Destroy the multi-list box for displayed data
        mlb_result <- get("mlb_result", pos = env_y_result)
        tkdestroy(mlb_result)

        # Define the scroll bar for displayed data
        scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))

        # Define the multi-list box for displayed data
        mlb_result <- tk2mclistbox(frame_result_1, width = 120, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(mlb_result, "add", label = "Query", width = 15)
        tk2column(mlb_result, "add", label = "Reference", width = 15)
        tk2column(mlb_result, "add", label = "Number of ignored loci", width = 30)
        tk2column(mlb_result, "add", label = "Number of mismatched loci", width = 30)
        tk2column(mlb_result, "add", label = "Maximum mutational step", width = 30)

        # Extract displayed data
        data_display <- data_all[pos_extract, , drop = FALSE]
        data_display <- data_display[order(as.numeric(data_display[, 5])), , drop = FALSE]
        data_display <- data_display[order(as.numeric(data_display[, 3])), , drop = FALSE]
        data_display <- data_display[order(as.numeric(data_display[, 4])), , drop = FALSE]
        data_display[which(as.numeric(data_display[, 5]) == 99), 5] <- "Not integer"
        tk2insert.multi(mlb_result, "end", data_display)

        # Grid the scroll bar and the multi-list box
        tkgrid(mlb_result, scr1)
        tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")

        # Assign objects to the environment "env_y_result"
        assign("mlb_result", mlb_result, envir = env_y_result)
        assign("scr1", scr1, envir = env_y_result)
        assign("data_display", data_display, envir = env_y_result)

        # Destroy the top frame
        tkdestroy(tf)

      # If there is no row index to extract displayed data
      }else{
        tkmessageBox(message = "There is no data that meet the condition!", icon = "error", type = "ok")
      }
    }

    # The function to show data in detail
    show_detail <- function(){

      # Get the multi-list box for displayed data from the environment "env_y_result"
      mlb_result <- get("mlb_result", pos = env_y_result)

      # If the user does not select one line
      if(tclvalue(tkcurselection(mlb_result)) == ""){
        tkmessageBox(message = "Select one line!", icon = "error", type = "ok")

      # If the user selects one line
      }else{

        # Get displayed data from the environment "env_y_result"
        data_display <- get("data_display", pos = env_y_result)

        # The row index which the user selected
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_result))) + 1

        # Search indices to extract data
        select_q_name <- data_display[pos_select, 1]
        pos_select_q <- which(sn_y_q == select_q_name)
        select_r_name <- data_display[pos_select, 2]
        pos_select_r <- which(sn_y_r == select_r_name)

        # Define a matrix for detailed data
        data_detail <- matrix("", n_l + 1, 6)
        colnames(data_detail) <- c("Locus",
                                  paste0("Query (", select_q_name, ")"),
                                  paste0("Reference (", select_r_name, ")"),
                                  "Ignored loci",
                                  "mismatched loci",
                                  "Mutational step")

        # 1st column for detailed data
        data_detail[, 1] <- c(colnames(hap_y_q), "overall")

        # 2nd column for detailed data
        data_detail[1:n_l, 2] <- hap_y_q[pos_select_q, ]

        # 3rd column for detailed data
        data_detail[1:n_l, 3] <- hap_y_r[pos_select_r, ]

        # 4th column for detailed data
        ignore_y_ext <- ignore_y[pos_select_q, pos_select_r, ]
        ignore_y_ext2 <- ignore_y_ext[1:n_l]
        ignore_y_ext2[which(ignore_y_ext2 == 0)] <- ""
        data_detail[, 4] <- c(ignore_y_ext2, ignore_y_ext[n_l + 1])

        # 5th column for detailed data
        mismatch_y_ext <- mismatch_y[pos_select_q, pos_select_r, ]
        mismatch_y_ext2 <- mismatch_y_ext[1:n_l]
        mismatch_y_ext2[which(mismatch_y_ext2 == 0)] <- ""
        data_detail[, 5] <- c(mismatch_y_ext2, mismatch_y_ext[n_l + 1])

        # 6th column for detailed data
        mustep_y_ext <- mustep_y[pos_select_q, pos_select_r, ]
        mustep_y_ext[which(mustep_y_ext == 0)] <- ""
        mustep_y_ext[which(mustep_y_ext == 99)] <- "Not integer"
        data_detail[, 6] <- mustep_y_ext

        # Make a top frame
        tf_detail <- tktoplevel()
        tkwm.title(tf_detail, "Y result in detail")

        # Define frames
        frame_detail_1 <- tkframe(tf_detail)
        frame_detail_2 <- tkframe(tf_detail)

        # Define a scrollbar for the multi-list box of the detailed data
        scr2 <- tkscrollbar(frame_detail_1, repeatinterval = 5, command = function(...) tkyview(mlb_detail, ...))

        # Define a multi-list box of the detailed data
        mlb_detail <- tk2mclistbox(frame_detail_1, width = 120, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr2, ...))
        tk2column(mlb_detail, "add", label = "locus", width = 20)
        tk2column(mlb_detail, "add", label = paste0("Query (", select_q_name, ")"), width = 20)
        tk2column(mlb_detail, "add", label = paste0("Reference (", select_r_name, ")"), width = 20)
        tk2column(mlb_detail, "add", label = "Ignored loci", width = 20)
        tk2column(mlb_detail, "add", label = "Mismatched loci", width = 20)
        tk2column(mlb_detail, "add", label = "Mutational step", width = 20)
        tk2insert.multi(mlb_detail, "end", data_detail)

        # Define a export button in frame_detail_2
        butt_export <- tkbutton(frame_detail_2, text = "    Export    ", cursor = "hand2", command = function() export_data(data_detail, FALSE))

        # Grid widgets
        tkgrid(mlb_detail, scr2)
        tkgrid.configure(scr2, rowspan = 30, sticky = "nsw")
        tkgrid(butt_export)

        # Grid frames
        tkgrid(frame_detail_1, padx = 10, pady = 5)
        tkgrid(frame_detail_2, padx = 10, pady = 5)
      }
    }

    # Define the environment "env_y_result"
    env_y_result <- new.env(parent = globalenv())

    # Get results from the environment "env_proj"
    hap_y_q <- get("hap_y_q", pos = env_proj)
    hap_y_r <- get("hap_y_r", pos = env_proj)
    sn_y_q <- get("sn_y_q", pos = env_proj)
    sn_y_r <- get("sn_y_r", pos = env_proj)
    mismatch_y <- get("mismatch_y", pos = env_proj)
    ignore_y <- get("ignore_y", pos = env_proj)
    mustep_y <- get("mustep_y", pos = env_proj)

    # Get criteria from the environment "env_proj"
    max_mismatch_y <- get("max_mismatch_y", pos = env_proj)
    max_ignore_y <- get("max_ignore_y", pos = env_proj)
    max_mustep_y <- get("max_mustep_y", pos = env_proj)

    # The number of samples in each database
    n_q <- length(sn_y_q)
    n_r <- length(sn_y_r)

    # The number of loci
    n_l <- ncol(hap_y_q)

    # The total number of mismatched loci
    total_mismatch <- mismatch_y[, , n_l + 1]
    rownames(total_mismatch) <- sn_y_q
    colnames(total_mismatch) <- sn_y_r

    # The total number of ignored loci
    total_ignore <- ignore_y[, , n_l + 1]
    rownames(total_ignore) <- sn_y_q
    colnames(total_ignore) <- sn_y_r

    # The maximum mutational steps of all loci
    total_mustep <- mustep_y[, , n_l + 1]
    rownames(total_mustep) <- sn_y_q
    colnames(total_mustep) <- sn_y_r

    # Define vectors for making displayed data
    sn_y_q_vec <- rep(sn_y_q, n_r)
    sn_y_r_vec <- as.vector(sapply(sn_y_r, rep, n_q))
    total_mismatch_vec <- as.vector(total_mismatch)
    total_ignore_vec <- as.vector(total_ignore)
    total_mustep_vec <- as.vector(total_mustep)

    # The number of comparisons between query and reference haplotypes
    n_data <- n_q * n_r

    # Define a matrix for all data
    data_all <- matrix(0, n_data, 5)
    colnames(data_all) <- c("Query", "Reference", "Number of ignored loci", "Number of mismatched loci", "Maximum mutational step")
    data_all[, 1] <- sn_y_q_vec
    data_all[, 2] <- sn_y_r_vec
    data_all[, 3] <- total_ignore_vec
    data_all[, 4] <- total_mismatch_vec
    data_all[, 5] <- total_mustep_vec

    # Define a matrix for the displayed data
    data_display <- data_all[which(as.numeric(data_all[, 4]) <= max_mismatch_y), , drop = FALSE]
    data_display <- data_display[which(as.numeric(data_display[, 3]) <= max_ignore_y), , drop = FALSE]
    data_display <- data_display[which(as.numeric(data_display[, 5]) <= max_mustep_y), , drop = FALSE]
    data_display <- data_all
    data_display <- data_display[order(as.numeric(data_display[, 3])), , drop = FALSE]
    data_display <- data_display[order(as.numeric(data_display[, 4])), , drop = FALSE]

    # Reset frame_tab4
    tabs <- get("tabs", pos = env_gui)
    tab4 <- get("tab4", pos = env_gui)
    frame_tab4 <- get("frame_tab4", pos = env_gui)
    tkdestroy(frame_tab4)
    frame_tab4 <- tkframe(tab4)

    # Define frames
    frame_result_1 <- tkframe(frame_tab4)
    frame_result_2 <- tkframe(frame_tab4)

    # Define a scrollbar for the multi-list box of the displayed data
    scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))

    # Define a multi-list box of the displayed data
    mlb_result <- tk2mclistbox(frame_result_1, width = 120, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(mlb_result, "add", label = "Query", width = 15)
    tk2column(mlb_result, "add", label = "Reference", width = 15)
    tk2column(mlb_result, "add", label = "Number of ignored loci", width = 30)
    tk2column(mlb_result, "add", label = "Number of mismatched loci", width = 30)
    tk2column(mlb_result, "add", label = "Maximum mutational step", width = 30)
    tk2insert.multi(mlb_result, "end", data_display)

    # Define widgets in frame_result_2
    butt_display <- tkbutton(frame_result_2, text = "    Set display    ", cursor = "hand2", command = function() set_display_1())
    butt_detail <- tkbutton(frame_result_2, text = "    Show detail    ", cursor = "hand2", command = function() show_detail())
    butt_export <- tkbutton(frame_result_2, text = "    Export displayed data    ", cursor = "hand2", command = function() export_data(get("data_display", pos = env_y_result), FALSE))

    # Grid widgets
    tkgrid(mlb_result, scr1)
    tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")
    tkgrid(butt_display, butt_detail, butt_export, padx = 10, pady = 5)

    # Grid frames
    tkgrid(frame_result_1, padx = 10, pady = 5)
    tkgrid(frame_result_2)
    tkgrid(frame_tab4)

    # Assign objects to the environment "env_y_result"
    assign("mlb_result", mlb_result, envir = env_y_result)
    assign("scr1", scr1, envir = env_y_result)
    assign("data_display", data_display, envir = env_y_result)

    # Assign frame_tab4 to the environment "env_gui"
    assign("frame_tab4", frame_tab4, envir = env_gui)

    # Select tab4
    tk2notetab.select(tabs, "Y results")

  # If the Y-sTR analysis has not been finished
  }else{

    # Reset frame_tab4
    tab4 <- get("tab4", pos = env_gui)
    frame_tab4 <- get("frame_tab4", pos = env_gui)
    tkdestroy(frame_tab4)
    frame_tab4 <- tkframe(tab4)

    # Assign frame_tab4 to the environment "env_gui"
    assign("frame_tab4", frame_tab4, envir = env_gui)
  }
}
