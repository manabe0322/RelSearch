# The function to make tab4
make_tab4_DNU <- function(env_proj, env_gui){

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
