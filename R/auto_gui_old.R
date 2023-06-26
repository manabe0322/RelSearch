# The function to make a vector of genotypes for detailed data
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

# The function to make tab2
make_tab2_DNU <- function(env_proj, env_gui){

  # Get the end sign from the environment "env_proj"
  fin_auto <- get("fin_auto", pos = env_proj)

  # If the autosomal STR analysis has been already finished
  if(fin_auto){

    # The function to make a window for setting displayed data
    set_display_1 <- function(){

      # Define tcl variables
      cand_q <- c("All", data_auto_q[, SampleName])
      sn_q_select_var <- tclVar("All")
      cand_r <- c("All", data_auto_r[, SampleName])
      sn_r_select_var <- tclVar("All")
      cand_rel <- c("All", unique(lr_all[, AssumedRelationship]))
      rel_select_var <- tclVar("All")
      min_lr_select_var <- tclVar(min_lr_auto)

      # Make a top frame
      tf <- tktoplevel()
      tkwm.title(tf, "Set display")

      # Define frames
      frame_display_1 <- tkframe(tf)
      frame_display_2 <- tkframe(tf)

      # Define widgets in frame_display_1
      label_title_1 <- tklabel(frame_display_1, text = "Query")
      label_title_2 <- tklabel(frame_display_1, text = "Reference")
      label_title_3 <- tklabel(frame_display_1, text = "Assumed relationship")
      label_title_4 <- tklabel(frame_display_1, text = "Minimum LR")
      combo_q <- ttkcombobox(frame_display_1, values = cand_q, textvariable = sn_q_select_var, state = "readonly")
      combo_r <- ttkcombobox(frame_display_1, values = cand_r, textvariable = sn_r_select_var, state = "readonly")
      combo_rel <- ttkcombobox(frame_display_1, values = cand_rel, textvariable = rel_select_var, state = "readonly")
      entry_min_lr <- tkentry(frame_display_1, textvariable = min_lr_select_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      # Define widgets in frame_display_2
      butt_set <- tkbutton(frame_display_2, text = "    Set    ", cursor = "hand2",
                           command = function() set_display_2(tf, tclvalue(sn_q_select_var), tclvalue(sn_r_select_var), tclvalue(rel_select_var), as.numeric(tclvalue(min_lr_select_var))))

      # Grid widgets
      tkgrid(label_title_1, label_title_2, label_title_3, label_title_4, padx = 10, pady = 5)
      tkgrid(combo_q, combo_r, combo_rel, entry_min_lr, padx = 10, pady = 5)
      tkgrid(butt_set, padx = 10, pady = 5)

      # Grid frames
      tkgrid(frame_display_1)
      tkgrid(frame_display_2)
    }

    # The function to overwrite displayed data
    set_display_2 <- function(tf, sn_q_select, sn_r_select, rel_select, min_lr_select){

      # The number of comparisons between query and reference haplotypes
      n_data <- nrow(lr_all)

      # Investigate row indices to extract displayed data
      if(sn_q_select == "All"){
        pos_q <- 1:n_data
      }else{
        pos_q <- which(lr_all[, Query] == sn_q_select)
      }
      if(sn_r_select == "All"){
        pos_r <- 1:n_data
      }else{
        pos_r <- which(lr_all[, Reference] == sn_r_select)
      }
      if(rel_select == "All"){
        pos_rel <- 1:n_data
      }else{
        pos_rel <- which(lr_all[, AssumedRelationship] == rel_select)
      }
      pos_lr <- which(lr_all[, Total] >= min_lr_select)
      pos_extract <- intersect(intersect(intersect(pos_q, pos_r), pos_rel), pos_lr)

      # If there is at least one row index to extract displayed data
      if(length(pos_extract) != 0){

        # Destroy the scroll bar for displayed data
        scr1 <- get("scr1", pos = env_auto_result)
        tkdestroy(scr1)

        # Destroy the multi-list box for displayed data
        mlb_result <- get("mlb_result", pos = env_auto_result)
        tkdestroy(mlb_result)

        # Define the scroll bar for displayed data
        scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))

        # Define the multi-list box for displayed data
        mlb_result <- tk2mclistbox(frame_result_1, width = 85, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(mlb_result, "add", label = "Query", width = 15)
        tk2column(mlb_result, "add", label = "Reference", width = 15)
        tk2column(mlb_result, "add", label = "Assumed relationship", width = 40)
        tk2column(mlb_result, "add", label = "LR", width = 15)

        # Extract displayed data
        data_display <- lr_all[, list(Query, Reference, AssumedRelationship, Total)]
        data_display <- data_display[pos_extract, , drop = FALSE]
        data_display <- data_display[order(data_display$Total, decreasing = TRUE), , drop = FALSE]
        tk2insert.multi(mlb_result, "end", data_display)

        # Assign objects to the environment "env_auto_result"
        assign("scr1", scr1, envir = env_auto_result)
        assign("mlb_result", mlb_result, envir = env_auto_result)
        assign("data_display", data_display, envir = env_auto_result)

        # Grid the scroll bar and the multi-list box
        tkgrid(mlb_result, scr1)
        tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")

        # Destroy the top frame
        tkdestroy(tf)

      # If there is no row index to extract displayed data
      }else{
        tkmessageBox(message = "There is no data that meet the condition!", icon = "error", type = "ok")
      }
    }

    # The function to show data in detail
    show_detail <- function(){

      # Get the multi-list box for displayed data from the environment "env_auto_result"
      mlb_result <- get("mlb_result", pos = env_auto_result)

      # If the user does not select one line
      if(tclvalue(tkcurselection(mlb_result)) == ""){
        tkmessageBox(message = "Select one result!", icon = "error", type = "ok")

      # If the user selects one line
      }else{

        # Get displayed data from the environment "env_auto_result"
        data_display <- get("data_display", pos = env_auto_result)

        # The row index which the user selected
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_result))) + 1

        # Search indices to extract data
        data_select <- data_display[pos_select, ]
        sn_q_select <- data_select[, Query]
        sn_r_select <- data_select[, Reference]
        rel_select <- data_select[, AssumedRelationship]

        # Define a matrix for detailed data
        data_detail <- matrix("", n_l + 1, 6)
        colnames(data_detail) <- c("Locus",
                                  paste("Query (", sn_q_select, ")", sep = ""),
                                  paste("Reference (", sn_r_select, ")", sep = ""),
                                  "Likelihood (related)",
                                  "Likelihood (unrelated)",
                                  "LR")

        # 1st column for detailed data
        data_detail[, 1] <- c(names(myus), "Total")

        # 2nd column for detailed data
        query_select <- data_auto_q[data_auto_q$SampleName == sn_q_select, ]
        prof_query_select <- as.numeric(query_select[, -"SampleName", with = FALSE])
        data_detail[1:n_l, 2] <- display_gt(prof_query_select)

        # 3rd column for detailed data
        ref_select <- data_auto_r[data_auto_r$SampleName == sn_r_select, ]
        prof_ref_select <- as.numeric(ref_select[, -c("SampleName", "Relationship"), with = FALSE])
        data_detail[1:n_l, 3] <- display_gt(prof_ref_select)

        # 4th column for detailed data
        tmp <- like_h1_all[like_h1_all$Query == sn_q_select, ]
        tmp <- tmp[tmp$Reference == sn_r_select, ]
        tmp <- tmp[tmp$AssumedRelationship == rel_select, ]
        like_h1_select <- as.numeric(tmp[, -c("Query", "Reference", "AssumedRelationship"), with = FALSE])
        data_detail[, 4] <- signif(like_h1_select, digits = 4)

        # 5th column for detailed data
        tmp <- like_h2_all[like_h2_all$Query == sn_q_select, ]
        tmp <- tmp[tmp$Reference == sn_r_select, ]
        tmp <- tmp[tmp$AssumedRelationship == rel_select, ]
        like_h2_select <- as.numeric(tmp[, -c("Query", "Reference", "AssumedRelationship"), with = FALSE])
        data_detail[, 5] <- signif(like_h2_select, digits = 4)

        # 6th column for detailed data
        tmp <- lr_all[lr_all$Query == sn_q_select, ]
        tmp <- tmp[tmp$Reference == sn_r_select, ]
        tmp <- tmp[tmp$AssumedRelationship == rel_select, ]
        lr_select <- as.numeric(tmp[, -c("Query", "Reference", "AssumedRelationship"), with = FALSE])
        data_detail[, 6] <- signif(lr_select, digits = 4)

        # Make a top frame
        tf_detail <- tktoplevel()
        tkwm.title(tf_detail, "STR result in detail")

        # Define frames
        frame_detail_1 <- tkframe(tf_detail)
        frame_detail_2 <- tkframe(tf_detail)
        frame_detail_3 <- tkframe(tf_detail)

        # Define widgets in frame_detail_1
        label_title <- tklabel(frame_detail_1, text = paste("Relationship : ", rel_select, sep = ""))

        # Define a scrollbar for the multi-list box of the detailed data
        scr2 <- tkscrollbar(frame_detail_2, repeatinterval = 5, command = function(...) tkyview(mlb_detail, ...))

        # Define a multi-list box of the detailed data
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

        # Grid frames
        tkgrid(frame_detail_1, padx = 10, pady = 5)
        tkgrid(frame_detail_2, padx = 10, pady = 5)
        tkgrid(frame_detail_3, padx = 10, pady = 5)
      }
    }

    # Define the environment "env_auto_result"
    env_auto_result <- new.env(parent = globalenv())

    # Get database from the environment "env_proj"
    data_auto_q <- get("data_auto_q", pos = env_proj)
    data_auto_r <- get("data_auto_r", pos = env_proj)
    data_auto_af <- get("data_auto_af", pos = env_proj)

    # Get results from the environment "env_proj"
    like_h1_all <- get("like_h1_all", pos = env_proj)
    like_h2_all <- get("like_h2_all", pos = env_proj)
    lr_all <- get("lr_all", pos = env_proj)

    # Get parameters from the environment "env_proj"
    min_lr_auto <- get("min_lr_auto", pos = env_proj)
#    pibd_all <- get("pibd_all", pos = env_proj)
    myus <- get("myus", pos = env_proj)
#    maf <- get("maf", pos = env_proj)
#    meth_d <- get("meth_d", pos = env_proj)
#    pd <- get("pd", pos = env_proj)

    # The number of samples in each database
    ##n_q <- nrow(data_auto_q)
    ##n_r <- length(sn_auto_r_new)

    # The number of loci
    n_l <- length(myus)

    # Cumulative likelihood ratio
    ## clr_all <- lr_all[, Total]
    ## clr_all <- rbind(rel_auto_r_new, clr_all)
    ## rownames(clr_all) <- c("relationship", sn_auto_q)
    ## colnames(clr_all) <- sn_auto_r_new

    # Define vectors for making displayed data
    ## sn_auto_q_vec <- rep(sn_auto_q, n_r)
    ## sn_auto_r_vec <- as.vector(sapply(sn_auto_r_new, rep, n_q))
    ## rel_auto_vec <- as.vector(sapply(rel_auto_r_new, rep, n_q))
    ## clr_all_vec <- as.vector(lr_all[, , n_l + 1])

    # Define a matrix for the displayed data
    data_display <- lr_all[, list(Query, Reference, AssumedRelationship, Total)]
    data_display <- data_display[data_display$Total >= min_lr_auto, , drop = FALSE]
    setorder(data_display, -Total)

    # Reset frame_tab2
    tabs <- get("tabs", pos = env_gui)
    tab2 <- get("tab2", pos = env_gui)
    frame_tab2 <- get("frame_tab2", pos = env_gui)
    tkdestroy(frame_tab2)
    frame_tab2 <- tkframe(tab2)

    # Define frames
    frame_result_1 <- tkframe(frame_tab2)
    frame_result_2 <- tkframe(frame_tab2)

    # Define a scrollbar for the multi-list box of the displayed data
    scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))

    # Define a multi-list box of the displayed data
    mlb_result <- tk2mclistbox(frame_result_1, width = 85, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(mlb_result, "add", label = "Query", width = 15)
    tk2column(mlb_result, "add", label = "Reference", width = 15)
    tk2column(mlb_result, "add", label = "Assumed relationship", width = 40)
    tk2column(mlb_result, "add", label = "LR", width = 15)
    tk2insert.multi(mlb_result, "end", data_display)

    # Define widgets in frame_result_2
    butt_display <- tkbutton(frame_result_2, text = "    Set display    ", cursor = "hand2", command = function() set_display_1())
    butt_detail <- tkbutton(frame_result_2, text = "    Show detail    ", cursor = "hand2", command = function() show_detail())
    butt_export1 <- tkbutton(frame_result_2, text = "    Export displayed data    ", cursor = "hand2", command = function() export_data(get("data_display", pos = env_auto_result), FALSE))
    butt_export2 <- tkbutton(frame_result_2, text = "    Export All LRs    ", cursor = "hand2", command = function() export_data(clr_all, TRUE))

    # Grid widgets
    tkgrid(mlb_result, scr1)
    tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")
    tkgrid(butt_display, butt_detail, butt_export1, butt_export2, padx = 10, pady = 5)

    # Grid frames
    tkgrid(frame_result_1, padx = 10, pady = 5)
    tkgrid(frame_result_2)
    tkgrid(frame_tab2)

    # Assign objects to the environment "env_auto_result"
    assign("mlb_result", mlb_result, envir = env_auto_result)
    assign("scr1", scr1, envir = env_auto_result)
    assign("data_display", data_display, envir = env_auto_result)

    # Assign frame_tab2 to the environment "env_gui"
    assign("frame_tab2", frame_tab2, envir = env_gui)

    # Select tab2
    tk2notetab.select(tabs, "STR results")

  # If the autosomal STR analysis has not been finished
  }else{

    # Reset frame_tab2
    tab2 <- get("tab2", pos = env_gui)
    frame_tab2 <- get("frame_tab2", pos = env_gui)
    tkdestroy(frame_tab2)
    frame_tab2 <- tkframe(tab2)

    # Assign frame_tab2 to the environment "env_gui"
    assign("frame_tab2", frame_tab2, envir = env_gui)
  }
}
