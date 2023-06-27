#############################
# The function to make tab2 #
#############################

make_tab2 <- function(env_proj, env_gui){

  # Get the end-sign of the analysis from the environment "env_proj"
  fin_auto <- get("fin_auto", pos = env_proj)
  fin_y <- get("fin_y", pos = env_proj)
  fin_mt <- get("fin_mt", pos = env_proj)

  if(any(fin_auto, fin_y, fin_mt)){

    show_detail <- function(){

      # Get the multi-list box for displayed data from the environment "env_result"
      mlb_result <- get("mlb_result", pos = env_result)

      # If the user does not select one line
      if(tclvalue(tkcurselection(mlb_result)) == ""){
        tkmessageBox(message = "Select one result!", icon = "error", type = "ok")

      # If the user selects one line
      }else{

        # Get displayed data from the environment "env_result"
        data_display <- get("data_display", pos = env_result)

        # Extract selected data
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_result))) + 1
        sn_v_select <- data_display[pos_select, "Victim"]
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
        label_sn_v_1 <- tklabel(frame_detail_sn, text = "    Victim")
        label_sn_v_2 <- tklabel(frame_detail_sn, text = paste0(": ", sn_v_select))
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
        tkgrid(label_sn_v_1, label_sn_v_2, padx = 10, pady = 5, sticky = "w")
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

    # Define an environment variable (env_result)
    env_result <- new.env(parent = globalenv())

    # Get the data table for all results
    dt_result <- get("dt_result", pos = env_proj)

    dt_display <- dt_result[, list(Victim, Reference, AssumedRel, LR_Total, EstimatedRel, Paternal, Maternal)]
    dt_display <- dt_display[EstimatedRel != "" | Paternal == "support" | Maternal == "support"]
    setorder(dt_display, cols = - "LR_Total", na.last = TRUE)

    # Reset frame_tab2
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
    mlb_result <- tk2mclistbox(frame_result_1, width = 140, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(mlb_result, "add", label = "Victim", width = 10)
    tk2column(mlb_result, "add", label = "Reference", width = 10)
    tk2column(mlb_result, "add", label = "Assumed relationship", width = 30)
    tk2column(mlb_result, "add", label = "LR", width = 20)
    tk2column(mlb_result, "add", label = "Estimated relationship", width = 30)
    tk2column(mlb_result, "add", label = "Paternal lineage", width = 20)
    tk2column(mlb_result, "add", label = "Maternal lineage", width = 20)
    tk2insert.multi(mlb_result, "end", dt_display)

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
    tkgrid(frame_tab2)

    # Assign data to the environment variable (env_result)
    assign("mlb_result", mlb_result, envir = env_result)
    assign("scr1", scr1, envir = env_result)
    assign("dt_display", dt_display, envir = env_result)

    # Assign data to the environment variable (env_gui)
    assign("frame_tab2", frame_tab2, envir = env_gui)

    # Select tab2
    tk2notetab.select(tabs, "Results")
  }
}
