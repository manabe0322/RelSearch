# The function to set criteria
set_criteria <- function(env_proj, env_gui){

  # The function to save criteria
  save_criteria <- function(){

    # Get the end sign from the environment "env_proj"
    fin_auto <- get("fin_auto", pos = env_proj)
    fin_y <- get("fin_y", pos = env_proj)
    fin_mt <- get("fin_mt", pos = env_proj)

    # Check whether all results are deleted or not
    if(any(fin_auto, fin_y, fin_mt)){
      sign_ok <- tclvalue(tkmessageBox(message = "All results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }else{
      sign_ok <- "ok"
    }

    # If all results are deleted
    if(sign_ok == "ok"){

      # Define a matrix for criteria
      criteria <- matrix("", 6, 2)
      colnames(criteria) <- c("Criteria", "Value")
      criteria[, 1] <- c("min_lr_auto",
                         "max_mismatch_y", "max_ignore_y", "max_mustep_y",
                         "min_share_mt", "max_mismatch_mt")
      criteria[, 2] <- c(tclvalue(min_lr_auto_var),
                         tclvalue(max_mismatch_y_var), tclvalue(max_ignore_y_var), tclvalue(max_mustep_y_var),
                         tclvalue(min_share_mt_var), tclvalue(max_mismatch_mt_var))

      # Save criteria
      write.csv(criteria, paste0(path_pack, "/extdata/parameters/criteria.csv"), row.names = FALSE)

      # Assign end signs
      assign("fin_auto", FALSE, envir = env_proj)
      assign("fin_y", FALSE, envir = env_proj)
      assign("fin_mt", FALSE, envir = env_proj)

      # Reset tab2, tab4, and tab6
      make_tab2(env_proj, env_gui)
      make_tab4(env_proj, env_gui)
      make_tab6(env_proj, env_gui)

      # Destroy the top frame
      tkdestroy(tf)
    }
  }

  # Get the package path
  path_pack <- get("path_pack", pos = env_gui)

  # Get file names in the folder "parameters"
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  # If the file "criteria.csv" is found
  if(is.element("criteria.csv", fn_par)){

    # Load the file "criteria.csv"
    criteria <- read.csv(paste0(path_pack, "/extdata/parameters/criteria.csv"), header = TRUE)

    # Extract values
    min_lr_auto <- criteria$Value[criteria$Criteria == "min_lr_auto"]
    max_mismatch_y <- criteria$Value[criteria$Criteria == "max_mismatch_y"]
    max_ignore_y <- criteria$Value[criteria$Criteria == "max_ignore_y"]
    max_mustep_y <- criteria$Value[criteria$Criteria == "max_mustep_y"]
    min_share_mt <- criteria$Value[criteria$Criteria == "min_share_mt"]
    max_mismatch_mt <- criteria$Value[criteria$Criteria == "max_mismatch_mt"]

  # If the file "criteria.csv" is missing
  }else{

    # Get default values from the environment "env_proj"
    min_lr_auto <- get("min_lr_auto_default", pos = env_proj)
    max_mismatch_y <- get("max_mismatch_y_default", pos = env_proj)
    max_ignore_y <- get("max_ignore_y_default", pos = env_proj)
    max_mustep_y <- get("max_mustep_y_default", pos = env_proj)
    min_share_mt <- get("min_share_mt_default", pos = env_proj)
    max_mismatch_mt <- get("max_mismatch_mt_default", pos = env_proj)

    # Define a matrix for criteria
    criteria <- matrix("", 6, 2)
    colnames(criteria) <- c("Criteria", "Value")
    criteria[, 1] <- c("min_lr_auto",
                       "max_mismatch_y", "max_ignore_y", "max_mustep_y",
                       "min_share_mt", "max_mismatch_mt")
    criteria[, 2] <- c(min_lr_auto, max_mismatch_y, max_ignore_y, max_mustep_y, min_share_mt, max_mismatch_mt)

    # Save the file "criteria.csv"
    write.csv(criteria, paste0(path_pack, "/extdata/parameters/criteria.csv"), row.names = FALSE)
  }

  # Define tcl variables
  min_lr_auto_var <- tclVar(min_lr_auto)
  max_mismatch_y_var <- tclVar(max_mismatch_y)
  max_ignore_y_var <- tclVar(max_ignore_y)
  max_mustep_y_var <- tclVar(max_mustep_y)
  min_share_mt_var <- tclVar(min_share_mt)
  max_mismatch_mt_var <- tclVar(max_mismatch_mt)

  # Create a top frame
  tf <- tktoplevel()
  tkwm.title(tf, "Set criteria")

  # Define widgets
  label_title_auto <- tklabel(tf, text = "Autosomal STR")
  label_min_lr_auto <- tklabel(tf, text = "    Minimum LR")
  entry_min_lr_auto <- tkentry(tf, textvariable = min_lr_auto_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
  label_title_y <- tklabel(tf, text = "Y-STR")
  label_max_mismatch_y <- tklabel(tf, text = "    Maximum number of mismatched loci")
  entry_max_mismatch_y <- tkentry(tf, textvariable = max_mismatch_y_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
  label_max_ignore_y <- tklabel(tf, text = "    Maximum number of ignored loci")
  entry_max_ignore_y <- tkentry(tf, textvariable = max_ignore_y_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
  label_max_mustep_y <- tklabel(tf, text = "    Maximum mutational steps")
  entry_max_mustep_y <- tkentry(tf, textvariable = max_mustep_y_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
  label_title_mt <- tklabel(tf, text = "mtDNA")
  label_min_share_mt <- tklabel(tf, text = "    Minimum shared length")
  entry_min_share_mt <- tkentry(tf, textvariable = min_share_mt_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
  label_max_mismatch_mt <- tklabel(tf, text = "    Maximum number of inconsistency")
  entry_max_mismatch_mt <- tkentry(tf, textvariable = max_mismatch_mt_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
  butt_save <- tkbutton(tf, text = "    Save    ", cursor = "hand2", command = function() save_criteria())

  # Grid widgets
  tkgrid(label_title_auto, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_min_lr_auto, entry_min_lr_auto, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_title_y, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_max_mismatch_y, entry_max_mismatch_y, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_max_ignore_y, entry_max_ignore_y, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_max_mustep_y, entry_max_mustep_y, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_title_mt, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_min_share_mt, entry_min_share_mt, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_max_mismatch_mt, entry_max_mismatch_mt, padx = 10, pady = 5, sticky = "w")
  tkgrid(butt_save, padx = 10, pady = 5, sticky = "w")
}

# Set mutation rates for autosomal STR
set_myu <- function(env_proj, env_gui){
  edit_myu_1 <- function(){
    mlb_myu <- get("mlb_myu", pos = env_myu)
    if(tclvalue(tkcurselection(mlb_myu)) == ""){
      tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
    }else{
      # Get the selected mutation rate
      pos_select <- as.numeric(tclvalue(tkcurselection(mlb_myu))) + 1
      myu_all <- get("myu_all", pos = env_myu)
      myu_select <- myu_all[pos_select]
      myu_select_var <- tclVar(myu_select)

      # Make a top frame
      tf <- tktoplevel()
      tkwm.title(tf, "Edit a mutation rate")

      # Define frames
      frame_edit_1 <- tkframe(tf)
      frame_edit_2 <- tkframe(tf)

      # Define widgets in frame_edit_1
      label_title_1 = tklabel(frame_edit_1, text = "Locus")
      label_title_2 = tklabel(frame_edit_1, text = "Mutation rate")
      label_locus <- tklabel(frame_edit_1, text = names(myu_select))
      entry_myu <- tkentry(frame_edit_1, textvariable = myu_select_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      # Define widgets in frame_edit_2
      butt_save <- tkbutton(frame_edit_2, text = "    Save    ", cursor = "hand2",
                            command = function() edit_myu_2(tf, mlb_myu, pos_select, as.numeric(tclvalue(myu_select_var))))

      # Grid widgets
      tkgrid(label_title_1, label_title_2, padx = 10, pady = 5)
      tkgrid(label_locus, entry_myu, padx = 10, pady = 5)
      tkgrid(butt_save, pady = 10)
      tkgrid(frame_edit_1, padx = 20)
      tkgrid(frame_edit_2)
    }
  }

  edit_myu_2 <- function(tf, mlb_myu, pos_select, myu_select){
    sign_ok <- "ok"
    fin_auto <- get("fin_auto", pos = env_proj)
    if(fin_auto){
      sign_ok <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(sign_ok == "ok"){
      assign("fin_auto", FALSE, envir = env_proj)
      make_tab2(env_proj, env_gui)

      myu_all <- get("myu_all", pos = env_myu)
      myu_all[pos_select] <- myu_select
      assign("myu_all", myu_all, envir = env_myu)
      myu_save <- cbind(names(myu_all), myu_all)
      colnames(myu_save) <- c("Marker", "Myu")
      write.csv(myu_save, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)

      # Get widgets from environment variable (env_myu)
      mlb_myu <- get("mlb_myu", pos = env_myu)
      scr1 <- get("scr1", pos = env_myu)

      # Destroy the scrollbar (scr1)
      tkdestroy(scr1)

      # Destroy the multi-list box (mlb_myu)
      tkdestroy(mlb_myu)

      # Define a scrollbar for a multi-list box (mlb_myu)
      scr1 <- tkscrollbar(frame_myu_1, repeatinterval = 5, command = function(...) tkyview(mlb_myu, ...))

      # Define a multi-list box (mlb_myu)
      mlb_myu <- tk2mclistbox(frame_myu_1, width = 30, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
      tk2column(mlb_myu, "add", label = "Locus", width = 15)
      tk2column(mlb_myu, "add", label = "Mutation rate", width = 15)
      tk2insert.multi(mlb_myu, "end", myu_save)

      # Grid widgets
      tkgrid(mlb_myu, scr1)
      tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")

      # Assign widgets to environment variable (env_myu)
      assign("mlb_myu", mlb_myu, envir = env_myu)
      assign("scr1", scr1, envir = env_myu)

      # Destroy the top frame
      tkdestroy(tf)
    }
  }

  add_myu_1 <- function(){
    # Define tclvalue
    locus_add_var <- tclVar("")
    myu_add_var <- tclVar("")

    # Make a top frame
    tf <- tktoplevel()
    tkwm.title(tf, "Add a mutation rate")

    # Define frames
    frame_add_1 <- tkframe(tf)
    frame_add_2 <- tkframe(tf)

    # Define widgets in frame_add_1
    label_title_1 <- tklabel(frame_add_1, text = "Locus name")
    label_title_2 <- tklabel(frame_add_1, text = "Mutation rate")
    entry_locus <- tkentry(frame_add_1, textvariable = locus_add_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    entry_myu <- tkentry(frame_add_1, textvariable = myu_add_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

    # Define widgets in frame_add_2
    butt_save <- tkbutton(frame_add_2, text = "    Save    ", cursor = "hand2",
                          command = function() add_myu_2(tf, tclvalue(locus_add_var), as.numeric(tclvalue(myu_add_var))))

    # Grid widgets
    tkgrid(label_title_1, label_title_2, padx = 10, pady = 5)
    tkgrid(entry_locus, entry_myu, padx = 10, pady = 5)
    tkgrid(butt_save, pady = 10)
    tkgrid(frame_add_1)
    tkgrid(frame_add_2)
  }

  add_myu_2 <- function(tf, locus_add, myu_add){
    sign_ok <- "ok"
    fin_auto <- get("fin_auto", pos = env_proj)
    if(fin_auto){
      sign_ok <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(sign_ok == "ok"){
      assign("fin_auto", FALSE, envir = env_proj)
      make_tab2(env_proj, env_gui)

      myu_all <- get("myu_all", pos = env_myu)
      locus_myu <- names(myu_all)
      myu_all[length(myu_all) + 1] <- myu_add
      names(myu_all) <- c(locus_myu, locus_add)
      assign("myu_all", myu_all, envir = env_myu)
      myu_save <- cbind(names(myu_all), myu_all)
      colnames(myu_save) <- c("Marker", "Myu")
      write.csv(myu_save, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)

      mlb_myu <- get("mlb_myu", pos = env_myu)
      tk2insert.multi(mlb_myu, "end", c(locus_add, myu_add))
      assign("mlb_myu", mlb_myu, envir = env_myu)
      tkdestroy(tf)
    }
  }

  delete_myu <- function(){
    # Get the multi-list box (mlb_myu) from environment variable (env_myu)
    mlb_myu <- get("mlb_myu", pos = env_myu)

    if(tclvalue(tkcurselection(mlb_myu)) == ""){
      tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
    }else{
      sign_ok <- "ok"
      fin_auto <- get("fin_auto", pos = env_proj)
      if(fin_auto){
        sign_ok <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
      }
      if(sign_ok == "ok"){
        assign("fin_auto", FALSE, envir = env_proj)
        make_tab2(env_proj, env_gui)

        myu_all <- get("myu_all", pos = env_myu)
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_myu))) + 1
        myu_all <- myu_all[-pos_select]
        assign("myu_all", myu_all, envir = env_myu)
        myu_save <- cbind(names(myu_all), myu_all)
        colnames(myu_save) <- c("Marker", "Myu")
        write.csv(myu_save, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)

        # Get the scrollbar (scr1) from environment variable (env_myu)
        scr1 <- get("scr1", pos = env_myu)

        # Destroy the scrollbar (scr1)
        tkdestroy(scr1)

        # Destroy the multi-list box (mlb_myu)
        tkdestroy(mlb_myu)

        # Define a scrollbar for a multi-list box (mlb_myu)
        scr1 <- tkscrollbar(frame_myu_1, repeatinterval = 5, command = function(...) tkyview(mlb_myu, ...))

        # Define a multi-list box (mlb_myu)
        mlb_myu <- tk2mclistbox(frame_myu_1, width = 30, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(mlb_myu, "add", label = "Locus", width = 15)
        tk2column(mlb_myu, "add", label = "Mutation rate", width = 15)
        tk2insert.multi(mlb_myu, "end", myu_save)

        # Grid widgets
        tkgrid(mlb_myu, scr1)
        tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")

        # Assign widgets to environment variable (env_myu)
        assign("mlb_myu", mlb_myu, envir = env_myu)
        assign("scr1", scr1, envir = env_myu)
      }
    }
  }

  # Get the package path
  path_pack <- get("path_pack", pos = env_gui)

  # Get file names in the folder "parameters"
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  # If the file "myu.csv" is found
  if(is.element("myu.csv", fn_par)){

    # Load the file "myu.csv"
    myu_data <- read.csv(paste0(path_pack, "/extdata/parameters/myu.csv"), header = TRUE)
    myu_data <- as.matrix(myu_data)
    locus_myu <- myu_data[, colnames(myu_data) == "Marker"]
    myu_all <- as.numeric(myu_data[, colnames(myu_data) == "Myu"])
    names(myu_all) <- locus_myu
  }else{
    myu_all <- get("myu_all_default", pos = env_proj)
    locus_myu <- names(myu_all)
    myu_save <- cbind(locus_myu, myu_all)
    colnames(myu_save) <- c("Marker", "Myu")
    write.csv(myu_save, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)
  }

  env_myu <- new.env(parent = globalenv())
  assign("myu_all", myu_all, envir = env_myu)

  # Make a top frame
  tf <- tktoplevel()
  tkwm.title(tf, "Set mutation rates")

  # Define frames
  frame_myu_1 <- tkframe(tf)
  frame_myu_2 <- tkframe(tf)

  # Define a scrollbar for a multi-list box (mlb_myu)
  scr1 <- tkscrollbar(frame_myu_1, repeatinterval = 5, command = function(...) tkyview(mlb_myu, ...))

  # Define a multi-list box (mlb_myu)
  mlb_myu <- tk2mclistbox(frame_myu_1, width = 30, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
  tk2column(mlb_myu, "add", label = "Locus", width = 15)
  tk2column(mlb_myu, "add", label = "Mutation rate", width = 15)
  tk2insert.multi(mlb_myu, "end", cbind(locus_myu, myu_all))

  # Define widgets in frame_myu2
  butt_edit <- tkbutton(frame_myu_2, text = "    Edit    ", cursor = "hand2", command = function() edit_myu_1())
  butt_add <- tkbutton(frame_myu_2, text = "    Add    ", cursor = "hand2", command = function() add_myu_1())
  butt_delete <- tkbutton(frame_myu_2, text = "    Delete    ", cursor = "hand2", command = function() delete_myu())

  # Grid widgets
  tkgrid(mlb_myu, scr1)
  tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")
  tkgrid(butt_edit, butt_add, butt_delete, padx = 20, pady = 5)
  tkgrid(frame_myu_1)
  tkgrid(frame_myu_2)

  # Assign widgets to environment variable (env_myu)
  assign("mlb_myu", mlb_myu, envir = env_myu)
  assign("scr1", scr1, envir = env_myu)
}

# Set relationships
set_rel <- function(env_proj, env_gui){

  draw_tree <- function(){

  }

  edit_rel <- function(){

    # Get the multi-list box from the environment "env_rel"
    mlb_rel <- get("mlb_rel", pos = env_rel)

    if(tclvalue(tkcurselection(mlb_rel)) == ""){
      tkmessageBox(message = "Select one relationship!", icon = "error", type = "ok")
    }else{

      # Get relationship data from the environment "env_rel"
      rel_data <- get("rel_data", pos = env_rel)

      # Get the selected relationship
      pos_select <- as.numeric(tclvalue(tkcurselection(mlb_rel))) + 1
      rel_select <- rel_data[pos_select, "Name_relationship"]

      # Define a tcl variable
      rel_select_var <- tclVar(rel_select)

      # Make a top frame
      tf <- tktoplevel()
      tkwm.title(tf, "Edit the name of a relationship")

      # Define widgets in tf
      label_title <- tklabel(tf, text = "Name of relationship")
      entry_name <- tkentry(tf, textvariable = rel_select_var, width = 40, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      butt_save <- tkbutton(tf, text = "    Save    ", cursor = "hand2", command = function() save_name_rel(tclvalue(rel_select_var)))

      # Grid widgets
      tkgrid(label_title, padx = 10, pady = 5)
      tkgrid(entry_name, padx = 10, pady = 5)
      tkgrid(butt_save, padx = 10, pady = 5)
    }
  }

  save_name_rel <- function(rel_select){

  }

  add_rel <- function(){

  }

  delete_rel <- function(){

    # Get the multi-list box from the environment "env_rel"
    mlb_rel <- get("mlb_rel", pos = env_rel)

    if(tclvalue(tkcurselection(mlb_myu)) == ""){
      tkmessageBox(message = "Select one relationship!", icon = "error", type = "ok")
    }else{

      # Get relationship data from the environment "env_rel"
      rel_data <- get("rel_data", pos = env_rel)

      # Get a row index deleted
      pos_select <- as.numeric(tclvalue(tkcurselection(mlb_rel))) + 1

      #
      rel_data <- rel_data[-pos_select, ]

      #
      assign("rel_data", rel_data, envir = env_rel)
    }
  }

  reset_rel <- function(){

  }

  # Get the package path
  path_pack <- get("path_pack", pos = env_gui)

  # Get file names in the folder "parameters"
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  if(is.element("rel.csv", fn_par)){
    rel_data <- read.csv(paste0(path_pack, "/extdata/parameters/rel.csv"), header = TRUE)
    rel_display <- rel_data[, 1:4]
  }else{
    rel_display <- NULL
  }

  # Make an environment
  env_rel <- new.env(parent = globalenv())

  # Make a top frame
  tf <- tktoplevel()
  tkwm.title(tf, "Set relationships")

  # Define frames
  frame_rel_1 <- tkframe(tf)
  frame_rel_2 <- tkframe(tf)

  # Define a scrollbar for the multi-list box of information on relationships
  scr1 <- tkscrollbar(frame_rel_1, repeatinterval = 5, command = function(...) tkyview(mlb_rel, ...))

  # Define a multi-list box of information on relationships
  mlb_rel <- tk2mclistbox(frame_rel_1, width = 100, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
  tk2column(mlb_rel, "add", label = "Name of relationship", width = 40)
  tk2column(mlb_rel, "add", label = "Degree", width = 20)
  tk2column(mlb_rel, "add", label = "Paternal lineage", width = 20)
  tk2column(mlb_rel, "add", label = "Maternal lineage", width = 20)
  tk2insert.multi(mlb_rel, "end",  rel_display)

  # Define widgets in frame_rel_2
  butt_tree <- tkbutton(frame_rel_2, text = "    Family tree    ", cursor = "hand2", command = function() draw_tree())
  butt_edit <- tkbutton(frame_rel_2, text = "    Edit    ", cursor = "hand2", command = function() edit_rel())
  butt_add <- tkbutton(frame_rel_2, text = "    Add    ", cursor = "hand2", command = function() add_rel())
  butt_delete <- tkbutton(frame_rel_2, text = "    Delete    ", cursor = "hand2", command = function() delete_rel())
  butt_reset <- tkbutton(frame_rel_2, text = "    Reset    ", cursor = "hand2", command = function() reset_rel())

  # Grid widgets
  tkgrid(mlb_rel, scr1)
  tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")
  tkgrid(butt_tree, butt_edit, butt_add, butt_delete, butt_reset, padx = 10, pady = 5)
  tkgrid(frame_rel_1)
  tkgrid(frame_rel_2)

  # Assign widgets to the environment "env_rel"
  assign("mlb_rel", mlb_rel, envir = env_rel)

  # Assign data to the environment "env_rel"
  assign("rel_data", rel_data, envir = env_rel)
}

# Set IBD probabilities for autosomal STR
set_pibd <- function(env_proj, env_gui){
  edit_pibd_1 <- function(){
    mlb_pibd <- get("mlb_pibd", pos = env_pibd)
    if(tclvalue(tkcurselection(mlb_pibd)) == ""){
      tkmessageBox(message = "Select one relationship!", icon = "error", type = "ok")
    }else{
      # Get the selected IBD probabilities
      pos_select <- as.numeric(tclvalue(tkcurselection(mlb_pibd))) + 1
      pibd_all <- get("pibd_all", pos = env_pibd)
      pibd_select <- pibd_all[pos_select, ]
      rel_select <- rownames(pibd_all)[pos_select]

      # Define tclvalue
      pibd2_select_var <- tclVar(pibd_select[1])
      pibd1_select_var <- tclVar(pibd_select[2])
      pibd0_select_var <- tclVar(pibd_select[3])

      # Make a top frame
      tf <- tktoplevel()
      tkwm.title(tf, "Edit IBD probabilities")

      # Define frames
      frame_edit_1 <- tkframe(tf)
      frame_edit_2 <- tkframe(tf)

      # Define widgets in frame_edit_1
      label_title_1 <- tklabel(frame_edit_1, text = "Relationship")
      label_title_2 <- tklabel(frame_edit_1, text = "Pr (IBD = 2)")
      label_title_3 <- tklabel(frame_edit_1, text = "Pr (IBD = 1)")
      label_title_4 <- tklabel(frame_edit_1, text = "Pr (IBD = 0)")
      label_rel <- tklabel(frame_edit_1, text = rel_select)
      entry_pibd2 <- tkentry(frame_edit_1, textvariable = pibd2_select_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      entry_pibd1 <- tkentry(frame_edit_1, textvariable = pibd1_select_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      entry_pibd0 <- tkentry(frame_edit_1, textvariable = pibd0_select_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      # Define widgets in frame_edit_2
      butt_save <- tkbutton(frame_edit_2, text = "    Save    ", cursor = "hand2",
                            command = function() edit_pibd_2(tf, mlb_pibd, pos_select, as.numeric(tclvalue(pibd2_select_var)), as.numeric(tclvalue(pibd1_select_var)), as.numeric(tclvalue(pibd0_select_var))))

      # Grid widgets
      tkgrid(label_title_1, label_title_2, label_title_3, label_title_4, padx = 10, pady = 5)
      tkgrid(label_rel, entry_pibd2, entry_pibd1, entry_pibd0, padx = 10, pady = 5)
      tkgrid(butt_save, pady = 10)
      tkgrid(frame_edit_1)
      tkgrid(frame_edit_2)
    }
  }

  edit_pibd_2 <- function(tf, mlb_pibd, pos_select, pibd2_select, pibd1_select, pibd0_select){
    sign_ok <- "ok"
    fin_auto <- get("fin_auto", pos = env_proj)
    if(fin_auto){
      sign_ok <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(sign_ok == "ok"){
      assign("fin_auto", FALSE, envir = env_proj)
      make_tab2(env_proj, env_gui)

      pibd_all <- get("pibd_all", pos = env_pibd)
      pibd_rel <- rownames(pibd_all)
      pibd_all[pos_select, ] <- c(pibd2_select, pibd1_select, pibd0_select)
      assign("pibd_all", pibd_all, envir = env_pibd)
      write.csv(pibd_all, paste0(path_pack, "/extdata/parameters/pibd.csv"))

      # Get widgets from environment variable (env_pibd)
      mlb_pibd <- get("mlb_pibd", pos = env_pibd)
      scr1 <- get("scr1", pos = env_pibd)

      # Destroy the scrollbar (scr1)
      tkdestroy(scr1)

      # Destroy the multi-list box (mlb_myu)
      tkdestroy(mlb_pibd)

      # Define a scrollbar for a multi-list box (mlb_pibd)
      scr1 <- tkscrollbar(frame_pibd_1, repeatinterval = 5, command = function(...) tkyview(mlb_pibd, ...))

      # Define a multi-list box (mlb_pibd)
      mlb_pibd <- tk2mclistbox(frame_pibd_1, width = 60, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
      tk2column(mlb_pibd, "add", label = "Relationship", width = 15)
      tk2column(mlb_pibd, "add", label = "Pr (IBD = 2)", width = 15)
      tk2column(mlb_pibd, "add", label = "Pr (IBD = 1)", width = 15)
      tk2column(mlb_pibd, "add", label = "Pr (IBD = 0)", width = 15)
      tk2insert.multi(mlb_pibd, "end", cbind(pibd_rel, pibd_all))

      # Grid widgets
      tkgrid(mlb_pibd, scr1)
      tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")

      # Assign widgets to environment variable (env_pibd)
      assign("mlb_pibd", mlb_pibd, envir = env_pibd)
      assign("scr1", scr1, envir = env_pibd)

      # Destroy the top frame
      tkdestroy(tf)
    }
  }

  add_pibd_1 <- function(){
    # Define tclvalue
    rel_add_var <- tclVar("")
    pibd2_add_var <- tclVar("")
    pibd1_add_var <- tclVar("")
    pibd0_add_var <- tclVar("")

    # Make a top frame
    tf <- tktoplevel()
    tkwm.title(tf, "Add IBD probabilities")

    # Define frames
    frame_add_1 <- tkframe(tf)
    frame_add_2 <- tkframe(tf)

    # Define widgets in frame_add_1
    label_title_1 <- tklabel(frame_add_1, text = "Relationship")
    label_title_2 <- tklabel(frame_add_1, text = "Pr (IBD = 2)")
    label_title_3 <- tklabel(frame_add_1, text = "Pr (IBD = 1)")
    label_title_4 <- tklabel(frame_add_1, text = "Pr (IBD = 0)")
    entry_rel <- tkentry(frame_add_1, textvariable = rel_add_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    entry_pibd2 <- tkentry(frame_add_1, textvariable = pibd2_add_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    entry_pibd1 <- tkentry(frame_add_1, textvariable = pibd1_add_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    entry_pibd0 <- tkentry(frame_add_1, textvariable = pibd0_add_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

    # Define widgets in frame_add_2
    butt_save <- tkbutton(frame_add_2, text = "    Save    ", cursor = "hand2",
                          command = function() add_pibd_2(tf, tclvalue(rel_add_var), as.numeric(tclvalue(pibd2_add_var)), as.numeric(tclvalue(pibd1_add_var)), as.numeric(tclvalue(pibd0_add_var))))

    # Grid widgets
    tkgrid(label_title_1, label_title_2, label_title_3, label_title_4, padx = 10, pady = 5)
    tkgrid(entry_rel, entry_pibd2, entry_pibd1, entry_pibd0, padx = 10, pady = 5)
    tkgrid(butt_save, pady = 10)
    tkgrid(frame_add_1)
    tkgrid(frame_add_2)
  }

  add_pibd_2 <- function(tf, rel_add, pibd2_add, pibd1_add, pibd0_add){
    sign_ok <- "ok"
    fin_auto <- get("fin_auto", pos = env_proj)
    if(fin_auto){
      sign_ok <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(sign_ok == "ok"){
      assign("fin_auto", FALSE, envir = env_proj)
      make_tab2(env_proj, env_gui)

      pibd_all <- get("pibd_all", pos = env_pibd)
      pibd_rel <- rownames(pibd_all)
      pibd_all <- rbind(pibd_all, c(pibd2_add, pibd1_add, pibd0_add))
      rownames(pibd_all) <- c(pibd_rel, rel_add)
      assign("pibd_all", pibd_all, envir = env_pibd)
      write.csv(pibd_all, paste0(path_pack, "/extdata/parameters/pibd.csv"))

      mlb_pibd <- get("mlb_pibd", pos = env_pibd)
      tk2insert.multi(mlb_pibd, "end", c(rel_add, pibd2_add, pibd1_add, pibd0_add))
      assign("mlb_pibd", mlb_pibd, envir = env_pibd)
      tkdestroy(tf)
    }
  }

  delete_pibd <- function(){
    # Get the multi-list box (mlb_pibd) from environment variable (env_pibd)
    mlb_pibd <- get("mlb_pibd", pos = env_pibd)
    if(tclvalue(tkcurselection(mlb_pibd)) == ""){
      tkmessageBox(message = "Select one relationship!", icon = "error", type = "ok")
    }else{
      sign_ok <- "ok"
      fin_auto <- get("fin_auto", pos = env_proj)
      if(fin_auto){
        sign_ok <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
      }
      if(sign_ok == "ok"){
        assign("fin_auto", FALSE, envir = env_proj)
        make_tab2(env_proj, env_gui)

        pibd_all <- get("pibd_all", pos = env_pibd)
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_pibd))) + 1
        pibd_all <- pibd_all[-pos_select, , drop = FALSE]
        pibd_rel <- rownames(pibd_all)
        assign("pibd_all", pibd_all, envir = env_pibd)
        write.csv(pibd_all, paste0(path_pack, "/extdata/parameters/pibd.csv"))

        # Get the scrollbar (scr1) from environment variable (env_pibd)
        scr1 <- get("scr1", pos = env_pibd)

        # Destroy the scrollbar (scr1)
        tkdestroy(scr1)

        # Destroy the multi-list box (mlb_myu)
        tkdestroy(mlb_pibd)

        # Define a scrollbar for a multi-list box (mlb_pibd)
        scr1 <- tkscrollbar(frame_pibd_1, repeatinterval = 5, command = function(...) tkyview(mlb_pibd, ...))

        # Define a multi-list box (mlb_pibd)
        mlb_pibd <- tk2mclistbox(frame_pibd_1, width = 60, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(mlb_pibd, "add", label = "Relationship", width = 15)
        tk2column(mlb_pibd, "add", label = "Pr (IBD = 2)", width = 15)
        tk2column(mlb_pibd, "add", label = "Pr (IBD = 1)", width = 15)
        tk2column(mlb_pibd, "add", label = "Pr (IBD = 0)", width = 15)
        tk2insert.multi(mlb_pibd, "end", cbind(pibd_rel, pibd_all))

        # Grid widgets
        tkgrid(mlb_pibd, scr1)
        tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")

        # Assign widgets to environment variable (env_pibd)
        assign("mlb_pibd", mlb_pibd, envir = env_pibd)
        assign("scr1", scr1, envir = env_pibd)
      }
    }
  }

  path_pack <- get("path_pack", pos = env_gui)
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))
  if(is.element("pibd.csv", fn_par)){
    pibd_all <- read.csv(paste0(path_pack, "/extdata/parameters/pibd.csv"), header = TRUE, row.names = 1)
    pibd_all <- as.matrix(pibd_all)
  }else{
    pibd_all <- get("pibd_all_default", pos = env_proj)
    write.csv(pibd_all, paste0(path_pack, "/extdata/parameters/pibd.csv"))
  }
  env_pibd <- new.env(parent = globalenv())
  assign("pibd_all", pibd_all, envir = env_pibd)
  pibd_rel <- rownames(pibd_all)

  # Make a top frame
  tf <- tktoplevel()
  tkwm.title(tf, "Set IBD probabilities")

  # Define frames
  frame_pibd_1 <- tkframe(tf)
  frame_pibd_2 <- tkframe(tf)

  # Define a scrollbar for a multi-list box (mlb_pibd)
  scr1 <- tkscrollbar(frame_pibd_1, repeatinterval = 5, command = function(...) tkyview(mlb_pibd, ...))

  # Define a multi-list box (mlb_pibd)
  mlb_pibd <- tk2mclistbox(frame_pibd_1, width = 60, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
  tk2column(mlb_pibd, "add", label = "Relationship", width = 15)
  tk2column(mlb_pibd, "add", label = "Pr (IBD = 2)", width = 15)
  tk2column(mlb_pibd, "add", label = "Pr (IBD = 1)", width = 15)
  tk2column(mlb_pibd, "add", label = "Pr (IBD = 0)", width = 15)
  tk2insert.multi(mlb_pibd, "end", cbind(pibd_rel, pibd_all))

  # Define widgets in frame_pibd_2
  butt_edit <- tkbutton(frame_pibd_2, text = "    Edit    ", cursor = "hand2", command = function() edit_pibd_1())
  butt_add <- tkbutton(frame_pibd_2, text = "    Add    ", cursor = "hand2", command = function() add_pibd_1())
  butt_delete <- tkbutton(frame_pibd_2, text = "    Delete    ", cursor = "hand2", command = function() delete_pibd())

  # Grid widgets
  tkgrid(mlb_pibd, scr1)
  tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")
  tkgrid(butt_edit, butt_add, butt_delete, padx = 10, pady = 5)
  tkgrid(frame_pibd_1)
  tkgrid(frame_pibd_2)

  # Assign widgets to environment variable (env_pibd)
  assign("mlb_pibd", mlb_pibd, envir = env_pibd)
  assign("scr1", scr1, envir = env_pibd)
}

# Set analysis method for autosomal STR
set_auto <- function(env_proj, env_gui){
  save_auto <- function(){
    sign_ok <- "ok"
    fin_auto <- get("fin_auto", pos = env_proj)
    if(fin_auto){
      sign_ok <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(sign_ok == "ok"){
      par_auto <- matrix("", 3, 2)
      colnames(par_auto) <- c("Parameter", "Value")
      par_auto[, 1] <- c("Minimum allele frequency", "Drop-out of query alleles", "Probability of drop-out")
      par_auto[, 2] <- c(tclvalue(maf_var), tclvalue(meth_d_var), tclvalue(pd_var))
      write.csv(par_auto, paste0(path_pack, "/extdata/parameters/par_auto.csv"), row.names = FALSE)
      assign("fin_auto", FALSE, envir = env_proj)
      make_tab2(env_proj, env_gui)
      tkdestroy(tf)
    }
  }

  path_pack <- get("path_pack", pos = env_gui)
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))
  if(is.element("par_auto.csv", fn_par)){
    par_auto <- read.csv(paste0(path_pack, "/extdata/parameters/par_auto.csv"), header = TRUE)
    maf <- par_auto$Value[par_auto$Parameter == "Minimum allele frequency"]
    meth_d <- par_auto$Value[par_auto$Parameter == "Drop-out of query alleles"]
    pd <- par_auto$Value[par_auto$Parameter == "Probability of drop-out"]
  }else{
    maf <- get("maf_default", pos = env_proj)
    meth_d <- get("meth_d_default", pos = env_proj)
    pd <- get("pd_default", pos = env_proj)
    par_auto <- matrix("", 3, 2)
    colnames(par_auto) <- c("Parameter", "Value")
    par_auto[, 1] <- c("Minimum allele frequency", "Drop-out of query alleles", "Probability of drop-out")
    par_auto[, 2] <- c(maf, meth_d, pd)
    write.csv(par_auto, paste0(path_pack, "/extdata/parameters/par_auto.csv"), row.names = FALSE)
  }

  # Define tclvalue
  maf_var <- tclVar(maf)
  meth_d_var <- tclVar(meth_d)
  pd_var <- tclVar(pd)

  # Make a top frame
  tf <- tktoplevel()
  tkwm.title(tf, "Set parameters for autosomal STRs")

  # Define widgets for maf
  label_maf <- tklabel(tf, text = "Minimum allele frequency")
  entry_maf <- tkentry(tf, textvariable = maf_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

  # Define widgets for meth_d
  label_meth_d <- tklabel(tf, text = "Drop-out of query alleles")
  radio_meth_d0 <- tkradiobutton(tf, anchor = "w", width = 60, state = "normal", text = "Not consider", variable = meth_d_var, value = 0)
  radio_meth_d1 <- tkradiobutton(tf, anchor = "w", width = 60, state = "normal", text = "Consider only in the case that one allele is designated", variable = meth_d_var, value = 1)
  radio_meth_d2 <- tkradiobutton(tf, anchor = "w", width = 60, state = "normal", text = "Consider also in the case that two alleles in homozygotes are designated", variable = meth_d_var, value = 2)

  # Define widgets for pd
  label_pd <- tklabel(tf, text = "Probability of drop-out")
  entry_pd <- tkentry(tf, textvariable = pd_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

  # Define a save button
  butt_save <- tkbutton(tf, text = "    Save    ", cursor = "hand2", command = function() save_auto())

  # Grid widgets
  tkgrid(label_maf, entry_maf, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_meth_d, radio_meth_d0, padx = 10, pady = 5, sticky = "w")
  tkgrid(tklabel(tf, text = ""), radio_meth_d1, padx = 10, pady = 5, sticky = "w")
  tkgrid(tklabel(tf, text = ""), radio_meth_d2, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_pd, entry_pd, padx = 10, pady = 5, sticky = "w")
  tkgrid(butt_save, padx = 10, pady = 5, sticky = "w")
}
