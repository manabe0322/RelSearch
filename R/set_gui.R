################################################################
# The function to check whether all results are deleted or not #
################################################################

check_delete_all <- function(env_proj){

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

  return(sign_ok)
}


##############################################################################
# The function to check whether the autosomal STR results are deleted or not #
##############################################################################

check_delete_auto <- function(env_proj){

  # Get the end sign of the autosomal STR analysis from the environment "env_proj"
  fin_auto <- get("fin_auto", pos = env_proj)

  # Check whether the autosomal STR results are deleted or not
  if(fin_auto){
    sign_ok <- tclvalue(tkmessageBox(message = "Autosomal STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
  }else{
    sign_ok <- "ok"
  }

  return(sign_ok)
}


################################
# The function to set criteria #
################################

set_criteria <- function(env_proj, env_gui){

  ##################################################
  # The function to create the file "criteria.csv" #
  ##################################################

  create_criteria_csv <- function(min_lr_auto, max_mismatch_y, max_ignore_y, max_mustep_y, max_mismatch_mt, min_share_mt){

    # Define a data.table for criteria
    names_criteria <- c("min_lr_auto",
                        "max_mismatch_y", "max_ignore_y", "max_mustep_y",
                        "max_mismatch_mt", "min_share_mt")
    values_criteria <- c(min_lr_auto, max_mismatch_y, max_ignore_y, max_mustep_y, max_mismatch_mt, min_share_mt)
    dt_criteria <- data.table(Criteria = names_criteria, Value = values_criteria)

    # Save the data.table as a .csv file"
    write.csv(dt_criteria, paste0(path_pack, "/extdata/parameters/criteria.csv"), row.names = FALSE)
  }

  #################################
  # The function to save criteria #
  #################################

  save_criteria <- function(){

    # Check whether all results are deleted or not
    sign_ok <- check_delete_all(env_proj)

    # If all results are deleted
    if(sign_ok == "ok"){

      # Create the file "criteria.csv"
      create_criteria_csv(tclvalue(min_lr_auto_var),
                          tclvalue(max_mismatch_y_var), tclvalue(max_ignore_y_var), tclvalue(max_mustep_y_var),
                          tclvalue(max_mismatch_mt_var), tclvalue(min_share_mt_var))

      # Reset the environment variables
      set_env_proj_auto(env_proj, FALSE)
      set_env_proj_y(env_proj, FALSE)
      set_env_proj_mt(env_proj, FALSE)

      # Reset tab2
      make_tab2(env_proj, env_gui)

      # Destroy the top frame
      tkdestroy(tf)
    }
  }

  ##################################
  # The function to reset criteria #
  ##################################

  reset_criteria <- function(){

    # Check whether all results are deleted or not
    sign_ok <- check_delete_all(env_proj)

    # If all results are deleted
    if(sign_ok == "ok"){

      # Get default values from the environment "env_proj"
      dt_criteria <- get("dt_criteria_default", pos = env_proj)

      # Reset tcl variables
      tclvalue(min_lr_auto_var) <- dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"]
      tclvalue(max_mismatch_y_var) <- dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_y"]
      tclvalue(max_ignore_y_var) <- dt_criteria$Value[dt_criteria$Criteria == "max_ignore_y"]
      tclvalue(max_mustep_y_var) <- dt_criteria$Value[dt_criteria$Criteria == "max_mustep_y"]
      tclvalue(max_mismatch_mt_var) <- dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_mt"]
      tclvalue(min_share_mt_var) <- dt_criteria$Value[dt_criteria$Criteria == "min_share_mt"]

      # Create the file "criteria.csv"
      create_criteria_csv(tclvalue(min_lr_auto_var),
                          tclvalue(max_mismatch_y_var), tclvalue(max_ignore_y_var), tclvalue(max_mustep_y_var),
                          tclvalue(max_mismatch_mt_var), tclvalue(min_share_mt_var))

      # Reset the environment variables
      set_env_proj_auto(env_proj, FALSE)
      set_env_proj_y(env_proj, FALSE)
      set_env_proj_mt(env_proj, FALSE)

      # Reset tab2
      make_tab2(env_proj, env_gui)
    }
  }

  # Get the package path
  path_pack <- get("path_pack", pos = env_gui)

  # Get file names in the folder "parameters"
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  # If the file "criteria.csv" is found
  if(is.element("criteria.csv", fn_par)){

    # Load the file "criteria.csv"
    dt_criteria <- fread(paste0(path_pack, "/extdata/parameters/criteria.csv"))

    # Extract values
    min_lr_auto <- dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"]
    max_mismatch_y <- dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_y"]
    max_ignore_y <- dt_criteria$Value[dt_criteria$Criteria == "max_ignore_y"]
    max_mustep_y <- dt_criteria$Value[dt_criteria$Criteria == "max_mustep_y"]
    max_mismatch_mt <- dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_mt"]
    min_share_mt <- dt_criteria$Value[dt_criteria$Criteria == "min_share_mt"]

  # If the file "criteria.csv" is missing
  }else{

    # Get default values from the environment "env_proj"
    dt_criteria <- get("dt_criteria_default", pos = env_proj)

    # Extract values
    min_lr_auto <- dt_criteria$Value[dt_criteria$Criteria == "min_lr_auto"]
    max_mismatch_y <- dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_y"]
    max_ignore_y <- dt_criteria$Value[dt_criteria$Criteria == "max_ignore_y"]
    max_mustep_y <- dt_criteria$Value[dt_criteria$Criteria == "max_mustep_y"]
    max_mismatch_mt <- dt_criteria$Value[dt_criteria$Criteria == "max_mismatch_mt"]
    min_share_mt <- dt_criteria$Value[dt_criteria$Criteria == "min_share_mt"]

    # Create the file "criteria.csv"
    create_criteria_csv(min_lr_auto, max_mismatch_y, max_ignore_y, max_mustep_y, max_mismatch_mt, min_share_mt)
  }

  # Define tcl variables
  min_lr_auto_var <- tclVar(min_lr_auto)
  max_mismatch_y_var <- tclVar(max_mismatch_y)
  max_ignore_y_var <- tclVar(max_ignore_y)
  max_mustep_y_var <- tclVar(max_mustep_y)
  max_mismatch_mt_var <- tclVar(max_mismatch_mt)
  min_share_mt_var <- tclVar(min_share_mt)

  # Create a top frame
  tf <- tktoplevel()
  tkwm.title(tf, "Set criteria")

  # Define frames
  frame_criteria_1 <- tkframe(tf)
  frame_criteria_2 <- tkframe(tf)

  # Define widgets in frame_criteria_1
  label_title_auto <- tklabel(frame_criteria_1, text = "Autosomal STR")
  label_min_lr_auto <- tklabel(frame_criteria_1, text = "    Minimum LR")
  entry_min_lr_auto <- tkentry(frame_criteria_1, textvariable = min_lr_auto_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
  label_title_y <- tklabel(frame_criteria_1, text = "Y-STR")
  label_max_mismatch_y <- tklabel(frame_criteria_1, text = "    Maximum number of mismatched loci")
  entry_max_mismatch_y <- tkentry(frame_criteria_1, textvariable = max_mismatch_y_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
  label_max_ignore_y <- tklabel(frame_criteria_1, text = "    Maximum number of ignored loci")
  entry_max_ignore_y <- tkentry(frame_criteria_1, textvariable = max_ignore_y_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
  label_max_mustep_y <- tklabel(frame_criteria_1, text = "    Maximum mutational steps")
  entry_max_mustep_y <- tkentry(frame_criteria_1, textvariable = max_mustep_y_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
  label_title_mt <- tklabel(frame_criteria_1, text = "mtDNA")
  label_max_mismatch_mt <- tklabel(frame_criteria_1, text = "    Maximum number of inconsistency")
  entry_max_mismatch_mt <- tkentry(frame_criteria_1, textvariable = max_mismatch_mt_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
  label_min_share_mt <- tklabel(frame_criteria_1, text = "    Minimum shared length")
  entry_min_share_mt <- tkentry(frame_criteria_1, textvariable = min_share_mt_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

  # Define widgets in frame_criteria_2
  butt_save <- tkbutton(frame_criteria_2, text = "    Save    ", cursor = "hand2", command = function() save_criteria())
  butt_reset <- tkbutton(frame_criteria_2, text = "    Reset    ", cursor = "hand2", command = function() reset_criteria())

  # Grid widgets in frame_criteria_1
  tkgrid(label_title_auto, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_min_lr_auto, entry_min_lr_auto, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_title_y, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_max_mismatch_y, entry_max_mismatch_y, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_max_ignore_y, entry_max_ignore_y, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_max_mustep_y, entry_max_mustep_y, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_title_mt, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_max_mismatch_mt, entry_max_mismatch_mt, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_min_share_mt, entry_min_share_mt, padx = 10, pady = 5, sticky = "w")

  # Grid widgets in frame_criteria_2
  tkgrid(butt_save, butt_reset, padx = 10, pady = 5, sticky = "w")

  # Grid frames
  tkgrid(frame_criteria_1, sticky = "w")
  tkgrid(frame_criteria_2, sticky = "w")
}


########################################################
# The function to set mutation rates for autosomal STR #
########################################################

set_myu <- function(env_proj, env_gui){

  #############################################
  # The function to create the file "myu.csv" #
  #############################################

  create_myu_csv <- function(dt_myu){
    write.csv(dt_myu, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)
  }

  ########################################
  # The function to edit a mutation rate #
  ########################################

  edit_myu_1 <- function(){

    # Get the multi-list box from the environment "env_myu"
    mlb_myu <- get("mlb_myu", pos = env_myu)

    # Check whether the user selects one locus or not
    if(tclvalue(tkcurselection(mlb_myu)) == ""){
      tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
    }else{

      # Get the selected mutation rate
      pos_select <- as.numeric(tclvalue(tkcurselection(mlb_myu))) + 1
      dt_myu <- get("dt_myu", pos = env_myu)
      myu_select <- dt_myu[pos_select, Myu]
      myu_select_var <- tclVar(myu_select)

      # Create a top frame
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

      # Grid frames
      tkgrid(frame_edit_1, padx = 20)
      tkgrid(frame_edit_2)
    }
  }

  #################################################
  # The function to save the edited mutation rate #
  #################################################

  edit_myu_2 <- function(tf, mlb_myu, pos_select, myu_select){

    # Check whether the autosomal STR results are deleted or not
    sign_ok <- check_delete_auto(env_proj)

    # If the autosomal STR results are deleted
    if(sign_ok == "ok"){

      # Get the current mutation rates from the environment "env_myu"
      dt_myu <- get("dt_myu", pos = env_myu)

      # Update mutation rates
      dt_myu[pos_select, "Myu"] <- myu_select

      # Update the file "myu.csv"
      create_myu_csv(dt_myu)

      # Get widgets from the environment "env_myu"
      scr1 <- get("scr1", pos = env_myu)
      mlb_myu <- get("mlb_myu", pos = env_myu)

      # Destroy widgets
      tkdestroy(scr1)
      tkdestroy(mlb_myu)

      # Define a scrollbar for the multi-list box of mutation rates
      scr1 <- tkscrollbar(frame_myu_1, repeatinterval = 5, command = function(...) tkyview(mlb_myu, ...))

      # Define a multi-list box of mutation rates
      mlb_myu <- tk2mclistbox(frame_myu_1, width = 30, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
      tk2column(mlb_myu, "add", label = "Locus", width = 15)
      tk2column(mlb_myu, "add", label = "Mutation rate", width = 15)
      tk2insert.multi(mlb_myu, "end", dt_myu)

      # Grid widgets
      tkgrid(mlb_myu, scr1)
      tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")

      # Assign widgets to the environment "env_myu"
      assign("scr1", scr1, envir = env_myu)
      assign("mlb_myu", mlb_myu, envir = env_myu)

      # Assign mutation rates to the environment "env_myu"
      assign("dt_myu", dt_myu, envir = env_myu)

      # Reset environment variables for autosomal STR
      set_env_proj_auto(env_proj, FALSE)

      # Reset tab2
      make_tab2(env_proj, env_gui)

      # Destroy the top frame
      tkdestroy(tf)
    }
  }

  #######################################
  # The function to add a mutation rate #
  #######################################

  add_myu_1 <- function(){

    # Define tcl variables
    locus_add_var <- tclVar("")
    myu_add_var <- tclVar("")

    # Create a top frame
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

    # Grid frames
    tkgrid(frame_add_1)
    tkgrid(frame_add_2)
  }

  ################################################
  # The function to save the added mutation rate #
  ################################################

  add_myu_2 <- function(tf, locus_add, myu_add){

    # Check whether the autosomal STR results are deleted or not
    sign_ok <- check_delete_auto(env_proj)

    # If the autosomal STR results are deleted
    if(sign_ok == "ok"){

      # Get the current mutation rates from the environment "env_myu"
      dt_myu <- get("dt_myu", pos = env_myu)

      # Update mutation rates
      dt_new <- data.table(Marker = locus_add, Myu = myu_add)
      dt_myu <- rbind(dt_myu, dt_new)

      # Update the file "myu.csv"
      create_myu_csv(dt_myu)

      # Get the multi-list box of mutation rates from the environment "env_myu"
      mlb_myu <- get("mlb_myu", pos = env_myu)

      # update the multi-list box
      tk2insert.multi(mlb_myu, "end", c(locus_add, myu_add))

      # Assign the multi-list box to the environment "env_myu"
      assign("mlb_myu", mlb_myu, envir = env_myu)

      # Assign mutation rates to the environment "env_myu"
      assign("dt_myu", dt_myu, envir = env_myu)

      # Reset environment variables for autosomal STR
      set_env_proj_auto(env_proj, FALSE)

      # Reset tab2
      make_tab2(env_proj, env_gui)

      # Destroy the top frame
      tkdestroy(tf)
    }
  }

  ##########################################
  # The function to delete a mutation rate #
  ##########################################

  delete_myu <- function(){

    # Get the multi-list box of mutation rates from the environment "env_myu"
    mlb_myu <- get("mlb_myu", pos = env_myu)

    # Check whether the user selects one locus or not
    if(tclvalue(tkcurselection(mlb_myu)) == ""){
      tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
    }else{

      # Check whether the autosomal STR results are deleted or not
      sign_ok <- check_delete_auto(env_proj)

      # If the autosomal STR results are deleted
      if(sign_ok == "ok"){

        # Get the current mutation rates from the environment "env_myu"
        dt_myu <- get("dt_myu", pos = env_myu)

        # Update mutation rates
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_myu))) + 1
        dt_myu <- dt_myu[-pos_select, ]

        # Update the file "myu.csv"
        create_myu_csv(dt_myu)

        # Get the scrollbar from the environment "env_myu"
        scr1 <- get("scr1", pos = env_myu)

        # Destroy widgets
        tkdestroy(scr1)
        tkdestroy(mlb_myu)

        # Define a scrollbar for the multi-list box of mutation rates
        scr1 <- tkscrollbar(frame_myu_1, repeatinterval = 5, command = function(...) tkyview(mlb_myu, ...))

        # Define a multi-list box of mutation rates
        mlb_myu <- tk2mclistbox(frame_myu_1, width = 30, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(mlb_myu, "add", label = "Locus", width = 15)
        tk2column(mlb_myu, "add", label = "Mutation rate", width = 15)
        tk2insert.multi(mlb_myu, "end", dt_myu)

        # Grid widgets
        tkgrid(mlb_myu, scr1)
        tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")

        # Assign widgets to environment variable (env_myu)
        assign("mlb_myu", mlb_myu, envir = env_myu)
        assign("scr1", scr1, envir = env_myu)

        # Assign mutation rates to the environment "env_myu"
        assign("dt_myu", dt_myu, envir = env_myu)

        # Reset environment variables for autosomal STR
        set_env_proj_auto(env_proj, FALSE)

        # Reset tab2
        make_tab2(env_proj, env_gui)
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
    dt_myu <- fread(paste0(path_pack, "/extdata/parameters/myu.csv"))

  # If the file "myu.csv" is missing
  }else{

    # Get default mutation rates from the environment "env_proj"
    dt_myu <- get("dt_myu_default", pos = env_proj)

    # Update the file "myu.csv"
    create_myu_csv(dt_myu)
  }

  # Make an environment
  env_myu <- new.env(parent = globalenv())

  # Make a top frame
  tf <- tktoplevel()
  tkwm.title(tf, "Set mutation rates")

  # Define frames
  frame_myu_1 <- tkframe(tf)
  frame_myu_2 <- tkframe(tf)

  # Define a scrollbar for the multi-list box for mutation rates
  scr1 <- tkscrollbar(frame_myu_1, repeatinterval = 5, command = function(...) tkyview(mlb_myu, ...))

  # Define a multi-list box for mutation rates
  mlb_myu <- tk2mclistbox(frame_myu_1, width = 30, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
  tk2column(mlb_myu, "add", label = "Locus", width = 15)
  tk2column(mlb_myu, "add", label = "Mutation rate", width = 15)
  tk2insert.multi(mlb_myu, "end", dt_myu)

  # Define widgets in frame_myu2
  butt_edit <- tkbutton(frame_myu_2, text = "    Edit    ", cursor = "hand2", command = function() edit_myu_1())
  butt_add <- tkbutton(frame_myu_2, text = "    Add    ", cursor = "hand2", command = function() add_myu_1())
  butt_delete <- tkbutton(frame_myu_2, text = "    Delete    ", cursor = "hand2", command = function() delete_myu())

  # Grid widgets
  tkgrid(mlb_myu, scr1)
  tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")
  tkgrid(butt_edit, butt_add, butt_delete, padx = 20, pady = 5)

  # Grid frames
  tkgrid(frame_myu_1)
  tkgrid(frame_myu_2)

  # Assign widgets to the environment "env_myu"
  assign("mlb_myu", mlb_myu, envir = env_myu)
  assign("scr1", scr1, envir = env_myu)

  # Assign dt_myu to the environment "env_myu"
  assign("dt_myu", dt_myu, envir = env_myu)
}

##########################################
# The function to judge paternal lineage #
##########################################

judge_paternal <- function(p1, p2, id, fid, sex){

  ###########################################
  # The function to search paternal founder #
  ###########################################

  search_paternal_founder <- function(pos_start, pos_another){

    id_target <- pos_start

    bool_founder <- FALSE

    while(!bool_founder){
      father <- fid[id_target]

      # Lineal relationship
      if(father == id[pos_another]){
        bool_founder <- TRUE
        result <- "lineal"

      # Reach a founder
      }else if(father == 0){
        bool_founder <- TRUE
        result <- id[id_target]

      }else{
        id_target <- which(id == father)
      }
    }

    return(result)
  }

  # Investigate positions of p1 and p2
  pos_p1 <- match(p1, id)
  pos_p2 <- match(p2, id)

  # If both p1 and p2 are males
  if((sex[pos_p1] == 1) && (sex[pos_p2] == 1)){

    if((fid[pos_p1] == 0) && (fid[pos_p2] == 0)){
      result <- "not paternal"
    }else{

      # Search the paternal founder of p1
      founder_p1 <- search_paternal_founder(pos_p1, pos_p2)

      # Search the paternal founder of p2
      founder_p2 <- search_paternal_founder(pos_p2, pos_p1)

      if((founder_p1 == "lineal") || (founder_p2 == "lineal")){
        result <- "lineal"
      }else if(founder_p1 == founder_p2){
        result <- "collateral"
      }else{
        result <- "not paternal"
      }
    }

  }else{
    result <- "not paternal"
  }

  return(result)
}


##########################################
# The function to judge maternal lineage #
##########################################

judge_maternal <- function(p1, p2, id, mid){

  ###########################################
  # The function to search maternal founder #
  ###########################################

  search_maternal_founder <- function(pos_start, pos_another){

    id_target <- pos_start

    bool_founder <- FALSE

    while(!bool_founder){
      mother <- mid[id_target]

      # Lineal relationship
      if(mother == id[pos_another]){
        bool_founder <- TRUE
        result <- "lineal"

      # Reach a founder
      }else if(mother == 0){
        bool_founder <- TRUE
        result <- id[id_target]

      }else{
        id_target <- which(id == mother)
      }
    }

    return(result)
  }

  # Investigate positions of p1 and p2
  pos_p1 <- match(p1, id)
  pos_p2 <- match(p2, id)

  if((mid[pos_p1] == 0) && (mid[pos_p2] == 0)){
    result <- "not maternal"
  }else{

    # Search the maternal founder of p1
    founder_p1 <- search_maternal_founder(pos_p1, pos_p2)

    # Search the maternal founder of p2
    founder_p2 <- search_maternal_founder(pos_p2, pos_p1)

    if((founder_p1 == "lineal") || (founder_p2 == "lineal")){
      result <- "lineal"
    }else if(founder_p1 == founder_p2){
      result <- "collateral"
    }else{
      result <- "not maternal"
    }
  }

  return(result)
}


###########################################
# The function to make a displayed degree #
###########################################

make_deg_display <- function(deg, k1 = -1){
  if(is.na(deg)){
    deg_display <- "unr"
  }else if(deg == 1 && k1 == 1){
    deg_display <- "1st_pc"
  }else if(deg == 1 && k1 == 0.5){
    deg_display <- "1st_sib"
  }else if(is.element(deg, c(11, 12, 13))){
    deg_display <- paste0(deg, "th")
  }else if(deg %% 10 == 1){
    deg_display <- paste0(deg, "st")
  }else if(deg %% 10 == 2){
    deg_display <- paste0(deg, "nd")
  }else if(deg %% 10 == 3){
    deg_display <- paste0(deg, "rd")
  }else{
    deg_display <- paste0(deg, "th")
  }
  return(deg_display)
}


#####################################
# The function to set relationships #
#####################################

set_rel <- function(env_proj, env_gui){

  #############################################
  # The function to create the file "rel.csv" #
  #############################################

  create_rel_csv <- function(dt_rel){
    write.csv(dt_rel, paste0(path_pack, "/extdata/parameters/rel.csv"), row.names = FALSE)
  }

  ###########################################################################
  # The function to remake a multi-list box of information on relationships #
  ###########################################################################

  remake_mlb_rel <- function(dt_rel){

    # Get widgets from the environment "env_rel"
    scr1 <- get("scr1", pos = env_rel)
    mlb_rel <- get("mlb_rel", pos = env_rel)

    # Destroy widgets
    tkdestroy(scr1)
    tkdestroy(mlb_rel)

    # Define a scrollbar for the multi-list box of information on relationships
    scr1 <- tkscrollbar(frame_rel_1, repeatinterval = 5, command = function(...) tkyview(mlb_rel, ...))

    # Define a multi-list box of information on relationships
    mlb_rel <- tk2mclistbox(frame_rel_1, width = 120, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(mlb_rel, "add", label = "Name of relationship", width = 40)
    tk2column(mlb_rel, "add", label = "Degree", width = 20)
    tk2column(mlb_rel, "add", label = "Pr (IBD = 2)", width = 20)
    tk2column(mlb_rel, "add", label = "Pr (IBD = 1)", width = 20)
    tk2column(mlb_rel, "add", label = "Pr (IBD = 0)", width = 20)
    tk2insert.multi(mlb_rel, "end",  dt_rel)

    # Grid widgets
    tkgrid(mlb_rel, scr1)
    tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")

    # Assign widgets to the environment "env_rel"
    assign("scr1", scr1, envir = env_rel)
    assign("mlb_rel", mlb_rel, envir = env_rel)
  }

  ###################################################
  # The function to edit the name of a relationship #
  ###################################################

  edit_rel_1 <- function(){

    # Get the multi-list box from the environment "env_rel"
    mlb_rel <- get("mlb_rel", pos = env_rel)

    # If the user does not select one relationship
    if(tclvalue(tkcurselection(mlb_rel)) == ""){
      tkmessageBox(message = "Select one relationship!", icon = "error", type = "ok")

    # If the user selects one relationship
    }else{

      # Get relationship data from the environment "env_rel"
      dt_rel <- get("dt_rel", pos = env_rel)

      # Get the selected relationship
      pos_select <- as.numeric(tclvalue(tkcurselection(mlb_rel))) + 1
      rel_select <- dt_rel[pos_select, "Name_relationship"]

      # Define a tcl variable
      rel_select_var <- tclVar(rel_select)

      # Make a top frame
      tf <- tktoplevel()
      tkwm.title(tf, "Edit the name of a relationship")

      # Define widgets in tf
      label_title <- tklabel(tf, text = "Name of relationship")
      entry_name <- tkentry(tf, textvariable = rel_select_var, width = 40, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      butt_save <- tkbutton(tf, text = "    Save    ", cursor = "hand2", command = function() edit_rel_2(tf, mlb_rel, pos_select, tclvalue(rel_select_var)))

      # Grid widgets
      tkgrid(label_title, padx = 10, pady = 5)
      tkgrid(entry_name, padx = 10, pady = 5)
      tkgrid(butt_save, padx = 10, pady = 5)
    }
  }

  ##########################################################
  # The function to save the edited name of a relationship #
  ##########################################################

  edit_rel_2 <- function(tf, mlb_rel, pos_select, rel_select){

    # Check whether the autosomal STR results are deleted or not
    sign_ok <- check_delete_auto(env_proj)

    # If all results are deleted
    if(sign_ok == "ok"){

      # Get the current relationship data from the environment "env_rel"
      dt_rel <- get("dt_rel", pos = env_rel)

      # Update the name of the selected relationship
      dt_rel[pos_select, "Name_relationship"] <- rel_select

      # Update the file "rel.csv"
      create_rel_csv(dt_rel)

      # Remake a multi-list box of information on relationships
      remake_mlb_rel(dt_rel)

      # Assign data to the environment "env_rel"
      assign("dt_rel", dt_rel, envir = env_rel)

      # Reset environment variables for autosomal STR
      set_env_proj_auto(env_proj, FALSE)

      # Reset tab2
      make_tab2(env_proj, env_gui)

      # Destroy the top frame
      tkdestroy(tf)
    }
  }

  #########################################
  # The function to check the family tree #
  #########################################

  check_tree <- function(){

    # Get tcl variables from the environment "env_rel"
    sex_p1_var <- get("sex_p1_var", pos = env_rel)
    father_p1_var <- get("father_p1_var", pos = env_rel)
    mother_p1_var <- get("mother_p1_var", pos = env_rel)
    sex_p2_var <- get("sex_p2_var", pos = env_rel)
    father_p2_var <- get("father_p2_var", pos = env_rel)
    mother_p2_var <- get("mother_p2_var", pos = env_rel)

    # Get lists for tcl variables from the environment "env_rel"
    sex_unk_vars <- get("sex_unk_vars", pos = env_rel)
    father_unk_vars <- get("father_unk_vars", pos = env_rel)
    mother_unk_vars <- get("mother_unk_vars", pos = env_rel)

    # Extract values from tcl variables for unknowns
    sex_unk <- sapply(sex_unk_vars, tclvalue)
    sex_unk[which(sex_unk == "Male")] <- 1
    sex_unk[which(sex_unk == "Female")] <- 2
    father_unk <- sapply(father_unk_vars, tclvalue)
    father_unk[which(father_unk == "")] <- 0
    mother_unk <- sapply(mother_unk_vars, tclvalue)
    mother_unk[which(mother_unk == "")] <- 0

    # Extract values from tcl variables for p1
    sex_p1 <- tclvalue(sex_p1_var)
    if(sex_p1 == "Male"){
      sex_p1 <- 1
    }else if(sex_p1 == "Female"){
      sex_p1 <- 2
    }
    father_p1 <- tclvalue(father_p1_var)
    if(father_p1 == ""){
      father_p1 <- 0
    }
    mother_p1 <- tclvalue(mother_p1_var)
    if(mother_p1 == ""){
      mother_p1 <- 0
    }

    # Extract values from tcl variables for p2
    sex_p2 <- tclvalue(sex_p2_var)
    if(sex_p2 == "Male"){
      sex_p2 <- 1
    }else if(sex_p2 == "Female"){
      sex_p2 <- 2
    }
    father_p2 <- tclvalue(father_p2_var)
    if(father_p2 == ""){
      father_p2 <- 0
    }
    mother_p2 <- tclvalue(mother_p2_var)
    if(mother_p2 == ""){
      mother_p2 <- 0
    }

    # Define a family tree
    tree <- try(ped(id = c(paste0("Unk ", 1:length(sex_unk)), "Person 1", "Person 2"),
                    fid = c(father_unk, father_p1, father_p2),
                    mid = c(mother_unk, mother_p1, mother_p2),
                    sex = c(sex_unk, sex_p1, sex_p2)),
                silent = TRUE)

    return(tree)
  }

  ######################################
  # The function to add a relationship #
  ######################################

  add_rel_1 <- function(){

    ###################################################
    # The function to add a person to the family tree #
    ###################################################

    add_person <- function(){

      # Get candidates of the father and the mother
      fm_candidate <- get("fm_candidate", pos = env_rel)

      # Get tcl variables from the environment "env_rel"
      sex_unk_vars <- get("sex_unk_vars", pos = env_rel)
      father_unk_vars <- get("father_unk_vars", pos = env_rel)
      mother_unk_vars <- get("mother_unk_vars", pos = env_rel)
      founder_unk_vars <- get("founder_unk_vars", pos = env_rel)

      # Get widgets from the environment "env_rel"
      labels_name_unk <- get("labels_name_unk", pos = env_rel)
      combos_sex_unk <- get("combos_sex_unk", pos = env_rel)
      combos_father_unk <- get("combos_father_unk", pos = env_rel)
      combos_mother_unk <- get("combos_mother_unk", pos = env_rel)
      checks_founder_unk <- get("checks_founder_unk", pos = env_rel)

      # Define a row index for the added person
      pos_add <- length(sex_unk_vars) + 1

      # Define the name of the added person
      name_add <- paste0("Unk ", pos_add)

      # Add the name of the added person to the candidate of the father and the mother
      fm_candidate <- c(fm_candidate, name_add)

      # Update the list of the combobox for setting the father and the mother
      tkconfigure(combo_father_p1, values = fm_candidate)
      tkconfigure(combo_mother_p1, values = fm_candidate)
      tkconfigure(combo_father_p2, values = fm_candidate)
      tkconfigure(combo_mother_p2, values = fm_candidate)
      if(pos_add > 1){
        for(i in 1:(pos_add - 1)){
          tkconfigure(combos_father_unk[[i]], values = fm_candidate)
          tkconfigure(combos_mother_unk[[i]], values = fm_candidate)
        }
      }

      # Define tcl variables for the added person
      sex_unk_vars[[pos_add]] <- tclVar("")
      father_unk_vars[[pos_add]] <- tclVar("")
      mother_unk_vars[[pos_add]] <- tclVar("")
      founder_unk_vars[[pos_add]] <- tclVar("0")

      # Define widgets for the added person
      labels_name_unk[[pos_add]] <- tklabel(frame_family, text = name_add)
      combos_sex_unk[[pos_add]] <- ttkcombobox(frame_family, values = sex_candidate, textvariable = sex_unk_vars[[pos_add]], width = 10, state = "readonly")
      combos_father_unk[[pos_add]] <- ttkcombobox(frame_family, values = fm_candidate, textvariable = father_unk_vars[[pos_add]], width = 20, state = "readonly")
      combos_mother_unk[[pos_add]] <- ttkcombobox(frame_family, values = fm_candidate, textvariable = mother_unk_vars[[pos_add]], width = 20, state = "readonly")
      checks_founder_unk[[pos_add]] <- tkcheckbutton(frame_family, variable = founder_unk_vars[[pos_add]], width = 5, cursor = "hand2", command = function() change_founder("unk", pos_add))

      # Grid widgets for the added person
      tkgrid(labels_name_unk[[pos_add]], row = pos_add + 2, column = 0, padx = 5, pady = 5, sticky = "w")
      tkgrid(combos_sex_unk[[pos_add]], row = pos_add + 2, column = 1, padx = 5, pady = 5, sticky = "w")
      tkgrid(combos_father_unk[[pos_add]], row = pos_add + 2, column = 2, padx = 5, pady = 5, sticky = "w")
      tkgrid(combos_mother_unk[[pos_add]], row = pos_add + 2, column = 3, padx = 5, pady = 5, sticky = "w")
      tkgrid(checks_founder_unk[[pos_add]], row = pos_add + 2, column = 4, padx = 5, pady = 5, sticky = "w")

      # Update the scrollbar
      tkconfigure(canvas_family, scrollregion = c(0, 0, 1000, 36 * (pos_add + 2)))

      # Assign candidates of the father and the mother
      assign("fm_candidate", fm_candidate, envir = env_rel)

      # Assign tcl variables to the environment "env_rel"
      assign("sex_unk_vars", sex_unk_vars, envir = env_rel)
      assign("father_unk_vars", father_unk_vars, envir = env_rel)
      assign("mother_unk_vars", mother_unk_vars, envir = env_rel)
      assign("founder_unk_vars", founder_unk_vars, envir = env_rel)

      # Assign widgets to the environment "env_rel"
      assign("labels_name_unk", labels_name_unk, envir = env_rel)
      assign("combos_sex_unk", combos_sex_unk, envir = env_rel)
      assign("combos_father_unk", combos_father_unk, envir = env_rel)
      assign("combos_mother_unk", combos_mother_unk, envir = env_rel)
      assign("checks_founder_unk", checks_founder_unk, envir = env_rel)
    }

    ######################################################
    # The function to delete a person to the family tree #
    ######################################################

    delete_person <- function(){

      # Get candidates of the father and the mother
      fm_candidate <- get("fm_candidate", pos = env_rel)

      # Get tcl variables from the environment "env_rel"
      sex_unk_vars <- get("sex_unk_vars", pos = env_rel)
      father_unk_vars <- get("father_unk_vars", pos = env_rel)
      mother_unk_vars <- get("mother_unk_vars", pos = env_rel)
      founder_unk_vars <- get("founder_unk_vars", pos = env_rel)

      # Get widgets from the environment "env_rel"
      labels_name_unk <- get("labels_name_unk", pos = env_rel)
      combos_sex_unk <- get("combos_sex_unk", pos = env_rel)
      combos_father_unk <- get("combos_father_unk", pos = env_rel)
      combos_mother_unk <- get("combos_mother_unk", pos = env_rel)
      checks_founder_unk <- get("checks_founder_unk", pos = env_rel)

      # Define a row index for the deleted person
      pos_del <- length(sex_unk_vars)

      if(pos_del > 0){

        # Destroy widgets for the deleted person
        tkdestroy(labels_name_unk[[pos_del]])
        tkdestroy(combos_sex_unk[[pos_del]])
        tkdestroy(combos_father_unk[[pos_del]])
        tkdestroy(combos_mother_unk[[pos_del]])
        tkdestroy(checks_founder_unk[[pos_del]])

        # Update the list of the combobox for setting the father and the mother
        fm_candidate <- fm_candidate[1:(length(fm_candidate) - 1)]
        tkconfigure(combo_father_p1, values = fm_candidate)
        tkconfigure(combo_mother_p1, values = fm_candidate)
        tkconfigure(combo_father_p2, values = fm_candidate)
        tkconfigure(combo_mother_p2, values = fm_candidate)
        if(pos_del > 1){
          for(i in 1:(pos_del - 1)){
            tkconfigure(combos_father_unk[[i]], values = fm_candidate)
            tkconfigure(combos_mother_unk[[i]], values = fm_candidate)
          }
        }

        # Update the scrollbar
        tkconfigure(canvas_family, scrollregion = c(0, 0, 1000, 36 * (pos_del + 1)))

        # Update tcl variables
        sex_unk_vars[[pos_del]] <- NULL
        father_unk_vars[[pos_del]] <- NULL
        mother_unk_vars[[pos_del]] <- NULL
        founder_unk_vars[[pos_del]] <- NULL

        # Update widgets for the deleted person
        labels_name_unk[[pos_del]] <- NULL
        combos_sex_unk[[pos_del]] <- NULL
        combos_father_unk[[pos_del]] <- NULL
        combos_mother_unk[[pos_del]] <- NULL
        checks_founder_unk[[pos_del]] <- NULL

        # Assign candidates of the father and the mother
        assign("fm_candidate", fm_candidate, envir = env_rel)

        # Assign tcl variables to the environment "env_rel"
        assign("sex_unk_vars", sex_unk_vars, envir = env_rel)
        assign("father_unk_vars", father_unk_vars, envir = env_rel)
        assign("mother_unk_vars", mother_unk_vars, envir = env_rel)
        assign("founder_unk_vars", founder_unk_vars, envir = env_rel)

        # Assign widgets to the environment "env_rel"
        assign("labels_name_unk", labels_name_unk, envir = env_rel)
        assign("combos_sex_unk", combos_sex_unk, envir = env_rel)
        assign("combos_father_unk", combos_father_unk, envir = env_rel)
        assign("combos_mother_unk", combos_mother_unk, envir = env_rel)
        assign("checks_founder_unk", checks_founder_unk, envir = env_rel)
      }
    }

    ########################################
    # The function to view the family tree #
    ########################################

    view_tree <- function(){

      # Check the family tree
      tree <- check_tree()

      # Check whether the family tree is appropriate or not
      if(class(tree) == "try-error"){
        tkmessageBox(message = "Incorrect setting of the family tree", icon = "error", type = "ok")
      }else{

        # Plot the family tree
        plot(tree, hatched = c("Person 1", "Person 2"))
      }
    }

    ######################################
    # The function to change the founder #
    ######################################

    change_founder <- function(who, pos = numeric(0)){

      # Change the founder of person 1
      if(who == "p1"){
        if(tclvalue(founder_p1_var) == "0"){
          tkconfigure(combo_father_p1, state = "readonly")
          tkconfigure(combo_mother_p1, state = "readonly")
        }else{
          tclvalue(father_p1_var) <- ""
          tclvalue(mother_p1_var) <- ""
          tkconfigure(combo_father_p1, textvariable = father_p1_var, state = "disable")
          tkconfigure(combo_mother_p1, textvariable = mother_p1_var, state = "disable")
        }

      # Change the founder of person 2
      }else if(who == "p2"){
        if(tclvalue(founder_p2_var) == "0"){
          tkconfigure(combo_father_p2, state = "readonly")
          tkconfigure(combo_mother_p2, state = "readonly")
        }else{
          tclvalue(father_p2_var) <- ""
          tclvalue(mother_p2_var) <- ""
          tkconfigure(combo_father_p2, textvariable = father_p2_var, state = "disable")
          tkconfigure(combo_mother_p2, textvariable = mother_p2_var, state = "disable")
        }

      # Change the founder of an unknown person
      }else{

        # Get tcl variables from the environment "env_rel"
        father_unk_vars <- get("father_unk_vars", pos = env_rel)
        mother_unk_vars <- get("mother_unk_vars", pos = env_rel)
        founder_unk_vars <- get("founder_unk_vars", pos = env_rel)

        # Get widgets from the environment "env_rel"
        combos_father_unk <- get("combos_father_unk", pos = env_rel)
        combos_mother_unk <- get("combos_mother_unk", pos = env_rel)

        # Update widgets for the founder
        if(tclvalue(founder_unk_vars[[pos]]) == "0"){
          tkconfigure(combos_father_unk[[pos]], state = "readonly")
          tkconfigure(combos_mother_unk[[pos]], state = "readonly")
        }else{
          tclvalue(father_unk_vars[[pos]]) <- ""
          tclvalue(mother_unk_vars[[pos]]) <- ""
          tkconfigure(combos_father_unk[[pos]], textvariable = father_unk_vars[[pos]], state = "disable")
          tkconfigure(combos_mother_unk[[pos]], textvariable = mother_unk_vars[[pos]], state = "disable")
        }

        # Assign tcl variables to the environment "env_rel"
        assign("father_unk_vars", father_unk_vars, envir = env_rel)
        assign("mother_unk_vars", mother_unk_vars, envir = env_rel)

        # Assign widgets to the environment "env_rel"
        assign("combos_father_unk", combos_father_unk, envir = env_rel)
        assign("combos_mother_unk", combos_mother_unk, envir = env_rel)
      }
    }

    # Define the initial family members
    sex_candidate <- c("Male", "Female")
    fm_candidate <- c("Person 1", "Person 2")

    # Define tcl variables
    name_rel_var <- tclVar("")
    sex_p1_var <- tclVar("")
    father_p1_var <- tclVar("")
    mother_p1_var <- tclVar("")
    founder_p1_var <- tclVar("0")
    sex_p2_var <- tclVar("")
    father_p2_var <- tclVar("")
    mother_p2_var <- tclVar("")
    founder_p2_var <- tclVar("0")

    # Define lists for tcl variables
    sex_unk_vars <- list()
    father_unk_vars <- list()
    mother_unk_vars <- list()
    founder_unk_vars <- list()

    # Define lists for widgets
    labels_name_unk <- list()
    combos_sex_unk <- list()
    combos_father_unk <- list()
    combos_mother_unk <- list()
    checks_founder_unk <- list()

    # Make a top frame
    tf <- tktoplevel()
    tkwm.title(tf, "Add a relationship")

    # Define main frames
    frame_add_1 <- tkframe(tf, relief = "groove", borderwidth = 2)
    frame_add_2 <- tkframe(tf, relief = "groove", borderwidth = 2)
    frame_add_3 <- tkframe(tf)

    # Define widgets in frame_add_1
    label_title_name_rel <- tklabel(frame_add_1, text = "Name of relationship", font = "Helvetica 10 bold")
    entry_name_rel <- tkentry(frame_add_1, textvariable = name_rel_var, width = 40, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

    # Define buttons to set family tree in frame_add_2
    label_title_family <- tklabel(frame_add_2, text = "Set family tree", font = "Helvetica 10 bold")
    frame_add_2_1 <- tkframe(frame_add_2)
    butt_add <- tkbutton(frame_add_2_1, text = "    Add    ", cursor = "hand2", command = function() add_person())
    butt_delete <- tkbutton(frame_add_2_1, text = "    Delete    ", cursor = "hand2", command = function() delete_person())
    butt_veiw <- tkbutton(frame_add_2_1, text = "    View family tree    ", cursor = "hand2", command = function() view_tree())

    # Define a scrollbar in frame_add_2
    scr_family <- tkscrollbar(frame_add_2, repeatinterval = 5, command = function(...) tkyview(canvas_family, ...))
    canvas_family <- tkcanvas(frame_add_2, width = 550, height = 300, scrollregion = c(0, 0, 1000, 72), yscrollcommand = function(...) tkset(scr_family, ...))
    frame_family <- tkframe(canvas_family)

    # Define title labels in frame_family
    label_name <- tklabel(frame_family, text = "Name")
    label_sex <- tklabel(frame_family, text = "Sex")
    label_father <- tklabel(frame_family, text = "Father")
    label_mother <- tklabel(frame_family, text = "Mother")
    label_founder <- tklabel(frame_family, text = "Founder")

    # Define widgets for person 1
    label_name_p1 <- tklabel(frame_family, text = "Person 1")
    combo_sex_p1 <- ttkcombobox(frame_family, values = sex_candidate, textvariable = sex_p1_var, width = 10, state = "readonly")
    combo_father_p1 <- ttkcombobox(frame_family, values = fm_candidate, textvariable = father_p1_var, width = 20, state = "readonly")
    combo_mother_p1 <- ttkcombobox(frame_family, values = fm_candidate, textvariable = mother_p1_var, width = 20, state = "readonly")
    check_founder_p1 <- tkcheckbutton(frame_family, variable = founder_p1_var, width = 5, cursor = "hand2", command = function() change_founder("p1"))

    # Define widgets for person 2
    label_name_p2 <- tklabel(frame_family, text = "Person 2")
    combo_sex_p2 <- ttkcombobox(frame_family, values = sex_candidate, textvariable = sex_p2_var, width = 10, state = "readonly")
    combo_father_p2 <- ttkcombobox(frame_family, values = fm_candidate, textvariable = father_p2_var, width = 20, state = "readonly")
    combo_mother_p2 <- ttkcombobox(frame_family, values = fm_candidate, textvariable = mother_p2_var, width = 20, state = "readonly")
    check_founder_p2 <- tkcheckbutton(frame_family, variable = founder_p2_var, width = 5, cursor = "hand2", command = function() change_founder("p2"))

    # Define a save button in frame_add_3
    butt_save <- tkbutton(frame_add_3, text = "    Save    ", cursor = "hand2", command = function() add_rel_2(tf))

    # Grid widgets in frame_add_1
    tkgrid(label_title_name_rel, padx = 5, pady = 5, sticky = "w")
    tkgrid(entry_name_rel, padx = 25, pady = 5, sticky = "w")

    # Grid buttons in frame_add_2
    tkgrid(label_title_family, padx = 5, pady = 5, sticky = "w")
    tkgrid(frame_add_2_1, padx = 25, pady = 5, sticky = "w")
    tkgrid(butt_add, butt_delete, butt_veiw, padx = 5, pady = 5, sticky = "w")

    # Grid title labels in frame_family
    tkgrid(label_name, row = 0, column = 0, padx = 5, pady = 5, sticky = "w")
    tkgrid(label_sex, row = 0, column = 1, padx = 5, pady = 5, sticky = "w")
    tkgrid(label_father, row = 0, column = 2, padx = 5, pady = 5, sticky = "w")
    tkgrid(label_mother, row = 0, column = 3, padx = 5, pady = 5, sticky = "w")
    tkgrid(label_founder, row = 0, column = 4, padx = 5, pady = 5, sticky = "w")

    # Grid widgets for person 1
    tkgrid(label_name_p1, row = 1, column = 0, padx = 5, pady = 5, sticky = "w")
    tkgrid(combo_sex_p1, row = 1, column = 1, padx = 5, pady = 5, sticky = "w")
    tkgrid(combo_father_p1, row = 1, column = 2, padx = 5, pady = 5, sticky = "w")
    tkgrid(combo_mother_p1, row = 1, column = 3, padx = 5, pady = 5, sticky = "w")
    tkgrid(check_founder_p1, row = 1, column = 4, padx = 5, pady = 5, sticky = "w")

    # Grid widgets for person 2
    tkgrid(label_name_p2, row = 2, column = 0, padx = 5, pady = 5, sticky = "w")
    tkgrid(combo_sex_p2, row = 2, column = 1, padx = 5, pady = 5, sticky = "w")
    tkgrid(combo_father_p2, row = 2, column = 2, padx = 5, pady = 5, sticky = "w")
    tkgrid(combo_mother_p2, row = 2, column = 3, padx = 5, pady = 5, sticky = "w")
    tkgrid(check_founder_p2, row = 2, column = 4, padx = 5, pady = 5, sticky = "w")

    # Grid "frame_add_2_2" and "frame_family"
    tkgrid(frame_family)

    # Grid the scrollbar
    tkgrid(canvas_family, scr_family)
    tkgrid.configure(scr_family, rowspan = 10, sticky = "nsw")
    tkcreate(canvas_family, "window", 0, 0, anchor = "nw", window = frame_family)

    # Grid the save button
    tkgrid(butt_save)

    # Grid main frames
    tkgrid(frame_add_1, padx = 10, pady = 10, sticky = "w")
    tkgrid(frame_add_2, padx = 10, pady = 10, sticky = "w")
    tkgrid(frame_add_3, padx = 10, pady = 10)

    # Assign candidates of the father and the mother
    assign("fm_candidate", fm_candidate, envir = env_rel)

    # Assign tcl variables to the environment "env_rel"
    assign("name_rel_var", name_rel_var, envir = env_rel)
    assign("sex_p1_var", sex_p1_var, envir = env_rel)
    assign("father_p1_var", father_p1_var, envir = env_rel)
    assign("mother_p1_var", mother_p1_var, envir = env_rel)
    assign("founder_p1_var", founder_p1_var, envir = env_rel)
    assign("sex_p2_var", sex_p2_var, envir = env_rel)
    assign("father_p2_var", father_p2_var, envir = env_rel)
    assign("mother_p2_var", mother_p2_var, envir = env_rel)
    assign("founder_p2_var", founder_p2_var, envir = env_rel)

    # Assign lists for tcl variables to the environment "env_rel"
    assign("sex_unk_vars", sex_unk_vars, envir = env_rel)
    assign("father_unk_vars", father_unk_vars, envir = env_rel)
    assign("mother_unk_vars", mother_unk_vars, envir = env_rel)
    assign("founder_unk_vars", founder_unk_vars, envir = env_rel)

    # Assign lists for widgets to the environment "env_rel"
    assign("labels_name_unk", labels_name_unk, envir = env_rel)
    assign("combos_sex_unk", combos_sex_unk, envir = env_rel)
    assign("combos_father_unk", combos_father_unk, envir = env_rel)
    assign("combos_mother_unk", combos_mother_unk, envir = env_rel)
    assign("checks_founder_unk", checks_founder_unk, envir = env_rel)
  }

  ###############################################
  # The function to save the added relationship #
  ###############################################

  add_rel_2 <- function(tf){

    # Get a new relationship name from the environment "env_rel"
    name_rel <- tclvalue(get("name_rel_var", pos = env_rel))

    if(name_rel == ""){
      tkmessageBox(message = "Enter the name of the relationshiop!", icon = "error", type = "ok")
    }else{

      # Check the family tree
      tree <- check_tree()

      # Check whether the family tree is appropriate or not
      if(class(tree) == "try-error"){
        tkmessageBox(message = "Incorrect setting of the family tree!", icon = "error", type = "ok")
      }else{

        # Check whether the autosomal STR results are deleted or not
        sign_ok <- check_delete_auto(env_proj)

        # If all results are deleted
        if(sign_ok == "ok"){

          # Make the coefficient table
          coeff_tree <- coeffTable(tree)

          # Extract k2, k1, and k0
          pos_row <- intersect(which(is.element(coeff_tree[, "id1"], c("Person 1", "Person 2")) == TRUE),
                               which(is.element(coeff_tree[, "id2"], c("Person 1", "Person 2")) == TRUE))
          pibd <- as.numeric(coeff_tree[pos_row, c("k2", "k1", "k0")])
          deg <- as.numeric(coeff_tree[pos_row, "deg"])

          # Check inbred relationship
          if(any(is.na(pibd))){
            tkmessageBox(message = "Person 1 and 2 are inbred individuals!", icon = "error", type = "ok")
          }else{

            # Get relationship data from the environment "env_rel"
            dt_rel <- get("dt_rel", pos = env_rel)

            # Make a displayed degree
            deg_display <- make_deg_display(deg, pibd[2])

            # Update the relationship data
            dt_rel <- rbind(dt_rel, c(name_rel, deg_display, pibd))

            # Update the file "rel.csv"
            create_rel_csv(dt_rel)

            # Remake a multi-list box of information on relationships
            remake_mlb_rel(dt_rel)

            # Assign data to the environment "env_rel"
            assign("dt_rel", dt_rel, envir = env_rel)

            # Reset environment variables for autosomal STR
            set_env_proj_auto(env_proj, FALSE)

            # Reset tab2
            make_tab2(env_proj, env_gui)

            # Destroy the top frame
            tkdestroy(tf)
          }
        }
      }
    }
  }

  #########################################
  # The function to delete a relationship #
  #########################################

  delete_rel <- function(){

    # Get the multi-list box from the environment "env_rel"
    mlb_rel <- get("mlb_rel", pos = env_rel)

    # If the user does not select one relationship
    if(tclvalue(tkcurselection(mlb_rel)) == ""){
      tkmessageBox(message = "Select one relationship!", icon = "error", type = "ok")

    # If the user selects one relationship
    }else{

      # Check whether the autosomal STR results are deleted or not
      sign_ok <- check_delete_auto(env_proj)

      # If all results are deleted
      if(sign_ok == "ok"){

        # Get relationship data from the environment "env_rel"
        dt_rel <- get("dt_rel", pos = env_rel)

        # Get a row index deleted
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_rel))) + 1

        # Delete a selected relationship
        dt_rel <- dt_rel[-pos_select, ]

        # Update the file "rel.csv"
        create_rel_csv(dt_rel)

        # Remake a multi-list box of information on relationships
        remake_mlb_rel(dt_rel)

        # Assign data to the environment "env_rel"
        assign("dt_rel", dt_rel, envir = env_rel)

        # Reset environment variables for autosomal STR
        set_env_proj_auto(env_proj, FALSE)

        # Reset tab2
        make_tab2(env_proj, env_gui)
      }
    }
  }

  ##########################################
  # The function to reset the relationship #
  ##########################################

  reset_rel <- function(){

    # Check whether the autosomal STR results are deleted or not
    sign_ok <- check_delete_auto(env_proj)

    # If all results are deleted
    if(sign_ok == "ok"){

      # Get default relationship data from the environment "env_proj"
      dt_rel <- get("rel_data_default", pos = env_proj)

      # Update the file "rel.csv"
      create_rel_csv(dt_rel)

      # Remake a multi-list box of information on relationships
      remake_mlb_rel(dt_rel)

      # Assign data to the environment "env_rel"
      assign("dt_rel", dt_rel, envir = env_rel)

      # Reset environment variables for autosomal STR
      set_env_proj_auto(env_proj, FALSE)

      # Reset tab2
      make_tab2(env_proj, env_gui)
    }
  }

  # Get the package path
  path_pack <- get("path_pack", pos = env_gui)

  # Get file names in the folder "parameters"
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  # Define "dt_rel"
  if(is.element("rel.csv", fn_par)){
    dt_rel <- fread(paste0(path_pack, "/extdata/parameters/rel.csv"))
  }else{
    dt_rel <- get("rel_data_default", pos = env_proj)
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
  mlb_rel <- tk2mclistbox(frame_rel_1, width = 120, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
  tk2column(mlb_rel, "add", label = "Name of relationship", width = 40)
  tk2column(mlb_rel, "add", label = "Degree", width = 20)
  tk2column(mlb_rel, "add", label = "Pr (IBD = 2)", width = 20)
  tk2column(mlb_rel, "add", label = "Pr (IBD = 1)", width = 20)
  tk2column(mlb_rel, "add", label = "Pr (IBD = 0)", width = 20)
  tk2insert.multi(mlb_rel, "end",  dt_rel)

  # Define widgets in frame_rel_2
  butt_edit <- tkbutton(frame_rel_2, text = "    Edit    ", cursor = "hand2", command = function() edit_rel_1())
  butt_add <- tkbutton(frame_rel_2, text = "    Add    ", cursor = "hand2", command = function() add_rel_1())
  butt_delete <- tkbutton(frame_rel_2, text = "    Delete    ", cursor = "hand2", command = function() delete_rel())
  butt_reset <- tkbutton(frame_rel_2, text = "    Reset    ", cursor = "hand2", command = function() reset_rel())

  # Grid widgets
  tkgrid(mlb_rel, scr1)
  tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")
  tkgrid(butt_edit, butt_add, butt_delete, butt_reset, padx = 10, pady = 5)

  # Grid frames
  tkgrid(frame_rel_1)
  tkgrid(frame_rel_2)

  # Assign widgets to the environment "env_rel"
  assign("scr1", scr1, envir = env_rel)
  assign("mlb_rel", mlb_rel, envir = env_rel)

  # Assign data to the environment "env_rel"
  assign("dt_rel", dt_rel, envir = env_rel)
}


#########################################################
# The function to set analysis method for autosomal STR #
#########################################################

set_auto <- function(env_proj, env_gui){

  ##################################################
  # The function to create the file "par_auto.csv" #
  ##################################################

  create_par_auto_csv <- function(maf, meth_d, pd){
    names_par <- c("maf", "meth_d", "pd")
    values_par <- c(maf, meth_d, pd)
    dt_par_auto <- data.table(Parameter = names_par, Value = values_par)
    write.csv(dt_par_auto, paste0(path_pack, "/extdata/parameters/par_auto.csv"), row.names = FALSE)
  }

  ###################################
  # The function to save parameters #
  ###################################

  save_auto <- function(){

    # Check whether the autosomal STR results are deleted or not
    sign_ok <- check_delete_auto(env_proj)

    # If the autosomal STR results are deleted
    if(sign_ok == "ok"){

      # Update the file "par_auto.csv"
      create_par_auto_csv(tclvalue(maf_var), tclvalue(meth_d_var), tclvalue(pd_var))

      # Reset environment variables for autosomal STR
      set_env_proj_auto(env_proj, FALSE)

      # Reset tab2
      make_tab2(env_proj, env_gui)

      # Destroy the top frame
      tkdestroy(tf)
    }
  }

  # Get the package path
  path_pack <- get("path_pack", pos = env_gui)

  # Get file names in the folder "parameters"
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  # If the file "par_auto.csv" is found
  if(is.element("par_auto.csv", fn_par)){

    # Load the file "par_auto.csv"
    dt_par_auto <- fread(paste0(path_pack, "/extdata/parameters/par_auto.csv"))

    # Extract parameters
    maf <- dt_par_auto$Value[dt_par_auto$Parameter == "maf"]
    meth_d <- dt_par_auto$Value[dt_par_auto$Parameter == "meth_d"]
    pd <- dt_par_auto$Value[dt_par_auto$Parameter == "pd"]

  # If the file "par_auto" is missing
  }else{

    # Get default parameters from the environment "env_proj"
    maf <- get("maf_default", pos = env_proj)
    meth_d <- get("meth_d_default", pos = env_proj)
    pd <- get("pd_default", pos = env_proj)

    # Update the file "par_auto.csv
    create_par_auto_csv(maf, meth_d, pd)
  }

  # Define tcl variables
  maf_var <- tclVar(maf)
  meth_d_var <- tclVar(meth_d)
  pd_var <- tclVar(pd)

  # Create a top frame
  tf <- tktoplevel()
  tkwm.title(tf, "Set parameters for autosomal STRs")

  # Define widgets for maf
  label_maf <- tklabel(tf, text = "Minimum allele frequency")
  entry_maf <- tkentry(tf, textvariable = maf_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

  # Define widgets for meth_d
  label_meth_d <- tklabel(tf, text = "Consideration of drop-out")
  radio_meth_d0 <- tkradiobutton(tf, anchor = "w", width = 60, state = "normal", text = "Not consider", variable = meth_d_var, value = 0)
  radio_meth_d1 <- tkradiobutton(tf, anchor = "w", width = 60, state = "normal", text = "Consider", variable = meth_d_var, value = 1)

  # Define widgets for pd
  label_pd <- tklabel(tf, text = "Probability of drop-out")
  entry_pd <- tkentry(tf, textvariable = pd_var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

  # Define a save button
  butt_save <- tkbutton(tf, text = "    Save    ", cursor = "hand2", command = function() save_auto())

  # Grid widgets
  tkgrid(label_maf, entry_maf, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_meth_d, radio_meth_d0, padx = 10, pady = 5, sticky = "w")
  tkgrid(tklabel(tf, text = ""), radio_meth_d1, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_pd, entry_pd, padx = 10, pady = 5, sticky = "w")
  tkgrid(butt_save, padx = 10, pady = 5, sticky = "w")
}
