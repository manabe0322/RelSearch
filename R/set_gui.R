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
      mlb_myu <- tk2mclistbox(frame_myu_1, width = 30, height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
      tk2column(mlb_myu, "add", label = "Locus", width = 15)
      tk2column(mlb_myu, "add", label = "Mutation rate", width = 15)
      tk2insert.multi(mlb_myu, "end", myu_save)

      # Grid widgets
      tkgrid(mlb_myu, scr1)
      tkgrid.configure(scr1, rowspan = 25, sticky = "nsw")

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
        mlb_myu <- tk2mclistbox(frame_myu_1, width = 30, height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(mlb_myu, "add", label = "Locus", width = 15)
        tk2column(mlb_myu, "add", label = "Mutation rate", width = 15)
        tk2insert.multi(mlb_myu, "end", myu_save)

        # Grid widgets
        tkgrid(mlb_myu, scr1)
        tkgrid.configure(scr1, rowspan = 25, sticky = "nsw")

        # Assign widgets to environment variable (env_myu)
        assign("mlb_myu", mlb_myu, envir = env_myu)
        assign("scr1", scr1, envir = env_myu)
      }
    }
  }

  path_pack <- get("path_pack", pos = env_gui)
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))
  if(is.element("myu.csv", fn_par)){
    myu_data <- read.csv(paste0(path_pack, "/extdata/parameters/myu.csv"), header = TRUE)
    myu_data <- as.matrix(myu_data)
    locus_myu <- myu_data[, colnames(myu_data) == "Marker"]
    myu_all <- as.numeric(myu_data[, colnames(myu_data) == "Myu"])
    names(myu_all) <- locus_myu
  }else{
    myu_all <- c(0.001474647, 0.002858327, 0.001479789, 0.002240583, 0.000227000,
                0.001433812, 0.001130039, 0.001588339,
                0.001521043, 0.001069792, 0.000092200, 0.002602109,
                0.001521043, 0.001848550, 0.001574558, 0.001179836, 0.001521043,
                0.001521043, 0.001521043, 0.001521043, 0.001130039)
    locus_myu <- c("D3S1358", "vWA", "D16S539", "CSF1PO", "TPOX",
                   "D8S1179", "D21S11", "D18S51",
                   "D2S441", "D19S433", "TH01", "FGA",
                   "D22S1045", "D5S818", "D13S317", "D7S820", "SE33",
                   "D10S1248", "D1S1656", "D12S391", "D2S1338")
    names(myu_all) <- locus_myu
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
  mlb_myu <- tk2mclistbox(frame_myu_1, width = 30, height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
  tk2column(mlb_myu, "add", label = "Locus", width = 15)
  tk2column(mlb_myu, "add", label = "Mutation rate", width = 15)
  tk2insert.multi(mlb_myu, "end", cbind(locus_myu, myu_all))

  # Define widgets in frame_myu2
  butt_edit <- tkbutton(frame_myu_2, text = "    Edit    ", cursor = "hand2", command = function() edit_myu_1())
  butt_add <- tkbutton(frame_myu_2, text = "    Add    ", cursor = "hand2", command = function() add_myu_1())
  butt_delete <- tkbutton(frame_myu_2, text = "    Delete    ", cursor = "hand2", command = function() delete_myu())

  # Grid widgets
  tkgrid(mlb_myu, scr1)
  tkgrid.configure(scr1, rowspan = 25, sticky = "nsw")
  tkgrid(butt_edit, butt_add, butt_delete, padx = 20, pady = 5)
  tkgrid(frame_myu_1)
  tkgrid(frame_myu_2)

  # Assign widgets to environment variable (env_myu)
  assign("mlb_myu", mlb_myu, envir = env_myu)
  assign("scr1", scr1, envir = env_myu)
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
      write.csv(pibd_all, paste0(path_pack, "/extdata/parameters/ibd.csv"))

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
      write.csv(pibd_all, paste0(path_pack, "/extdata/parameters/ibd.csv"))

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
        write.csv(pibd_all, paste0(path_pack, "/extdata/parameters/ibd.csv"))

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
  if(is.element("ibd.csv", fn_par)){
    pibd_all <- read.csv(paste0(path_pack, "/extdata/parameters/ibd.csv"), header = TRUE, row.names = 1)
    pibd_all <- as.matrix(pibd_all)
  }else{
    pibd_all <- matrix(0, 5, 3)
    pibd_all[1, ] <- c(1, 0, 0)
    pibd_all[2, ] <- c(0, 1, 0)
    pibd_all[3, ] <- c(0.25, 0.5, 0.25)
    pibd_all[4, ] <- c(0, 0.5, 0.5)
    pibd_all[5, ] <- c(0, 0.25, 0.75)
    rownames(pibd_all) <- c("direct match", "parent-child", "sibling", "2nd-degree", "3rd-degree")
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
    maf <- 0.001
    meth_d <- 1
    pd <- 0.5
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

# Set analysis method for Y-STR
set_y <- function(){

}

# Set analysis method for mtDNA
set_mt <- function(){

}
