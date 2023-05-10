# Set objects of env_proj for autosomal STR
set_env_proj_auto <- function(env_proj, bool_new){
  if(bool_new){
    # Assign objects for files
    assign("data_auto_q", NULL, envir = env_proj)
    assign("fp_auto_q", character(0), envir = env_proj)
    assign("fn_auto_q", character(0), envir = env_proj)
    assign("data_auto_r", NULL, envir = env_proj)
    assign("fp_auto_r", character(0), envir = env_proj)
    assign("fn_auto_r", character(0), envir = env_proj)
    assign("data_auto_af", NULL, envir = env_proj)
    assign("fp_auto_af", character(0), envir = env_proj)
    assign("fn_auto_af", character(0), envir = env_proj)

    # Assign objects for criteria
    assign("min_lr_auto", numeric(0), envir = env_proj)

    # Assign objects for mutation rates
    assign("myu_all", NULL, envir = env_proj)

    # Assign objects for IBD probabilities
    assign("pibd_all", NULL, envir = env_proj)

    # Assign objects for parameters
    assign("maf", numeric(0), envir = env_proj)
    assign("meth_d", numeric(0), envir = env_proj)
    assign("pd", numeric(0), envir = env_proj)
  }
  assign("fin_auto", FALSE, envir = env_proj)
}

# Set objects of env_proj for Y
set_env_proj_y <- function(env_proj, bool_new){
  if(bool_new){
    # Assign objects for files
    assign("data_y_q", NULL, envir = env_proj)
    assign("fp_y_q", character(0), envir = env_proj)
    assign("fn_y_q", character(0), envir = env_proj)
    assign("data_y_r", NULL, envir = env_proj)
    assign("fp_y_r", character(0), envir = env_proj)
    assign("fn_y_r", character(0), envir = env_proj)

    # Assign objects for criteria
    assign("max_mismatch_y", numeric(0), envir = env_proj)
    assign("max_ignore_y", numeric(0), envir = env_proj)
    assign("max_mustep_y", numeric(0), envir = env_proj)
  }
  assign("fin_y", FALSE, envir = env_proj)
}

# Set objects of env_proj for mtDNA
set_env_proj_mt <- function(env_proj, bool_new){
  if(bool_new){
    # Assign objects for files
    assign("data_mt_q", NULL, envir = env_proj)
    assign("fp_mt_q", character(0), envir = env_proj)
    assign("fn_mt_q", character(0), envir = env_proj)
    assign("data_mt_r", NULL, envir = env_proj)
    assign("fp_mt_r", character(0), envir = env_proj)
    assign("fn_mt_r", character(0), envir = env_proj)

    # Assign objects for criteria
    assign("min_share_mt", numeric(0), envir = env_proj)
    assign("max_mismatch_mt", numeric(0), envir = env_proj)
  }
  assign("fin_mt", FALSE, envir = env_proj)
}

# Set objects of env_proj for default criteria and parameters
set_env_proj_default <- function(env_proj){
  # Set default criteria
  assign("min_lr_auto_default", 100, envir = env_proj)
  assign("max_mismatch_y_default", 2, envir = env_proj)
  assign("max_ignore_y_default", 10, envir = env_proj)
  assign("max_mustep_y_default", 2, envir = env_proj)
  assign("min_share_mt_default", 300, envir = env_proj)
  assign("max_mismatch_mt_default", 1, envir = env_proj)

  # Set default mutation rates
  myu_all <- c(0.001474647, 0.002858327, 0.001479789, 0.002240583, 0.000227000,
               0.001433812, 0.001130039, 0.001588339,
               0.001521043, 0.001069792, 0.000092200, 0.002602109,
               0.001521043, 0.001848550, 0.001574558, 0.001179836, 0.001521043,
               0.001521043, 0.001521043, 0.001521043, 0.001130039)
  names(myu_all) <- c("D3S1358", "vWA", "D16S539", "CSF1PO", "TPOX",
                      "D8S1179", "D21S11", "D18S51",
                      "D2S441", "D19S433", "TH01", "FGA",
                      "D22S1045", "D5S818", "D13S317", "D7S820", "SE33",
                      "D10S1248", "D1S1656", "D12S391", "D2S1338")
  assign("myu_all_default", myu_all, envir = env_proj)

  # Set default IBD probabilities
  pibd_all <- matrix(0, 5, 3)
  pibd_all[1, ] <- c(1, 0, 0)
  pibd_all[2, ] <- c(0, 1, 0)
  pibd_all[3, ] <- c(0.25, 0.5, 0.25)
  pibd_all[4, ] <- c(0, 0.5, 0.5)
  pibd_all[5, ] <- c(0, 0.25, 0.75)
  rownames(pibd_all) <- c("direct match", "parent-child", "sibling", "2nd-degree", "3rd-degree")
  colnames(pibd_all) <- c("Pr_IBD2", "Pr_IBD1", "Pr_IBD0")
  assign("pibd_all_default", pibd_all, envir = env_proj)

  # Set default parameters for autosomal STR
  assign("maf_default", 0.001, envir = env_proj)
  assign("meth_d_default", 1, envir = env_proj)
  assign("pd_default", 0.5, envir = env_proj)
}

# Set objects of env_proj for sample names
set_env_proj_sn <- function(env_proj, bool_new, sn_q_new = character(0), sn_r_new = character(0)){
  if(bool_new){
    assign("sn_q_all", character(0), envir = env_proj)
    assign("sn_r_all", character(0), envir = env_proj)
  }else{
    sn_q_all <- get("sn_q_all", pos = env_proj)
    sn_r_all <- get("sn_r_all", pos = env_proj)
    assign("sn_q_all", union(sn_q_all, sn_q_new), envir = env_proj)
    assign("sn_r_all", union(sn_r_all, sn_r_new), envir = env_proj)
  }
}

# Open a new project
renew_proj <- function(env_proj, env_gui){
  sign_ok <- "ok"
  fin_auto <- get("fin_auto", pos = env_proj)
  fin_y <- get("fin_y", pos = env_proj)
  fin_mt <- get("fin_mt", pos = env_proj)
  if(fin_auto || fin_y || fin_mt){
    sign_ok <- tclvalue(tkmessageBox(message = "Unsaved data will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
  }
  if(sign_ok == "ok"){
    set_env_proj_auto(env_proj, TRUE)
    set_env_proj_y(env_proj, TRUE)
    set_env_proj_mt(env_proj, TRUE)
    make_tab1(env_proj, env_gui)
    make_tab2(env_proj, env_gui)
    make_tab3(env_proj, env_gui)
    make_tab4(env_proj, env_gui)
    make_tab5(env_proj, env_gui)
    make_tab6(env_proj, env_gui)
  }
}

# Load a new project
load_proj <- function(tf, env_proj_old, env_gui){
  sign_ok <- "ok"
  fin_auto <- get("fin_auto", pos = env_proj_old)
  fin_y <- get("fin_y", pos = env_proj_old)
  fin_mt <- get("fin_mt", pos = env_proj_old)
  if(fin_auto || fin_y || fin_mt){
    sign_ok <- tclvalue(tkmessageBox(message = "Unsaved data will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
  }
  if(sign_ok == "ok"){
    fp_var <- tclVar("")
    fp <- tclvalue(tkgetOpenFile(parent = tf, initialdir = tclvalue(fp_var), multiple = "true", filetypes = "{{R Data Files} {.Rdata}}"))
    if(!nchar(fp)){
      tkmessageBox(message = "No file was selected!", icon = "error", type = "ok")
    }else{
      load(fp)
      for(i in ls(env_proj, all.names = TRUE)){
        assign(i, get(i, env_proj), env_proj_old)
      }
      load(fp)
      make_tab1(env_proj_old, env_gui)
      make_tab2(env_proj_old, env_gui)
      make_tab3(env_proj_old, env_gui)
      make_tab4(env_proj_old, env_gui)
      make_tab5(env_proj_old, env_gui)
      make_tab6(env_proj_old, env_gui)
    }
  }
}

# Save a project
save_proj <- function(env_proj){
  save_as <- tkgetSaveFile(filetypes = "{{R Data Files} {.RData}}")
  if(tclvalue(save_as) != ""){
    if(substr(tclvalue(save_as), nchar(tclvalue(save_as)) - 5, nchar(tclvalue(save_as))) == ".RData"){
      name_report <- tclvalue(save_as)
    }else{
      name_report <- paste0(tclvalue(save_as), ".RData")
    }
    save(env_proj, file = name_report)
  }
}

# Quit relsearch
quit_relsearch <- function(env_proj, tf){
  fin_auto <- get("fin_auto", pos = env_proj)
  fin_y <- get("fin_y", pos = env_proj)
  fin_mt <- get("fin_mt", pos = env_proj)

  sign_ok <- "ok"
  if(any(fin_auto, fin_y, fin_mt)){
    sign_ok <- tclvalue(tkmessageBox(message = "Unsaved data will be deleted. Do you want to quit?", type = "okcancel", icon = "warning"))
  }
  if(sign_ok == "ok"){
    tkdestroy(tf)
  }
}

# Export data
export_data <- function(data, bool_rownames){
  save_as <- tkgetSaveFile(filetypes = "{{CSV Files} {.csv}}")
  if(tclvalue(save_as) != ""){
    if(substr(tclvalue(save_as), nchar(tclvalue(save_as)) - 3, nchar(tclvalue(save_as))) == ".csv"){
      fp <- tclvalue(save_as)
    }else{
      fp <- paste(tclvalue(save_as), ".csv", sep = "")
    }
    write.csv(data, file = fp, row.names = bool_rownames)
  }
}

#' relsearch
#'
#' @description Main window of relsearch
#' @usage relsearch()
#' @export
relsearch <- function(){
  # Set environment variable (env_proj)
  if(is.element("env_proj", ls(envir = parent.env(environment())))){
    env_proj <- get("env_proj", pos = parent.env(environment()))
  }else{
    env_proj <- new.env(parent = globalenv())
    set_env_proj_auto(env_proj, TRUE)
    set_env_proj_y(env_proj, TRUE)
    set_env_proj_mt(env_proj, TRUE)
    set_env_proj_default(env_proj)
    set_env_proj_sn(env_proj, TRUE)
  }

  # Set environment variable (env_gui)
  env_gui <- new.env(parent = globalenv())

  # Set software version
  ver_soft <- packageVersion("relsearch")
  assign("ver_soft", ver_soft, envir = env_gui)

  # Set package path
#  path_pack <- "D:/RStudio_GitHub/relsearch/inst"
  path_pack <- path.package("relsearch", quiet = FALSE)
  assign("path_pack", path_pack, envir = env_gui)

  # Make a top frame
  tf <- tktoplevel()
  tkwm.title(tf, paste("relsearch ver. ", ver_soft, sep = ""))

  # Make top menu
  top_menu <- tkmenu(tf)
  tkconfigure(tf, menu = top_menu)

  # Make file menu
  file_menu <- tkmenu(top_menu, tearoff = FALSE, activebackground = "lightskyblue1")
  tkadd(top_menu, "cascade", label = "File", menu = file_menu)
  tkadd(file_menu, "command", label = "New project", command = function() renew_proj(env_proj, env_gui))
  tkadd(file_menu, "command", label = "Load project", command = function() load_proj(tf, env_proj, env_gui))
  tkadd(file_menu, "command", label = "Save project", command = function() save_proj(env_proj))
  tkadd(file_menu, "separator")
  tkadd(file_menu, "command", label = "Quit", command = function() quit_relsearch(env_proj, tf))

  # Make tools menu
  tools_menu <- tkmenu(top_menu, tearoff = FALSE, activebackground = "lightskyblue1")
  tkadd(top_menu, "cascade", label = "Tools", menu = tools_menu)
  tkadd(tools_menu, "command", label = "Set criteria", command = function() set_criteria(env_proj, env_gui))
  tkadd(tools_menu, "command", label = "Set mutation rates for autosomal STR", command = function() set_myu(env_proj, env_gui))
  tkadd(tools_menu, "command", label = "Set IBD probabilities for autosomal STR", command = function() set_pibd(env_proj, env_gui))
  tkadd(tools_menu, "command", label = "Set analysis methods for autosomal STR", command = function() set_auto(env_proj, env_gui))

  # Make help menu
  help_menu <- tkmenu(top_menu, tearoff = FALSE, activebackground = "lightskyblue1")
  tkadd(top_menu, "cascade", label = "Help", menu = help_menu)
  tkadd(help_menu, "command", label = "Manual", command = function() browseURL(paste0(path_pack, "/extdata/manual/relsearch_", ver_soft, "_manual.pdf")))

  # Define tabs
  tabs <- tk2notebook(tf, tabs = c("STR analysis", "STR results", "Y analysis", "Y results", "mtDNA analysis", "mtDNA results", "Combined results"))
  tab1 <- tk2notetab(tabs, "STR analysis")
  tab2 <- tk2notetab(tabs, "STR results")
  tab3 <- tk2notetab(tabs, "Y analysis")
  tab4 <- tk2notetab(tabs, "Y results")
  tab5 <- tk2notetab(tabs, "mtDNA analysis")
  tab6 <- tk2notetab(tabs, "mtDNA results")
  tab7 <- tk2notetab(tabs, "Combined results")

  # Define frames
  frame_tab1 <- tkframe(tab1)
  frame_tab2 <- tkframe(tab2)
  frame_tab3 <- tkframe(tab3)
  frame_tab4 <- tkframe(tab4)
  frame_tab5 <- tkframe(tab5)
  frame_tab6 <- tkframe(tab6)
  frame_tab7 <- tkframe(tab7)

  assign("tabs", tabs, envir = env_gui)
  assign("tab1", tab1, envir = env_gui)
  assign("tab2", tab2, envir = env_gui)
  assign("tab3", tab3, envir = env_gui)
  assign("tab4", tab4, envir = env_gui)
  assign("tab5", tab5, envir = env_gui)
  assign("tab6", tab6, envir = env_gui)
  assign("tab7", tab7, envir = env_gui)
  assign("frame_tab1", frame_tab1, envir = env_gui)
  assign("frame_tab2", frame_tab2, envir = env_gui)
  assign("frame_tab3", frame_tab3, envir = env_gui)
  assign("frame_tab4", frame_tab4, envir = env_gui)
  assign("frame_tab5", frame_tab5, envir = env_gui)
  assign("frame_tab6", frame_tab6, envir = env_gui)
  assign("frame_tab7", frame_tab7, envir = env_gui)

  tkpack(tabs, fill = "both", expand = 1)
  make_tab1(env_proj, env_gui)
  make_tab3(env_proj, env_gui)
  make_tab5(env_proj, env_gui)
}
