# Set objects of env_proj for STR
set_env_proj_auto <- function(env_proj, bool_files){
  if(bool_files){
    assign("data_auto_q", NULL, envir = env_proj)
    assign("fp_auto_q", character(0), envir = env_proj)
    assign("fn_auto_q", character(0), envir = env_proj)
    assign("data_auto_r", NULL, envir = env_proj)
    assign("fp_auto_r", character(0), envir = env_proj)
    assign("fn_auto_r", character(0), envir = env_proj)
    assign("data_auto_af", NULL, envir = env_proj)
    assign("fp_auto_af", character(0), envir = env_proj)
    assign("fn_auto_af", character(0), envir = env_proj)
  }
  assign("fin_auto", FALSE, envir = env_proj)
}

# Set objects of env_proj for Y
set_env_proj_y <- function(env_proj, bool_files){
  if(bool_files){
    assign("data_y_q", NULL, envir = env_proj)
    assign("fp_y_q", character(0), envir = env_proj)
    assign("fn_y_q", character(0), envir = env_proj)
    assign("data_y_r", NULL, envir = env_proj)
    assign("fp_y_r", character(0), envir = env_proj)
    assign("fn_y_r", character(0), envir = env_proj)
  }
  assign("fin_y", FALSE, envir = env_proj)
}

# Set objects of env_proj for mtDNA
set_env_proj_mt <- function(env_proj, bool_files){
  if(bool_files){
    assign("data_mt_q", NULL, envir = env_proj)
    assign("fp_mt_q", character(0), envir = env_proj)
    assign("fn_mt_q", character(0), envir = env_proj)
    assign("data_mt_r", NULL, envir = env_proj)
    assign("fp_mt_r", character(0), envir = env_proj)
    assign("fn_mt_r", character(0), envir = env_proj)
  }
  assign("fin_mt", FALSE, envir = env_proj)
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
  tkadd(tools_menu, "command", label = "Set mutation rates for autosomal STR", command = function() set_myu(env_proj, env_gui))
  tkadd(tools_menu, "command", label = "Set IBD probabilities for autosomal STR", command = function() set_pibd(env_proj, env_gui))
  tkadd(tools_menu, "command", label = "Set analysis method for autosomal STR", command = function() set_auto(env_proj, env_gui))
  tkadd(tools_menu, "command", label = "Set analysis method for Y-STR", command = function() set_y())
  tkadd(tools_menu, "command", label = "Set analysis method for mtDNA", command = function() set_mt())

  # Make help menu
  help_menu <- tkmenu(top_menu, tearoff = FALSE, activebackground = "lightskyblue1")
  tkadd(top_menu, "cascade", label = "Help", menu = help_menu)
  tkadd(help_menu, "command", label = "Manual", command = function() browseURL(paste0(path_pack, "/extdata/manual/relsearch_", ver_soft, "_manual.pdf")))

  # Define tabs
  tabs <- tk2notebook(tf, tabs = c("STR analysis", "STR results", "Y analysis", "Y results", "mtDNA analysis", "mtDNA results"))
  tab1 <- tk2notetab(tabs, "STR analysis")
  tab2 <- tk2notetab(tabs, "STR results")
  tab3 <- tk2notetab(tabs, "Y analysis")
  tab4 <- tk2notetab(tabs, "Y results")
  tab5 <- tk2notetab(tabs, "mtDNA analysis")
  tab6 <- tk2notetab(tabs, "mtDNA results")

  # Define frames
  frame_tab1 <- tkframe(tab1)
  frame_tab2 <- tkframe(tab2)
  frame_tab3 <- tkframe(tab3)
  frame_tab4 <- tkframe(tab4)
  frame_tab5 <- tkframe(tab5)
  frame_tab6 <- tkframe(tab6)

  assign("tabs", tabs, envir = env_gui)
  assign("tab1", tab1, envir = env_gui)
  assign("tab2", tab2, envir = env_gui)
  assign("tab3", tab3, envir = env_gui)
  assign("tab4", tab4, envir = env_gui)
  assign("tab5", tab5, envir = env_gui)
  assign("tab6", tab6, envir = env_gui)
  assign("frame_tab1", frame_tab1, envir = env_gui)
  assign("frame_tab2", frame_tab2, envir = env_gui)
  assign("frame_tab3", frame_tab3, envir = env_gui)
  assign("frame_tab4", frame_tab4, envir = env_gui)
  assign("frame_tab5", frame_tab5, envir = env_gui)
  assign("frame_tab6", frame_tab6, envir = env_gui)

  tkpack(tabs, fill = "both", expand = 1)
  make_tab1(env_proj, env_gui)
  make_tab3(env_proj, env_gui)
  make_tab5(env_proj, env_gui)
}
