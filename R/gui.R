# Set objects of envProj for STR
setEnvProj_str <- function(envProj, setfiles){
  if(setfiles){
    assign("qStrInput", NULL, envir = envProj)
    assign("qFpStr", character(0), envir = envProj)
    assign("qFnStr", character(0), envir = envProj)
    assign("rStrInput", NULL, envir = envProj)
    assign("rFpStr", character(0), envir = envProj)
    assign("rFnStr", character(0), envir = envProj)
    assign("afInput", NULL, envir = envProj)
    assign("afFpStr", character(0), envir = envProj)
    assign("afFnStr", character(0), envir = envProj)
  }
  assign("maf", 0.001, envir = envProj)
  assign("pd", 0.5, envir = envProj)
  assign("dropMethStr", 1, envir = envProj)
  assign("finStr", FALSE, envir = envProj)
}

# Set objects of envProj for Y
setEnvProj_y <- function(envProj, setfiles){
  if(setfiles){
    assign("qYInput", NULL, envir = envProj)
    assign("qFpY", character(0), envir = envProj)
    assign("qFnY", character(0), envir = envProj)
    assign("rYInput", NULL, envir = envProj)
    assign("rFpY", character(0), envir = envProj)
    assign("rFnY", character(0), envir = envProj)
  }
  assign("finY", FALSE, envir = envProj)
}

# Set objects of envProj for mtDNA
setEnvProj_mt <- function(envProj, setfiles){
  if(setfiles){
    assign("qMtInput", NULL, envir = envProj)
    assign("qFpMt", character(0), envir = envProj)
    assign("qFnMt", character(0), envir = envProj)
    assign("rMtInput", NULL, envir = envProj)
    assign("rFpMt", character(0), envir = envProj)
    assign("rFnMt", character(0), envir = envProj)
  }
  assign("finMt", FALSE, envir = envProj)
}

# Open a new project
newProj <- function(envProj, envGUI){
  inputOk <- "ok"
  finStr <- get("finStr", pos = envProj)
  finY <- get("finY", pos = envProj)
  finMt <- get("finMt", pos = envProj)
  if(finStr || finY || finMt){
    inputOk <- tclvalue(tkmessageBox(message = "Unsaved data will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
  }
  if(inputOk == "ok"){
    setEnvProj_str(envProj, TRUE)
    setEnvProj_y(envProj, TRUE)
    setEnvProj_mt(envProj, TRUE)
    tabStrSet(envProj, envGUI)
    tabStrResult(envProj, envGUI)
    tabYSet(envProj, envGUI)
    tabYResult(envProj, envGUI)
    tabMtSet(envProj, envGUI)
    tabMtResult(envProj, envGUI)
  }
}

# Load a new project
loadProj <- function(tf, envProj_old, envGUI){
  inputOk <- "ok"
  finStr <- get("finStr", pos = envProj_old)
  finY <- get("finY", pos = envProj_old)
  finMt <- get("finMt", pos = envProj_old)
  if(finStr || finY || finMt){
    inputOk <- tclvalue(tkmessageBox(message = "Unsaved data will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
  }
  if(inputOk == "ok"){
    fpVar <- tclVar("")
    fileName <- tclvalue(tkgetOpenFile(parent = tf, initialdir = tclvalue(fpVar), multiple = "true", filetypes = "{{R Data Files} {.Rdata}}"))
    if(!nchar(fileName)){
      tkmessageBox(message = "No file was selected!", icon = "error", type = "ok")
    }else{
      load(fileName)
      for(i in ls(envProj, all.names = TRUE)){
        assign(i, get(i, envProj), envProj_old)
      }
      load(fileName)
      tabStrSet(envProj_old, envGUI)
      tabStrResult(envProj_old, envGUI)
      tabYSet(envProj_old, envGUI)
      tabYResult(envProj_old, envGUI)
      tabMtSet(envProj_old, envGUI)
      tabMtResult(envProj_old, envGUI)
    }
  }
}

# Save a project
saveProj <- function(envProj){
  saveAs <- tkgetSaveFile(filetypes = "{{R Data Files} {.RData}}")
  if(tclvalue(saveAs) != ""){
    if(substr(tclvalue(saveAs), nchar(tclvalue(saveAs)) - 5, nchar(tclvalue(saveAs))) == ".RData"){
      reportName <- tclvalue(saveAs)
    }else{
      reportName <- paste0(tclvalue(saveAs), ".RData")
    }
    save(envProj, file = reportName)
  }
}

# Export data
exportData <- function(dataMat, rowNames){
  saveAs <- tkgetSaveFile(filetypes = "{{CSV Files} {.csv}}")
  if(tclvalue(saveAs) != ""){
    if(substr(tclvalue(saveAs), nchar(tclvalue(saveAs)) - 3, nchar(tclvalue(saveAs))) == ".csv"){
      fileName <- tclvalue(saveAs)
    }else{
      fileName <- paste(tclvalue(saveAs), ".csv", sep = "")
    }
    write.csv(dataMat, file = fileName, row.names = rowNames)
  }
}

#' relsearch
#'
#' @description Main window of relsearch
#' @usage relsearch()
#' @export
relsearch <- function(){
  if(is.element("envProj", ls(envir = parent.env(environment())))){
    envProj <- get("envProj", pos = parent.env(environment()))
  }else{
    envProj <- new.env(parent = globalenv())
    setEnvProj_str(envProj, TRUE)
    setEnvProj_y(envProj, TRUE)
    setEnvProj_mt(envProj, TRUE)
  }

  envGUI <- new.env(parent = globalenv())
  softVer <- packageVersion("relsearch")
#  pathPack <- "D:/RStudio_GitHub/relsearch/inst"
  pathPack <- path.package("relsearch", quiet = FALSE)
  assign("softVer", softVer, envir = envGUI)
  assign("pathPack", pathPack, envir = envGUI)
  assign("finStr", FALSE, envir = envGUI)

  tf <- tktoplevel()
  tkwm.title(tf, paste("relsearch ver. ", softVer, sep = ""))

  topMenu <- tkmenu(tf)
  tkconfigure(tf, menu = topMenu)

  file_menu <- tkmenu(topMenu, tearoff = FALSE, activebackground = "lightskyblue1")
  tkadd(topMenu, "cascade", label = "File", menu = file_menu)
  tkadd(file_menu, "command", label = "New project", command = function() newProj(envProj, envGUI))
  tkadd(file_menu, "command", label = "Load project", command = function() loadProj(tf, envProj, envGUI))
  tkadd(file_menu, "command", label = "Save project", command = function() saveProj(envProj))
  tkadd(file_menu, "separator")
  tkadd(file_menu, "command", label = "Quit", command = function() tkdestroy(tf))

  help_menu <- tkmenu(topMenu, tearoff = FALSE, activebackground = "lightskyblue1")
  tkadd(topMenu, "cascade", label = "Help", menu = help_menu)
  tkadd(help_menu, "command", label = "Manual", command = function() browseURL(paste0(pathPack, "/manual/relsearch_v", softVer, "_manual.html")))

  tabs <- tk2notebook(tf, tabs = c("STR analysis", "STR results", "Y analysis", "Y results", "mtDNA analysis", "mtDNA results"))
  tab1 <- tk2notetab(tabs, "STR analysis")
  tab2 <- tk2notetab(tabs, "STR results")
  tab3 <- tk2notetab(tabs, "Y analysis")
  tab4 <- tk2notetab(tabs, "Y results")
  tab5 <- tk2notetab(tabs, "mtDNA analysis")
  tab6 <- tk2notetab(tabs, "mtDNA results")
  frameTab1 <- tkframe(tab1)
  frameTab2 <- tkframe(tab2)
  frameTab3 <- tkframe(tab3)
  frameTab4 <- tkframe(tab4)
  frameTab5 <- tkframe(tab5)
  frameTab6 <- tkframe(tab6)

  assign("tabs", tabs, envir = envGUI)
  assign("tab1", tab1, envir = envGUI)
  assign("tab2", tab2, envir = envGUI)
  assign("tab3", tab3, envir = envGUI)
  assign("tab4", tab4, envir = envGUI)
  assign("tab5", tab5, envir = envGUI)
  assign("tab6", tab6, envir = envGUI)
  assign("frameTab1", frameTab1, envir = envGUI)
  assign("frameTab2", frameTab2, envir = envGUI)
  assign("frameTab3", frameTab3, envir = envGUI)
  assign("frameTab4", frameTab4, envir = envGUI)
  assign("frameTab5", frameTab5, envir = envGUI)
  assign("frameTab6", frameTab6, envir = envGUI)

  tkpack(tabs, fill = "both", expand = 1)
  tabStrSet(envProj, envGUI)
  tabYSet(envProj, envGUI)
  tabMtSet(envProj, envGUI)
}
