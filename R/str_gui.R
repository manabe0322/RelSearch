conditionStr <- function(envProj, envGUI){
  saveConditionStr <- function(){
    inputOk <- "ok"
    finStr <- get("finStr", pos = envProj)
    if(finStr){
      inputOk <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(inputOk == "ok"){
      assign("maf", as.numeric(tclvalue(mafVar)), envir = envProj)
      assign("dropMethStr", as.numeric(tclvalue(dropMethStrVar)), envir = envProj)
      assign("pd", as.numeric(tclvalue(pdVar)), envir = envProj)
      assign("finStr", FALSE, envir = envProj)
      tabStrResult(envProj, envGUI)
      tkdestroy(tf)
    }
  }

  maf <- get("maf", pos = envProj)
  mafVar <- tclVar(maf)
  dropMethStr <- get("dropMethStr", pos = envProj)
  dropMethStrVar <- tclVar(dropMethStr)
  pd <- get("pd", pos = envProj)
  pdVar <- tclVar(pd)

  tf <- tktoplevel()
  tkwm.title(tf, "STR condition")

  labelMaf <- tklabel(tf, text = "Minimum allele frequency")
  entryMaf <- tkentry(tf, textvariable = mafVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

  labelDrop <- tklabel(tf, text = "Drop-out of query genotypes")
  radioDropMeth0 <- tkradiobutton(tf, anchor = "w", width = 60, state = "normal", text = "Not consider", variable = dropMethStrVar, value = 0)
  radioDropMeth1 <- tkradiobutton(tf, anchor = "w", width = 60, state = "normal", text = "Consider only in the case that one allele is designated", variable = dropMethStrVar, value = 1)
  radioDropMeth2 <- tkradiobutton(tf, anchor = "w", width = 60, state = "normal", text = "Consider also in the case that two alleles in homozygotes are designated", variable = dropMethStrVar, value = 2)

  labelPD <- tklabel(tf, text = "Probability of drop-out")
  entryPD <- tkentry(tf, textvariable = pdVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
  buttSave <- tkbutton(tf, text = "    Save    ", cursor = "hand2", command = function() saveConditionStr())

  tkgrid(labelMaf, entryMaf, padx = 10, pady = 5, sticky = "w")
  tkgrid(labelDrop, radioDropMeth0, padx = 10, pady = 5, sticky = "w")
  tkgrid(tklabel(tf, text = ""), radioDropMeth1, padx = 10, pady = 5, sticky = "w")
  tkgrid(tklabel(tf, text = ""), radioDropMeth2, padx = 10, pady = 5, sticky = "w")
  tkgrid(labelPD, entryPD, padx = 10, pady = 5, sticky = "w")
  tkgrid(buttSave, padx = 10, pady = 5, sticky = "w")
}

mutationStr <- function(envProj, envGUI){
  editMutationStr1 <- function(){
    myuMlb <- get("myuMlb", pos = envMyu)
    if(tclvalue(tkcurselection(myuMlb)) == ""){
      tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
    }else{
      tf <- tktoplevel()
      tkwm.title(tf, "Edit a mutation rate")

      posSelect <- as.numeric(tclvalue(tkcurselection(myuMlb))) + 1
      myuStr <- get("myuStr", pos = envMyu)
      myuOneL <- myuStr[posSelect]

      frameEdit1 <- tkframe(tf)
      tkgrid(tklabel(frameEdit1, text = "Locus name"), tklabel(frameEdit1, text = "Mutation rate"), padx = 10, pady = 5)
      labelEditL <- tklabel(frameEdit1, text = names(myuOneL))
      editMyuVar <- tclVar(myuOneL)
      entryEditMyu <- tkentry(frameEdit1, textvariable = editMyuVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      tkgrid(labelEditL, entryEditMyu, padx = 10, pady = 5)
      tkgrid(frameEdit1, padx = 20)

      frameEdit2 <- tkframe(tf)
      tkgrid(tkbutton(frameEdit2, text = "    Save    ", cursor = "hand2", command = function() editMutationStr2(tf, myuMlb, posSelect, as.numeric(tclvalue(editMyuVar)))), pady = 10)
      tkgrid(frameEdit2)
    }
  }

  editMutationStr2 <- function(tf, myuMlb, posSelect, editMyu){
    inputOk <- "ok"
    finStr <- get("finStr", pos = envProj)
    if(finStr){
      inputOk <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(inputOk == "ok"){
      assign("finStr", FALSE, envir = envProj)
      tabStrResult(envProj, envGUI)

      myuStr <- get("myuStr", pos = envMyu)
      myuStr[posSelect] <- editMyu
      assign("myuStr", myuStr, envir = envMyu)
      myuSave <- cbind(names(myuStr), myuStr)
      colnames(myuSave) <- c("Marker", "Myu")
      write.csv(myuSave, paste0(pathPack, "/parameters/myu.csv"), row.names = FALSE)

      tkdestroy(myuMlb)
      myuMlb <- tk2mclistbox(frameMyu1, width = 30, height = 20, resizablecolumns = TRUE, selectmode = "single")
      tk2column(myuMlb, "add", label = "Locus", width = 15)
      tk2column(myuMlb, "add", label = "Mutation rate", width = 15)
      tkgrid(myuMlb)
      tk2insert.multi(myuMlb, "end", cbind(names(myuStr), myuStr))
      assign("myuMlb", myuMlb, envir = envMyu)

      tkdestroy(tf)
    }
  }

  addMutationStr1 <- function(){
    tf <- tktoplevel()
    tkwm.title(tf, "Add a locus")

    frameAdd1 <- tkframe(tf)
    tkgrid(tklabel(frameAdd1, text = "Locus name"), tklabel(frameAdd1, text = "Mutation rate"), padx = 10, pady = 5)
    nameAddLVar <- tclVar("")
    entryAddL <- tkentry(frameAdd1, textvariable = nameAddLVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    addMyuVar <- tclVar("")
    entryAddMyu <- tkentry(frameAdd1, textvariable = addMyuVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    tkgrid(entryAddL, entryAddMyu, padx = 10, pady = 5)
    tkgrid(frameAdd1)

    frameAdd2 <- tkframe(tf)
    tkgrid(tkbutton(frameAdd2, text = "    Save    ", cursor = "hand2", command = function() addMutationStr2(tf, tclvalue(nameAddLVar), as.numeric(tclvalue(addMyuVar)))), pady = 10)
    tkgrid(frameAdd2)
  }

  addMutationStr2 <- function(tf, nameAddL, addMyu){
    inputOk <- "ok"
    finStr <- get("finStr", pos = envProj)
    if(finStr){
      inputOk <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(inputOk == "ok"){
      assign("finStr", FALSE, envir = envProj)
      tabStrResult(envProj, envGUI)

      myuStr <- get("myuStr", pos = envMyu)
      nameL <- names(myuStr)
      myuStr[length(myuStr) + 1] <- addMyu
      names(myuStr) <- c(nameL, nameAddL)
      assign("myuStr", myuStr, envir = envMyu)
      myuSave <- cbind(names(myuStr), myuStr)
      colnames(myuSave) <- c("Marker", "Myu")
      write.csv(myuSave, paste0(pathPack, "/parameters/myu.csv"), row.names = FALSE)

      myuMlb <- get("myuMlb", pos = envMyu)
      tk2insert.multi(myuMlb, "end", c(nameAddL, addMyu))
      assign("myuMlb", myuMlb, envir = envMyu)
      tkdestroy(tf)
    }
  }

  deleteMutationStr <- function(){
    myuMlb <- get("myuMlb", pos = envMyu)
    if(tclvalue(tkcurselection(myuMlb)) == ""){
      tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
    }else{
      inputOk <- "ok"
      finStr <- get("finStr", pos = envProj)
      if(finStr){
        inputOk <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
      }
      if(inputOk == "ok"){
        assign("finStr", FALSE, envir = envProj)
        tabStrResult(envProj, envGUI)

        myuStr <- get("myuStr", pos = envMyu)
        posSelect <- as.numeric(tclvalue(tkcurselection(myuMlb))) + 1
        myuStr <- myuStr[-posSelect]
        assign("myuStr", myuStr, envir = envMyu)
        myuSave <- cbind(names(myuStr), myuStr)
        colnames(myuSave) <- c("Marker", "Myu")
        write.csv(myuSave, paste0(pathPack, "/parameters/myu.csv"), row.names = FALSE)

        tkdestroy(myuMlb)
        myuMlb <- tk2mclistbox(frameMyu1, width = 30, height = 20, resizablecolumns = TRUE, selectmode = "single")
        tk2column(myuMlb, "add", label = "Locus", width = 15)
        tk2column(myuMlb, "add", label = "Mutation rate", width = 15)
        tkgrid(myuMlb)
        tk2insert.multi(myuMlb, "end", cbind(names(myuStr), myuStr))
        assign("myuMlb", myuMlb, envir = envMyu)
      }
    }
  }

  pathPack <- get("pathPack", pos = envGUI)
  parFn <- list.files(paste0(pathPack, "/parameters"))
  if(is.element("myu.csv", parFn)){
    myuStr <- read.csv(paste0(pathPack, "/parameters/myu.csv"), header = TRUE)
    myuStr <- as.matrix(myuStr)
    nameL <- myuStr[, colnames(myuStr) == "Marker"]
    myuStr <- as.numeric(myuStr[, colnames(myuStr) == "Myu"])
    names(myuStr) <- nameL
  }else{
    myuStr <- myuStrDefault
    nameL <- names(myuStr)
  }
  envMyu <- new.env(parent = globalenv())
  assign("myuStr", myuStr, envir = envMyu)

  tf <- tktoplevel()
  tkwm.title(tf, "STR mutation")

  frameMyu1 <- tkframe(tf)
  myuMlb <- tk2mclistbox(frameMyu1, width = 30, height = 20, resizablecolumns = TRUE, selectmode = "single")
  tk2column(myuMlb, "add", label = "Locus", width = 15)
  tk2column(myuMlb, "add", label = "Mutation rate", width = 15)
  tkgrid(myuMlb)
  tk2insert.multi(myuMlb, "end", cbind(nameL, myuStr))
  assign("myuMlb", myuMlb, envir = envMyu)
  tkgrid(frameMyu1)

  frameMyu2 <- tkframe(tf)
  buttEdit <- tkbutton(frameMyu2, text = "    Edit    ", cursor = "hand2", command = function() editMutationStr1())
  buttAdd <- tkbutton(frameMyu2, text = "    Add    ", cursor = "hand2", command = function() addMutationStr1())
  buttDelete <- tkbutton(frameMyu2, text = "    Delete    ", cursor = "hand2", command = function() deleteMutationStr())
  tkgrid(buttEdit, buttAdd, buttDelete, padx = 20, pady = 5)
  tkgrid(frameMyu2)
}

probIBDStr <- function(envProj, envGUI){
  editIBD1 <- function(){
    ibdMlb <- get("ibdMlb", pos = envIBD)
    if(tclvalue(tkcurselection(ibdMlb)) == ""){
      tkmessageBox(message = "Select one relationship!", icon = "error", type = "ok")
    }else{
      tf <- tktoplevel()
      tkwm.title(tf, "Edit IBD probabilities")

      posSelect <- as.numeric(tclvalue(tkcurselection(ibdMlb))) + 1
      probIBDAll <- get("probIBDAll", pos = envIBD)
      pIBD <- probIBDAll[posSelect, ]
      rel <- rownames(probIBDAll)[posSelect]

      frameEdit1 <- tkframe(tf)
      tkgrid(tklabel(frameEdit1, text = "Relationship"),
             tklabel(frameEdit1, text = "Pr (IBD = 2)"),
             tklabel(frameEdit1, text = "Pr (IBD = 1)"),
             tklabel(frameEdit1, text = "Pr (IBD = 0)"),
             padx = 10, pady = 5)
      labelEditRel <- tklabel(frameEdit1, text = rel)
      editPrIBD2Var <- tclVar(pIBD[1])
      editPrIBD1Var <- tclVar(pIBD[2])
      editPrIBD0Var <- tclVar(pIBD[3])
      entryEditPrIBD2 <- tkentry(frameEdit1, textvariable = editPrIBD2Var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      entryEditPrIBD1 <- tkentry(frameEdit1, textvariable = editPrIBD1Var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      entryEditPrIBD0 <- tkentry(frameEdit1, textvariable = editPrIBD0Var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      tkgrid(labelEditRel, entryEditPrIBD2, entryEditPrIBD1, entryEditPrIBD0, padx = 10, pady = 5)
      tkgrid(frameEdit1)

      frameEdit2 <- tkframe(tf)
      tkgrid(tkbutton(frameEdit2, text = "    Save    ", cursor = "hand2", command = function() editIBD2(tf, ibdMlb, posSelect, as.numeric(tclvalue(editPrIBD2Var)), as.numeric(tclvalue(editPrIBD1Var)), as.numeric(tclvalue(editPrIBD0Var)))), pady = 10)
      tkgrid(frameEdit2)
    }
  }

  editIBD2 <- function(tf, ibdMlb, posSelect, editPrIBD2, editPrIBD1, editPrIBD0){
    inputOk <- "ok"
    finStr <- get("finStr", pos = envProj)
    if(finStr){
      inputOk <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(inputOk == "ok"){
      assign("finStr", FALSE, envir = envProj)
      tabStrResult(envProj, envGUI)

      probIBDAll <- get("probIBDAll", pos = envIBD)
      relationship <- rownames(probIBDAll)
      probIBDAll[posSelect, ] <- c(editPrIBD2, editPrIBD1, editPrIBD0)
      assign("probIBDAll", probIBDAll, envir = envIBD)
      write.csv(probIBDAll, paste0(pathPack, "/parameters/ibd.csv"))

      tkdestroy(ibdMlb)
      ibdMlb <- tk2mclistbox(frameIBD1, width = 60, height = 20, resizablecolumns = TRUE, selectmode = "single")
      tk2column(ibdMlb, "add", label = "Relationship", width = 15)
      tk2column(ibdMlb, "add", label = "Pr (IBD = 2)", width = 15)
      tk2column(ibdMlb, "add", label = "Pr (IBD = 1)", width = 15)
      tk2column(ibdMlb, "add", label = "Pr (IBD = 0)", width = 15)
      tkgrid(ibdMlb)
      tk2insert.multi(ibdMlb, "end", cbind(relationship, probIBDAll))
      assign("ibdMlb", ibdMlb, envir = envIBD)

      tkdestroy(tf)
    }
  }

  addIBD1 <- function(){
    tf <- tktoplevel()
    tkwm.title(tf, "Add a relationship")

    frameAdd1 <- tkframe(tf)
    tkgrid(tklabel(frameAdd1, text = "Relationship"),
           tklabel(frameAdd1, text = "Pr (IBD = 2)"),
           tklabel(frameAdd1, text = "Pr (IBD = 1)"),
           tklabel(frameAdd1, text = "Pr (IBD = 0)"),
           padx = 10, pady = 5)
    nameAddRelVar <- tclVar("")
    entryAddRel <- tkentry(frameAdd1, textvariable = nameAddRelVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    addPrIBD2Var <- tclVar("")
    addPrIBD1Var <- tclVar("")
    addPrIBD0Var <- tclVar("")
    entryAddPrIBD2 <- tkentry(frameAdd1, textvariable = addPrIBD2Var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    entryAddPrIBD1 <- tkentry(frameAdd1, textvariable = addPrIBD1Var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    entryAddPrIBD0 <- tkentry(frameAdd1, textvariable = addPrIBD0Var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    tkgrid(entryAddRel, entryAddPrIBD2, entryAddPrIBD1, entryAddPrIBD0, padx = 10, pady = 5)
    tkgrid(frameAdd1)

    frameAdd2 <- tkframe(tf)
    tkgrid(tkbutton(frameAdd2, text = "    Save    ", cursor = "hand2", command = function() addIBD2(tf, tclvalue(nameAddRelVar), as.numeric(tclvalue(addPrIBD2Var)), as.numeric(tclvalue(addPrIBD1Var)), as.numeric(tclvalue(addPrIBD0Var)))), pady = 10)
    tkgrid(frameAdd2)
  }

  addIBD2 <- function(tf, nameAddRel, addPrIBD2, addPrIBD1, addPrIBD0){
    inputOk <- "ok"
    finStr <- get("finStr", pos = envProj)
    if(finStr){
      inputOk <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(inputOk == "ok"){
      assign("finStr", FALSE, envir = envProj)
      tabStrResult(envProj, envGUI)

      probIBDAll <- get("probIBDAll", pos = envIBD)
      relationship <- rownames(probIBDAll)
      probIBDAll <- rbind(probIBDAll, c(addPrIBD2, addPrIBD1, addPrIBD0))
      rownames(probIBDAll) <- c(relationship, nameAddRel)
      assign("probIBDAll", probIBDAll, envir = envIBD)
      write.csv(probIBDAll, paste0(pathPack, "/parameters/ibd.csv"))

      ibdMlb <- get("ibdMlb", pos = envIBD)
      tk2insert.multi(ibdMlb, "end", c(nameAddRel, addPrIBD2, addPrIBD1, addPrIBD0))
      assign("ibdMlb", ibdMlb, envir = envIBD)
      tkdestroy(tf)
    }
  }

  deleteIBD <- function(){
    ibdMlb <- get("ibdMlb", pos = envIBD)
    if(tclvalue(tkcurselection(ibdMlb)) == ""){
      tkmessageBox(message = "Select one relationship!", icon = "error", type = "ok")
    }else{
      inputOk <- "ok"
      finStr <- get("finStr", pos = envProj)
      if(finStr){
        inputOk <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
      }
      if(inputOk == "ok"){
        assign("finStr", FALSE, envir = envProj)
        tabStrResult(envProj, envGUI)

        probIBDAll <- get("probIBDAll", pos = envIBD)
        posSelect <- as.numeric(tclvalue(tkcurselection(ibdMlb))) + 1
        probIBDAll <- probIBDAll[-posSelect, , drop = FALSE]
        relationship <- rownames(probIBDAll)
        assign("probIBDAll", probIBDAll, envir = envIBD)
        write.csv(probIBDAll, paste0(pathPack, "/parameters/ibd.csv"))

        tkdestroy(ibdMlb)
        ibdMlb <- tk2mclistbox(frameIBD1, width = 60, height = 20, resizablecolumns = TRUE, selectmode = "single")
        tk2column(ibdMlb, "add", label = "Relationship", width = 15)
        tk2column(ibdMlb, "add", label = "Pr (IBD = 2)", width = 15)
        tk2column(ibdMlb, "add", label = "Pr (IBD = 1)", width = 15)
        tk2column(ibdMlb, "add", label = "Pr (IBD = 0)", width = 15)
        tkgrid(ibdMlb)
        tk2insert.multi(ibdMlb, "end", cbind(relationship, probIBDAll))
        assign("ibdMlb", ibdMlb, envir = envIBD)
      }
    }
  }

  pathPack <- get("pathPack", pos = envGUI)
  parFn <- list.files(paste0(pathPack, "/parameters"))
  if(is.element("ibd.csv", parFn)){
    probIBDAll <- read.csv(paste0(pathPack, "/parameters/ibd.csv"), header = TRUE, row.names = 1)
    probIBDAll <- as.matrix(probIBDAll)
  }else{
    probIBDAll <- probIBDDefault
  }
  envIBD <- new.env(parent = globalenv())
  assign("probIBDAll", probIBDAll, envir = envIBD)
  relationship <- rownames(probIBDAll)

  tf <- tktoplevel()
  tkwm.title(tf, "IBD probabilities")

  frameIBD1 <- tkframe(tf)
  ibdMlb <- tk2mclistbox(frameIBD1, width = 60, height = 20, resizablecolumns = TRUE, selectmode = "single")
  tk2column(ibdMlb, "add", label = "Relationship", width = 15)
  tk2column(ibdMlb, "add", label = "Pr (IBD = 2)", width = 15)
  tk2column(ibdMlb, "add", label = "Pr (IBD = 1)", width = 15)
  tk2column(ibdMlb, "add", label = "Pr (IBD = 0)", width = 15)
  tkgrid(ibdMlb)
  tk2insert.multi(ibdMlb, "end", cbind(relationship, probIBDAll))
  assign("ibdMlb", ibdMlb, envir = envIBD)
  tkgrid(frameIBD1)

  frameIBD2 <- tkframe(tf)
  buttEdit <- tkbutton(frameIBD2, text = "    Edit    ", cursor = "hand2", command = function() editIBD1())
  buttAdd <- tkbutton(frameIBD2, text = "    Add    ", cursor = "hand2", command = function() addIBD1())
  buttDelete <- tkbutton(frameIBD2, text = "    Delete    ", cursor = "hand2", command = function() deleteIBD())
  tkgrid(buttEdit, buttAdd, buttDelete, padx = 10, pady = 5)
  tkgrid(frameIBD2)
}

tabStrSet <- function(envProj, envGUI){
  openFile <- function(type){
    inputOk <- "ok"
    finStr <- get("finStr", pos = envProj)
    if(finStr){
      inputOk <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }

    if(inputOk == "ok"){
      setEnvProj_str(envProj, FALSE)
      tabStrResult(envProj, envGUI)

      if(type == "query"){
        fp <- get("qFpStr", pos = envProj)
        fn <- get("qFnStr", pos = envProj)
      }else if(type == "ref"){
        fp <- get("rFpStr", pos = envProj)
        fn <- get("rFnStr", pos = envProj)
      }else if(type == "af"){
        fp <- get("afFpStr", pos = envProj)
        fn <- get("afFnStr", pos = envProj)
      }
      fpVar <- tclVar(fp)
      fnVar <- tclVar(fn)

      fileName <- tclvalue(tkgetOpenFile(initialdir = tclvalue(fpVar), multiple = "true", filetypes = "{{CSV Files} {.csv}}"))
      if(!nchar(fileName)){
        tkmessageBox(message = "No file was selected!", icon = "error", type = "ok")
      }else{
        tmp <- sub("\\}", fileName, replacement = "")
        tmp2 <- sub("\\{", tmp, replacement = "")
        tclvalue(fpVar) <- tmp2
        foo3 <- strsplit(tmp2, "/")[[1]]
        tclvalue(fnVar) <- strsplit(foo3[length(foo3)], "\\.csv")[[1]][1]
        if(type == "query"){
          tkconfigure(labelQFile, textvariable = fnVar)
          qStrInput <- read.csv(tclvalue(fpVar), header = TRUE)
          qStrInput <- as.matrix(qStrInput)
          qStrInput[is.na(qStrInput)] <- ""
          assign("qStrInput", qStrInput, envir = envProj)
          assign("qFpStr", tclvalue(fpVar), envir = envProj)
          assign("qFnStr", tclvalue(fnVar), envir = envProj)
        }else if(type == "ref"){
          tkconfigure(labelRFile, textvariable = fnVar)
          rStrInput <- read.csv(tclvalue(fpVar), header = TRUE)
          rStrInput <- as.matrix(rStrInput)
          rStrInput[is.na(rStrInput)] <- ""
          assign("rStrInput", rStrInput, envir = envProj)
          assign("rFpStr", tclvalue(fpVar), envir = envProj)
          assign("rFnStr", tclvalue(fnVar), envir = envProj)
        }else if(type == "af"){
          tkconfigure(labelAfFile, textvariable = fnVar)
          afInput <- read.csv(tclvalue(fpVar), header = TRUE)
          afInput <- as.matrix(afInput)
          assign("afInput", afInput, envir = envProj)
          assign("afFpStr", tclvalue(fpVar), envir = envProj)
          assign("afFnStr", tclvalue(fnVar), envir = envProj)
        }
      }
    }
  }

  qFnStr <- get("qFnStr", pos = envProj)
  qFnStrVar <- tclVar(qFnStr)
  rFnStr <- get("rFnStr", pos = envProj)
  rFnStrVar <- tclVar(rFnStr)
  afFnStr <- get("afFnStr", pos = envProj)
  afFnStrVar <- tclVar(afFnStr)

  tab1 <- get("tab1", pos = envGUI)
  frameTab1 <- get("frameTab1", pos = envGUI)
  tkdestroy(frameTab1)
  frameTab1 <- tkframe(tab1)
  assign("frameTab1", frameTab1, envir = envGUI)

  frame1_1 <- tkframe(frameTab1, relief = "groove", borderwidth = 2)
  frame1_2 <- tkframe(frameTab1, relief = "groove", borderwidth = 2)
  frame1_3 <- tkframe(frameTab1)

  labelQ <- tklabel(frame1_1, text = "Query database")
  labelQFile <- tklabel(frame1_1, textvariable = qFnStrVar, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttQ <- tkbutton(frame1_1, text = "    Load    ", cursor = "hand2", command = function() openFile("query"))

  labelR <- tklabel(frame1_1, text = "Reference database")
  labelRFile <- tklabel(frame1_1, textvariable = rFnStrVar, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttR <- tkbutton(frame1_1, text = "    Load    ", cursor = "hand2", command = function() openFile("ref"))

  labelAf <- tklabel(frame1_1, text = "Allele frequencies")
  labelAfFile <- tklabel(frame1_1, textvariable = afFnStrVar, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttAf <- tkbutton(frame1_1, text = "    Load    ", cursor = "hand2", command = function() openFile("af"))

  tkgrid(tklabel(frame1_1, text = "Input files", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
  tkgrid(labelQ, labelQFile, buttQ, padx = 10, pady = 5, sticky = "w")
  tkgrid(labelR, labelRFile, buttR, padx = 10, pady = 5, sticky = "w")
  tkgrid(labelAf, labelAfFile, buttAf, padx = 10, pady = 5, sticky = "w")
  tkgrid(frame1_1, padx = 10, pady = 5, sticky = "w")

  buttCondition <- tkbutton(frame1_2, text = "    Condition    ", cursor = "hand2", command = function() conditionStr(envProj, envGUI))
  buttMutation <- tkbutton(frame1_2, text = "    Mutation rate    ", cursor = "hand2", command = function() mutationStr(envProj, envGUI))
  buttProbIBD <- tkbutton(frame1_2, text = "    IBD probabilities    ", cursor = "hand2", command = function() probIBDStr(envProj, envGUI))

  tkgrid(tklabel(frame1_2, text = "Setting", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
  tkgrid(buttCondition, buttMutation, buttProbIBD, padx = 10, pady = 5, sticky = "w")
  tkgrid(frame1_2, padx = 10, pady = 5, sticky = "w")

  buttScreening <- tkbutton(frame1_3, text = "    Screening    ", cursor = "hand2", command = function() guiScreenStr(envProj, envGUI))
  tkgrid(buttScreening, pady = 10)
  tkgrid(frame1_3, padx = 10, pady = 5)

  tkgrid(frameTab1)
}

guiScreenStr <- function(envProj, envGUI){
  qStrInput <- get("qStrInput", pos = envProj)
  rStrInput <- get("rStrInput", pos = envProj)
  afInput <- get("afInput", pos = envProj)
  if(any(c(length(qStrInput) == 0, length(rStrInput) == 0, length(afInput) == 0))){
    tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
  }else{
    pathPack <- get("pathPack", pos = envGUI)

    myuStr <- read.csv(paste0(pathPack, "/parameters/myu.csv"), header = TRUE)
    myuStr <- as.matrix(myuStr)
    nameMyuL <- myuStr[, colnames(myuStr) == "Marker"]
    myuStr <- as.numeric(myuStr[, colnames(myuStr) == "Myu"])
    names(myuStr) <- nameMyuL

    probIBDAll <- read.csv(paste0(pathPack, "/parameters/ibd.csv"), header = TRUE, row.names = 1)
    probIBDAll <- as.matrix(probIBDAll)

    qCol <- colnames(qStrInput)
    posQName <- grep("Sample", qCol)
    qStrName <- qStrInput[, posQName]
    qStrData <- qStrInput[, -posQName, drop = FALSE]
    nameQL <- colnames(qStrData)[which(1:ncol(qStrData) %% 2 == 1)]

    rCol <- colnames(rStrInput)
    posRName <- grep("Sample", rCol)
    rStrNameInput <- rStrInput[, posRName]
    posRelation <- grep("Relationship", rCol)
    relInput <- rStrInput[, posRelation]
    nEmpRel <- length(which(relInput == ""))
    rStrDataInput <- rStrInput[, -c(posRName, posRelation), drop = FALSE]
    nameRL <- colnames(rStrDataInput)[which(1:ncol(rStrDataInput) %% 2 == 1)]

    nameAfL <- colnames(afInput)[-1]

    judgeL1 <- all(mapply(setequal, nameQL, nameRL))
    judgeL2 <- all(mapply(setequal, nameQL, nameAfL))
    judgeL3 <- all(is.element(nameQL, nameMyuL))
    judgeR1 <- all(is.element(setdiff(relInput, ""), rownames(probIBDAll)))
    if(all(c(judgeL1, judgeL2, judgeL3, judgeR1))){
      pb <- tkProgressBar("STR screening", "0% done", 0, 100, 0)

      maf <- get("maf", pos = envProj)
      dropMethStr <- get("dropMethStr", pos = envProj)
      pd <- get("pd", pos = envProj)

      afData <- freqSetting(qStrData, rStrDataInput, afInput, maf)
      afList <- afData[[1]]
      afAlList <- afData[[2]]

      nQ <- nrow(qStrData)
      nR <- nrow(rStrDataInput)
      nL <- length(nameQL)

      myuAll <- rep(0, nL)
      for(i in 1:nL){
        myuAll[i] <- myuStr[which(nameMyuL == nameQL[i])]
      }
      names(myuAll) <- nameQL

      apeAll <- rep(0, nL)
      for(i in 1:nL){
        apeAll[i] <- calcApe(afList[[i]])
      }
      names(apeAll) <- nameQL

      likeH1All <- likeH2All <- lrAll <- array(0, dim = c(nQ, nR + 3 * nEmpRel, nL + 1))
      rStrData <- matrix(0, nR + 3 * nEmpRel, ncol(rStrDataInput))
      rStrName <- relStr <- rep(0, nR + 3 * nEmpRel)
      countCf <- 1
      for(i in 1:nR){
        ref <- as.numeric(rStrDataInput[i, ])
        rn <- rStrNameInput[i]
        rel <- relInput[i]
        if(rel == ""){
          probIBDs <- matrix(c(1, 0, 0, 
                               0, 1, 0, 
                               0.25, 0.5, 0.25, 
                               0, 0.5, 0.5), 
                             nrow = 4, byrow = TRUE)
          rStrName[countCf:(countCf + 3)] <- rn
          relStr[countCf:(countCf + 3)] <- c("direct", "parent-child", "sibling", "2nd-degree")
          consMu <- c(FALSE, TRUE, FALSE, FALSE)
        }else{
          probIBDs <- probIBDAll[rownames(probIBDAll) == rel, , drop = FALSE]
          rStrName[countCf] <- rn
          relStr[countCf] <- rel
          if(probIBDs[1, 3] == 0){
            consMu <- TRUE
          }else{
            consMu <- FALSE
          }
        }
        for(j in 1:nrow(probIBDs)){
          rStrData[countCf, ] <- ref
          for(k in 1:nQ){
            query <- as.numeric(qStrData[k, ])
            lrData <- calcKinLr(query, ref, afList, afAlList, probIBDs[j, ], consMu[j], myuAll, apeAll, dropMethStr, pd)
            likeH1All[k, countCf, ] <- lrData[1, ]
            likeH2All[k, countCf, ] <- lrData[2, ]
            lrAll[k, countCf, ] <- lrData[3, ]
            info <- sprintf("%d%% done", round((nQ * (countCf - 1) + k) * 100 / (nQ * (nR + 3 * nEmpRel))))
            setTkProgressBar(pb, (nQ * (countCf - 1) + k) * 100 / (nQ * (nR + 3 * nEmpRel)), sprintf("STR screening"), info)
          }
          countCf <- countCf + 1
        }
      }
      assign("qStrData", qStrData, envir = envProj)
      assign("rStrData", rStrData, envir = envProj)
      assign("qStrName", qStrName, envir = envProj)
      assign("rStrName", rStrName, envir = envProj)
      assign("relStr", relStr, envir = envProj)
      assign("likeH1All", likeH1All, envir = envProj)
      assign("likeH2All", likeH2All, envir = envProj)
      assign("lrAll", lrAll, envir = envProj)
#      assign("probIBDAll", probIBDAll, envir = envProj)
      assign("myuAll", myuAll, envir = envProj)
      assign("finStr", TRUE, envir = envProj)
      tabStrResult(envProj, envGUI)
      close(pb)
    }else if(!judgeL1){
      tkmessageBox(message = "Locus set is not the same between query data and reference data!", icon = "error", type = "ok")
    }else if(!judgeL2){
      tkmessageBox(message = "Locus set is not the same between query data and allele frequencies!", icon = "error", type = "ok")
    }else if(!judgeL3){
      tkmessageBox(message = "There are some loci without mutation rates!", icon = "error", type = "ok")
    }else if(!judgeR1){
      tkmessageBox(message = "There are some relationships without IBD probabilities!", icon = "error", type = "ok")
    }
  }
}

gtDisplay <- function(gtData){
  nL <- length(gtData) / 2
  gtMat <- rep("", nL)
  for(i in 1:nL){
    gt <- as.numeric(gtData[c(2 * i - 1, 2 * i)])
    gt <- gt[!is.na(gt)]
    if(length(gt) == 1){
      gtMat[i] <- gt
    }else if(length(gt) == 2){
      gtMat[i] <- paste(gt[1], ", ", gt[2], sep = "")
    }
  }
  return(gtMat)
}

tabStrResult <- function(envProj, envGUI){
  finStr <- get("finStr", pos = envProj)
  if(finStr){
    setDisplay1 <- function(){
      tf <- tktoplevel()
      tkwm.title(tf, "Set display")

      frameD1 <- tkframe(tf)
      tkgrid(tklabel(frameD1, text = "Query"),
             tklabel(frameD1, text = "Reference"),
             tklabel(frameD1, text = "Relationship"),
             tklabel(frameD1, text = "Minimum LR"),
             padx = 10, pady = 5)

      candQ <- c("All", qStrName)
      selectQ <- tclVar("All")
      comboQ <- ttkcombobox(frameD1, values = candQ, textvariable = selectQ, state = "readonly")

      candR <- c("All", unique(rStrName))
      selectR <- tclVar("All")
      comboR <- ttkcombobox(frameD1, values = candR, textvariable = selectR, state = "readonly")

      candRel <- c("All", unique(relStr))
      selectRel <- tclVar("All")
      comboRel <- ttkcombobox(frameD1, values = candRel, textvariable = selectRel, state = "readonly")

      selectMinLr <- tclVar("1")
      entryMinLr <- tkentry(frameD1, textvariable = selectMinLr, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      tkgrid(comboQ, comboR, comboRel, entryMinLr, padx = 10, pady = 5)
      tkgrid(frameD1)

      frameD2 <- tkframe(tf)
      tkgrid(tkbutton(frameD2, text = "    Set    ", cursor = "hand2",
                      command = function() setDisplay2(tf, tclvalue(selectQ), tclvalue(selectR), tclvalue(selectRel), as.numeric(tclvalue(selectMinLr)))),
             padx = 10, pady = 5)
      tkgrid(frameD2)
    }

    setDisplay2 <- function(tf, selectQ, selectR, selectRel, selectMinLr){
      if(selectQ == "All"){
        posQ <- 1:nD
      }else{
        posQ <- which(displayQ == selectQ)
      }
      if(selectR == "All"){
        posR <- 1:nD
      }else{
        posR <- which(displayR == selectR)
      }
      if(selectRel == "All"){
        posRel <- 1:nD
      }else{
        posRel <- which(displayRel == selectRel)
      }
      posLr <- which(overLrAll > selectMinLr)

      posExtract <- intersect(intersect(intersect(posQ, posR), posRel), posLr)

      if(length(posExtract) != 0){
        resultMlb <- get("resultMlb", pos = envStrResult)
        tkdestroy(resultMlb)
        scr1 <- get("scr1", pos = envStrResult)
        tkdestroy(scr1)
        scr1 <- tkscrollbar(frameResult1, repeatinterval = 5, command = function(...) tkyview(resultMlb, ...))
        resultMlb <- tk2mclistbox(frameResult1, width = 60, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(resultMlb, "add", label = "Query", width = 15)
        tk2column(resultMlb, "add", label = "Reference", width = 15)
        tk2column(resultMlb, "add", label = "Relationship", width = 15)
        tk2column(resultMlb, "add", label = "LR", width = 15)
        tkgrid(resultMlb, scr1)
        displayData <- displayDefault[posExtract, , drop = FALSE]
        displayData <- displayData[order(as.numeric(displayData[, 4]), decreasing = TRUE), ]
        tk2insert.multi(resultMlb, "end", displayData)
        assign("resultMlb", resultMlb, envir = envStrResult)
        assign("scr1", scr1, envir = envStrResult)
        assign("displayData", displayData, envir = envStrResult)
        tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")
        tkdestroy(tf)
      }else{
        tkmessageBox(message = "There is no data that meet the condition!", icon = "error", type = "ok")
      }
    }

    showDetail <- function(){
      resultMlb <- get("resultMlb", pos = envStrResult)
      if(tclvalue(tkcurselection(resultMlb)) == ""){
        tkmessageBox(message = "Select one result!", icon = "error", type = "ok")
      }else{
        displayData <- get("displayData", pos = envStrResult)
        posShow <- as.numeric(tclvalue(tkcurselection(resultMlb))) + 1
        selectQName <- displayData[posShow, 1]
        posShowQ <- which(qStrName == selectQName)
        selectRName <- displayData[posShow, 2]
        posShowR <- which(rStrName == selectRName)
        selectRel <- displayData[posShow, 3]
        posShowRel <- which(relStr == selectRel)
        posShowR <- intersect(posShowR, posShowRel)
        detailData <- matrix("", nL + 1, 6)
        detailData[, 1] <- c(names(myuAll), "overall")
        colnames(detailData) <- c("Locus",
                                  paste("Query genotype (", selectQName, ")", sep = ""),
                                  paste("Reference genotype (", selectRName, ")", sep = ""),
                                  "Likelihood (related)",
                                  "Likelihood (unrelated)",
                                  "LR")
        detailData[1:nL, 2] <- gtDisplay(qStrData[posShowQ, ])
        detailData[1:nL, 3] <- gtDisplay(rStrData[posShowR, ])
        detailData[, 4] <- signif(likeH1All[posShowQ, posShowR, ], digits = 4)
        detailData[, 5] <- signif(likeH2All[posShowQ, posShowR, ], digits = 4)
        detailData[, 6] <- signif(lrAll[posShowQ, posShowR, ], digits = 4)

        tfDetail <- tktoplevel()
        tkwm.title(tfDetail, "STR result in detail")

        frameDetail1 <- tkframe(tfDetail)
        tkgrid(tklabel(frameDetail1, text = paste("Relationship : ", selectRel, sep = "")))
        tkgrid(frameDetail1, padx = 10, pady = 5)

        frameDetail2 <- tkframe(tfDetail)
        scr2 <- tkscrollbar(frameDetail2, repeatinterval = 5, command = function(...) tkyview(detailMlb, ...))
        detailMlb <- tk2mclistbox(frameDetail2, width = 140, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr2, ...))
        tk2column(detailMlb, "add", label = "locus", width = 20)
        tk2column(detailMlb, "add", label = paste("Query genotype (", selectQName, ")", sep = ""), width = 30)
        tk2column(detailMlb, "add", label = paste("Reference genotype (", selectRName, ")", sep = ""), width = 30)
        tk2column(detailMlb, "add", label = "likelihood (related)", width = 20)
        tk2column(detailMlb, "add", label = "likelihood (unrelated)", width = 20)
        tk2column(detailMlb, "add", label = "LR", width = 20)
        tkgrid(detailMlb, scr2)
        tk2insert.multi(detailMlb, "end", detailData)
        tkgrid.configure(scr2, rowspan = 20, sticky = "nsw")
        tkgrid(frameDetail2, padx = 10, pady = 5)

        frameDetail3 <- tkframe(tfDetail)
        tkgrid(tkbutton(frameDetail3, text = "    Export    ", cursor = "hand2", command = function() exportData(detailData, FALSE)))
        tkgrid(frameDetail3, padx = 10, pady = 5)
      }
    }

    envStrResult <- new.env(parent = globalenv())

    qStrData <- get("qStrData", pos = envProj)
    rStrData <- get("rStrData", pos = envProj)
    qStrName <- get("qStrName", pos = envProj)
    rStrName <- get("rStrName", pos = envProj)
    relStr <- get("relStr", pos = envProj)
    likeH1All <- get("likeH1All", pos = envProj)
    likeH2All <- get("likeH2All", pos = envProj)
    lrAll <- get("lrAll", pos = envProj)
#    probIBDAll <- get("probIBDAll", pos = envProj)
    myuAll <- get("myuAll", pos = envProj)
#    maf <- get("maf", pos = envProj)
#    dropMethStr <- get("dropMethStr", pos = envProj)
#    pd <- get("pd", pos = envProj)

    nL <- length(myuAll)
    nQ <- length(qStrName)
    nR <- length(rStrName)

    overLrAllMat <- lrAll[, , nL + 1]
    rownames(overLrAllMat) <- qStrName
    colnames(overLrAllMat) <- rStrName
    overLrAll <- as.vector(overLrAllMat)

    nD <- nQ * nR
    displayDefault <- matrix(0, nD, 4)
    colnames(displayDefault) <- c("Query", "Reference", "Relationship", "LR")
    displayQ <- rep(qStrName, nR)
    displayDefault[, 1] <- displayQ
    displayR <- as.vector(sapply(rStrName, rep, nQ))
    displayDefault[, 2] <- displayR
    displayRel <- as.vector(sapply(relStr, rep, nQ))
    displayDefault[, 3] <- displayRel
    displayDefault[, 4] <- signif(overLrAll, digits = 4)
    displayData <- displayDefault[which(overLrAll >= 1), , drop = FALSE]
    displayData <- displayData[order(as.numeric(displayData[, 4]), decreasing = TRUE), , drop = FALSE]
    assign("displayData", displayData, envir = envStrResult)

    tabs <- get("tabs", pos = envGUI)
    tab2 <- get("tab2", pos = envGUI)
    frameTab2 <- get("frameTab2", pos = envGUI)
    tkdestroy(frameTab2)
    frameTab2 <- tkframe(tab2)

    frameResult1 <- tkframe(frameTab2)
    scr1 <- tkscrollbar(frameResult1, repeatinterval = 5, command = function(...) tkyview(resultMlb, ...))
    resultMlb <- tk2mclistbox(frameResult1, width = 60, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(resultMlb, "add", label = "Query", width = 15)
    tk2column(resultMlb, "add", label = "Reference", width = 15)
    tk2column(resultMlb, "add", label = "Relationship", width = 15)
    tk2column(resultMlb, "add", label = "LR", width = 15)
    tkgrid(resultMlb, scr1)
    tk2insert.multi(resultMlb, "end", displayData)
    assign("resultMlb", resultMlb, envir = envStrResult)
    assign("scr1", scr1, envir = envStrResult)
    tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")
    tkgrid(frameResult1, padx = 10, pady = 5)

    frameResult2 <- tkframe(frameTab2)
    buttDisplay <- tkbutton(frameResult2, text = "    Set display    ", cursor = "hand2", command = function() setDisplay1())
    buttDetail <- tkbutton(frameResult2, text = "    Show detail    ", cursor = "hand2", command = function() showDetail())
    buttExport1 <- tkbutton(frameResult2, text = "    Export displayed data    ", cursor = "hand2", command = function() exportData(get("displayData", pos = envStrResult), FALSE))
    buttExport2 <- tkbutton(frameResult2, text = "    Export All LRs    ", cursor = "hand2", command = function() exportData(overLrAllMat, TRUE))
    tkgrid(buttDisplay, buttDetail, buttExport1, buttExport2, padx = 10, pady = 5)
    tkgrid(frameResult2)

    tkgrid(frameTab2)
    tk2notetab.select(tabs, "STR results")
    assign("frameTab2", frameTab2, envir = envGUI)
  }else{
    tab2 <- get("tab2", pos = envGUI)
    frameTab2 <- get("frameTab2", pos = envGUI)
    tkdestroy(frameTab2)
    frameTab2 <- tkframe(tab2)
    assign("frameTab2", frameTab2, envir = envGUI)
  }
}
