tabYSet <- function(envProj, envGUI){
  openFile <- function(type){
    inputOk <- "ok"
    finY <- get("finY", pos = envProj)
    if(finY){
      inputOk <- tclvalue(tkmessageBox(message = "Y-STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }

    if(inputOk == "ok"){
      setEnvProj_y(envProj, FALSE)
      tabYResult(envProj, envGUI)

      if(type == "query"){
        fp <- get("qFpY", pos = envProj)
        fn <- get("qFnY", pos = envProj)
      }else if(type == "ref"){
        fp <- get("rFpY", pos = envProj)
        fn <- get("rFnY", pos = envProj)
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
          qYInput <- read.csv(tclvalue(fpVar), header = TRUE)
          qYInput <- as.matrix(qYInput)
          qYInput[is.na(qYInput)] <- ""
          assign("qYInput", qYInput, envir = envProj)
          assign("qFpY", tclvalue(fpVar), envir = envProj)
          assign("qFnY", tclvalue(fnVar), envir = envProj)
        }else if(type == "ref"){
          tkconfigure(labelRFile, textvariable = fnVar)
          rYInput <- read.csv(tclvalue(fpVar), header = TRUE)
          rYInput <- as.matrix(rYInput)
          rYInput[is.na(rYInput)] <- ""
          assign("rYInput", rYInput, envir = envProj)
          assign("rFpY", tclvalue(fpVar), envir = envProj)
          assign("rFnY", tclvalue(fnVar), envir = envProj)
        }
      }
    }
  }

  qFnY <- get("qFnY", pos = envProj)
  qFnYVar <- tclVar(qFnY)
  rFnY <- get("rFnY", pos = envProj)
  rFnYVar <- tclVar(rFnY)

  tab3 <- get("tab3", pos = envGUI)
  frameTab3 <- get("frameTab3", pos = envGUI)
  tkdestroy(frameTab3)
  frameTab3 <- tkframe(tab3)
  assign("frameTab3", frameTab3, envir = envGUI)

  frame3_1 <- tkframe(frameTab3, relief = "groove", borderwidth = 2)
  frame3_2 <- tkframe(frameTab3)

  labelQ <- tklabel(frame3_1, text = "Query database")
  labelQFile <- tklabel(frame3_1, textvariable = qFnYVar, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttQ <- tkbutton(frame3_1, text = "    Load    ", cursor = "hand2", command = function() openFile("query"))

  labelR <- tklabel(frame3_1, text = "Reference database")
  labelRFile <- tklabel(frame3_1, textvariable = rFnYVar, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttR <- tkbutton(frame3_1, text = "    Load    ", cursor = "hand2", command = function() openFile("ref"))

  tkgrid(tklabel(frame3_1, text = "Input files", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
  tkgrid(labelQ, labelQFile, buttQ, padx = 10, pady = 5, sticky = "w")
  tkgrid(labelR, labelRFile, buttR, padx = 10, pady = 5, sticky = "w")
  tkgrid(frame3_1, padx = 10, pady = 5, sticky = "w")

  buttScreening <- tkbutton(frame3_2, text = "    Screening    ", cursor = "hand2", command = function() guiScreenY(envProj, envGUI))
  tkgrid(buttScreening, pady = 10)
  tkgrid(frame3_2, padx = 10, pady = 5)

  tkgrid(frameTab3)
}

guiScreenY <- function(envProj, envGUI){
  qYInput <- get("qYInput", pos = envProj)
  rYInput <- get("rYInput", pos = envProj)
  if(any(c(length(qYInput) == 0, length(rYInput) == 0))){
    tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
  }else{
    qCol <- colnames(qYInput)
    posQName <- grep("Sample", qCol)
    qYName <- qYInput[, posQName]
    qYData <- qYInput[, -posQName, drop = FALSE]
    nameQL <- colnames(qYData)

    rCol <- colnames(rYInput)
    posRName <- grep("Sample", rCol)
    rYName <- rYInput[, posRName]
    rYData <- rYInput[, -posRName, drop = FALSE]
    nameRL <- colnames(rYData)

    judgeL1 <- all(mapply(setequal, nameQL, nameRL))
    if(judgeL1){
      pb <- tkProgressBar("Y screening", "0% done", 0, 100, 0)
      rYData <- rYData[, match(nameQL, nameRL)]
      nQ <- nrow(qYData)
      nR <- nrow(rYData)
      nL <- length(nameQL)
      mismatch_y <- qDrop_y <- muStep_y <- array(0, dim = c(nQ, nR, nL + 1))
      for(i in 1:nR){
        rHap <- rYData[i, ]
        for(j in 1:nQ){
          qHap <- qYData[j, ]
          judgeMat <- matchY(qHap, rHap)
          mismatch_y[j, i, ] <- judgeMat[[1]]
          qDrop_y[j, i, ] <- judgeMat[[2]]
          muStep_y[j, i, ] <- judgeMat[[3]]
          info <- sprintf("%d%% done", round((nQ * (i - 1) + j) * 100 / (nQ * nR)))
          setTkProgressBar(pb, (nQ * (i - 1) + j) * 100 / (nQ * nR), sprintf("Y screening"), info)
        }
      }
      assign("qYData", qYData, envir = envProj)
      assign("rYData", rYData, envir = envProj)
      assign("qYName", qYName, envir = envProj)
      assign("rYName", rYName, envir = envProj)
      assign("mismatch_y", mismatch_y, envir = envProj)
      assign("qDrop_y", qDrop_y, envir = envProj)
      assign("muStep_y", muStep_y, envir = envProj)
      assign("finY", TRUE, envir = envProj)
      tabYResult(envProj, envGUI)
      close(pb)
    }else{
      tkmessageBox(message = "Locus set is not the same between query data and reference data!", icon = "error", type = "ok")
    }
  }
}

tabYResult <- function(envProj, envGUI){
  finY <- get("finY", pos = envProj)
  if(finY){
    setDisplay1 <- function(){
      tf <- tktoplevel()
      tkwm.title(tf, "Set display")

      frameD1 <- tkframe(tf)
      tkgrid(tklabel(frameD1, text = "Query"),
             tklabel(frameD1, text = "Reference"),
             tklabel(frameD1, text = "Maximum number of inconsistent loci"),
             tklabel(frameD1, text = "Maximum number of ignored loci"),
             tklabel(frameD1, text = "Maximum mutational step"),
             padx = 10, pady = 5)

      candQ <- c("All", qYName)
      selectQVar <- tclVar("All")
      comboQ <- ttkcombobox(frameD1, values = candQ, textvariable = selectQVar, state = "readonly")

      candR <- c("All", rYName)
      selectRVar <- tclVar("All")
      comboR <- ttkcombobox(frameD1, values = candR, textvariable = selectRVar, state = "readonly")

      selectNL_iVar <- tclVar(1)
      entryNL_i <- tkentry(frameD1, textvariable = selectNL_iVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      selectNL_dVar <- tclVar(1)
      entryNL_d <- tkentry(frameD1, textvariable = selectNL_dVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      selectMMSVar <- tclVar(2)
      entryMMS <- tkentry(frameD1, textvariable = selectMMSVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      tkgrid(comboQ, comboR, entryNL_i, entryNL_d, entryMMS, padx = 10, pady = 5)
      tkgrid(frameD1)

      frameD2 <- tkframe(tf)
      tkgrid(tkbutton(frameD2, text = "    Set    ", cursor = "hand2",
                      command = function() setDisplay2(tf, tclvalue(selectQVar), tclvalue(selectRVar),
                                                       as.numeric(tclvalue(selectNL_iVar)), as.numeric(tclvalue(selectNL_dVar)), as.numeric(tclvalue(selectMMSVar)))),
             padx = 10, pady = 5)
      tkgrid(frameD2)
    }

    setDisplay2 <- function(tf, selectQ, selectR, selectNL_i, selectNL_d, selectMMS){
      selectMat <- matrix(TRUE, nD, 5)
      if(selectQ != "All"){
        selectMat[, 1] <- displayQ == selectQ
      }
      if(selectR != "All"){
        selectMat[, 2] <- displayR == selectR
      }
      selectMat[, 3] <- nL_i <= selectNL_i
      selectMat[, 4] <- nL_d <= selectNL_d
      selectMat[, 5] <- mms <= selectMMS
      posExtract <- which(apply(selectMat, 1, all) == TRUE)

      if(length(posExtract) != 0){
        resultMlb <- get("resultMlb", pos = envYResult)
        tkdestroy(resultMlb)
        scr1 <- get("scr1", pos = envYResult)
        tkdestroy(scr1)
        scr1 <- tkscrollbar(frameResult1, repeatinterval = 5, command = function(...) tkyview(resultMlb, ...))
        resultMlb <- tk2mclistbox(frameResult1, width = 120, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(resultMlb, "add", label = "Query", width = 15)
        tk2column(resultMlb, "add", label = "Reference", width = 15)
        tk2column(resultMlb, "add", label = "Number of inconsistent loci", width = 30)
        tk2column(resultMlb, "add", label = "Number of ignored loci", width = 30)
        tk2column(resultMlb, "add", label = "Maximum mutational step", width = 30)
        tkgrid(resultMlb, scr1)
        displayData <- displayDefault[posExtract, , drop = FALSE]
        displayData <- displayData[order(as.numeric(displayData[, 5])), , drop = FALSE]
        displayData <- displayData[order(as.numeric(displayData[, 4])), , drop = FALSE]
        displayData <- displayData[order(as.numeric(displayData[, 3])), , drop = FALSE]
        displayData[which(as.numeric(displayData[, 5]) == 99), 5] <- "Not integer"
        tk2insert.multi(resultMlb, "end", displayData)
        assign("resultMlb", resultMlb, envir = envYResult)
        assign("scr1", scr1, envir = envYResult)
        assign("displayData", displayData, envir = envYResult)
        tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")
        tkdestroy(tf)
      }else{
        tkmessageBox(message = "There is no data that meet the condition!", icon = "error", type = "ok")
      }
    }

    showDetail <- function(){
      resultMlb <- get("resultMlb", pos = envYResult)
      if(tclvalue(tkcurselection(resultMlb)) == ""){
        tkmessageBox(message = "Select one result!", icon = "error", type = "ok")
      }else{
        displayData <- get("displayData", pos = envYResult)
        posShow <- as.numeric(tclvalue(tkcurselection(resultMlb))) + 1
        selectQName <- displayData[posShow, 1]
        posShowQ <- which(qYName == selectQName)
        selectRName <- displayData[posShow, 2]
        posShowR <- which(rYName == selectRName)
        detailData <- matrix("", nL + 1, 6)
        detailData[, 1] <- c(colnames(qYData), "overall")
        colnames(detailData) <- c("Locus",
                                  paste0("Query haplotype (", selectQName, ")"),
                                  paste0("Reference haplotype (", selectRName, ")"),
                                  "Number of inconsistent loci",
                                  "Number of ignored loci",
                                  "Mutational step")
        detailData[1:nL, 2] <- qYData[posShowQ, ]
        detailData[1:nL, 3] <- rYData[posShowR, ]
        mismatch_y_ext <- mismatch_y[posShowQ, posShowR, ]
        mismatch_y_ext2 <- mismatch_y_ext[1:nL]
        mismatch_y_ext2[which(mismatch_y_ext2 == 0)] <- ""
        detailData[, 4] <- c(mismatch_y_ext2, mismatch_y_ext[nL + 1])
        qDrop_y_ext <- qDrop_y[posShowQ, posShowR, ]
        qDrop_y_ext2 <- qDrop_y_ext[1:nL]
        qDrop_y_ext2[which(qDrop_y_ext2 == 0)] <- ""
        detailData[, 5] <- c(qDrop_y_ext2, qDrop_y_ext[nL + 1])
        muStep_y_ext <- muStep_y[posShowQ, posShowR, ]
        muStep_y_ext[which(muStep_y_ext == 0)] <- ""
        muStep_y_ext[which(muStep_y_ext == 99)] <- "Not integer"
        detailData[, 6] <- muStep_y_ext

        tfDetail <- tktoplevel()
        tkwm.title(tfDetail, "Y result in detail")

        frameDetail2 <- tkframe(tfDetail)
        scr2 <- tkscrollbar(frameDetail2, repeatinterval = 5, command = function(...) tkyview(detailMlb, ...))
        detailMlb <- tk2mclistbox(frameDetail2, width = 160, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr2, ...))
        tk2column(detailMlb, "add", label = "locus", width = 20)
        tk2column(detailMlb, "add", label = paste0("Query haplotype (", selectQName, ")"), width = 30)
        tk2column(detailMlb, "add", label = paste0("Reference haplotype (", selectRName, ")"), width = 30)
        tk2column(detailMlb, "add", label = "Number of inconsistent loci", width = 30)
        tk2column(detailMlb, "add", label = "Number of ignored loci", width = 30)
        tk2column(detailMlb, "add", label = "Mutational step", width = 20)
        tkgrid(detailMlb, scr2)
        tk2insert.multi(detailMlb, "end", detailData)
        tkgrid.configure(scr2, rowspan = 20, sticky = "nsw")
        tkgrid(frameDetail2, padx = 10, pady = 5)

        frameDetail3 <- tkframe(tfDetail)
        tkgrid(tkbutton(frameDetail3, text = "    Export    ", cursor = "hand2", command = function() exportData(detailData, FALSE)))
        tkgrid(frameDetail3, padx = 10, pady = 5)
      }
    }

    envYResult <- new.env(parent = globalenv())

    qYData <- get("qYData", pos = envProj)
    rYData <- get("rYData", pos = envProj)
    qYName <- get("qYName", pos = envProj)
    rYName <- get("rYName", pos = envProj)
    mismatch_y <- get("mismatch_y", pos = envProj)
    qDrop_y <- get("qDrop_y", pos = envProj)
    muStep_y <- get("muStep_y", pos = envProj)

    nL <- ncol(qYData)
    nQ <- length(qYName)
    nR <- length(rYName)

    numMismatch <- mismatch_y[, , nL + 1]
    rownames(numMismatch) <- qYName
    colnames(numMismatch) <- rYName
    numQDrop <- qDrop_y[, , nL + 1]
    rownames(numQDrop) <- qYName
    colnames(numQDrop) <- rYName
    maxMuStep <- muStep_y[, , nL + 1]
    rownames(maxMuStep) <- qYName
    colnames(maxMuStep) <- rYName

    nD <- nQ * nR
    displayDefault <- matrix(0, nD, 5)
    colnames(displayDefault) <- c("Query", "Reference", "Number of inconsistent loci", "Number of ignored loci", "Maximum mutational step")
    displayQ <- rep(qYName, nR)
    displayDefault[, 1] <- displayQ
    displayR <- as.vector(sapply(rYName, rep, nQ))
    displayDefault[, 2] <- displayR
    nL_i <- as.vector(numMismatch)
    displayDefault[, 3] <- nL_i
    nL_d <- as.vector(numQDrop)
    displayDefault[, 4] <- nL_d
    mms <- as.vector(maxMuStep)
    displayDefault[, 5] <- mms
    displayData <- displayDefault[which(as.numeric(displayDefault[, 3]) <= 1), , drop = FALSE]
    displayData <- displayData[which(as.numeric(displayData[, 4]) <= 1), , drop = FALSE]
    displayData <- displayData[which(as.numeric(displayData[, 5]) <= 2), , drop = FALSE]
    displayData <- displayData[order(as.numeric(displayData[, 4])), , drop = FALSE]
    displayData <- displayData[order(as.numeric(displayData[, 3])), , drop = FALSE]
    assign("displayData", displayData, envir = envYResult)

    tabs <- get("tabs", pos = envGUI)
    tab4 <- get("tab4", pos = envGUI)
    frameTab4 <- get("frameTab4", pos = envGUI)
    tkdestroy(frameTab4)
    frameTab4 <- tkframe(tab4)

    frameResult1 <- tkframe(frameTab4)
    scr1 <- tkscrollbar(frameResult1, repeatinterval = 5, command = function(...) tkyview(resultMlb, ...))
    resultMlb <- tk2mclistbox(frameResult1, width = 120, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(resultMlb, "add", label = "Query", width = 15)
    tk2column(resultMlb, "add", label = "Reference", width = 15)
    tk2column(resultMlb, "add", label = "Number of inconsistent loci", width = 30)
    tk2column(resultMlb, "add", label = "Number of ignored loci", width = 30)
    tk2column(resultMlb, "add", label = "Maximum mutational step", width = 30)
    tkgrid(resultMlb, scr1)
    tk2insert.multi(resultMlb, "end", displayData)
    assign("resultMlb", resultMlb, envir = envYResult)
    assign("scr1", scr1, envir = envYResult)
    tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")
    tkgrid(frameResult1, padx = 10, pady = 5)

    frameResult2 <- tkframe(frameTab4)
    buttDisplay <- tkbutton(frameResult2, text = "    Set display    ", cursor = "hand2", command = function() setDisplay1())
    buttDetail <- tkbutton(frameResult2, text = "    Show detail    ", cursor = "hand2", command = function() showDetail())
    buttExport1 <- tkbutton(frameResult2, text = "    Export displayed data    ", cursor = "hand2", command = function() exportData(get("displayData", pos = envYResult), FALSE))
    tkgrid(buttDisplay, buttDetail, buttExport1, padx = 10, pady = 5)
    tkgrid(frameResult2)

    tkgrid(frameTab4)
    tk2notetab.select(tabs, "Y results")
    assign("frameTab4", frameTab4, envir = envGUI)
  }else{
    tab4 <- get("tab4", pos = envGUI)
    frameTab4 <- get("frameTab4", pos = envGUI)
    tkdestroy(frameTab4)
    frameTab4 <- tkframe(tab4)
    assign("frameTab4", frameTab4, envir = envGUI)
  }
}
