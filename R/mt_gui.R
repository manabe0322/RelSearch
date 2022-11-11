tabMtSet <- function(envProj, envGUI){
  openFile <- function(type){
    inputOk <- "ok"
    finMt <- get("finMt", pos = envProj)
    if(finMt){
      inputOk <- tclvalue(tkmessageBox(message = "mtDNA results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }

    if(inputOk == "ok"){
      setEnvProj_mt(envProj, FALSE)
      tabMtResult(envProj, envGUI)

      if(type == "query"){
        fp <- get("qFpMt", pos = envProj)
        fn <- get("qFnMt", pos = envProj)
      }else if(type == "ref"){
        fp <- get("rFpMt", pos = envProj)
        fn <- get("rFnMt", pos = envProj)
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
          qMtInput <- read.csv(tclvalue(fpVar), header = TRUE)
          qMtInput <- as.matrix(qMtInput)
          qMtInput[is.na(qMtInput)] <- ""
          assign("qMtInput", qMtInput, envir = envProj)
          assign("qFpMt", tclvalue(fpVar), envir = envProj)
          assign("qFnMt", tclvalue(fnVar), envir = envProj)
        }else if(type == "ref"){
          tkconfigure(labelRFile, textvariable = fnVar)
          rMtInput <- read.csv(tclvalue(fpVar), header = TRUE)
          rMtInput <- as.matrix(rMtInput)
          rMtInput[is.na(rMtInput)] <- ""
          assign("rMtInput", rMtInput, envir = envProj)
          assign("rFpMt", tclvalue(fpVar), envir = envProj)
          assign("rFnMt", tclvalue(fnVar), envir = envProj)
        }
      }
    }
  }

  qFnMt <- get("qFnMt", pos = envProj)
  qFnMtVar <- tclVar(qFnMt)
  rFnMt <- get("rFnMt", pos = envProj)
  rFnMtVar <- tclVar(rFnMt)

  tab5 <- get("tab5", pos = envGUI)
  frameTab5 <- get("frameTab5", pos = envGUI)
  tkdestroy(frameTab5)
  frameTab5 <- tkframe(tab5)
  assign("frameTab5", frameTab5, envir = envGUI)

  frame5_1 <- tkframe(frameTab5, relief = "groove", borderwidth = 2)
  frame5_2 <- tkframe(frameTab5)

  labelQ <- tklabel(frame5_1, text = "Query database")
  labelQFile <- tklabel(frame5_1, textvariable = qFnMtVar, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttQ <- tkbutton(frame5_1, text = "    Load    ", cursor = "hand2", command = function() openFile("query"))

  labelR <- tklabel(frame5_1, text = "Reference database")
  labelRFile <- tklabel(frame5_1, textvariable = rFnMtVar, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttR <- tkbutton(frame5_1, text = "    Load    ", cursor = "hand2", command = function() openFile("ref"))

  tkgrid(tklabel(frame5_1, text = "Input files", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
  tkgrid(labelQ, labelQFile, buttQ, padx = 10, pady = 5, sticky = "w")
  tkgrid(labelR, labelRFile, buttR, padx = 10, pady = 5, sticky = "w")
  tkgrid(frame5_1, padx = 10, pady = 5, sticky = "w")

  buttScreening <- tkbutton(frame5_2, text = "    Screening    ", cursor = "hand2", command = function() guiScreenMt(envProj, envGUI))
  tkgrid(buttScreening, pady = 10)
  tkgrid(frame5_2, padx = 10, pady = 5)

  tkgrid(frameTab5)
}

guiScreenMt <- function(envProj, envGUI){
  qMtInput <- get("qMtInput", pos = envProj)
  rMtInput <- get("rMtInput", pos = envProj)
  if(any(c(length(qMtInput) == 0, length(rMtInput) == 0))){
    tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
  }else{
    qCol <- colnames(qMtInput)
    posQName <- grep("Sample", qCol)
    qMtName <- qMtInput[, posQName]
    qMtRange <- qMtInput[, "Range"]
    qMtHap <- qMtInput[, "Haplotype"]

    rCol <- colnames(rMtInput)
    posRName <- grep("Sample", rCol)
    rMtName <- rMtInput[, posRName]
    rMtRange <- rMtInput[, "Range"]
    rMtHap <- rMtInput[, "Haplotype"]

    pb <- tkProgressBar("mtDNA screening", "0% done", 0, 100, 0)
    nQ <- length(qMtHap)
    nR <- length(rMtHap)
    mismatch_mt <- shareRange_mt <- lenShare_mt <- matrix("", nrow = nQ, ncol = nR)
    rownames(mismatch_mt) <- qMtName
    colnames(mismatch_mt) <- rMtName
    rownames(shareRange_mt) <- qMtName
    colnames(shareRange_mt) <- rMtName
    rownames(lenShare_mt) <- qMtName
    colnames(lenShare_mt) <- rMtName
    for(i in 1:nR){
      rRan <- rMtRange[i]
      rHap <- rMtHap[i]
      for(j in 1:nQ){
        qRan <- qMtRange[j]
        qHap <- qMtHap[j]
        result_mt <- matchMt(qHap, qRan, rHap, rRan)
        mismatch_mt[j, i] <- result_mt[1]
        shareRange_mt[j, i] <- result_mt[2]
        lenShare_mt[j, i] <- result_mt[3]
        info <- sprintf("%d%% done", round((nQ * (i - 1) + j) * 100 / (nQ * nR)))
        setTkProgressBar(pb, (nQ * (i - 1) + j) * 100 / (nQ * nR), sprintf("mtDNA screening"), info)
      }
    }
    assign("qMtName", qMtName, envir = envProj)
    assign("qMtRange", qMtRange, envir = envProj)
    assign("qMtHap", qMtHap, envir = envProj)
    assign("rMtName", rMtName, envir = envProj)
    assign("rMtRange", rMtRange, envir = envProj)
    assign("rMtHap", rMtHap, envir = envProj)
    assign("mismatch_mt", mismatch_mt, envir = envProj)
    assign("shareRange_mt", shareRange_mt, envir = envProj)
    assign("lenShare_mt", lenShare_mt, envir = envProj)
    assign("finMt", TRUE, envir = envProj)
    tabMtResult(envProj, envGUI)
    close(pb)
  }
}

tabMtResult <- function(envProj, envGUI){
  finMt <- get("finMt", pos = envProj)
  if(finMt){
    setDisplay1 <- function(){
      tf <- tktoplevel()
      tkwm.title(tf, "Set display")

      frameD1 <- tkframe(tf)
      tkgrid(tklabel(frameD1, text = "Query"),
             tklabel(frameD1, text = "Reference"),
             tklabel(frameD1, text = "Minimum shared length"),
             tklabel(frameD1, text = "Maximum number of inconsistency"),
             padx = 10, pady = 5)

      candQ <- c("All", qMtName)
      selectQVar <- tclVar("All")
      comboQ <- ttkcombobox(frameD1, values = candQ, textvariable = selectQVar, state = "readonly")

      candR <- c("All", rMtName)
      selectRVar <- tclVar("All")
      comboR <- ttkcombobox(frameD1, values = candR, textvariable = selectRVar, state = "readonly")

      selectLenShareVar <- tclVar(300)
      entryLenShare <- tkentry(frameD1, textvariable = selectLenShareVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      selectNMisVar <- tclVar(1)
      entryNMis <- tkentry(frameD1, textvariable = selectNMisVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      tkgrid(comboQ, comboR, entryLenShare, entryNMis, padx = 10, pady = 5)
      tkgrid(frameD1)

      frameD2 <- tkframe(tf)
      tkgrid(tkbutton(frameD2, text = "    Set    ", cursor = "hand2",
                      command = function() setDisplay2(tf, tclvalue(selectQVar), tclvalue(selectRVar), as.numeric(tclvalue(selectLenShareVar)), as.numeric(tclvalue(selectNMisVar)))),
             padx = 10, pady = 5)
      tkgrid(frameD2)
    }

    setDisplay2 <- function(tf, selectQ, selectR, selectLenShare, selectNMis){
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
      posLenShare <- which(lenShareAll >= selectLenShare)
      posNMis <- which(nMisAll <= selectNMis)

      posExtract <- intersect(intersect(intersect(posQ, posR), posLenShare), posNMis)

      if(length(posExtract) != 0){
        resultMlb <- get("resultMlb", pos = envMtResult)
        tkdestroy(resultMlb)
        scr1 <- get("scr1", pos = envMtResult)
        tkdestroy(scr1)
        scr1 <- tkscrollbar(frameResult1, repeatinterval = 5, command = function(...) tkyview(resultMlb, ...))
        resultMlb <- tk2mclistbox(frameResult1, width = 130, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(resultMlb, "add", label = "Query", width = 15)
        tk2column(resultMlb, "add", label = "Reference", width = 15)
        tk2column(resultMlb, "add", label = "Shared range", width = 50)
        tk2column(resultMlb, "add", label = "Shared length", width = 20)
        tk2column(resultMlb, "add", label = "Number of inconsistency", width = 30)
        tkgrid(resultMlb)
        displayData <- displayDefault[posExtract, , drop = FALSE]
        displayData <- displayData[order(as.numeric(displayData[, 4]), decreasing = TRUE), , drop = FALSE]
        displayData <- displayData[order(as.numeric(displayData[, 5])), , drop = FALSE]
        tk2insert.multi(resultMlb, "end", displayData)
        assign("resultMlb", resultMlb, envir = envMtResult)
        assign("scr1", scr1, envir = envMtResult)
        assign("displayData", displayData, envir = envMtResult)
        tkdestroy(tf)
      }else{
        tkmessageBox(message = "There is no data that meet the condition!", icon = "error", type = "ok")
      }
    }

    showDetail <- function(){
      resultMlb <- get("resultMlb", pos = envMtResult)
      if(tclvalue(tkcurselection(resultMlb)) == ""){
        tkmessageBox(message = "Select one result!", icon = "error", type = "ok")
      }else{
        displayData <- get("displayData", pos = envMtResult)
        posShow <- as.numeric(tclvalue(tkcurselection(resultMlb))) + 1

        selectQName <- displayData[posShow, 1]
        posShowQ <- which(qMtName == selectQName)
        qRan <- qMtRange[posShowQ]
        qHap <- qMtHap[posShowQ]
        qHap <- strsplit(qHap, " ")[[1]]
        qHap <- setdiff(qHap, "")
        qHap <- qHap[order(parse_number(qHap))]

        selectRName <- displayData[posShow, 2]
        posShowR <- which(rMtName == selectRName)
        rRan <- rMtRange[posShowR]
        rHap <- rMtHap[posShowR]
        rHap <- strsplit(rHap, " ")[[1]]
        rHap <- setdiff(rHap, "")
        rHap <- rHap[order(parse_number(rHap))]

        qrHap <- union(qHap, rHap)
        qrHap <- qrHap[order(parse_number(qrHap))]
        nHap <- length(qrHap)

        posMtQR <- extPosMtQR(qRan, rRan)

        detailData <- matrix("", nHap, 4)
        colnames(detailData) <- c(paste0("Query haplotype (", selectQName, ")"),
                                  paste0("Reference haplotype (", selectRName, ")"),
                                  "Out of shared range",
                                  "Inconsistency")
        detailData[is.element(qrHap, qHap), 1] <- qHap
        detailData[is.element(qrHap, rHap), 2] <- rHap
        posCommon <- is.element(round(parse_number(qrHap), 0), posMtQR)
        detailData[!posCommon, 3] <- "X"
        posInconsistent <- apply(rbind(!is.element(qrHap, qHap), !is.element(qrHap, rHap)), 2, any)
        detailData[apply(rbind(posCommon, posInconsistent), 2, all), 4] <- "X"

        tfDetail <- tktoplevel()
        tkwm.title(tfDetail, "mtDNA result in detail")

        tkgrid(tklabel(tfDetail, text = paste0("Shared range : ", shareRange_mt[posShowQ, posShowR])), padx = 10, pady = 5)

        frameDetail2 <- tkframe(tfDetail)
        scr2 <- tkscrollbar(frameDetail2, repeatinterval = 5, command = function(...) tkyview(detailMlb, ...))
        detailMlb <- tk2mclistbox(frameDetail2, width = 105, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr2, ...))
        tk2column(detailMlb, "add", label = paste0("Query haplotype (", selectQName, ")"), width = 30)
        tk2column(detailMlb, "add", label = paste0("Reference haplotype (", selectRName, ")"), width = 30)
        tk2column(detailMlb, "add", label = "Out of shared range", width = 25)
        tk2column(detailMlb, "add", label = "Inconsistency", width = 20)
        tkgrid(detailMlb, scr2)
        tk2insert.multi(detailMlb, "end", detailData)
        tkgrid.configure(scr2, rowspan = 20, sticky = "nsw")
        tkgrid(frameDetail2, padx = 10, pady = 5)

        frameDetail3 <- tkframe(tfDetail)
        tkgrid(tkbutton(frameDetail3, text = "    Export    ", cursor = "hand2", command = function() exportData(detailData, FALSE)))
        tkgrid(frameDetail3, padx = 10, pady = 5)
      }
    }

    envMtResult <- new.env(parent = globalenv())

    qMtName <- get("qMtName", pos = envProj)
    qMtRange <- get("qMtRange", pos = envProj)
    qMtHap <- get("qMtHap", pos = envProj)
    rMtName <- get("rMtName", pos = envProj)
    rMtRange <- get("rMtRange", pos = envProj)
    rMtHap <- get("rMtHap", pos = envProj)
    mismatch_mt <- get("mismatch_mt", pos = envProj)
    shareRange_mt <- get("shareRange_mt", pos = envProj)
    lenShare_mt <- get("lenShare_mt", pos = envProj)

    nQ <- length(qMtName)
    nR <- length(rMtName)

    nD <- nQ * nR
    displayDefault <- matrix(0, nD, 5)
    colnames(displayDefault) <- c("Query", "Reference", "Shared range", "Shared length", "Number of inconsistency")
    displayQ <- rep(qMtName, nR)
    displayDefault[, 1] <- displayQ
    displayR <- as.vector(sapply(rMtName, rep, nQ))
    displayDefault[, 2] <- displayR
    segAll <- as.vector(shareRange_mt)
    displayDefault[, 3] <- segAll
    lenShareAll <- as.numeric(as.vector(lenShare_mt))
    displayDefault[, 4] <- lenShareAll
    nMisAll <- as.numeric(as.vector(mismatch_mt))
    displayDefault[, 5] <- nMisAll
    displayData <- displayDefault[which(as.numeric(displayDefault[, 4]) >= 300), , drop = FALSE]
    displayData <- displayData[which(as.numeric(displayData[, 5]) <= 1), , drop = FALSE]
    displayData <- displayData[order(as.numeric(displayData[, 4]), decreasing = TRUE), , drop = FALSE]
    displayData <- displayData[order(as.numeric(displayData[, 5])), , drop = FALSE]
    assign("displayData", displayData, envir = envMtResult)

    tabs <- get("tabs", pos = envGUI)
    tab6 <- get("tab6", pos = envGUI)
    frameTab6 <- get("frameTab6", pos = envGUI)
    tkdestroy(frameTab6)
    frameTab6 <- tkframe(tab6)

    frameResult1 <- tkframe(frameTab6)
    scr1 <- tkscrollbar(frameResult1, repeatinterval = 5, command = function(...) tkyview(resultMlb, ...))
    resultMlb <- tk2mclistbox(frameResult1, width = 130, height = 20, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(resultMlb, "add", label = "Query", width = 15)
    tk2column(resultMlb, "add", label = "Reference", width = 15)
    tk2column(resultMlb, "add", label = "Shared range", width = 50)
    tk2column(resultMlb, "add", label = "Shared length", width = 20)
    tk2column(resultMlb, "add", label = "Number of inconsistency", width = 30)
    tkgrid(resultMlb, scr1)
    tk2insert.multi(resultMlb, "end", displayData)
    assign("resultMlb", resultMlb, envir = envMtResult)
    assign("scr1", scr1, envir = envMtResult)
    tkgrid.configure(scr1, rowspan = 20, sticky = "nsw")
    tkgrid(frameResult1, padx = 10, pady = 5)

    frameResult2 <- tkframe(frameTab6)
    buttDisplay <- tkbutton(frameResult2, text = "    Set display    ", cursor = "hand2", command = function() setDisplay1())
    buttDetail <- tkbutton(frameResult2, text = "    Show detail    ", cursor = "hand2", command = function() showDetail())
    buttExport1 <- tkbutton(frameResult2, text = "    Export displayed data    ", cursor = "hand2", command = function() exportData(get("displayData", pos = envMtResult), FALSE))
    tkgrid(buttDisplay, buttDetail, buttExport1, padx = 10, pady = 5)
    tkgrid(frameResult2)

    tkgrid(frameTab6)
    tk2notetab.select(tabs, "mtDNA results")
    assign("frameTab6", frameTab6, envir = envGUI)
  }else{
    tab6 <- get("tab6", pos = envGUI)
    frameTab6 <- get("frameTab6", pos = envGUI)
    tkdestroy(frameTab6)
    frameTab6 <- tkframe(tab6)
    assign("frameTab6", frameTab6, envir = envGUI)
  }
}
