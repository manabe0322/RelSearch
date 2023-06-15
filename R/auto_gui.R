# The function to make tab1
make_tab1 <- function(env_proj, env_gui){

  # The function to load required files for the autosomal STR analysis
  open_file <- function(type){

    # Get the end sign of the autosomal STR analysis from the environment "env_proj"
    fin_auto <- get("fin_auto", pos = env_proj)

    # Confirm that the user allows to delete the autosomal STR results
    if(fin_auto){
      sign_ok <- tclvalue(tkmessageBox(message = "STR results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }else{
      sign_ok <- "ok"
    }
    if(sign_ok == "ok"){

      # Reset the environmental variables for the autosomal STR
      set_env_proj_auto(env_proj, FALSE)

      # Reset tab2
      make_tab2(env_proj, env_gui)

      # Get a file path and a file name from the environment "env_proj"
      if(type == "query"){
        fp <- get("fp_auto_q", pos = env_proj)
        fn <- get("fn_auto_q", pos = env_proj)
      }else if(type == "ref"){
        fp <- get("fp_auto_r", pos = env_proj)
        fn <- get("fn_auto_r", pos = env_proj)
      }else if(type == "af"){
        fp <- get("fp_auto_af", pos = env_proj)
        fn <- get("fn_auto_af", pos = env_proj)
      }

      # Define tcl variables for the file path and the file name
      fp_var <- tclVar(fp)
      fn_var <- tclVar(fn)

      # Open a window to select a file
      path_file <- tclvalue(tkgetOpenFile(initialdir = tclvalue(fp_var), multiple = "true", filetypes = "{{CSV Files} {.csv}}"))

      # If the user does not select a file
      if(!nchar(path_file)){
        tkmessageBox(message = "No file was selected!", icon = "error", type = "ok")

      # If the user selects a file
      }else{

        # Update tcl variables for the file path and the file name
        tmp <- sub("\\}", path_file, replacement = "")
        tmp2 <- sub("\\{", tmp, replacement = "")
        tclvalue(fp_var) <- tmp2
        foo3 <- strsplit(tmp2, "/")[[1]]
        tclvalue(fn_var) <- strsplit(foo3[length(foo3)], "\\.csv")[[1]][1]

        # If the user clicks the "Load" button for query database
        if(type == "query"){

          # Update the name of query database
          tkconfigure(label_q_name, textvariable = fn_var)

          # Load query database
          data_auto_q <- fread(tclvalue(fp_var))
          #data_auto_q <- as.matrix(data_auto_q)
          #data_auto_q[is.na(data_auto_q)] <- ""

          # Assign objects to the environment "env_proj"
          assign("data_auto_q", data_auto_q, envir = env_proj)
          assign("fp_auto_q", tclvalue(fp_var), envir = env_proj)
          assign("fn_auto_q", tclvalue(fn_var), envir = env_proj)

        # If the user clicks the "Load" button for reference database
        }else if(type == "ref"){

          # Update the name of reference database
          tkconfigure(label_r_name, textvariable = fn_var)

          # Load reference database
          data_auto_r <- fread(tclvalue(fp_var))
          #data_auto_r <- as.matrix(data_auto_r)
          #data_auto_r[is.na(data_auto_r)] <- ""

          # Assign objects to the environment "env_proj"
          assign("data_auto_r", data_auto_r, envir = env_proj)
          assign("fp_auto_r", tclvalue(fp_var), envir = env_proj)
          assign("fn_auto_r", tclvalue(fn_var), envir = env_proj)

        # If the user clicks the "Load" button for allele frequencies
        }else if(type == "af"){

          # Update the name of allele frequencies
          tkconfigure(label_af_name, textvariable = fn_var)

          # Load allele frequencies
          data_auto_af <- fread(tclvalue(fp_var))
          #data_auto_af <- as.matrix(data_auto_af)

          # Assign objects to the environment "env_proj"
          assign("data_auto_af", data_auto_af, envir = env_proj)
          assign("fp_auto_af", tclvalue(fp_var), envir = env_proj)
          assign("fn_auto_af", tclvalue(fn_var), envir = env_proj)
        }
      }
    }
  }

  # Get file names from the environment "env_proj"
  fn_auto_q <- get("fn_auto_q", pos = env_proj)
  fn_auto_r <- get("fn_auto_r", pos = env_proj)
  fn_auto_af <- get("fn_auto_af", pos = env_proj)

  # Define tcl variables
  fp_auto_q_var <- tclVar(fn_auto_q)
  fn_auto_r_var <- tclVar(fn_auto_r)
  fn_auto_afVar <- tclVar(fn_auto_af)

  # Reset frame_tab1
  tab1 <- get("tab1", pos = env_gui)
  frame_tab1 <- get("frame_tab1", pos = env_gui)
  tkdestroy(frame_tab1)
  frame_tab1 <- tkframe(tab1)

  # Define frames
  frame_1_1 <- tkframe(frame_tab1, relief = "groove", borderwidth = 2)
  frame_1_2 <- tkframe(frame_tab1)

  # Define widgets in frame_1_1
  label_q_title <- tklabel(frame_1_1, text = "Query database")
  label_q_name <- tklabel(frame_1_1, textvariable = fp_auto_q_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  butt_q <- tkbutton(frame_1_1, text = "    Load    ", cursor = "hand2", command = function() open_file("query"))
  label_r_title <- tklabel(frame_1_1, text = "Reference database")
  label_r_name <- tklabel(frame_1_1, textvariable = fn_auto_r_var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  butt_r <- tkbutton(frame_1_1, text = "    Load    ", cursor = "hand2", command = function() open_file("ref"))
  label_af_title <- tklabel(frame_1_1, text = "Allele frequencies")
  label_af_name <- tklabel(frame_1_1, textvariable = fn_auto_afVar, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  butt_af <- tkbutton(frame_1_1, text = "    Load    ", cursor = "hand2", command = function() open_file("af"))

  # Define widgets in frame_1_2
  butt_search <- tkbutton(frame_1_2, text = "    Search    ", cursor = "hand2", command = function() search_auto(env_proj, env_gui))

  # Grid widgets
  tkgrid(tklabel(frame_1_1, text = "Input files", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
  tkgrid(label_q_title, label_q_name, butt_q, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_r_title, label_r_name, butt_r, padx = 10, pady = 5, sticky = "w")
  tkgrid(label_af_title, label_af_name, butt_af, padx = 10, pady = 5, sticky = "w")
  tkgrid(butt_search, pady = 10)

  # Grid frames
  tkgrid(frame_1_1, padx = 10, pady = 5, sticky = "w")
  tkgrid(frame_1_2, padx = 10, pady = 5)
  tkgrid(frame_tab1)

  # Assign frame_tab1
  assign("frame_tab1", frame_tab1, envir = env_gui)
}

# The function to perform screening relatives using the autosomal STR
search_auto <- function(env_proj, env_gui){

  # Get input data from environment variable (env_proj)
  data_auto_q <- get("data_auto_q", pos = env_proj)
  data_auto_r <- get("data_auto_r", pos = env_proj)
  data_auto_af <- get("data_auto_af", pos = env_proj)

  # Confirm that all database is loaded
  if(any(c(length(data_auto_q) == 0, length(data_auto_r) == 0, length(data_auto_af) == 0))){
    tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
  }else{

    # Get package path from the environment "env_gui"
    path_pack <- get("path_pack", pos = env_gui)

    # List up file names in 'extdata > parameters'.
    fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

    # If all required csv files are located in 'extdata > parameters'.
    if(all(is.element(c("criteria.csv", "myu.csv", "rel.csv", "par_auto.csv"), fn_par))){

      # Load criteria
      criteria <- read.csv(paste0(path_pack, "/extdata/parameters/criteria.csv"), header = TRUE)
      min_lr_auto <- criteria$Value[criteria$Criteria == "min_lr_auto"]

      # Load mutation rates
      myu_all <- read.csv(paste0(path_pack, "/extdata/parameters/myu.csv"), header = TRUE)
      myu_all <- as.matrix(myu_all)
      locus_myu <- myu_all[, colnames(myu_all) == "Marker"]
      myu_all <- as.numeric(myu_all[, colnames(myu_all) == "Myu"])
      names(myu_all) <- locus_myu

      # Load IBD probabilities
      rel_data <- read.csv(paste0(path_pack, "/extdata/parameters/rel.csv"), header = TRUE)

      # Extract relationship names from rel_data
      rel_pibd <- rel_data[, "Name_relationship"]

      # Load analysis methods
      par_auto <- read.csv(paste0(path_pack, "/extdata/parameters/par_auto.csv"), header = TRUE)
      maf <- par_auto$Value[par_auto$Parameter == "maf"]
      meth_d <- par_auto$Value[par_auto$Parameter == "meth_d"]
      pd <- par_auto$Value[par_auto$Parameter == "pd"]

      # Extract loci from each database
      locus_q <- setdiff(names(data_auto_q), c("SampleName", "Relationship"))
      locus_r <- setdiff(names(data_auto_r), c("SampleName", "Relationship"))
      locus_af <- setdiff(names(data_auto_af), "Allele")

      # Whether the locus set of the query database is the same as that of the reference database or not
      #bool_locus_1 <- all(mapply(setequal, locus_q, locus_r))
      bool_locus_1 <- setequal(locus_q, locus_r)

      # Whether the locus set of the query database is the same as that of the allele frequencies or not
      #bool_locus_2 <- all(mapply(setequal, locus_q, locus_af))
      bool_locus_2 <- setequal(locus_q, locus_af)

      # Whether all loci of the query database is included in the locus set of the mutation rates or not
      bool_locus_3 <- all(is.element(locus_q, locus_myu))

      # Whether all relationships of the reference database is included in the relationships of the IBD probabilities or not
      bool_rel_1 <- all(is.element(setdiff(data_auto_r[, Relationship], ""), rel_pibd))

      # If above 4 conditions are satisfied
      if(all(c(bool_locus_1, bool_locus_2, bool_locus_3, bool_rel_1))){

        # The number of loci
        n_l <- length(locus_q)

        # Order loci of each database
        pos_q <- rep(0, 2 * n_l + 1)
        pos_r <- rep(0, 2 * n_l + 2)
        pos_af <- rep(0, n_l + 1)

        pos_q[1] <- which(is.element(names(data_auto_q), "SampleName"))
        pos_r[1] <- which(is.element(names(data_auto_r), "SampleName"))
        pos_r[2] <- which(is.element(names(data_auto_r), "Relationship"))
        pos_af[1] <- which(is.element(names(data_auto_af), "Allele"))
        for(i in 1:n_l){
          pos_q[c(2 * i, 2 * i + 1)] <- which(is.element(names(data_auto_q), locus_q[i]))
          pos_r[c(2 * i + 1, 2 * i + 2)] <- which(is.element(names(data_auto_r), locus_q[i]))
          pos_af[i + 1] <- which(is.element(names(data_auto_af), locus_q[i]))
        }

        data_auto_q <- data_auto_q[, pos_q, with = FALSE]
        data_auto_r <- data_auto_r[, pos_r, with = FALSE]
        data_auto_af <- data_auto_af[, pos_af, with = FALSE]

        # Make progress bars
        # tf <- tktoplevel()
        # label_pb1 <- tk2label(tf)
        # pb1 <- tk2progress(tf, length = 300)
        # tkconfigure(pb1, value = 0, maximum = 100)
        # tkgrid(label_pb1)
        # tkgrid(pb1)
        pb <- tkProgressBar("Searching", "0% done", 0, 100, 0)

        # Set allele frequencies
        tmp <- set_af(data_auto_q, data_auto_r, data_auto_af, maf)
        af_list <- tmp[[1]]
        af_al_list <- tmp[[2]]

        # The numbers of samples
        n_q <- nrow(data_auto_q)
        n_r <- nrow(data_auto_r)

        # The number of empty cells in the "Relationship" column of the reference database
        n_emp_rel <- length(which(data_auto_r[, Relationship] == ""))

        # Extract mutation rates
        myus <- rep(0, n_l)
        for(i in 1:n_l){
          myus[i] <- myu_all[which(locus_myu == locus_q[i])]
        }
        names(myus) <- locus_q

        # The number of relationships for the IBD probabilities
        n_pibd_rel <- nrow(rel_data)

        # Extract the IBD probabilities
        pibd_base <- rel_data[, c("Pr_IBD2", "Pr_IBD1", "Pr_IBD0")]

        # Set consideration of mutations for each relationship
        bool_cons_mu_all <- rep(FALSE, n_pibd_rel)
        bool_cons_mu_all[rel_data[, "Degree"] == "1st_pc"] <- TRUE

        # Calculate average probabilities of exclusion
        apes <- rep(0, n_l)
        for(i in 1:n_l){
          apes[i] <- calc_ape(af_list[[i]])
        }
        names(apes) <- locus_q

        # Number of comparisons
        n_data <- n_q * (n_r + (n_pibd_rel - 1) * n_emp_rel)

        # Define an data table to save information on the likelihoods of the numerator hypotheses
        like_h1_all <- data.frame(matrix(0, nrow = n_data, ncol = n_l + 4))
        names(like_h1_all) <- c("Query", "Reference", "AssumedRelationship", locus_q, "Total")
        like_h1_all$Query <- as.character(like_h1_all$Query)
        like_h1_all$Reference <- as.character(like_h1_all$Reference)
        like_h1_all$AssumedRelationship <- as.character(like_h1_all$AssumedRelationship)

        #like_h1_all <- data.table(matrix(0, nrow = n_q * (n_r + (n_pibd_rel - 1) * n_emp_rel), ncol = n_l + 4))
        #names(like_h1_all) <- c("Query", "Reference", "AssumedRelationship", locus_q, "Total")
        #like_h1_all <- mutate(like_h1_all, Query = as.character(Query))
        #like_h1_all <- mutate(like_h1_all, Reference = as.character(Reference))
        #like_h1_all <- mutate(like_h1_all, AssumedRelationship = as.character(AssumedRelationship))

        # Define an data table to save information on the likelihoods of the denominator hypotheses
        #like_h2_all <- copy(like_h1_all)
        like_h2_all <- like_h1_all

        # Define an data table to save information on the likelihood ratio
        #lr_all <- copy(like_h1_all)
        lr_all <- like_h1_all

        # Set the initial number of counts for rows
        count <- 1

        # Repetitive execution for each reference genotype
        for(i in 1:n_r){

          # Extract a reference data
          ref <- data_auto_r[i, ]
          sn_ref <- ref[, SampleName]
          rel <- ref[, Relationship]
          prof_ref <- as.numeric(ref[, -c("SampleName", "Relationship"), with = FALSE])

          # The NA is replaced to -99 to deal with the C++ program
          prof_ref[is.na(prof_ref)] <- -99

          # If the relationship of the current reference is not defined
          if(rel == ""){

            # Set the IBD probabilities
            pibd_all <- pibd_base

            # Set relationships
            rel <- rel_pibd

            # Set consideration of mutations
            bool_cons_mu <- bool_cons_mu_all

          # If the relationship of the current reference is defined
          }else{

            # Extract the IBD probabilities
            pibd_all <- pibd_base[rel_pibd == rel, , drop = FALSE]

            # Set consideration of mutations
            if(rel == "parent-child"){
              bool_cons_mu <- TRUE
            }else{
              bool_cons_mu <- FALSE
            }
          }

          # Repetitive execution for each relationship
          for(j in 1:nrow(pibd_all)){

            # Extract k2, k1, and k0 of one relationship
            pibds <- as.numeric(pibd_all[j, ])

            # Repetitive execution for each query genotype
            for(k in 1:n_q){

              # Extract a query data
              query <- data_auto_q[k, ]
              sn_query <- query[, SampleName]
              prof_query <- as.numeric(query[, -"SampleName", with = FALSE])

              # The NA is replaced to -99 to deal with the C++ program
              prof_query[is.na(prof_query)] <- -99

              # Calculate a likelihood ratio
              tmp <- calc_kin_lr(prof_query, prof_ref, af_list, af_al_list, pibds, bool_cons_mu[j], myus, apes, meth_d, pd)

              #like_h1_all[count, Query := sn_query]
              #like_h1_all[count, Reference := sn_ref]
              #like_h1_all[count, AssumedRelationship := rel[j]]
              #like_h1_all[count, 4:(n_l + 4) := tmp[[1]]]

              like_h1_all[count, 1] <- sn_query
              like_h1_all[count, 2] <- sn_ref
              like_h1_all[count, 3] <- rel[j]
              like_h1_all[count, 4:(n_l + 4)] <- tmp[[1]]

              like_h2_all[count, 1] <- sn_query
              like_h2_all[count, 2] <- sn_ref
              like_h2_all[count, 3] <- rel[j]
              like_h2_all[count, 4:(n_l + 4)] <- tmp[[2]]

              lr_all[count, 1] <- sn_query
              lr_all[count, 2] <- sn_ref
              lr_all[count, 3] <- rel[j]
              lr_all[count, 4:(n_l + 4)] <- tmp[[3]]

              # Update the number of counts for rows
              count <- count + 1

              # Update the progress bar
              # tkconfigure(label_pb1, text = paste())
              #tkconfigure(pb1, value = (n_q * (count - 1) + k) * 100 / (n_q * (n_r + (n_pibd_rel - 1) * n_emp_rel)))
              info <- sprintf("%d%% done", round(count * 100 / n_data))
              setTkProgressBar(pb, count * 100 / n_data, sprintf("Searching"), info)
            }
          }
        }

        # Update sample names
        set_env_proj_sn(env_proj, FALSE, data_auto_q[, SampleName], data_auto_r[, SampleName])

        # Assign updated input data (ordered loci)
        assign("data_auto_q", data_auto_q, envir = env_proj)
        assign("data_auto_r", data_auto_r, envir = env_proj)
        assign("data_auto_af", data_auto_af, envir = env_proj)

        # Convert matrix to data.table
        setDT(like_h1_all)
        setDT(like_h2_all)
        setDT(lr_all)

        # Assign results to the environment "env_proj"
        assign("like_h1_all", like_h1_all, envir = env_proj)
        assign("like_h2_all", like_h2_all, envir = env_proj)
        assign("lr_all", lr_all, envir = env_proj)

        # Assign parameters to the environment "env_proj"
        assign("min_lr_auto", min_lr_auto, envir = env_proj)
        assign("myu_all", myu_all, envir = env_proj)
        assign("pibd_all", pibd_all, envir = env_proj)
        assign("maf", maf, envir = env_proj)
        assign("meth_d", meth_d, envir = env_proj)
        assign("pd", pd, envir = env_proj)

        # Assign mutation rates to the environment "env_proj"
        assign("myus", myus, envir = env_proj)

        # Assign the average probability of exclusion to the environment "env_proj"
        assign("apes", apes, envir = env_proj)

        # Assign the end sign to the environment "env_proj"
        assign("fin_auto", TRUE, envir = env_proj)

        # Make tabs
        make_tab2(env_proj, env_gui)
        #make_tab7(env_proj, env_gui)

        # Close the progress bar
        # tkdestroy(tf)
        close(pb)

        # If the locus set of the query database is not the same as that of the reference database
      }else if(!bool_locus_1){
        tkmessageBox(message = "Locus set is not the same between query data and reference data!", icon = "error", type = "ok")

        # If the locus set of the query database is not the same as that of the allele frequencies
      }else if(!bool_locus_2){
        tkmessageBox(message = "Locus set is not the same between query data and allele frequencies!", icon = "error", type = "ok")

        # If some loci of the query database is not included in the locus set of the mutation rates
      }else if(!bool_locus_3){
        tkmessageBox(message = "There are some loci without mutation rates!", icon = "error", type = "ok")

        # If some relationships of the reference database is not included in the relationships of the IBD probabilities
      }else if(!bool_rel_1){
        tkmessageBox(message = "There are some relationships without IBD probabilities!", icon = "error", type = "ok")
      }

    # If the file 'criteria.csv' is found
    }else if(!is.element("criteria.csv", fn_par)){
      tkmessageBox(message = paste0("Set criteria via 'Tools > Set criteria.'"), icon = "error", type = "ok")

    # If the file 'myu.csv' is not found
    }else if(!is.element("myu.csv", fn_par)){
      tkmessageBox(message = paste0("Set mutation rates via 'Tools > Set mutation rates for autosomal STR.'"), icon = "error", type = "ok")

    # If the file 'rel.csv' is not found
    }else if(!is.element("rel.csv", fn_par)){
      tkmessageBox(message = paste0("Set relationships via 'Tools > Set relationships.'"), icon = "error", type = "ok")

    # If the file 'par_auto.csv' is not found
    }else if(!is.element("par_auto.csv", fn_par)){
      tkmessageBox(message = paste0("Set analysis methods via 'Tools > Set analysis method for autosomal STR.'"), icon = "error", type = "ok")
    }
  }
}

# The function to make a vector of genotypes for detailed data
display_gt <- function(data_gt){
  # The number of loci
  n_l <- length(data_gt) / 2

  # Define a vector of genotypes for detailed data
  gt_vec <- rep("", n_l)

  # Repetitive execution for each locus
  for(i in 1:n_l){

    # Extract a genotype in one locus
    gt <- as.numeric(data_gt[c(2 * i - 1, 2 * i)])

    # Remove NA
    gt <- gt[!is.na(gt)]

    # If homozygote
    if(length(gt) == 1){
      gt_vec[i] <- gt

    # If heterozygote
    }else if(length(gt) == 2){
      gt_vec[i] <- paste(gt[1], ", ", gt[2], sep = "")
    }
  }
  return(gt_vec)
}

# The function to make tab2
make_tab2 <- function(env_proj, env_gui){

  # Get the end sign from the environment "env_proj"
  fin_auto <- get("fin_auto", pos = env_proj)

  # If the autosomal STR analysis has been already finished
  if(fin_auto){

    # The function to make a window for setting displayed data
    set_display_1 <- function(){

      # Define tcl variables
      cand_q <- c("All", data_auto_q[, SampleName])
      sn_q_select_var <- tclVar("All")
      cand_r <- c("All", data_auto_r[, SampleName])
      sn_r_select_var <- tclVar("All")
      cand_rel <- c("All", unique(lr_all[, AssumedRelationship]))
      rel_select_var <- tclVar("All")
      min_lr_select_var <- tclVar(min_lr_auto)

      # Make a top frame
      tf <- tktoplevel()
      tkwm.title(tf, "Set display")

      # Define frames
      frame_display_1 <- tkframe(tf)
      frame_display_2 <- tkframe(tf)

      # Define widgets in frame_display_1
      label_title_1 <- tklabel(frame_display_1, text = "Query")
      label_title_2 <- tklabel(frame_display_1, text = "Reference")
      label_title_3 <- tklabel(frame_display_1, text = "Assumed relationship")
      label_title_4 <- tklabel(frame_display_1, text = "Minimum LR")
      combo_q <- ttkcombobox(frame_display_1, values = cand_q, textvariable = sn_q_select_var, state = "readonly")
      combo_r <- ttkcombobox(frame_display_1, values = cand_r, textvariable = sn_r_select_var, state = "readonly")
      combo_rel <- ttkcombobox(frame_display_1, values = cand_rel, textvariable = rel_select_var, state = "readonly")
      entry_min_lr <- tkentry(frame_display_1, textvariable = min_lr_select_var, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

      # Define widgets in frame_display_2
      butt_set <- tkbutton(frame_display_2, text = "    Set    ", cursor = "hand2",
                           command = function() set_display_2(tf, tclvalue(sn_q_select_var), tclvalue(sn_r_select_var), tclvalue(rel_select_var), as.numeric(tclvalue(min_lr_select_var))))

      # Grid widgets
      tkgrid(label_title_1, label_title_2, label_title_3, label_title_4, padx = 10, pady = 5)
      tkgrid(combo_q, combo_r, combo_rel, entry_min_lr, padx = 10, pady = 5)
      tkgrid(butt_set, padx = 10, pady = 5)

      # Grid frames
      tkgrid(frame_display_1)
      tkgrid(frame_display_2)
    }

    # The function to overwrite displayed data
    set_display_2 <- function(tf, sn_q_select, sn_r_select, rel_select, min_lr_select){

      # The number of comparisons between query and reference haplotypes
      n_data <- nrow(lr_all)

      # Investigate row indices to extract displayed data
      if(sn_q_select == "All"){
        pos_q <- 1:n_data
      }else{
        pos_q <- which(lr_all[, Query] == sn_q_select)
      }
      if(sn_r_select == "All"){
        pos_r <- 1:n_data
      }else{
        pos_r <- which(lr_all[, Reference] == sn_r_select)
      }
      if(rel_select == "All"){
        pos_rel <- 1:n_data
      }else{
        pos_rel <- which(lr_all[, AssumedRelationship] == rel_select)
      }
      pos_lr <- which(lr_all[, Total] >= min_lr_select)
      pos_extract <- intersect(intersect(intersect(pos_q, pos_r), pos_rel), pos_lr)

      # If there is at least one row index to extract displayed data
      if(length(pos_extract) != 0){

        # Destroy the scroll bar for displayed data
        scr1 <- get("scr1", pos = env_auto_result)
        tkdestroy(scr1)

        # Destroy the multi-list box for displayed data
        mlb_result <- get("mlb_result", pos = env_auto_result)
        tkdestroy(mlb_result)

        # Define the scroll bar for displayed data
        scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))

        # Define the multi-list box for displayed data
        mlb_result <- tk2mclistbox(frame_result_1, width = 85, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
        tk2column(mlb_result, "add", label = "Query", width = 15)
        tk2column(mlb_result, "add", label = "Reference", width = 15)
        tk2column(mlb_result, "add", label = "Assumed relationship", width = 40)
        tk2column(mlb_result, "add", label = "LR", width = 15)

        # Extract displayed data
        data_display <- lr_all[, list(Query, Reference, AssumedRelationship, Total)]
        data_display <- data_display[pos_extract, , drop = FALSE]
        data_display <- data_display[order(data_display$Total, decreasing = TRUE), , drop = FALSE]
        tk2insert.multi(mlb_result, "end", data_display)

        # Assign objects to the environment "env_auto_result"
        assign("scr1", scr1, envir = env_auto_result)
        assign("mlb_result", mlb_result, envir = env_auto_result)
        assign("data_display", data_display, envir = env_auto_result)

        # Grid the scroll bar and the multi-list box
        tkgrid(mlb_result, scr1)
        tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")

        # Destroy the top frame
        tkdestroy(tf)

      # If there is no row index to extract displayed data
      }else{
        tkmessageBox(message = "There is no data that meet the condition!", icon = "error", type = "ok")
      }
    }

    # The function to show data in detail
    show_detail <- function(){

      # Get the multi-list box for displayed data from the environment "env_auto_result"
      mlb_result <- get("mlb_result", pos = env_auto_result)

      # If the user does not select one line
      if(tclvalue(tkcurselection(mlb_result)) == ""){
        tkmessageBox(message = "Select one result!", icon = "error", type = "ok")

      # If the user selects one line
      }else{

        # Get displayed data from the environment "env_auto_result"
        data_display <- get("data_display", pos = env_auto_result)

        # The row index which the user selected
        pos_select <- as.numeric(tclvalue(tkcurselection(mlb_result))) + 1

        # Search indices to extract data
        data_select <- data_display[pos_select, ]
        sn_q_select <- data_select[, Query]
        sn_r_select <- data_select[, Reference]
        rel_select <- data_select[, AssumedRelationship]

        # Define a matrix for detailed data
        data_detail <- matrix("", n_l + 1, 6)
        colnames(data_detail) <- c("Locus",
                                  paste("Query (", sn_q_select, ")", sep = ""),
                                  paste("Reference (", sn_r_select, ")", sep = ""),
                                  "Likelihood (related)",
                                  "Likelihood (unrelated)",
                                  "LR")

        # 1st column for detailed data
        data_detail[, 1] <- c(names(myus), "Total")

        # 2nd column for detailed data
        query_select <- data_auto_q[data_auto_q$SampleName == sn_q_select, ]
        prof_query_select <- as.numeric(query_select[, -"SampleName", with = FALSE])
        data_detail[1:n_l, 2] <- display_gt(prof_query_select)

        # 3rd column for detailed data
        ref_select <- data_auto_r[data_auto_r$SampleName == sn_r_select, ]
        prof_ref_select <- as.numeric(ref_select[, -c("SampleName", "Relationship"), with = FALSE])
        data_detail[1:n_l, 3] <- display_gt(prof_ref_select)

        # 4th column for detailed data
        tmp <- like_h1_all[like_h1_all$Query == sn_q_select, ]
        tmp <- tmp[tmp$Reference == sn_r_select, ]
        tmp <- tmp[tmp$AssumedRelationship == rel_select, ]
        like_h1_select <- as.numeric(tmp[, -c("Query", "Reference", "AssumedRelationship"), with = FALSE])
        data_detail[, 4] <- signif(like_h1_select, digits = 4)

        # 5th column for detailed data
        tmp <- like_h2_all[like_h2_all$Query == sn_q_select, ]
        tmp <- tmp[tmp$Reference == sn_r_select, ]
        tmp <- tmp[tmp$AssumedRelationship == rel_select, ]
        like_h2_select <- as.numeric(tmp[, -c("Query", "Reference", "AssumedRelationship"), with = FALSE])
        data_detail[, 5] <- signif(like_h2_select, digits = 4)

        # 6th column for detailed data
        tmp <- lr_all[lr_all$Query == sn_q_select, ]
        tmp <- tmp[tmp$Reference == sn_r_select, ]
        tmp <- tmp[tmp$AssumedRelationship == rel_select, ]
        lr_select <- as.numeric(tmp[, -c("Query", "Reference", "AssumedRelationship"), with = FALSE])
        data_detail[, 6] <- signif(lr_select, digits = 4)

        # Make a top frame
        tf_detail <- tktoplevel()
        tkwm.title(tf_detail, "STR result in detail")

        # Define frames
        frame_detail_1 <- tkframe(tf_detail)
        frame_detail_2 <- tkframe(tf_detail)
        frame_detail_3 <- tkframe(tf_detail)

        # Define widgets in frame_detail_1
        label_title <- tklabel(frame_detail_1, text = paste("Relationship : ", rel_select, sep = ""))

        # Define a scrollbar for the multi-list box of the detailed data
        scr2 <- tkscrollbar(frame_detail_2, repeatinterval = 5, command = function(...) tkyview(mlb_detail, ...))

        # Define a multi-list box of the detailed data
        mlb_detail <- tk2mclistbox(frame_detail_2, width = 120, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr2, ...))
        tk2column(mlb_detail, "add", label = "locus", width = 20)
        tk2column(mlb_detail, "add", label = paste("Query (", sn_q_select, ")", sep = ""), width = 20)
        tk2column(mlb_detail, "add", label = paste("Reference (", sn_r_select, ")", sep = ""), width = 20)
        tk2column(mlb_detail, "add", label = "likelihood (related)", width = 20)
        tk2column(mlb_detail, "add", label = "likelihood (unrelated)", width = 20)
        tk2column(mlb_detail, "add", label = "LR", width = 20)
        tk2insert.multi(mlb_detail, "end", data_detail)

        # Define widgets in frame_detail_3
        butt_export <- tkbutton(frame_detail_3, text = "    Export    ", cursor = "hand2", command = function() export_data(data_detail, FALSE))

        # Grid widgets
        tkgrid(label_title)
        tkgrid(mlb_detail, scr2)
        tkgrid.configure(scr2, rowspan = 30, sticky = "nsw")
        tkgrid(butt_export)

        # Grid frames
        tkgrid(frame_detail_1, padx = 10, pady = 5)
        tkgrid(frame_detail_2, padx = 10, pady = 5)
        tkgrid(frame_detail_3, padx = 10, pady = 5)
      }
    }

    # Define the environment "env_auto_result"
    env_auto_result <- new.env(parent = globalenv())

    # Get database from the environment "env_proj"
    data_auto_q <- get("data_auto_q", pos = env_proj)
    data_auto_r <- get("data_auto_r", pos = env_proj)
    data_auto_af <- get("data_auto_af", pos = env_proj)

    # Get results from the environment "env_proj"
    like_h1_all <- get("like_h1_all", pos = env_proj)
    like_h2_all <- get("like_h2_all", pos = env_proj)
    lr_all <- get("lr_all", pos = env_proj)

    # Get parameters from the environment "env_proj"
    min_lr_auto <- get("min_lr_auto", pos = env_proj)
#    pibd_all <- get("pibd_all", pos = env_proj)
    myus <- get("myus", pos = env_proj)
#    maf <- get("maf", pos = env_proj)
#    meth_d <- get("meth_d", pos = env_proj)
#    pd <- get("pd", pos = env_proj)

    # The number of samples in each database
    ##n_q <- nrow(data_auto_q)
    ##n_r <- length(sn_auto_r_new)

    # The number of loci
    n_l <- length(myus)

    # Cumulative likelihood ratio
    ## clr_all <- lr_all[, Total]
    ## clr_all <- rbind(rel_auto_r_new, clr_all)
    ## rownames(clr_all) <- c("relationship", sn_auto_q)
    ## colnames(clr_all) <- sn_auto_r_new

    # Define vectors for making displayed data
    ## sn_auto_q_vec <- rep(sn_auto_q, n_r)
    ## sn_auto_r_vec <- as.vector(sapply(sn_auto_r_new, rep, n_q))
    ## rel_auto_vec <- as.vector(sapply(rel_auto_r_new, rep, n_q))
    ## clr_all_vec <- as.vector(lr_all[, , n_l + 1])

    # Define a matrix for the displayed data
    data_display <- lr_all[, list(Query, Reference, AssumedRelationship, Total)]
    data_display <- data_display[data_display$Total >= min_lr_auto, , drop = FALSE]
    data_display <- data_display[order(data_display$Total, decreasing = TRUE), , drop = FALSE]

    # Reset frame_tab2
    tabs <- get("tabs", pos = env_gui)
    tab2 <- get("tab2", pos = env_gui)
    frame_tab2 <- get("frame_tab2", pos = env_gui)
    tkdestroy(frame_tab2)
    frame_tab2 <- tkframe(tab2)

    # Define frames
    frame_result_1 <- tkframe(frame_tab2)
    frame_result_2 <- tkframe(frame_tab2)

    # Define a scrollbar for the multi-list box of the displayed data
    scr1 <- tkscrollbar(frame_result_1, repeatinterval = 5, command = function(...) tkyview(mlb_result, ...))

    # Define a multi-list box of the displayed data
    mlb_result <- tk2mclistbox(frame_result_1, width = 85, height = 30, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scr1, ...))
    tk2column(mlb_result, "add", label = "Query", width = 15)
    tk2column(mlb_result, "add", label = "Reference", width = 15)
    tk2column(mlb_result, "add", label = "Assumed relationship", width = 40)
    tk2column(mlb_result, "add", label = "LR", width = 15)
    tk2insert.multi(mlb_result, "end", data_display)

    # Define widgets in frame_result_2
    butt_display <- tkbutton(frame_result_2, text = "    Set display    ", cursor = "hand2", command = function() set_display_1())
    butt_detail <- tkbutton(frame_result_2, text = "    Show detail    ", cursor = "hand2", command = function() show_detail())
    butt_export1 <- tkbutton(frame_result_2, text = "    Export displayed data    ", cursor = "hand2", command = function() export_data(get("data_display", pos = env_auto_result), FALSE))
    butt_export2 <- tkbutton(frame_result_2, text = "    Export All LRs    ", cursor = "hand2", command = function() export_data(clr_all, TRUE))

    # Grid widgets
    tkgrid(mlb_result, scr1)
    tkgrid.configure(scr1, rowspan = 30, sticky = "nsw")
    tkgrid(butt_display, butt_detail, butt_export1, butt_export2, padx = 10, pady = 5)

    # Grid frames
    tkgrid(frame_result_1, padx = 10, pady = 5)
    tkgrid(frame_result_2)
    tkgrid(frame_tab2)

    # Assign objects to the environment "env_auto_result"
    assign("mlb_result", mlb_result, envir = env_auto_result)
    assign("scr1", scr1, envir = env_auto_result)
    assign("data_display", data_display, envir = env_auto_result)

    # Assign frame_tab2 to the environment "env_gui"
    assign("frame_tab2", frame_tab2, envir = env_gui)

    # Select tab2
    tk2notetab.select(tabs, "STR results")

  # If the autosomal STR analysis has not been finished
  }else{

    # Reset frame_tab2
    tab2 <- get("tab2", pos = env_gui)
    frame_tab2 <- get("frame_tab2", pos = env_gui)
    tkdestroy(frame_tab2)
    frame_tab2 <- tkframe(tab2)

    # Assign frame_tab2 to the environment "env_gui"
    assign("frame_tab2", frame_tab2, envir = env_gui)
  }
}
