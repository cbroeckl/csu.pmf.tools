#' startProject
#'
#' @details take info from iLab and iLab sample submission form and create sample prep and data acquisition templates and directories on the R:  
#' @param prep.batch.size integer - what is the sample preparation batch size?  default = 48. 
#' @param run.batch.size integer - what is the analytical batch size? default = 96.
#' @param QC  integer - every 'QC' injections will be a QC sample.  Default = 6  
#' @param randomize logical - should prep and run order be randomized?  
#' @param stack should the injection sample list be set up for stacked injections (TOF platforms only, currenly)
#' @param prep.blanks integer - how many extraction blanks per prep batch? prep.blanks will be inserted into the prep sequence in random sequence position(s). 
#' @param LTR  logical - If TRUE, an long term reference sample line will be included at the beginning and end of each run batch, with each LTR followed by an inserted solvent blank.  
#' @param destination.dir valid path to directory target output directory, by default: "R:/RSTOR-PMF/Projects/"
#' @return returns nothing, files written to directory selected by user.
#' @author Corey Broeckling
#' @export 


startProject<-function (
  prep.batch.size = 60,
  run.batch.size=96,
  QC=6,
  randomize=TRUE,
  stack = FALSE,
  prep.blanks = 3,
  per.qc.volume = 50,
  LTR = 0,
  remove.invariant.factors = TRUE,
  destination.dir = "R:/RSTOR-PMF/Projects/", # 
  sub.project.name = NULL  ## add optional subproject.name
) {
  
  ## NEED TO ADD BATCH ORDER RANDOMIZATION
  ## CONSIDER THE BENEFITS OF ADDING RUN ORDER TO SAMPLE NAME VS REFERENCING BOTH SEQUENCE.CSV AND SEQ.CSV
  ## PLATFORM SPECIFIC COPYING OF R SCRIPTS TO R_SCRIPTS FOLDER
  
  require(tcltk)
  require(xlsxjars)
  require(xlsx)
  
  #  do some checks to ensure we have integer values and even numbers, when appropriate. 
  if(LTR) {
    LTR <- 1
    solvent.blanks <- 1
  } else {
    LTR <- 0
    solvent.blanks <- 0
  }
  
  redo = FALSE 
  
  while(!redo) {
    
    cat('\n', '\n', "please highlight all data in iLab project overview for your project", '\n',
        " - view all requests tab", '\n',
        " - expand project", '\n',
        " - expland 'overview", '\n',
        " - highlight all lines from 'core identifier' to 'labels'",'\n',
        " - right click  and copy or 'ctrl+c", '\n','\n')
    
    readline(prompt="Press [enter] to continue")
    
    suppressWarnings(d<-read.delim("clipboard", row.names = 1, header = FALSE, stringsAsFactors = FALSE))
    
    if(ncol(d) == 0) {
      d2 <- dimnames(d)[[1]]
      d3<-  rep(NA, length(d2))
      for (x in 1:length(d3))  {
        tmp<- unlist(strsplit(d2[x], ":"))
        dimnames(d)[[1]][x] <- paste(tmp[1], ": ", sep = "")
        d3[x] <- paste(tmp[2:length(tmp)], collapse = ":")
      }
      d <- data.frame(d, d3, stringsAsFactors = FALSE)
    }
    
    names(d)<-"value"
    cat('\n', '\n', "project name: ", d["Service id:",], '\n',
        "Lab: ", d["Lab Name:",],'\n',
        "PI: ", d["Lab PI(s):",], '\n', '\n', sep=""
    )
    
    feedback<- readline(prompt=cat(
      "please check the above data: ", '\n',
      " - to try again press [t]", '\n',
      " - to accept press [enter]", '\n', '\n'
    ))
    
    if(feedback != "t") {
      redo = TRUE 
    }
    
  }
  
  ##############
  ##############
  ##############
  ##############
  ##############
  
  projdir<-paste0(destination.dir, d["Service id:",], "/")
  dir.create(projdir)
  
  
  ## if subproject name is selected, modify projdir
  if(!is.null(sub.project.name)) {
    projdir <- paste0(projdir, sub.project.name, "/")
    dir.create(projdir)
  }
  setwd(projdir)
  
  write.csv(d, file = paste0(projdir, "ilab_overview.csv"), row.names = TRUE)
  
  #cat("Choose PI Directory (Lastname_Firstname)", '\n', '\n')
  #pidir<-choose.dir(default="K:/", caption = "Choose PI Directory (Lastname_Firstname)")
  cat("Choose Sample Submission Form", '\n',
      "  A window should have opened and may not move to the front", '\n',
      "  of all windows, try minimizing the Rstudio window if need be.",  '\n')
  
  ## if subproject name is selected, modify projdir
  if(!is.null(sub.project.name)) {
    warning("Please ensure that your sample submission form", '\n',
            "  contains only those samples you wish to include",'\n',
            "  in the subproject directory. You will need to", '\n',
            "  manually edit the form. ", '\n')
  }
  
  submissionform <- choose.files(caption = "please navigate to and select the sample submission excel file.")
  
  
  select.platforms <- function(){
    platforms<-c( 
      "GCMS_EI",      #1
      "TOF_PH_Pos",   #2
      "TOF_PH_Neg",   #3
      "TOF_T3_Pos",   #4
      "TOF_T3_Neg",   #5
      "TOF_Hil_Pos",  #6
      "TOF_Hil_Neg",  #7
      "other1",      #8
      "other2",     #9
      "other3"      #10
    )
    
    
    ## ADD A LEVEL HERE FOR EACH PLATFORM ADDED ###########
    avar <- tclVar(0)
    bvar <- tclVar(0)
    cvar <- tclVar(0)
    dvar <- tclVar(0)
    evar <- tclVar(0)
    fvar <- tclVar(0)
    gvar <- tclVar(0)
    hvar <- tclVar(0)
    ivar <- tclVar(0)
    jvar <- tclVar(0)
    
    
    tt <- tktoplevel()
    tkwm.title(tt,"Platform Selection Window")
    
    ## ADD A LEVEL HERE FOR EACH PLATFORM ADDED ###########
    a.entry <- tkcheckbutton(tt, variable=avar)
    b.entry <- tkcheckbutton(tt, variable=bvar)
    c.entry <- tkcheckbutton(tt, variable=cvar)
    d.entry <- tkcheckbutton(tt, variable=dvar)
    e.entry <- tkcheckbutton(tt, variable=evar)
    f.entry <- tkcheckbutton(tt, variable=fvar)
    g.entry <- tkcheckbutton(tt, variable=gvar)
    h.entry <- tkcheckbutton(tt, variable=hvar)
    i.entry <- tkcheckbutton(tt, variable=ivar)
    j.entry <- tkcheckbutton(tt, variable=jvar)
    
    reset <- function()
    {
      ## ADD A LEVEL HERE FOR EACH PLATFORM ADDED ###########
      
      tclvalue(avar)<-0
      tclvalue(bvar)<-0
      tclvalue(cvar)<-0
      tclvalue(dvar)<-0
      tclvalue(evar)<-0
      tclvalue(fvar)<-0
      tclvalue(gvar)<-0
      tclvalue(hvar)<-0
      tclvalue(ivar)<-0
      tclvalue(jvar)<-0
    }
    
    submit <- function() {
      ## ADD A LEVEL HERE FOR EACH PLATFORM ADDED ###########
      a <- as.logical(as.numeric(tclvalue(avar)))
      b <- as.logical(as.numeric(tclvalue(bvar)))
      c <- as.logical(as.numeric(tclvalue(cvar)))
      d <- as.logical(as.numeric(tclvalue(dvar)))
      e <- as.logical(as.numeric(tclvalue(evar)))
      f <- as.logical(as.numeric(tclvalue(fvar)))
      g <- as.logical(as.numeric(tclvalue(gvar)))
      h <- as.logical(as.numeric(tclvalue(hvar)))
      i <- as.logical(as.numeric(tclvalue(ivar)))
      j <- as.logical(as.numeric(tclvalue(jvar)))
      
      en <- parent.env(environment())
      
      ## ADD A LEVEL HERE FOR EACH PLATFORM ADDED ###########
      en$a <- a
      en$b <- b
      en$c <- c
      en$d <- d
      en$e <- e
      en$f <- f
      en$g <- g
      en$h <- h
      en$i <- i
      en$j <- j
      
      
      tkdestroy(tt)
    }
    
    reset.but <- tkbutton(tt, text="Reset", command=reset)
    submit.but <- tkbutton(tt, text="submit", command=submit)
    
    tkgrid(tklabel(tt,text="Select Platforms"),columnspan=2)
    
    ## ADD A LEVEL HERE FOR EACH PLATFORM ADDED ###########
    tkgrid(tklabel(tt, text=platforms[1]), a.entry, pady = 10, padx =50)
    tkgrid(tklabel(tt, text=platforms[2]), b.entry, pady = 10, padx =50)
    tkgrid(tklabel(tt, text=platforms[3]), c.entry, pady = 10, padx =50)
    tkgrid(tklabel(tt, text=platforms[4]), d.entry, pady = 10, padx =50)
    tkgrid(tklabel(tt, text=platforms[5]), e.entry, pady = 10, padx =50)
    tkgrid(tklabel(tt, text=platforms[6]), f.entry, pady = 10, padx =50)
    tkgrid(tklabel(tt, text=platforms[7]), g.entry, pady = 10, padx =50)
    tkgrid(tklabel(tt, text=platforms[8]), h.entry, pady = 10, padx =50)
    tkgrid(tklabel(tt, text=platforms[9]), i.entry, pady = 10, padx =50)
    tkgrid(tklabel(tt, text=platforms[10]), j.entry, pady = 10, padx =50)
    
    
    tkgrid(submit.but, reset.but)
    
    tkwait.window(tt)
    
    ## ADD A LEVEL HERE FOR EACH PLATFORM ADDED ###########
    return(data.frame(platforms, 'select'=c(a,b,c,d,e,f,g,h,i,j), stringsAsFactors=FALSE))
  }
  platforms <- select.platforms()
  
  paramsets<-list(
    TOF_Hil_Pos=data.frame(c(chrominst="Waters UPLC",
                             msinst="Waters Xevo G2-XS Q-TOF",
                             column="Waters Premier Amide, 2 x 100 mm, 1.7 uM",
                             solvA="Water, 10mM Ammonium Hydroxide, 0.1% Formic acid",
                             solvB="95% Acetonitrile, 5% Water, 10mM Ammonium Hydroxide, 0.1% Formic acid",
                             MSlevs=2,
                             CE1="6",
                             CE2="15-30",
                             mstype="Q-TOF",
                             mzdifftof=0.02,
                             msmode="P",
                             ionization="ESI",
                             ESIvoltage="700",
                             colgas="Ar",
                             msscanrange="50-1200",
                             conevolt="30")),
    
    TOF_Hil_Neg=data.frame(c(chrominst="Waters UPLC",
                             msinst="Waters Xevo G2-XS Q-TOF",
                             column="Waters Premier Amide, 2 x 100 mm, 1.7 uM",
                             solvA="Water, 10mM Ammonium Hydroxide, 0.1% Formic acid",
                             solvB="95% Acetonitrile, 5% Water, 10mM Ammonium Hydroxide, 0.1% Formic acid",
                             MSlevs=2,
                             CE1="6",
                             CE2="15-30",
                             mstype="Q-TOF",
                             mzdifftof=0.02,
                             msmode="N",
                             ionization="ESI",
                             ESIvoltage="2000",
                             colgas="Ar",
                             msscanrange="50-1200",
                             conevolt="30")),
    
    TOF_T3_Pos=data.frame(c(chrominst="Waters UPLC",
                            msinst="Waters Xevo G2-XS Q-TOF",
                            column="Waters HSST3 C18, 2 x 100 mm, 1.8 uM",
                            solvA="Water, 0.1% formic acid",
                            solvB="ACN, 0.1% formic acid",
                            MSlevs=2,
                            CE1="6",
                            CE2="15-30",
                            mstype="Q-TOF",
                            mzdifftof=0.02,
                            msmode="P",
                            ionization="ESI",
                            ESIvoltage="700",
                            colgas="Ar",
                            msscanrange="50-1200",
                            conevolt="30")) ,
    
    TOF_T3_Neg=data.frame(c(chrominst="Waters UPLC: C18 ACN Gradient",
                            msinst="Waters Xevo G2-XS Q-TOF",
                            column="Waters HSST3 C18, 2 x 100 mm, 1.8 uM",
                            solvA="Water, 0.1% formic acid",
                            solvB="ACN, 0.1% formic acid",
                            MSlevs=2,
                            CE1="6",
                            CE2="15-30",
                            mstype="TOF",
                            mzdifftof=0.02,
                            msmode="N",
                            ionization="ESI",
                            ESIvoltage="2200",
                            colgas="Ar",
                            msscanrange="50-1200",
                            conevolt="30")) ,
    
    TOF_PH_Pos=data.frame(c(chrominst="Waters UPLC: ACN Gradient",
                            msinst="Waters Xevo G2-XS Q-TOF",
                            column="Waters CSH PhenylHexyl, 1 x 100 mm, 1.7 uM",
                            solvA="Water, 0.1% formic acid, 2mM AmOH",
                            solvB="ACN, 0.1% formic acid",
                            MSlevs=2,
                            CE1="6",
                            CE2="15-30",
                            mstype="Q-TOF",
                            mzdifftof=0.02,
                            msmode="P",
                            ionization="ESI",
                            ESIvoltage="700",
                            colgas="Ar",
                            msscanrange="50-1200",
                            conevolt="30")) ,
    
    TOF_PH_Neg=data.frame(c(chrominst="Waters UPLC: ACN Gradient",
                            msinst="Waters Xevo G2-XS Q-TOF",
                            column="Waters CSH PhenylHexyl, 1 x 100 mm, 1.7 uM",
                            solvA="Water, 0.1% formic acid, 2mM AmOH",
                            solvB="ACN, 0.1% formic acid",
                            MSlevs=2,
                            CE1="6",
                            CE2="15-30",
                            mstype="Q-TOF",
                            mzdifftof=0.02,
                            msmode="N",
                            ionization="ESI",
                            ESIvoltage="2000",
                            colgas="Ar",
                            msscanrange="50-1200",
                            conevolt="30")) ,
    
    GCMS_EI=data.frame(c(chrominst="Thermo Trace GC: TG-5MS column",
                         msinst="Thermo ISQ",
                         MSlevs=1,
                         InletTemp="280",
                         TransferTemp="280",
                         mstype="QUAD",
                         mzdiffquad=0.5,
                         msmode="P",
                         ionization="EI",
                         msscanrange="50-650",
                         scantime="0.2",
                         energy="70",
                         deriv="Methoxyamine+MSTFA")) ,
    
    newLC=data.frame(c(chrominst="Waters UPLC",
                       msinst="",
                       column="",
                       MSlevs=2,
                       solvA="",
                       solvB="",
                       CE1="6",
                       CE2="",
                       mstype="",
                       msmode="P",
                       ionization="ESI",
                       colgas="Ar",
                       msscanrange="50-1200",
                       conevolt="30")),
    
    newGC=data.frame(c(chrominst="Thermo Trace GC: TG-5MS column",
                       msinst="Thermo ISQ",
                       MSlevs=1,
                       InletTemp="280",
                       TransferTemp="280",
                       mstype="QUAD",
                       mzdiffquad=0.5,
                       msmode="P",
                       ionization="EI",
                       msscanrange="50-650",
                       scantime="0.2",
                       energy="70",
                       deriv="Methoxyamine+MSTFA"))
  )
  
  # set up basic, return later to add factor names
  ExpVars<-c(Experiment=d["Service id:",1],
             PI=unlist(strsplit(d["Lab PI(s):",1], ":"))[1],
             User="",
             Species="",
             Sample="",
             platform="",
             delim="-"
  )
  #  c(paste0("fact", 1:100, "name"))
  VarDesc<-c("experiment name, no spaces",
             "Name of Principle Investigator",
             "Name of User (person submitting samples)",
             "species name (binomial latin name)",
             "sample type",
             paste(names(paramsets), sep=" ", collapse=" "),
             "factor delimitor in your sample names",
             "Assign a name for your factors",
             "",           
             "",           
             "",           
             "",           
             "",
             "",
             "",
             "",
             ""
  )
  
  fnrange<-grep("fact1name", names(ExpVars))
  
  rename<-intersect(grep("other", platforms[,1]), which(platforms[,2]))
  if(length(rename) >0 )  {
    for(i in rename) {
      cat ("PLEASE ENTER NEW NAME and press 'ENTER'", as.character(platforms[i,1]), ":", '\n')
      platforms[i,1] <- readline()
    }
  }
  
  answer <- "t"
  while(answer == "t") {
    smp<-xlsx::read.xlsx(submissionform, sheetName = "SampleList", 
                         startRow=4, header=TRUE, check.names = FALSE, 
                         stringsAsFactors=FALSE)
    orig.smp <- smp
    #cat(" Reading and checking sample submission form: ", '\n', '\n')
    cat ("PLEASE CHECK TO ENSURE FACTOR NAMES ARE ALL CORRECT: ", "\n", '\n')
    
    narow<-function(x) {return(!all(is.na(smp[x,])))}
    narows<-sapply(1:nrow(smp), FUN=narow)
    smp<-smp[narows,]
    nacol<-function(x) {return(!all(is.na(smp[,x])))}
    nacols<-sapply(1:ncol(smp), FUN=nacol)
    smp<-smp[,nacols]
    orig.smp <- smp
    numeric.cols <- which(sapply(1:ncol(smp), FUN = function(x) {is.numeric(smp[,x])}))
    
    ## trim all whitespace
    for(i in 1:ncol(smp)) {
      smp[,i] <- trimws(smp[,i])
    }
    
    ## replace empty string cells with NA values
    smp[which(smp == "", arr.ind = TRUE)] <- NA
    
    ## remove any rows and/or columns with nothing but empty cells
    for(i in nrow(smp):1) {
      all.nas <- all(is.na(smp[i,]))
      if(all.nas) {
        smp <- smp[-i,]
      }
    }
    for(i in ncol(smp):1) {
      all.nas <- all(is.na(smp[,i]))
      if(all.nas) {
        smp <- smp[,-i]
      }
    }
    
    for(i in 1:ncol(smp)) {
      smp[,i] <- trimws(smp[,i])
      smp[,i] <- gsub("-", "_", smp[,i])
      smp[,i] <- gsub(" ", "_", smp[,i])
    }
    
    names(smp) <- trimws(names(smp))
    names(smp) <- gsub("-", "_", names(smp))
    names(smp) <- gsub(" ", "_", names(smp))
    
    
    ## replace NA values with "_"
    smp[which(is.na(smp), arr.ind = TRUE)] <- "_"
    
    for (i in 1:ncol(smp)) {
      cat(names(smp)[i], '\n')
      unique.levels <- unique(smp[,i])
      cat("  levels:", if(
        length(unique.levels) < length(smp[,i])
      ) {
        unique.levels} else { "unique value for each sample"
        }, '\n', '\n')
    }
    
    cat ("PRESS [enter] IF CORRECT", '\n',
         " or if incorrect, edit and save excel file and enter 't' to try again", '\n', sep="")
    answer <- readline()
    
    if(answer == 't') {
      cat('\n', '\n', "PLEASE CORRECT SAMPLE NAMES IN SAMPLE SUBMISSION FORM AND SAVE", '\n',
          "when complete, press [enter] to continue and try again", '\n', '\n', sep="")
      readline()
    } else {
      answer == "go"
    }
    
    if(length(numeric.cols)>0) {
      for(x in numeric.cols) {
        smp[,x] <- as.numeric(smp[,x])
      }
    }
    
    if(remove.invariant.factors) {
      invariant.factors <- which(sapply(
        1:ncol(smp), FUN = function(x){
          length(unique(smp[,x]))==1
        }
      ))
      if(length(invariant.factors)>0) {
        smp <- smp[,-invariant.factors, drop = FALSE]
      }
    }
  }
  
  
  
  ## determine number of prep batches
  n.prep.batches <- ceiling(nrow(smp)/(prep.batch.size-prep.blanks))
  
  
  ## assign prep batch
  prep_batch <- vector(length = 0, mode = "integer")
  for(i in 1:n.prep.batches) {prep_batch <- c(prep_batch, rep(i, (prep.batch.size - prep.blanks)))}
  prep_batch <- prep_batch[1:nrow(smp)]
  samples <- smp
  smp <- data.frame(
    # "pmf.sn" = 1:nrow(smp),  # removed, as prep_order serves as pmf.sn instead.
    "prep_order" = if(randomize) {
      sample(1:length(prep_batch), nrow(samples), replace = FALSE)
    } else {
      1:length(prep_batch)
    },
    "prep_batch" = prep_batch,
    "run_order" = rep(NA, nrow(samples)),
    "run_batch" = rep(NA, nrow(samples)),
    samples, stringsAsFactors = FALSE
  )
  
  ## add prep blanks for each prep batch:
  for(i in 1:n.prep.batches) {
    available.order <- smp$prep_order[which(smp$prep_batch == i)]
    prep.blank.rows <- smp[0,,drop = TRUE]
    prep.blank.rows[1,] <- "prep.blank"
    prep.blank.rows$prep_batch[1] <- i
    for(j in 1:prep.blanks) {
      prep.blank.rows$prep_order[1] <- sample(available.order, 1)
      smp <- rbind(smp, prep.blank.rows)
    }
  }
  
  ## randomize prep batch order
  if(randomize) {
    prep.batch.order <- as.numeric(smp$prep_batch)
    rand.batch.order <- sample(unique(prep.batch.order), length(unique(prep.batch.order)), replace = FALSE)
    new.prep.batch.order <- rep(NA, length(prep.batch.order))
    for(i in 1:length(rand.batch.order)) {
      new.prep.batch.order[which(prep.batch.order == i)] <- rand.batch.order[i]
    }
    smp$prep_batch <- new.prep.batch.order
  }

  ## assign final prep order: 
  prep.order <- smp$prep_order
  for(i in 1:n.prep.batches) {
    do <- which(smp$prep_batch == i)
    prep.order[do] <- if(randomize) {order(prep.order[do])} else {prep.order[do]}
  }
  prep.order <- as.numeric(prep.order)
  smp$prep_order <- prep.order
  

  ## write prep .csv 
  smp <- smp[order(as.numeric(smp$prep_order)),]
  smp <- smp[order(as.numeric(smp$prep_batch)),]

  write.csv(smp[,-grep("run_", names(smp))], file = "prep.sample.list.csv", row.names = FALSE)
  
  ## get project name and shorten for convenience
  projname<-d["Service id:",1]
  tmp<-unlist(strsplit(projname, "-"))
  short.projectname <- paste(tmp[length(tmp)-1], tmp[length(tmp)], sep = "-")
  rm(tmp)
  
  ## copy submissionform file to project directory
  file.copy(from=submissionform, to=paste0(projdir,  ".", basename(submissionform)))
  
  ## which platforms are we using? 
  doplatforms<-which(platforms[,2])
  smp.backup <- smp
  
  ## for each platform
  for(x in doplatforms) {
    
    ## create dir name, create dir, and move into that directory
    setwd(projdir)
    platform.dir <- paste0(projdir, platforms[x,1], "/")
    dir.create(platform.dir)
    setwd(platform.dir)
    smp <- smp.backup
    
    ## build sequence for output
    out <- smp[0,, drop = TRUE]
    ltr <- out
    ltr[1,] <- "ltr"
    solvent.blank <- out
    solvent.blank[1,] <- 'solvent.blank'
    prep.blank <- out
    prep.blank[1,] <- "prep.blank"
    qc <- out
    qc[1,] <- "qc"
    
    ## determine run batch size, accounting for solvent blanks and long term reference samples
    rbc <- run.batch.size - LTR - solvent.blanks
    qc.rows <- c( (LTR + solvent.blanks + 1), 
                  seq((1 + QC + LTR + solvent.blanks), 96-LTR-solvent.blanks-QC, QC),
                  (run.batch.size)
    )
    qc.rows.1 <- qc.rows
    for(i in 1:999) {
      qc.rows <- c(qc.rows, (run.batch.size*i)+qc.rows.1)
    }
    rbc <- rbc - 2 - length(qc.rows.1)
    on.batch <- 1
    on.batch.row <- 1
    
    batch.end.rows <- c(1:1000)*run.batch.size
    if(LTR > 0) {
      ltr.rows <- sort(c( (c(1:1000)*run.batch.size)-run.batch.size+1 , (c(1:1000)*run.batch.size)-1))
    } else  {ltr.rows <- c()
    }
    if(solvent.blanks > 0) {
      solvent.blank.rows <- ltr.rows + 1
    } else {
      solvent.blank.rows <- c()
    }

    nonsample.rows <- sort(c(ltr.rows, solvent.blank.rows, qc.rows))
    
    ## move samples to sequence
    while(nrow(smp) > 0) {
      
      if(on.batch.row %in% nonsample.rows) {
        if(on.batch.row %in% ltr.rows) {out[on.batch.row,] <- ltr}
        if(on.batch.row %in% solvent.blank.rows) {out[on.batch.row,] <- solvent.blank}
        if(on.batch.row %in% qc.rows) {out[on.batch.row,] <- qc}
        out[on.batch.row,"run_batch"] <- on.batch
        if(on.batch.row %in% batch.end.rows) {on.batch <- on.batch + 1}
        on.batch.row <- on.batch.row + 1
      } else {
        move.row <- if(randomize) {
          sample(1:nrow(smp),1)
        } else {
          1
        }
        out[on.batch.row,] <- smp[move.row,]
        out[on.batch.row,"run_batch"] <- on.batch
        if(on.batch.row %in% batch.end.rows) {on.batch <- on.batch + 1}
        on.batch.row <- on.batch.row + 1
        smp <- smp[-move.row,]
      }
    }
    
    ## add terminal LTR/solvent.blank/QCrows as needed.
    if(out[nrow(out), 1] != "qc") {
      out <- rbind(out, qc)
      out[nrow(out), "run_batch"] <- on.batch
      #  out[on.batch.row, "run_order"] <- on.batch.row
      on.batch.row <- on.batch.row + 1
    }
    if(LTR > 0) {
      for(i in 1:LTR) {
        out <- rbind(out, ltr, solvent.blank)
        out$run_batch[(nrow(out)-1):nrow(out)] <- on.batch
        out$run_batch[(nrow(out)-1):nrow(out)] <- on.batch.row:(on.batch.row + 1)
      }
    }
    
    out$run_batch <- as.integer(as.numeric(out$run_batch))
    n.run.batches <- max(out$run_batch, na.rm = TRUE)
    out$run_order <- 1:nrow(out)
    
    # for(i in 1:n.run.batches) {
    #   do <- which(out$run_batch == i)
    #   out$run_order[do] <- sample(1:length(do))
    #   tmp <- out[do,]
    #   tmp <- tmp[order(tmp$run_order),]
    #   out[do,] <- tmp
    # }
    
    out[which(out$prep_order == "ltr"), "prep_order"] <- NA
    out[which(out$prep_batch == "ltr"), "prep_batch"] <- NA
    
    out[which(out$prep_order == "qc"), "prep_order"] <- NA
    out[which(out$prep_batch == "qc"), "prep_batch"] <- NA
    
    out[which(out$prep_order == "solvent.blank"), "prep_order"] <- NA
    out[which(out$prep_batch == "solvent.blank"), "prep_batch"] <- NA
    
    # create factor labels and names based on smp input
    flabs <- c(paste0("fact", 1:ncol(smp), "name"))
    fnames <- names(smp)
    names(fnames) <- flabs
    
    ## add fnames to ExpVars
    ExpVars <- c(ExpVars, fnames)
    
    ## edit instrument parameters, if need be
    Exp<-ExpVars
    Exp["platform"]<-as.character(platforms[x,"platforms"])
    Experiment<-data.frame(Exp,stringsAsFactors=FALSE)
    cat("  Please edit instrument parameters as approriate: ", Exp["platform"], '\n')
    if(!is.null(paramsets[[as.character(Exp["platform"])]])) {
      instrument<-edit(paramsets[[as.character(Exp["platform"])]])
    } else {
      cat ("PLATFORM", as.character(platforms[x,"platforms"]) , ": TYPE 'g' FOR NEW GC-MS PLATFORM AND 'l' FOR NEW LC-MS PLATFORM", '\n')
      answer <- readline()
      if(grepl('g', answer)) {
        instrument<-edit(paramsets[['newGC']])
      } else {
        instrument<-edit(paramsets[['newLC']])
      }
    }
    
    #create ExpDes object for saving to disk and using in RAMClustR processing
    ExpDes<-list(Experiment, instrument)
    names(ExpDes)<-c("design", "instrument")
    save(ExpDes, file="ExpDes.Rdata")
    dir.create("R_scripts")
    

    
    ## create file and sample names for run .csv file
    out <- data.frame(
      "filename" = paste(platforms[x,1], short.projectname, formatC(1:nrow(out), width = nchar(nrow(out)), flag = 0), sep = "-"), 
      "sample.name" = sapply(1:nrow(out), FUN = function(x) {paste(out[x, 1:ncol(out)], collapse = "-")}),
      out
    )
    
    ## if using stacked injections, dublicate rows for stacking sequence
    if(stack) {
      
      smpstack<-out
      smpstack$filename<-paste(smpstack$filename, "_stack", sep="")
      smpinj<-out
      tmp<-out[0,]
      for(k in 1:nrow(smpstack)) {
        tmp<-rbind(tmp, smpstack[k,], smpinj[k,])
      }
      out <- tmp
      rm(tmp)
    }
    
    ## write sequence file
    write.csv(out, file="sequence.csv", row.names=FALSE)
    
    ## write QC prep instructions
    n.qc.samples <- length(grep("qc", out$sample.name))
    n.real.samples <- nrow(orig.smp)
    qc.prep.text <- paste(
      "The sample list contains", 
      n.qc.samples, 
      "qc samples and",
      n.real.samples, 
      "samples in the original sample list.",
      "At", per.qc.volume, 
      "microliters per qc sample with 25% overage to account for pippetting loss,",
      "we will need a total of",
      ceiling(n.qc.samples*per.qc.volume*1.25), 
      "microliters of qc sample volume.", 
      "This will require", 
      ceiling((n.qc.samples*per.qc.volume*1.25)/n.real.samples),
      "microliters of sample from each sample extract."
    )
    sink("qc.prep.instructions.txt")
    cat(qc.prep.text)
    sink()
    
    
    setwd(projdir)
  }
  
  setwd(destination.dir)
  cat("FINISHED: created directories for the following platforms", platforms[doplatforms, 1], '\n')
}
