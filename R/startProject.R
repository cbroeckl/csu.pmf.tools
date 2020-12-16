#' startProject
#'
#' @details take info from iLab and iLab sample submission form and create sample prep and data acquisition templates and directories on the R:  
#' @param prep.batch.size integer - what is the sample preparation batch size?  default = 48. 
#' @param run.batch.size integer - what is the analytical batch size? default = 96.
#' @param QC  integer - every 'QC' injections will be a QC sample.  Default = 6  
#' @param stack should the injection sample list be set up for stacked injections (TOF platforms only, currenly)
#' @param prep.blanks integer - how many extraction blanks per prep batch? prep.blanks will be inserted into the prep sequence in random sequence position(s). 
#' @param LTR  integer - how many long term reference material (LTR) vials per batch? default = 0.  LTR samples will be inserted at the start of each batch, and a solvent blank will ALwAYs be inserted after the LTR.
#' @param solvent.blanks integer - how many ADDITIONAL solvent blank vials  per batch? default = 0. solvent blanks will include one for each LTR automatically. Any additional solvent blanks specified here will be inserted randomly in each batch.
#' @param destination.dir valid path to directory target output directory, by default: "R:/RSTOR-PMF/Projects/"
#' @return returns nothing, files written to directory selected by user.
#' @author Corey Broeckling
#' @export 


startProject<-function (
  prep.batch.size = 48,
  run.batch.size=96,
  QC=6,
  randomize=TRUE,
  stack = FALSE,
  prep.blanks = 3,
  LTR = 1,
  solvent.blanks = 0,
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
  LTR <- round(LTR)
  
  solvent.blanks <- round(solvent.blanks)
  if(solvent.blanks < LTR) {
    solvent.blanks <- LTR
    warning(paste('  -  set solvent.blanks equal to', solvent.blanks, 'to ensure a solvent blank is injected after each LTR.', '\n'))
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
      "TQS_PH",       #8
      "TQS_T3",       #9
      "TQS_Hil",      #10
      "Orbi",        #11
      "other1",      #12
      "other2",     #13
      "other3"      #14
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
    kvar <- tclVar(0)
    lvar <- tclVar(0)
    mvar <- tclVar(0)
    nvar <- tclVar(0)
    
    
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
    k.entry <- tkcheckbutton(tt, variable=kvar)
    l.entry <- tkcheckbutton(tt, variable=lvar)
    m.entry <- tkcheckbutton(tt, variable=mvar)
    n.entry <- tkcheckbutton(tt, variable=nvar)
    
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
      tclvalue(kvar)<-0
      tclvalue(lvar)<-0
      tclvalue(mvar)<-0
      tclvalue(nvar)<-0
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
      k <- as.logical(as.numeric(tclvalue(kvar)))
      l <- as.logical(as.numeric(tclvalue(lvar)))
      m <- as.logical(as.numeric(tclvalue(mvar)))
      n <- as.logical(as.numeric(tclvalue(nvar)))
      
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
      en$k <- k
      en$l <- l
      en$m <- m
      en$n <- n
      
      
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
    tkgrid(tklabel(tt, text=platforms[11]), k.entry, pady = 10, padx =50)
    tkgrid(tklabel(tt, text=platforms[12]), l.entry, pady = 10, padx =50)
    tkgrid(tklabel(tt, text=platforms[13]), m.entry, pady = 10, padx =50)
    tkgrid(tklabel(tt, text=platforms[14]), n.entry, pady = 10, padx =50)
    
    
    tkgrid(submit.but, reset.but)
    
    tkwait.window(tt)
    
    ## ADD A LEVEL HERE FOR EACH PLATFORM ADDED ###########
    return(data.frame(platforms, 'select'=c(a,b,c,d,e,f,g,h,i,j,k,l,m,n), stringsAsFactors=FALSE))
  }
  platforms <- select.platforms()
  
  paramsets<-list(
    TOF_Hil_Pos=data.frame(c(chrominst="Waters UPLC: Sequant ZIC-pHILIC",
                             msinst="Waters Xevo G2-XS TOF",
                             column="Sequant ZIC-pHilic, 2 x 150 mm, 5 uM",
                             solvA="Water, 10 mM Ammomium Bicarbonate, pH 9.6",
                             solvB="Acetonitrile",
                             MSlevs=2,
                             CE1="6",
                             CE2="15-30",
                             mstype="TOF",
                             mzdifftof=0.05,
                             msmode="P",
                             ionization="ESI",
                             ESIvoltage="2200",
                             colgas="Ar",
                             msscanrange="50-1200",
                             conevolt="30")),
    
    TOF_Hil_Neg=data.frame(c(chrominst="Waters UPLC: Sequant ZIC-pHILIC",
                             msinst="Waters Xevo G2-XS TOF",
                             column="Sequant ZIC-pHilic, 2 x 150 mm, 5 uM",
                             solvA="Water, 10 mM Ammomium Bicarbonate, pH 9.6",
                             solvB="Acetonitrile",
                             MSlevs=2,
                             CE1="6",
                             CE2="15-30",
                             mstype="TOF",
                             mzdifftof=0.05,
                             msmode="N",
                             ionization="ESI",
                             ESIvoltage="2200",
                             colgas="Ar",
                             msscanrange="50-1200",
                             conevolt="30")),
    
    TOF_T3_Pos=data.frame(c(chrominst="Waters UPLC: C18 ACN Gradient",
                            msinst="Waters Xevo G2-XS QTOF",
                            column="Waters HSST3 C18, 1 x 100 mm, 1.8 uM",
                            solvA="Water, 0.1% formic acid",
                            solvB="ACN, 0.1% formic acid",
                            MSlevs=2,
                            CE1="6",
                            CE2="15-30",
                            mstype="TOF",
                            mzdifftof=0.05,
                            msmode="P",
                            ionization="ESI",
                            ESIvoltage="2200",
                            colgas="Ar",
                            msscanrange="50-1200",
                            conevolt="30")) ,
    
    TOF_T3_Neg=data.frame(c(chrominst="Waters UPLC: C18 ACN Gradient",
                            msinst="Waters Xevo G2-XS QTOF",
                            column="Waters HSST3 C18, 1 x 100 mm, 1.8 uM",
                            solvA="Water, 0.1% formic acid",
                            solvB="ACN, 0.1% formic acid",
                            MSlevs=2,
                            CE1="6",
                            CE2="15-30",
                            mstype="TOF",
                            mzdifftof=0.05,
                            msmode="N",
                            ionization="ESI",
                            ESIvoltage="2200",
                            colgas="Ar",
                            msscanrange="50-1200",
                            conevolt="30")) ,
    
    TOF_PH_Pos=data.frame(c(chrominst="Waters UPLC: ACN Gradient",
                            msinst="Waters Xevo G2-XS QTOF",
                            column="Waters CSH PhenylHexyl, 1 x 100 mm, 1.7 uM",
                            solvA="Water, 0.1% formic acid, 2mM AmOH",
                            solvB="ACN, 0.1% formic acid",
                            MSlevs=2,
                            CE1="6",
                            CE2="15-30",
                            mstype="TOF",
                            mzdifftof=0.05,
                            msmode="P",
                            ionization="ESI",
                            ESIvoltage="2200",
                            colgas="Ar",
                            msscanrange="50-2000",
                            conevolt="30")) ,
    
    TOF_PH_Neg=data.frame(c(chrominst="Waters UPLC: ACN Gradient",
                            msinst="Waters Xevo G2-XS QTOF",
                            column="Waters CSH PhenylHexyl, 1 x 100 mm, 1.7 uM",
                            solvA="Water, 0.1% formic acid, 2mM AmOH",
                            solvB="ACN, 0.1% formic acid",
                            MSlevs=2,
                            CE1="6",
                            CE2="15-30",
                            mstype="TOF",
                            mzdifftof=0.05,
                            msmode="N",
                            ionization="ESI",
                            ESIvoltage="2200",
                            colgas="Ar",
                            msscanrange="50-2000",
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
    
    Orbi=data.frame(c(chrominst="Waters nanoflow UPLC",
                      msinst="Thermo Orbitrap Velos Pro",
                      column="C18",
                      solvA="97% water, 1% ACN, 0.1% formic acid",
                      solvB="ACN, 0.1% formic acid",
                      MSlevs=1,
                      CE1="",
                      CE2="",
                      mstype="Orbitrap Velos Pro",
                      mzdifftof=0.05,
                      msmode="P",
                      ionization="NSI",
                      ESIvoltage="",
                      colgas="N",
                      msscanrange="100-2000",
                      conevolt="")),
    
    TQS_Hil=data.frame(c(chrominst="Waters UPLC: Sequant ZIC-pHilic",
                         msinst="Waters TQS",
                         column="Sequant ZIC-pHilic, 2 x 150 mm, 5 uM",
                         solvA="Water, 10mM AmBicarb pH 9.6",
                         solvB="ACN",
                         MSlevs=1,
                         CE1="6",
                         CE2="",
                         mstype="QQQ",
                         msmode="P",
                         ionization="ESI",
                         ESIvoltage="2200",
                         colgas="Ar",
                         msscanrange="50-1200",
                         conevolt="30")),
    
    TQS_PH=data.frame(c(chrominst="Waters UPLC: ACN Gradient",
                        msinst="Waters TQS",
                        column="Waters CSH PhenylHexyl, 1 x 100 mm, 1.7 uM",
                        solvA="Water, 0.1% formic acid, 2mM AmOH",
                        solvB="ACN, 0.1% formic acid",
                        MSlevs=1,
                        CE1="6",
                        CE2="",
                        mstype="QQQ",
                        msmode="P",
                        ionization="ESI",
                        ESIvoltage="2200",
                        colgas="Ar",
                        msscanrange="50-2000",
                        conevolt="30")) ,
    
    TQS_T3=data.frame(c(chrominst="Waters UPLC: ACN Gradient",
                        msinst="Waters TQS",
                        column="Waters T3 C18, 1 x 100 mm, 1.8 uM",
                        solvA="Water, 0.1% formic acid",
                        solvB="ACN, 0.1% formic acid",
                        MSlevs=1,
                        CE1="6",
                        CE2="",
                        mstype="QQQ",
                        msmode="P",
                        ionization="ESI",
                        ESIvoltage="2200",
                        colgas="Ar",
                        msscanrange="50-1200",
                        conevolt="30")) ,
    
    ICP=data.frame(c(chrominst="none",
                     msinst="PerkinElmer Elan DRC ICP-MS",
                     column="none",
                     solvA="none",
                     solvB="none",
                     MSlevs=1,
                     CE1="none",
                     CE2="none",
                     mstype="Quad",
                     mzdifftof="none",
                     msmode="P",
                     ionization="ICP",
                     ESIvoltage="",
                     colgas="Ar",
                     msscanrange="",
                     conevolt="")),
    
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
        length(unique.levels) < (0.9*length(smp[,i]))
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
    
  }
  
  # nb <- ceiling(nrow(smp)/prep.batch.size)
  
  n.prep.batches <- ceiling(nrow(smp)/(prep.batch.size-prep.blanks))
  prep_batch <- vector(length = 0, mode = "integer")
  for(i in 1:n.prep.batches) {prep_batch <- c(prep_batch, rep(i, (prep.batch.size - prep.blanks)))}
  prep_batch <- prep_batch[1:nrow(smp)]
  
  smp <- data.frame(
    # "pmf.sn" = 1:nrow(smp),  # removed, as prep_order serves as pmf.sn instead.
    "prep_order" = sample(1:prep.batch.size, nrow(smp), replace = TRUE),
    "prep_batch" = prep_batch,
    "run_order" = rep(NA, nrow(smp)),
    "run_batch" = rep(NA, nrow(smp)),
    smp, stringsAsFactors = FALSE
  )
  
  for(i in 1:n.prep.batches) {
    prep.blank.rows <- smp[0,,drop = TRUE]
    prep.blank.rows[1,] <- "prep.blank"
    prep.blank.rows$prep_batch[1] <- i
    prep.blank.rows$prep_order[1] <- sample(1:prep.batch.size, 1)
    for(j in 1:prep.blanks) {
      smp <- rbind(smp, prep.blank.rows)
    }
  }
  
  ## write prep instructional .csv  
  smp <- smp[order(smp$prep_batch),]
  smp <- smp[order(smp$prep_order),]
  write.csv(smp[,-grep("run_", names(smp))], file = "prep.sample.list.csv", row.names = FALSE)
  
  smp.backup <- smp
  
  projname<-d["Service id:",1]
  tmp<-unlist(strsplit(projname, "-"))
  short.projectname <- paste(tmp[length(tmp)-1], tmp[length(tmp)], sep = "-")
  rm(tmp)
  
  ## copy submissionform file to project directory
  file.copy(from=submissionform, to=paste0(projdir,  ".", basename(submissionform)))
  
  ## which platforms are we using? 
  doplatforms<-which(platforms[,2])
  
  ## for each platform
  for(x in doplatforms) {
    
    setwd(projdir)
    platform.dir <- paste0(projdir, platforms[x,1], "/")
    dir.create(platform.dir)
    setwd(platform.dir)
    
    smp <- smp.backup
    
    
    ## randomize for run order
    smp <- smp[sample(1:nrow(smp), replace = FALSE),]
    
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
    
    if(LTR > 0) {
      for(i in 1:LTR) {
        out <- rbind(out, ltr, solvent.blank)
      }
    }
    
    
    on.row <- nrow(out)
    qc.rows <- seq(on.row+1, run.batch.size, QC)
    if(max(qc.rows) < run.batch.size) qc.rows <- c(qc.rows, run.batch.size)
    too.close <- run.batch.size - qc.rows
    too.close <- which(too.close > 0 & too.close < 3)
    if(length(too.close) > 0) {
      qc.rows <- qc.rows[-too.close]
    }
    out[qc.rows,] <- qc
    
    smp.backup <- smp
    out.batch <- out
    out.template <- out.batch
    out <- out[0, , drop = FALSE]
    available.rows <- which(is.na(out.template[,1]))
    n.batches <- ceiling(nrow(smp)/(length(available.rows)-solvent.blanks))
    for(i in 1:n.batches) {
      out.batch <- out.template
      available.rows <- which(is.na(out.batch[,1]))
      sb.lines <- sample(available.rows, solvent.blanks)
      out.batch[sb.lines,] <- solvent.blank
      available.rows <- which(is.na(out.batch[,1]))
      remaining.samples <- min(nrow(smp), length(available.rows))
      out.batch[available.rows[1:remaining.samples],] <- smp[1:remaining.samples,]
      smp <- smp[-(1:remaining.samples),]
      if(i ==1) {from.order <- 1} else {from.order <- max(out$run_order)+1}
      out.batch$run_order <- from.order:(from.order+run.batch.size-1)
      out.batch$run_batch <- i
      out <- rbind(out, out.batch)
    }
    
    if(any(is.na(out[,1]))) {
      out <- out[-(which(is.na(out[,1]))[1]:nrow(out)),]
      if(out[nrow(out),1] != "qc") {
        out <- rbind(out, qc)
        out[nrow(out),"run_order"] <- 1+ as.numeric(out[nrow(out)-1,"run_order"])
        out[nrow(out),"run_batch"] <- out[nrow(out),"run_batch"]
      }
    }
    
    
    
    for(i in 1:n.prep.batches) {
      do <- which(smp$prep_batch == i)
      smp$prep_order[do] <- 1:length(do)
    }
    
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
    # dir.create(platforms[x,1])
    # setwd(platforms[x,1])
    save(ExpDes, file="ExpDes.Rdata")
    dir.create("R_scripts")
    
    
    # run.batches <- sort(unique(smp$run_batch))
    # # cat('run.batches:', run.batches , '\n')
    # 
    # if(LTR > 0)  {
    #   ltr.line <- smp[1,,drop = FALSE]
    #   ltr.line[1,1:ncol(ltr.line)] <- "LTR"
    #   sb.line <- smp[1,,drop = FALSE]
    #   sb.line[1,1:ncol(sb.line)] <- "solvent.blank"
    #   for(i in run.batches) {
    #     for(j in 1:LTR) {
    #       ltr.line$run_batch <- i
    #       sb.line$run_batch <- i
    #       ltr.line$run_order <- -2*j
    #       sb.line$run_order <- -2*j + 1
    #       smp <- rbind(smp, ltr.line, sb.line)
    #     }
    #   }
    # }
    # 
    # 
    # 
    # if(solvent.blanks > 0) {
    #   sb.line <- smp[1,,drop = FALSE]
    #   sb.line[1,1:ncol(sb.line)] <- "solvent.blank"
    #   
    #   for(i in run.batches) {
    #     sb.line$run_batch <- i
    #     sb.line$run_order <- sample(which(smp$run_batch == i), 1)
    #     smp <- rbind(smp, sb.line)
    #   }
    # }
    # 
    # 
    # ## update run order
    # smp <- smp[order(smp$run_batch),]
    # for(i in run.batches) {
    #   do <- which(smp$run_batch == i)
    #   smp[do,] <- smp[do[order(smp[do, "run_order"])],]
    #   smp[do,"run_order"] <- 1:length(do)
    # }
    # 
    ## replace QC, LTR, solvent blank in prep_order and prep_batch columns with NA
    out[which(out$prep_order == "ltr"), "prep_order"] <- NA
    out[which(out$prep_batch == "ltr"), "prep_batch"] <- NA
    
    out[which(out$prep_order == "qc"), "prep_order"] <- NA
    out[which(out$prep_batch == "qc"), "prep_batch"] <- NA
    
    out[which(out$prep_order == "solvent.blank"), "prep_order"] <- NA
    out[which(out$prep_batch == "solvent.blank"), "prep_batch"] <- NA
    
    ## create file and sample names for run .csv file
    out <- data.frame(
      "filename" = paste(platforms[x,1], short.projectname, formatC(1:nrow(out), width = nchar(nrow(out)), flag = 0), sep = "-"), 
      "sample.name" = sapply(1:nrow(out), FUN = function(x) {paste(out[x, 1:ncol(out)], collapse = "-")}),
      out
    )
    
    
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
    
    write.csv(out, file="sequence.csv", row.names=FALSE)
    setwd(projdir)
  }
  
  setwd(destination.dir)
  cat("FINISHED: created directories for the following platforms", platforms[doplatforms, 1], '\n')
}
