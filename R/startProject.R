#' startProject
#'
#' @details take info from iLab and iLab sample submission form and create sample prep and data acquisition templates and directories on the R:  
#' @param prep.batch.size integer - what is the sample preparation batch size?  default = 48. 
#' @param run.batch.size integer - what is the analytical batch size? default = 96.
#' @param QC  integer - every 'QC' injections will be a QC sample.  Default = 6  
#' @param stack should the injection sample list be set up for stacked injections (TOF platforms only, currenly)
#' @param prep.blanks integer - how many extraction blanks per prep batch?
#' @param solvent.blanks  integer - how many solvent blanks per run batch?.
#' @param destination.dir valid path to directory target output directory, by default: "R:/RSTOR-PMF/Projects/"
#' @return returns nothing, files written to directory selected by user.
#' @author Corey Broeckling
#' @examples 
#' startProject<-function (prep.batch.size = 48,run.batch.size=96, QC=6, randomize=TRUE, stack = FALSE, prep.blanks = 3, solvent.blanks = 3)
#' @export 


startProject<-function (
  prep.batch.size = 48,
  run.batch.size=96,
  QC=6,
  randomize=TRUE,
  stack = FALSE,
  prep.blanks = 3,
  destination.dir = "C:/Users/",
  sub.project.name = NULL  ## add optional subproject.name
) {
  
  ## NEED TO ADD BATCH ORDER RANDOMIZATION
  ## CONSIDER THE BENEFITS OF ADDING RUN ORDER TO SAMPLE NAME VS REFERENCING BOTH SEQUENCE.CSV AND SEQ.CSV
  ## PLATFORM SPECIFIC COPYING OF R SCRIPTS TO R_SCRIPTS FOLDER
  
  require(tcltk)
  require(xlsxjars)
  require(xlsx)
  
  # destination.dir = "R:/RSTOR-PMF/Projects/"
  
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
    tkgrid(tklabel(tt, text=platforms[1]), a.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[2]), b.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[3]), c.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[4]), d.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[5]), e.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[6]), f.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[7]), g.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[8]), h.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[9]), i.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[10]), j.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[11]), k.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[12]), l.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[13]), m.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt, text=platforms[14]), n.entry, pady = 10, padx =10)
    
    
    tkgrid(submit.but, reset.but)
    
    tkwait.window(tt)
    
    ## ADD A LEVEL HERE FOR EACH PLATFORM ADDED ###########
    return(data.frame(platforms, 'select'=c(a,b,c,d,e,f,g,h,i,j,k,l,m,n), stringsAsFactors=FALSE))
  }
  platforms <- select.platforms()
  
  paramsets<-list(
    TOF_Hil_Pos=data.frame(c(chrominst="Waters UPLC: Sequant ZIC-pHILIC",
                             msinst="Waters Xevo G2 TOF",
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
                             msinst="Waters Xevo G2 TOF",
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
                            msinst="Waters Xevo G2 QTOF",
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
                            msinst="Waters Xevo G2 QTOF",
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
                            msinst="Waters Xevo G2 QTOF",
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
                            msinst="Waters Xevo G2 QTOF",
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
    
    Orbi=data.frame(c(chrominst="Waters UPLC: C8 MeOH Gradient",
                      msinst="Waters Xevo G2 TOF",
                      column="Waters C8, 1 x 100 mm, 1.7 uM",
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
    smp<-read.xlsx(submissionform, sheetName = "SampleList", 
                   startRow=4, header=TRUE, check.names = FALSE, 
                   stringsAsFactors=FALSE)
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
      cat("  levels:", unique(smp[,i]), '\n', '\n')
    }
    
    cat ("TYPE [enter] IF CORRECT", '\n',
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

  
  ## assign random prep order, and empty batch column
  smp <- data.frame(
    # "pmf.sn" = 1:nrow(smp),  # removed, as prep_order serves as pmf.sn instead.
    "prep_order" = sample(1:nrow(smp), replace = FALSE),
    "prep_batch" = rep(NA, nrow(smp)),
    "run_order" = rep(NA, nrow(smp)),
    "run_batch" = rep(NA, nrow(smp)),
    smp, stringsAsFactors = FALSE
  )
  
  # create factor labels and names based on smp input
  flabs <- c(paste0("fact", 1:ncol(smp), "name"))
  fnames <- names(smp)
  names(fnames) <- flabs
  
  ## add fnames to ExpVars
  ExpVars <- c(ExpVars, fnames)
  

  
  ## adjust prep.batch.size and run.batch.size for blanks
  ## and QC samples.  Assign within-batch QC positions.
  prep.batch.size <- prep.batch.size - prep.blanks
  qc.per.batch <- floor(run.batch.size / (QC+1)) + 1
  1:(qc.per.batch-2) * run.batch.size/(qc.per.batch-1)
  qc.positions <- c(
    1,   
    round(1:(qc.per.batch-2) * run.batch.size/(qc.per.batch-1)), 
    run.batch.size)
  run.batch.size.full <- run.batch.size
  run.batch.size <- run.batch.size - solvent.blanks - qc.per.batch
  
  ## reorder 
  smp <- smp[order(smp$prep_order),]
  
  ## assign prep batch and insert prep blank lines
  smp[,"prep_batch"] <- 1 + floor((smp$prep_order-1)/prep.batch.size)
  pbline <- smp[1,]
  pbline[1,1:ncol(pbline)] <- "prep.blank"
  nb <- max(smp[,"prep_batch"])
  for(i in nb:1) {
    b <- which(smp[,"prep_batch"] == i)
    if((length(b)/prep.blanks) > 3) {
      ins.rows <- round(quantile(b, seq(0, 1, 1/(prep.blanks))))
      ins.rows <- sapply(1:(length(ins.rows)-1), 
                         FUN = function(x) {round(mean(ins.rows[x:(x+1)]))})
      for(j in length(ins.rows):1) {
        pbline$prep_batch <- i
        smp <- rbind(
          smp[1:(ins.rows[j]-1),],
          pbline,
          smp[ins.rows[j]:nrow(smp),]
        )
      }
    } else {
      ins.rows <- round(mean(b))
      for(j in length(ins.rows):1) {
        pbline$prep_batch <- i
        smp <- rbind(
          smp[1:(ins.rows[j]-1),],
          pbline,
          smp[ins.rows[j]:nrow(smp),]
        )
      }
    }
    
  }
  
  # ## replace prep.blank with vial numbers for prep
  # tmp <- which(smp$prep_order == "prep.blank")
  # smp$prep_order[tmp] <- 0
  # smp$prep_order <- as.integer(as.numeric(smp$prep_order))
  # smp$prep_order[tmp] <- (max(smp$prep_order)+1):(max(smp$prep_order)+length(tmp))
  # 
  ## update prep order
  ##########################
  smp$prep_order <- 1:nrow(smp)
  
  ## write prep instructional .csv
  write.csv(smp[,-grep("run_", names(smp))], file = "prep.sample.list.csv", row.names = FALSE)
  
  qcline <- smp[1,]
  qcline[1,1:ncol(qcline)] <- "QC"
  
  
  ## randomize run order for samples, then

  run.order <- sample(1:nrow(smp), replace = FALSE)
  smp <- smp[run.order,]
  
  ## expand qc.positions 
  tmp <- qc.positions
  while(max(qc.positions) < (3*nrow(smp))) {
    qc.positions <- c(qc.positions, (max(qc.positions) + tmp))
  }
  
  ## assign rows to be QC samples. 
  
  current.row <- 1
  while(current.row < nrow(smp)) {
    if(current.row %in% qc.positions) {
      
      if(current.row == 1) {
        head.lines <- 0
      } else {
        head.lines <- 1:(current.row - 1)
      }
      if(current.row == nrow(smp)) {
        tail.lines <- 0
      } else {
        tail.lines <- current.row:nrow(smp)
      }
      
      smp <- rbind(smp[head.lines,], qcline, smp[tail.lines,])
      cat(current.row, " ")
    }
    current.row <- current.row + 1
  }
  
  if(smp[nrow(smp),1] != "QC") {
    smp <- rbind(smp, qcline)
  }
  
  ## assign run batch 
  smp[,"run_batch"] <- 1 + floor(((1:nrow(smp))-1)/run.batch.size.full)
  
  ## update run order: 
  smp$run_order <- 1:nrow(smp)
  
  projname<-d["Service id:",1]
  tmp<-unlist(strsplit(projname, "-"))
  short.projectname <- paste(tmp[length(tmp)-1], tmp[length(tmp)], sep = "-")
  
  ## copy submissionform file to project directory
  file.copy(from=submissionform, to=paste0(projdir,  projname, ".", tools::file_ext(submissionform)))
  
  ## which platforms are we using? 
  doplatforms<-which(platforms[,2])
  
  ## for each platform
  for(i in doplatforms) {
    setwd(projdir)
    
    ## edit instrument parameters, if need be
    Exp<-ExpVars
    Exp["platform"]<-as.character(platforms[i,"platforms"])
    Experiment<-data.frame(Exp,stringsAsFactors=FALSE)
    cat("  Please edit instrument parameters as approriate: ", Exp["platform"], '\n')
    if(!is.null(paramsets[[as.character(Exp["platform"])]])) {
      instrument<-edit(paramsets[[as.character(Exp["platform"])]])
    } else {
      cat ("PLATFORM", as.character(platforms[i,"platforms"]) , ": TYPE 'g' FOR NEW GC-MS PLATFORM AND 'l' FOR NEW LC-MS PLATFORM", '\n')
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
    dir.create(platforms[i,1])
    setwd(platforms[i,1])
    save(ExpDes, file="ExpDes.Rdata")
    dir.create("R_scripts")
    
    tmp <- smp
    
    ## randomize run order for all non-QC samples
    reord <- sample(which(tmp[,"prep_order"] != "QC"), replace = FALSE)
    tmp[which(tmp[,"prep_order"] != "QC"),] <- tmp[reord,]
    
    ## create file and sample names for run .csv file
    tmp <- data.frame(
      "filename" = paste(platforms[i,1], short.projectname, formatC(1:nrow(tmp), width = nchar(nrow(tmp)), flag = 0), sep = "-"), 
      "sample.name" = sapply(1:nrow(tmp), FUN = function(x) {paste(tmp[x, 1:ncol(tmp)], collapse = "-")}),
      tmp
    )
    
    
    
    tmp[,"run_order"] <- 1:nrow(tmp)
    
    if(stack & grepl("TOF_", Exp["platform"])) {
      
      tmpstack<-tmp
      tmpstack$filename<-paste(tmpstack$filename, "_stack", sep="")
      tmpinj<-tmp
      tmp<-tmp[0,]
      for(k in 1:nrow(tmpstack)) {
        tmp<-rbind(tmp, tmpstack[k,], tmpinj[k,])
      }
    }
    
    write.csv(tmp, file="sequence.csv", row.names=FALSE)
    setwd(projdir)
  }
  setwd(projdir)
  cat("FINISHED: created directories for the following platforms", platforms[doplatforms, 1], '\n')
}

# startProject