## script to turn manually curated GC-MS spectra into an msp formatted library

build.gcms.library <- function(
    lib.directory = NULL,
    ri.file.name = "alkanes.xlsx",
    column = "DB-WAX-MS",
    lib.name = "terpenes-spme") {
  
  if(!dir.exists(lib.directory)) {
    stop("directory ", lib.directory, 'does not exist.', '\n')
  }
  
  ## are there subdirectories? 
  subdirs <- list.dirs(lib.directory)
  
  out <- vector(length = 0, mode = "character")
  
  for(i in 1:length(subdirs)) {
    ## find all xlsx files
    spec <- list.files(subdirs[i], recursive = FALSE, pattern = ".xlsx")
    ## remove any files which are not spectra
    spec <- spec[-grep("~", spec)]
    spec <- spec[-grep(ri.file.name, spec)]
    spec <- spec[-grep('template', spec)]
    spec <- spec[-grep("method", spec)]
    
    ## calculate RT/RI relationship
    alk <- suppressMessages(readxl::read_xlsx(paste0(subdirs, "/", ri.file.name)))
    ri <- 100*alk$C
    rt <- alk$rt*60
    any.na <- which(is.na(ri) | is.na(rt))
    if(length(any.na)>0) {
      ri <- ri[-any.na]
      rt <- rt[-any.na]
    }
    alk.cal <- data.frame(
      'ri' = ri,
      'rt' = rt
    )
    
    alk.fit <- lm(ri ~ poly(rt, 2), data = alk.cal)
    
    ## read in each file to generate msp output text
    for(j in 1:length(spec)) {
      d <- suppressMessages(data.frame(readxl::read_xlsx(paste0(subdirs[i], "/", spec[j]), col_names = FALSE)))
      ## check to make sure this looks like a spectrum file
      if(!grepl("Name", d[1,1])) {
        next
      }
      
      name <- d[1,2]
      metabolite.cid <- d[2,2]
      deriv.cid <- d[3,2]
      CAS <- d[4,2]
      rt  <- as.numeric(d[5,2]) * 60
      ri <- round(predict(alk.fit, data.frame('rt' = rt)), 0)
      concentration <- d[6,2]
      concentration.units <- "ug/mL"
      spec.range <- (1+(grep("m/z", d[,1], fixed = TRUE)):nrow(d))
      mz <- round(as.numeric(d[spec.range, 1]), digits = 1)
      int <- round(as.numeric(d[spec.range, 2]), digits = 0)
      any.na <- which(is.na(mz) | is.na(int))
      if(length(any.na) > 0) {
        mz <- mz[-any.na]
        int <- int[-any.na]
      }
      too.low <- which(int < 0.01*max(int))
      mz <- mz[-too.low]
      int <- int[-too.low]

      out <- c(
        out, 
        paste("NAME:", name), 
        paste("CASNO:", CAS),
        paste("Metabolite CID:", metabolite.cid),
        if(!is.na(deriv.cid)) {paste("Derivative.CID:", deriv.cid)},
        paste("RI:", ri),
        paste("RT:", rt),
        paste("Concentration:", concentration, concentration.units), 
        paste("Num peaks:", length(mz)) 
      )
      for(k in 1:length(mz)) {
        out <- c(
          out,
          paste(mz[k], int[k])
        )
      }
      out <- c(
        out, " "
      )
    }
    
  }
  
  
  sink(paste0(lib.directory, '/', lib.name, '.msp'))
  cat(out, sep = '\n')
  sink()
  
}

# build.gcms.library(
#   lib.directory = "R:/RSTOR-PMF/Projects/in-house libraries/GC-EI-volatiles/DBWAX-volatile-mtd01-20231106",
#   ri.file.name = "alkanes.xlsx",
#   column = "DB-WAX-MS",
#   lib.name = "terpenes-spme"
# )
