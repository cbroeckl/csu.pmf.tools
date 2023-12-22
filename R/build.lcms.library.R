#' lib.search
#'
#' spectral searching using Spectra::compare function(s) 
#' @param lib.directory allowable product ion mass error in spectral matching in parts per million.  generally, use ~ 15 for TOF, 500 for quad. 
#' @param ri.file.name allowable precursor ion mass error in spectral matching in parts per million.  generally, use ~ 15 for TOF, 500 for quad. Note though that quad isolation is NOT accurate mass, so you could consider using a larger window to account for potential chimeric spectra.
#' @param column retention time (or index) sigma for adjusting spectral match scores when retention similarity is used. use similarly to RAMClustR retention time similarity calculation. 
#' @param lib.name experimental spectra, as an R object of class 'Spectra' produced from the Spectra package
#' @param trim.to.precursor logical. should MS/MS mass range be trimmed to below the precursor mass? If TRUE, values above the precursor mass + 4 m/z are discarded
#' @param get.pubchem logical. should pubchem parameters be retrieved for inclusion in spectrum output? If TRUE, INCHIKEY, Smiles, formula, and a few other paramaters are added to the msp format. while NIST cannot use all these, R Spectra can import them.
#' @details function for building an LC-MS precursor (or not) MS/MS library in msp format for convervion to NIST format or searching within R. 
#' @return nothing - exports an msp formatted file named lib.name.msp
#' @concept ramclustR
#' @concept RAMClustR
#' @concept metabolomics
#' @concept mass spectrometry
#' @concept spectral matching
#' @author Corey Broeckling
#' @export
#' 
build.lcms.library <- function(
    lib.directory = 'R:/RSTOR-PMF/Projects/in-house libraries/LC-qTOF/CBD Terpenes/T3_20min',
    ri.file.name = NULL,
    column = "T3",
    lib.name = "cannabinoids-t3",
    trim.to.precursor = TRUE,
    intensity.filter = NULL) {
  
  if(!dir.exists(lib.directory)) {
    stop("directory ", lib.directory, 'does not exist.', '\n')
  }
  
  ## are there subdirectories? 
  subdirs <- list.dirs(lib.directory)
  rm.subdirs <- grep("__", subdirs)
  if(length(rm.subdirs) > 0) {
    subdirs <- subdirs[-rm.subdirs]
  }
  
  out <- vector(length = 0, mode = "character")
  
  for(i in 1:length(subdirs)) {
    ## find all xlsx files
    spec <- list.files(subdirs[i], recursive = FALSE, pattern = ".xlsx")
    ## remove any files which are not spectra
    rm <- grep("~", spec, fixed = TRUE)
    if(!is.null(ri.file.name)) rm <- c(rm, grep(ri.file.name, spec))
    rm <- c(rm, grep('template', spec))
    rm <- c(rm, grep("method", spec))
    rm <- c(rm, grep("__", spec))
    
    spec <- spec[-unique(rm)]
    
    
    ## calculate RT/RI relationship
    if(!is.null(ri.file.name)) {
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
    }
    
    ## read in each file to generate msp output text
    for(j in 1:length(spec)) {
      d <- suppressMessages(data.frame(readxl::read_xlsx(paste0(subdirs[i], "/", spec[j]), col_names = FALSE)))
      ms1 <- suppressMessages(data.frame(readxl::read_xlsx(paste0(subdirs[i], "/", spec[j]), col_names = FALSE, sheet = 2, skip = 1)))
      ms2 <- suppressMessages(data.frame(readxl::read_xlsx(paste0(subdirs[i], "/", spec[j]), col_names = FALSE, sheet = 3, skip = 2)))
      prec <- suppressMessages(data.frame(readxl::read_xlsx(paste0(subdirs[i], "/", spec[j]), col_names = FALSE, sheet = 3, skip = 0)))[1,2]
      
      ## check to make sure this looks like a spectrum file
      if(!grepl("Name", d[1,1])) {
        next
      }
      
      name <- d[1,2]
      metabolite.cid <- d[2,2]
      if(is.na(metabolite.cid)) {
        metabolite.cas <- d[4,2] # metabolite.cas <- '31932-13-5'
      }
      
      deriv.cid <- d[3,2]
      
      if(!is.na(metabolite.cid)) {
        metabolite <- csu.pmf.tools::rc.cmpd.get.pubchem(cmpd.cid = metabolite.cid, threads = 1)
      } else {
        metabolite <- csu.pmf.tools::rc.cmpd.get.pubchem(cmpd.names = metabolite.cas, threads = 1)
      }
      if(!is.na(deriv.cid)) {
        derivitive <- csu.pmf.tools::rc.cmpd.get.pubchem(cmpd.cid = deriv.cid, threads = 1)
      }
      
      CAS <- d[4,2]  
      rt  <- as.numeric(d[5,2]) * 60
      if(!is.null(ri.file.name)) {
        ri <- round(predict(alk.fit, data.frame('rt' = rt)), 0)} else {
          ri <- NA
        }
      concentration <- d[6,2]
      concentration.units <- "ug/mL"
      # spec.range <- (1+(grep("m/z", d[,1], fixed = TRUE)):nrow(d))
      max.mz <- if(is.na(prec)) {
        as.numeric(metabolite$properties$MonoisotopicMass) + 4
      } else {
        prec + 4
      }
      use.ms2 <- which(ms2[,1] <= max.mz)
      mz <- round(as.numeric(ms2[use.ms2, 1]), digits = 4)
      int <- round(as.numeric(ms2[use.ms2, 2]), digits = 0)
      
      if(is.numeric(intensity.filter)) {
        use <- which(int >= (max(int, na.rm = TRUE)*intensity.filter))
        mz <- mz[use]
        int <- int[use]
      }
      
      any.na <- which(is.na(mz) | is.na(int))
      if(length(any.na) > 0) {
        mz <- mz[-any.na]
        int <- int[-any.na]
      }
      
      pc <- sapply(1:length(metabolite$properties), FUN = function(k) {paste0(names(metabolite$properties)[k], ": ", metabolite$properties[1,k])})

      out <- c(
        out, 
        paste("NAME:", name), 
        paste("CASNO:", CAS),
        paste("Metabolite CID:", metabolite.cid),
        if(!is.na(deriv.cid)) {paste("Derivative.CID:", deriv.cid)},
        paste("RI:", ri),
        paste("RT:", rt),
        pc,
        if(!is.na(concentration)) {paste("Concentration:", concentration, concentration.units)}, 
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


build.lcms.library(
    lib.directory = 'R:/RSTOR-PMF/Projects/in-house libraries/LC-qTOF/CBD Terpenes/T3_20min',
    ri.file.name = NULL,
    column = "T3",
    lib.name = "cannabinoids-t3",
    trim.to.precursor = TRUE,
    intensity.filter = NULL) 
