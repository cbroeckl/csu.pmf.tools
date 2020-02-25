#' pmfxcms
#'
#' A wrapper function for XCMS using common PMF platforms. 
#' @details This function uses a template file as input to guide XCMS parameter selection.  
#' @details Peak detection using centwave (LC-TOF) or matchedFilter (GC-Quad), density based peak grouping, loess rt correction, regrouping, and fillPeaks. 
#' @details Additionally performs unsupervised removal of ultrawide chromatgoraphic peaks and outlier detection and removal after peak detection step.
#' @details an XCMS object is returned.  
#' 
#' @param filetype.ms1 File extension of raw data for MS level data.  This extention will be scanned in all raw data files in working directory and will be added to filenames in sequence .csv file
#' @param filetype.ms2 File extension of raw data for MSe level data.  If not MSe data, set to NA.  This extention will be scanned in all raw data files in working directory and will be added to filenames in sequence .csv file
#' @param sequence .csv file containing filenames (no extension) in first column, and sample name and/or factor names in remaining columns.
#' @param delim symbol used to delimit factors in sequence file.  "-" by default.  
#' @param ms.res numerical value representing mass resolution of the MS system.  if NA, assumed to be quad data (low res). if numeric, used to set mass tolerances for xcms.
#' @param peak.width numerical vector of length two giving peak width range in seconds i.e. c(3, 30)
#' @param sn signal to noise threshold for peak detection
#' @param minfrac minimum proportion of samples in which a feature must be present (at one MS level). i.e. 0.4 is 4 out of 10.
#' @param bw.pre bandwidth for pre-rt correction grouping.  suggested value is 3, but if there is a good deal of retention drift over the sample set a larger value should be used. 
#' @param bw.post bandwidth for post-rt correction grouping.  suggested values is 1.5.  
#' @return returns an xcms object
#' @concept RAMClustR xcms
#' @author Corey Broeckling

#' @export 
pmfxcms.2<-function(
  filetype.ms1 = "_01.mzML",
  filetype.ms2 = "_02.mzML",
  sequence = 'seq.csv',
  delim = "-",
  ms.res = 30000,
  peak.width = c(3,30),
  sn = 5,
  minfrac = 0.4,
  bw.pre = 3,
  bw.post = 1.2
) {
  
  require(xcms)
  #  require(pcaMethods)
  
  ##setup sample information, experimental design, and check raw data files against seq file  
  
  dir.create("datasets")
  dataset<-list.files(getwd(), pattern=filetype.ms1, recursive = FALSE, ignore.case=TRUE)
  if(length(dataset) == 0) {
    stop("no .ms1 data files found")
  }
  if(!is.na(filetype.ms2)) {
    dataset.2 <- list.files(getwd(), pattern=filetype.ms2, recursive = FALSE, ignore.case=TRUE)
    if(length(dataset.2) == 0) {
      stop("no .ms1 data files found. If this is not MSe data, set filetype.ms2 = NA")
    }
    dataset <- c(dataset, dataset.2)
  }
  
  seq <- read.csv(sequence, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
  
  # remove columns with no data, and remove leading and trailing whitespace
  r <- sapply(1:ncol(seq), FUN = function(x) {
    tmp <- which(is.na(seq[,x]))
    length(tmp)
  })
  r <- which(r == dim(seq)[[1]])
  if(length(r) > 0) {
    seq <- seq[,-r]
  }
  
  if(ncol(seq) < 2) {
    stop("sequence must contain at least two columns", '\n')
  }
  
  for(i in 1:ncol(seq)) {
    seq[,i] <- trimws(seq[,i])
  }
  
  ## ensure all files are present
  files <- paste0(seq[,1], filetype.ms1)
  if(!is.na(filetype.ms2)) {
    files <- c(files, paste0(seq[,1], filetype.ms2))
  }
  
  missing <- which(!files %in% dataset)
  if(length(missing)>0) {
    cat("missing files include: ", '\n')
    for(i in 1:length(missing)) {
      cat("  ", files[missing[i]], '\n')
    }
    stop("please correct missing file issue by either changing the sequence file or the filetype option", '\n')
  }
  
  ## if factors are provided in multiple columns, concatenate to single sample names
  if(ncol(seq) == 2) {
    sample.names <- seq[,2]
    factor.names <- names(seq)[2]
  } else {
    sample.names <- sapply(1:nrow(seq), FUN = function(x) {
      paste0(seq[x, 2:ncol(seq)], collapse = delim)
    })
    factor.names <- paste0(names(seq)[2:ncol(seq)], collapse = delim)
  }
  
  
  
  ########  xcms section
  
  ## generate OnDiskMSnExp MSn experiment
  ## using pd directly causes error
  if(is.na(filetype.ms2)) {
    pheno.data <- data.frame(sample.names)
  } else {
    pheno.data <- data.frame(c(sample.names, sample.names))
  }
  raw_data <- readMSData(files = files, 
                         pdata = new("NAnnotatedDataFrame", data.frame(sample.names)),
                         mode = "onDisk"
  )
  
  
  ## consider removing lockmass here so we do not have to do so in Pwiz
  
  ## which type of MS/MS data do we have (if any)?
  ## some basic data structure summary stats
  
  mse <- FALSE
  dda <- FALSE
  sonar <- FALSE
  hdmse <- FALSE
  swath <- FALSE
  
  h <- header(raw_data)
  mslevs <- table(h$msLevel)
  ms3 <- mslevs['3']; if(is.na(ms3)) {ms3 <- 0}
  ms2 <- mslevs['2']; if(is.na(ms2)) {ms2 <- 0}
  ms1 <- mslevs['1']
  
  is.ms2 <- which(h$msLevel == 2)
  is.ms1 <- which(h$msLevel == 1)
  
  file.1 <- which(h[,"fileIdx"] == 1)
  ms1.time.range <- range(h[intersect(file.1, is.ms1),"retentionTime"])
  ms1.scan.interval <- (ms1.time.range[2] - ms1.time.range[1])/length(intersect(file.1, is.ms1))
  ms.fwhm <- round(550/ms.res, digits = 3)
  ppm <- round((1000000*ms.fwhm)/(550*2.355))
  
  if(!is.na(ms2)) {
    set.masses <- unique(round(h$precursorMZ[is.ms2]))
  } else {set.masses <- NA}
  
  ## mse rules
  if(
    round(ms2/ms1, digits = 2) == 1
  ) {
    mse <- TRUE
  }
  
  ## dda/dsda rules
  if(
    !mse & 
    ms2 > 0 & 
    length(set.masses) > 205
  ) {
    dda <- TRUE
  }
  
  
  
  ### XCMS feature detection
  ## trick XCMS time
  orig.msLevel <- raw_data@featureData@data$msLevel
  raw_data@featureData@data$msLevel <- rep(1, length(orig.msLevel))
  
  ## set up parallel parameters
  mcpar <- SnowParam(workers = 4, type = "SOCK")
  
  if(is.na(ms.res)) {
    stop("not set up for matched.filter just yet", '\n')
  } else {
    ## centwave peak detection
    ## start XCMS processing: 
    ## peak detection with centWave
    cwp <- CentWaveParam(peakwidth = peak.width, 
                         ppm = ppm,
                         snthresh = sn,
                         mzdiff = ms.fwhm,
                         fitgauss = TRUE,
                         verboseColumns = TRUE)
    xdata <- findChromPeaks(raw_data, param = cwp, msLevel = 1, BPPARAM = mcpar)
    # if(mse) {
    #   xdata <- findChromPeaks(xdata, param = cwp, msLevel = 2, add = TRUE)
    # }
    # head(chromPeaks(xdata)) 
  }
  # table(orig.msLevel)
  # raw_data@featureData@data$msLevel <- rep(1, length(orig.msLevel))
  
  ## consider additionally defining sample groups based on experimental design factor levels
  if(is.na(filetype.ms2)) {
    sample.groups <- rep(1, length(xdata@.processHistory[[1]]@fileIndex))
  } else {
    sample.groups <- c(
      rep(1, length(xdata@.processHistory[[1]]@fileIndex)/2),
      rep(2, length(xdata@.processHistory[[1]]@fileIndex)/2)
    )
  }


  
  
  
  ### XCMS feature grouping, pre-RT correction
  
  pdp <- PeakDensityParam(sampleGroups = sample.groups,
                          minFraction = minfrac, bw = bw.pre)
  xdata <- groupChromPeaks(xdata, param = pdp) 
  
  
  
  
  ### XCMS retention time adjustment, peak density
  
  pgp <- PeakGroupsParam(
    minFraction = max(0.5, minfrac)
  )
  xdata <- adjustRtime(xdata, param = pgp) 
  
  
  ### XCMS feature grouping, pre-RT correction
  
  pdp <- PeakDensityParam(sampleGroups = sample.groups,
                          minFraction = minfrac, bw = bw.post)
  xdata <- groupChromPeaks(xdata, param = pdp) 
  
  
  ### XCMS fillPeaks
  
  fpp <- FillChromPeaksParam(expandMz = 0, expandRt = 0, ppm = 0)
  xdata <- fillChromPeaks(xdata, param = fpp, BPPARAM = mcpar)
  
  raw_data@featureData@data$msLevel <- orig.msLevel
  
  save(xdata, file="datasets/xcmsObject.Rdata")
  return(xdata)
}




#' 
#' 
#' 
#' 
#' 
#' #' pmfxcms
#' #'
#' #' A wrapper function for XCMS using common PMF platforms. 
#' #' @details This function uses a template file as input to guide XCMS parameter selection.  
#' #' @details Peak detection using centwave (LC-TOF) or matchedFilter (GC-Quad), density based peak grouping, loess rt correction, regrouping, and fillPeaks. 
#' #' @details Additionally performs unsupervised removal of ultrawide chromatgoraphic peaks and outlier detection and removal after peak detection step.
#' #' @details an XCMS object is returned.  
#' #' 
#' #' @param ExpDes R object generated by RAMClustR::defineExperiment function.
#' #' @param filetype File extension of raw data.  This extention will be scanned in all raw data files in working directory and will be added to filenames in sequence .csv file
#' #' @param sequence .csv file containing filenames (no extension) in first column, and sample name and/or factor names in remaining columns.
#' #' @param delim symbol used to delimit factors in sequence file.  "-" by default.  
#' #' @param ms.res numerical value representing mass resolution of the MS system.  if NA, assumed to be quad data (low res). if numeric, used to set mass tolerances for xcms.
#' #' @param peak.width numerical vector of length two giving peak width range in seconds i.e. c(3, 30)
#' #' @param sn signal to noise threshold for peak detection
#' #' @param minfrac minimum proportion of samples in which a feature must be present (at one MS level). i.e. 0.4 is 4 out of 10.
#' #' @param bw.pre bandwidth for pre-rt correction grouping.  suggested value is 3, but if there is a good deal of retention drift over the sample set a larger value should be used. 
#' #' @param bw.post bandwidth for post-rt correction grouping.  suggested values is 1.5.  
#' #' @return returns an xcms object
#' #' @concept RAMClustR xcms
#' #' @author Corey Broeckling
#' 
#' #' @export 
#' pmfxcms.2<-function(
#'   ExpDes = NULL,
#'   filetype = "_012.mzML",
#'   sequence = 'seq.csv',
#'   delim = "-",
#'   ms.res = 30000,
#'   peak.width = c(3,30),
#'   sn = 10,
#'   minfrac = 0.4,
#'   bw.pre = 3,
#'   bw.post = 1.5
#' ) {
#'   
#'   require(xcms)
#'   #  require(pcaMethods)
#'   
#'   ##setup sample information, experimental design, and check raw data files against seq file  
#'   dir.create("datasets")
#'   dataset<-list.files(getwd(), pattern=filetype, recursive = FALSE, ignore.case=TRUE)
#'   
#'   seq <- read.csv(sequence, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
#'   
#'   # remove columns with no data, and remove leading and trailing whitespace
#'   r <- sapply(1:ncol(seq), FUN = function(x) {
#'     tmp <- which(is.na(seq[,x]))
#'     length(tmp)
#'   })
#'   r <- which(r == dim(seq)[[1]])
#'   if(length(r) > 0) {
#'     seq <- seq[,-r]
#'   }
#'   
#'   if(ncol(seq) < 2) {
#'     stop("sequence must contain at least two columns", '\n')
#'   }
#'   
#'   for(i in 1:ncol(seq)) {
#'     seq[,i] <- trimws(seq[,i])
#'   }
#'   
#'   ## ensure all files are present
#'   files <- paste0(seq[,1], filetype)
#'   missing <- which(!files %in% dataset)
#'   if(length(missing)>0) {
#'     cat("missing files include: ", '\n')
#'     for(i in 1:length(missing)) {
#'       cat("  ", files[missing[i]], '\n')
#'     }
#'     stop("please correct missing file issue by either changing the sequence file or the filetype option", '\n')
#'   }
#'   
#'   ## if factors are provided in multiple columns, concatenate to single sample names
#'   if(ncol(seq) == 2) {
#'     sample.names <- seq[,2]
#'     factor.names <- names(seq)[2]
#'   } else {
#'     sample.names <- sapply(1:nrow(seq), FUN = function(x) {
#'       paste0(seq[x, 2:ncol(seq)], collapse = delim)
#'     })
#'     factor.names <- paste0(names(seq)[2:ncol(seq)], collapse = delim)
#'   }
#'   
#'   
#'   
#'   ########  xcms section
#'   
#'   ## generate OnDiskMSnExp MSn experiment
#'   ## using pd directly causes error
#'   raw_data <- readMSData(files = files, 
#'                          pdata = new("NAnnotatedDataFrame", data.frame(sample.names)),
#'                          mode = "onDisk"
#'   )
#'   
#'   ## consider removing lockmass here so we do not have to do so in Pwiz
#'   
#'   ## which type of MS/MS data do we have (if any)?
#'   ## some basic data structure summary stats
#'   
#'   mse <- FALSE
#'   dda <- FALSE
#'   sonar <- FALSE
#'   hdmse <- FALSE
#'   swath <- FALSE
#'   
#'   h <- header(raw_data)
#'   mslevs <- table(h$msLevel)
#'   ms3 <- mslevs['3']; if(is.na(ms3)) {ms3 <- 0}
#'   ms2 <- mslevs['2']; if(is.na(ms2)) {ms2 <- 0}
#'   ms1 <- mslevs['1']
#'   
#'   is.ms2 <- which(h$msLevel == 2)
#'   is.ms1 <- which(h$msLevel == 1)
#'   
#'   file.1 <- which(h[,"fileIdx"] == 1)
#'   ms1.time.range <- range(h[intersect(file.1, is.ms1),"retentionTime"])
#'   ms1.scan.interval <- (ms1.time.range[2] - ms1.time.range[1])/length(intersect(file.1, is.ms1))
#'   ms.fwhm <- round(550/ms.res, digits = 3)
#'   ppm <- round((1000000*ms.fwhm)/(550*2.355))
#'   
#'   if(!is.na(ms2)) {
#'     set.masses <- unique(round(h$precursorMZ[is.ms2]))
#'   } else {set.masses <- NA}
#'   
#'   ## mse rules
#'   if(
#'     round(ms2/ms1, digits = 2) == 1
#'   ) {
#'     mse <- TRUE
#'   }
#'   
#'   ## dda/dsda rules
#'   if(
#'     !mse & 
#'     ms2 > 0 & 
#'     length(set.masses) > 205
#'   ) {
#'     dda <- TRUE
#'   }
#'   
#'   
#'   
#'   ### XCMS feature detection
#'   
#'   if(is.na(ms.res)) {
#'     stop("not set up for matched.filter just yet", '\n')
#'   } else {
#'     ## centwave peak detection
#'     ## start XCMS processing: 
#'     ## peak detection with centWave
#'     cwp <- CentWaveParam(peakwidth = peak.width, 
#'                          ppm = ppm,
#'                          snthresh = 5,
#'                          mzdiff = ms.fwhm,
#'                          fitgauss = TRUE,
#'                          verboseColumns = TRUE)
#'     xdata <- findChromPeaks(raw_data, param = cwp, msLevel = 1)
#'     if(mse) {
#'       xdata <- findChromPeaks(xdata, param = cwp, msLevel = 2, add = TRUE)
#'       }
#'         # head(chromPeaks(xdata)) 
#'   }
#'   
#'   ## consider additionally defining sample groups based on experimental design factor levels
#'   sample.groups <- rep(1, length(xdata@.processHistory[[1]]@fileIndex))
#'   # if(mse) {
#'   #   sample.groups <- c(sample.groups, rep(2, length(xdata@.processHistory[[2]]@fileIndex)))
#'   # }
#'   
#'   
#'   
#'   ### XCMS feature grouping, pre-RT correction
#'   
#'   pdp <- PeakDensityParam(sampleGroups = sample.groups,
#'                           minFraction = minfrac, bw = bw.pre)
#'   xdata <- groupChromPeaks(xdata, param = pdp) 
#'   
#'   
#'   
#'    
#'   ### XCMS retention time adjustment, peak density
#'   
#'   pgp <- PeakGroupsParam(
#'     minFraction = max(0.5, minfrac)
#'   )
#'   xdata <- adjustRtime(xdata, param = pgp) 
#'   
#'   
#'   ### XCMS feature grouping, pre-RT correction
#'   
#'   pdp <- PeakDensityParam(sampleGroups = sample.groups,
#'                           minFraction = minfrac, bw = bw.post)
#'   xdata <- groupChromPeaks(xdata, param = pdp) 
#'   
#'   
#'   ### XCMS fillPeaks
#'   
#'   fpp <- FillChromPeaksParam(expandMz = 0, expandRt = 0, ppm = 0)
#'   xdata <- fillChromPeaks(xdata, param = fpp)
#'   
#'   save(xdata, file="datasets/xcmsObject.Rdata")
#'   return(xdata)
#' }
