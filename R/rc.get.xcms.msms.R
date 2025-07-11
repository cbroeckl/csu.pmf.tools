#' rc.get.xcms.msms
#'
#' utilizes downloaded and properly formatted local pubchem data created by 'get.pubchem.ftp' function
#' @details takes as input a ramclustR object and an XCMS object.  Extracts MS/MS data from xcms object to support annotation of RAMClustR feature groups.
#' @param xcmsObj xcms Object.  Should be same object that was used to generate RAMCLustR object and contain MS2 spectra.
#' @param ramClustObj RAMClustR object.  
#' @param expantRt logical.  rt expansion for matching precusor ion. default = 0.
#' @param expandMz logical.  mz expansion for matching precusor ion.  necessary since ion isolation for MS/MS is generally performed using a quad or ion trap, and isolation width is much larger than mass measurement error of TOF/Orbitrap. default = 0.3.
#' @param ppm numerical.  ppm similarity required to combine two signals.   
#' @param centroid logical. should CIDs be replaced with parent CIDs? 
#' @param in.memory logical. if TRUE, spectra will be stored in memory.  if false, stored on disk.  in.memory will be faster, assuming sufficient memory is available.
#' @param aggregate.spectra logical. should multiple spectra for the same feature be aggregated into a single spectrum?  Default = TRUE. 
#' @param weighted logical.  should weighting be used during spectrum aggregation if aggregate.spectra is TRUE?  Weights are signal intensity.
#' @param BPPARAM BiocParallel object, passed to "Spectra" and "XCMS" functions. default = SnowParam(2)
#' @return RAMClustR object. 
#' @author Corey Broeckling
#' 
#' @export 
#' 
rc.get.xcms.msms <- function(xcmsObj = x.data,
                             ramclustObj = RC,
                             expandRt = 0,
                             expandMz = 0.3,
                             ppm = 15,
                             centroid = TRUE,
                             in.memory = TRUE,
                             aggregate.spectra = TRUE,
                             weighted = TRUE,
                             BPPARAM = SnowParam(2)) {
  
  
  require(Spectra)
  require(xcms)

  user.params <- as.list(data.frame(
    'expandRt' = expandRt,
    'expandMz' = expandMz,
    'ppm' = ppm,
    'centroid' = centroid,
    'in.memory' = in.memory,
    'aggregate.spectra' = aggregate.spectra,
    'weighted' = weighted
  ))
  
  # get feature mapped MSMS using 
  # xcms featureSpectra function
  # return a 'Spectra' object
  
  if(is.null(ramclustObj$xcmsOrd)) {
    stop("this ramclustR object was not generated from xcms data.  this function only works with paired xcms/ramclustR objects", '\n')
  }
  
  cat(" -- getting features", '\n')
  tar.features <- ramclustObj$xcmsOrd
  # set.seed(123); tar.features <- tar.features[sample(1:length(tar.features), 300)]
  tar.feature.names <- row.names(featureDefinitions(xcmsObj))[tar.features]
  
  tar.files <- which(xcmsObj@featureData@data$msLevel == 2)
  tar.files <- unique(xcmsObj@featureData@data$fileIdx[tar.files])
  all.files <- unique(xcmsObj@featureData@data$fileIdx)
  keep <- rep(TRUE, length(tar.files))
  for(i in 1:length(tar.files)) {
    ms.n <- length(which(xcmsObj@featureData@data$fileIdx == tar.files[i]))
    msms <- which(xcmsObj@featureData@data$fileIdx == tar.files[i] & xcmsObj@featureData@data$msLevel == 2)
    msms.n <- length(msms)
    n.unique.precursors <- length(unique(xcmsObj@featureData@data$precursorMZ[msms]))
    if((n.unique.precursors/msms.n) < 0.08) keep[i] <- FALSE
  }
  tar.files <- tar.files[keep]
  
  # xr <- Spectra::Spectra(fileNames(xcmsObj))
  # h  <- head(xr)
  f <- xcms::chromPeaks(xcmsObj, bySample = FALSE, isFilledColumn = FALSE)
  
  
  g <- xcmsObj@msFeatureData@.xData$featureDefinitions  ## g is the unaligned features
  g <- g[tar.features, ]  ## order g to be same as RAMClustR object xcms order
  fg <- rep(NA, nrow(f))  ## fg maps correspondence features to unaligned features g, cbind to f
  for(i in 1:nrow(g)) {
    ind <- unlist(g$peakidx[i])
    fg[ind] <- rownames(g)[i]
  }
  rm <- which(is.na(fg))
  f <- data.frame(f, 'featurename' = fg)
  f <- f[-rm,]
  
  keep <- f[,"sample"] %in% tar.files
  f <- f[keep,]
  gc()
  samps <- sort(unique(f[,"sample"]))
  
  f.split <- as.list(rep(NA, length(samps)))
  names(f.split) <- samps
  for(i in 1:length(samps)) {
    f.split[[i]] <- f[which(f$sample == samps[i]),]
  }
  
  rm('fg', 'rm', 'f')
  gc()
  
  filenames <- fileNames(xcmsObj)
  filenames <- filenames[all.files[tar.files]]
  
  # f.sub <- filenames[i:(i+1)]
  # f.split.sub <- f.split[i:(i+1)]
  ## retrieve MS/MS spectra that map to features for all files
  # all.msms <- foreach::foreach (i = 1:length(samps)) %dopar% {
  # all.msms <- foreach::foreach(i = c(478:489), .packages=c('Spectra', 'xcms')) %do% 
  # i <- 487
  # f.sub <- filenames[i:(i+1)]
  # f.split.sub <- f.split[i:(i+1)]
  
  get.msms <- function(
    filename = NULL,
    f.data = NULL,
    sample.id = NULL,
    params = NULL
  ) { 
    
    params <- data.frame(t(params))
    cat("dim params?", dim(params),  '\n')
    expandRt <- params[,'expandRt'][1]
    expandMz <- params[,'expandMz'][1]
    ppm <- params[,'ppm'][1]
    centroid <- params[,"centroid"][1]
    in.memory <- params[,"in.memory"][1]
    
    gc()
    if(in.memory) { ##get Spectrum object for one file
      cat(filename)
      xr <- Spectra::Spectra(filename, backend = MsBackendMemory())
    } else {
      xr <- Spectra::Spectra(filename)
    }
    gc()
    
    cat(filename, '\n')
    # xr <- filterMsLevel(xr, 2)
    # sh <- xr  ## get the full header for the file.  sh represents raw data ms2 spectra
    use <- nrow(f.data)  ## which chromatographic peaks were detected from this file
    if(length(use) == 0) {spd <- NA} else {
      cat('inside first else', '\n')
      
      sf <- data.frame(f.data)  ## sub data frame of detected chromPeaks mapped to feature name
      
      ## for each MSMS, map to feature name
      mzdif <- abs(outer(precursorMz(xr), sf$mz, "-"))
      start.mz <- 0
      if(!is.null(expandMz)) start.mz <- start.mz + expandMz
      mzlim <- matrix(start.mz, nrow = nrow(mzdif), ncol = ncol(mzdif))
      if(!is.null(ppm)) {
        mzadd <- outer(rep(0, nrow(mzdif)), sf$mz * ppm/1000000, "+")
        mzlim <- mzlim + mzadd
        rm(mzadd)
      }
      prec.match <- (mzdif <= mzlim )
      rm(mzdif); rm(mzlim); gc()
      rt.min <-  outer(rtime(xr), sf$rtmin, "-") > 0 
      rt.max <-  outer(rtime(xr), sf$rtmax, "-") < 0 
      mtch <- prec.match * rt.min * rt.max
      mtch.ind <- which(mtch == 1, arr.ind = TRUE) 
      
      if(nrow(mtch.ind) == 0) {spd <- NA} else {
        cat('inside second else', '\n')
        # rm('rt.min', 'rt.max', 'mtch', 'prec.match'); gc()
        ## assign updated precursor mass based on feature accurate mass
        ## and estimate precursor intensity, if it is absent
        raw.prec <- Spectra::precursorMz(xr)[mtch.ind[,1]]
        new.prec <- sf$mz[mtch.ind[,2]]
        # raw.prec[mtch.ind[,2]] <- new.prec
        # new.prec <- raw.prec
        rm(raw.prec)
        # requested this functionality to be added.  using accurate mass precursors means we do not need to expand ppm. see below
        # Spectra::precursorMz(xr) <- raw.prec
        
        pi <- precursorIntensity(xr)
        if(max(pi, na.rm = TRUE) < 1) {
          pi <- Spectra::estimatePrecursorIntensity(xr, method = "interpolation", ppm = 20*params$ppm[1])
          pi <- pi[mtch.ind[,1]]
          # requested this functionality to be added.  
          # Spectra::precursorIntensity(xr) <- pi
        }
        
        ## add centroiding and any filtering processing here: 
        if(centroid) {
          xr <- Spectra::pickPeaks(xr, method = "SuperSmoother")
          # xr <- filterIntensity()
        }
        sample = rep(sample.id, nrow(mtch.ind))
        msLevel = as.integer(rep(2, nrow(mtch.ind))) 
        rtime = rtime(xr)[mtch.ind[,1]]
        precursorMz = new.prec 
        precursorIntensity = pi
        collisionEnergy = collisionEnergy(xr[mtch.ind[,1]])
        featureIndex = mtch.ind[,2]
        featureName = sf[mtch.ind[,2], 'featurename']
        
        cat(length(sample), '\n')
        cat(length(msLevel), '\n')
        cat(length(rtime), '\n')
        cat(length(precursorMz), '\n')
        cat(length(precursorIntensity), '\n')
        cat(length(collisionEnergy), '\n')
        cat(length(featureIndex), '\n')
        cat(length(featureName), '\n')
        
        spd <- S4Vectors::DataFrame(
          sample = rep(sample.id, nrow(mtch.ind)), 
          msLevel = as.integer(rep(2, nrow(mtch.ind))), 
          rtime = rtime(xr)[mtch.ind[,1]],
          precursorMz = new.prec, 
          precursorIntensity = pi,
          collisionEnergy = collisionEnergy(xr[mtch.ind[,1]]),
          featureIndex = mtch.ind[,2],
          featureName = sf[mtch.ind[,2], 'featurename']
        )
        spd$mz <- round(mz(xr[mtch.ind[,1]]), 5)
        spd$intensity <- intensity(xr[mtch.ind[,1]])
        spd <- Spectra(spd, backend = MsBackendMemory())
        gc()
        # for(j in 1:nrow(mtch.ind)) {
        #   if(class(msms[mtch.ind[j,2]]) == "Spectra") {
        #     msms[[mtch.ind[j,2]]] <- Spectra::concatenateSpectra(msms[mtch.ind[j,2]], spd[mtch.ind[j,1]])
        #   } else {
        #     msms[[mtch.ind[j,2]]] <- spd[mtch.ind[j,1]]
        #   }
        # }
        rm('mtch.ind', 'xr', 'sf')
        gc()
      }
    }
    spd
  } 
  
  all.msms <- mapply(FUN = get.msms, filename = as.list(filenames), f.data = f.split, sample.id = as.list(samps), params = user.params)
  
  # i <- 1
  ## single.  can just loop this to get it to work. 
  # test <- get.msms(filename = as.list(filenames)[[i]], f.data = f.split[[i]], sample.id = as.list(samps)[[i]], params = user.params); test
  ## multiple
  # test <- mapply(FUN = get.msms, filename = filenames[i:(i+1)], f.data = f.split[i:(i+1)]); test
  
  # test <- bpmapply(1:2, FUN = get.msms,
  #                  filename = filenames[i:(i+1)],
  #                  f.data = f.split[i:(i+1)],
  #                  x.expandRt = expandRt,
  #                  x.expandMz = expandMz,
  #                  x.ppm = ppm,
  #                  x.centroid = centroid,
  #                  x.in.memory = in.memory
  #                  ); test
  
  
  # stopCluster(cl)
  # bpstop()
  
  ## remove any elements of list which have no MSMS spectra
  keep <- sapply(1:length(all.msms), FUN = function(x) {
    if(class(all.msms[[x]]) == "Spectra") {x} else {NA}
  }
  )
  keep <- keep[which(!is.na(keep))]
  if(length(keep) == 0) {
    stop("no mapped MS/MS data found", '\n')
  }
  msms <- c(all.msms[[keep[1]]])
  if(length(keep) > 1) {
    for(i in 2:length(keep)) {
      msms <- c(msms, all.msms[[keep[i]]])
    }
  }
  rm(all.msms); gc()
  
  h <- msms@backend@spectraData
  
  out <- as.list(rep(NA, length(tar.features)))
  names(out) <- tar.feature.names
  # fmsms <- combineSpectra(msms, f = msms$featureIndex)
  # fmsms.index <- fmsms$featureIndex
  # fmsms.name <- fmsms$featureName
  
  for(i in 1:length(out)) {
    use <- which(h$featureName == names(out)[i])
    if(length(use)> 0) {
      tmp <- msms[use]
      if(aggregate.spectra) {
        tmp <- combineSpectra(tmp, tolerance = 2*ppm*mean(tmp$precursorMz)/1000000)
        out[[i]] <- tmp
      }
    }
  }
  
  # range(sapply(msms, 'length'))
  
  # test <- rep(NA, 12)
  # foreach::foreach(i = 1:12) %dopar% {
  #   test[i] <- i
  # }
  ## test above demonstrates that %do% can update an object while %dopar% cannot
  
  # 
  # cat(" -- getting feature-mapped MS2 spectra", '\n')
  # x.msms <- featureSpectra(
  #   xcmsObj, msLevel = 2, expandMz = expandMz, expandRt = expandRt, 
  #   ppm = ppm, return.type = "Spectra", skipFilled = TRUE, features = tar.features)
  # 
  # x.msms <- featureSpectra(
  #   xcmsObj, msLevel = 2, expandMz = expandMz, expandRt = expandRt, 
  #   ppm = ppm, return.type = "Spectra", skipFilled = TRUE)
  # 
  # if(in.memory) {
  #   cat(" -- converting to in-memory Spectra object", '\n')
  #   cat(" ---- Object size on disk: ", format(object.size(x.msms), "Gb"), '\n')
  #   Spectra::setBackend(x.msms, Spectra::MsBackendMemory())
  #   cat(" ---- Object size in memory: ", format(object.size(x.msms), "Gb"), '\n')
  # }
  # 
  # x.msms.names <- x.msms@backend@spectraData$feature_id
  # 
  # 
  # 
  # cat(" -- aggregating MS2 spectra by feature ID", '\n')
  # foreach::foreach(i in 1:length(tar.feature.names)) {
  #   tar.xcms.feat <- tar.features[i]
  #   tar.rc.feat <- i
  #   tar.mz <- ramclustObj$fmz[tar.rc.feat]
  #   tar.clus <- ramclustObj$featclus[i]
  #   if(tar.clus == 0) next
  #   tar.spectrum <- ramclustObj$findmain[[tar.clus]]$details[[1]]
  #   tar.add <- tar.spectrum[which(tar.spectrum$mz == tar.mz), "adduct"]
  #   tar.z <- tar.spectrum[which(tar.spectrum$mz == tar.mz), "charge"]
  #   tar.iso <- tar.spectrum[which(tar.spectrum$mz == tar.mz), "iso"]
  #   tar.label <- tar.spectrum[which(tar.spectrum$mz == tar.mz), "label"]
  #   
  #   do <- which(x.msms.names == tar.feature.names[i])
  #   if(length(do) == 0) next
  #   
  #   ## aggregate multiple spectra into one to return
  #   sub <- x.msms[do]
  #   sub <- suppressWarnings(Spectra::setBackend(sub, Spectra::MsBackendMemory()))
  #   sub.int <- sapply(intensity(sub), "sum")
  #   if(max(sub.int) == 0) next
  #   sub <- sub[which(sub.int > 0)]
  #   sub.int <- sub.int[which(sub.int > 0)]
  #   sub <- sub[order(sub.int, decreasing = TRUE)]
  #   
  #   if(centroid) {
  #     if(any(!sub$centroided)) {
  #       sub <- Spectra::pickPeaks(sub, method = "SuperSmoother")
  #     }
  #   }
  #   
  #   out <- Spectra::combineSpectra(sub, ppm = ppm, weighted = TRUE)
  #   if(!is.na(tar.z)) out$precursorCharge <- tar.z
  #   out$precursorMz <- tar.mz
  #   out$adduct <- tar.add
  #   out$z <- tar.z
  #   out$iso <- tar.iso
  #   out$label <- tar.label
  #   
  #   if(out$polarity == 1) {
  #     out$polarity <- "P"
  #   } else {
  #     out$polarity <- "N"
  #   }
  #   
  #   ## move spectrum to list, to be appended to ramclustObj
  #   msms[[i]] <- out
  #   rm(out)
  #   # cat("added", i, '\n')
  # }
  # 
  # cat(" -- finishing", '\n')
  # ramclustObj$feat.ms2.spectrum <- msms
  # nmapped <- length(which(!is.na(msms)))
  # nclus <- length(unique(ramclustObj$featclus[which(!is.na(msms))]))
  # par.out <- ""
  # for(i in 1:length(params)) {par.out <- paste0(par.out, names(params)[i], ":", params[i], ", ")}
  # history <- paste0(
  #   "The XCMS 'featureSpectra' function was called from within the RAMClustR rc.get.xcms.msms function to",
  #   " retreive all targeted MS/MS scans which map to XCMS detected features. ",
  #   length(ramclustObj$fmz), " XCMS features were retained. ",  "Of those, ", length(x.msms), " spectra mapped to those features, ",
  #   "which mapped to a total of ", nmapped, " features from ", nclus, " clusters.",  "Parameter settings: ", par.out, "."
  # )
  # history <- gsub(", .", ".", history, fixed = TRUE)
  # ramclustObj$history <- c(ramclustObj$history, history)
  # cat(" -- finished", '\n')
  # return(ramclustObj)
  
  return(out)
}