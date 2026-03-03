#' assign.xcms.mse.peaks
#'
#' this function associated fragments ions that have been detected and aligned by XCMS in the MSe channel to MS1 signals clusted by ramclustR.  the product ions are associated with the full MS1 spectrum, not inferred to have arisen from any given adduct type or ion.  Note that this workflow differs from the originally published ramclustR algorithm, in that the MSe data have not been fully aligned with the MS1 level features tables.  The logic is similar - similar retention time and correlational relatioships guide clustering - but in this new approch a single MSe peak could in theory be clustered with more than one MS1 level compound.    
#'
#' @param ramclustObj ramclustR object to annotate. 
#' @param min.cor numeric: minimum dataset-wide correlational r-value to be considered for assignment. 
#' @details This function is developed to assign fragment ions to ramclustR compounds after performing peak finding in the MSe (DIA MS/MS) data from centWave XCMS output.
#' @return  data is returned in a new slot called mse.spectra, as a 'Spectra::spectra' object. spectra names are assigned as the compound name (i.e. 'C1242'). precursorMz slot is left empty. 
#' @concept RAMClustR
#' @concept metabolomics
#' @concept mass spectrometry
#' @concept xcms
#' @author Corey Broeckling
#' @export 
#' 
#' 

assign.xcms.mse.peaks <- function(
    ramclustObj = NULL,
    min.cor = 0.6,
    rt.sd.factor = 4,
    min.rt.range = 0.75,
    round.digits = 6
) {
  
  if(is.null(ramclustObj)) {
    stop("must supply ramclustObj as input.  i.e. ramclustObj = RC", '\n')
  }
  
  spd <- DataFrame(
    msLevel = c(2L, 2L, 2L),
    polarity = c(1L, 1L, 1L),
    id = c("HMDB0000001", "HMDB0000001", "HMDB0001847"),
    name = c("1-Methylhistidine", "1-Methylhistidine", "Caffeine"),
    cmpd = c("1-Methylhistidine", "1-Methylhistidine", "Caffeine"),
    rtime = 123)
  
  ## Assign m/z and intensity values.
  spd$mz <- list(
    c(109.2, 124.2, 124.5, 170.16, 170.52),
    c(83.1, 96.12, 97.14, 109.14, 124.08, 125.1, 170.16),
    c(56.0494, 69.0447, 83.0603, 109.0395, 110.0712,
      111.0551, 123.0429, 138.0662, 195.0876))
  spd$intensity <- list(
    c(3.407, 47.494, 3.094, 100.0, 13.240),
    c(6.685, 4.381, 3.022, 16.708, 100.0, 4.565, 40.643),
    c(0.459, 2.585, 2.446, 0.508, 8.968, 0.524, 0.974, 100.0, 40.994))
  
  
  if(!is.null(ramclustObj$clri)) {
    spd$ri = 123
  }
  
  spd <- spd[0,]
  
  mzs <- ramclustObj$fmz.2
  rts <- ramclustObj$frt.2
  median.intensity <- apply(ramclustObj$MSMSpeak.data, 2, 'median', na.rm = TRUE)
  feat.names <- names(median.intensity)
  if(grep(".", dimnames(ramclustObj$MSdata)[[1]][1])) {
    new.dimnames <- sapply(1:length(dimnames(ramclustObj$MSdata)[[1]]), FUN = function(x) {
      unlist(strsplit(dimnames(ramclustObj$MSdata)[[1]][x], ".", fixed = TRUE))[1]
    }
    )
    dimnames(ramclustObj$MSdata)[[1]] <- new.dimnames
  }
  
  if(grep(".", dimnames(ramclustObj$MSMSpeak.data)[[1]][1])) {
    new.dimnames <- sapply(1:length(dimnames(ramclustObj$MSMSpeak.data)[[1]]), FUN = function(x) {
      unlist(strsplit(dimnames(ramclustObj$MSMSpeak.data)[[1]][x], ".", fixed = TRUE))[1]
    }
    )
    dimnames(ramclustObj$MSMSpeak.data)[[1]] <- new.dimnames
  }
  
  msms.data <- ramclustObj$MSMSpeak.data[dimnames(ramclustObj$MSdata)[[1]],]
  
  for (cl in 1:length(ramclustObj$cmpd)) {
    rt.range <- ramclustObj$clrt[cl] + (c(-1,1)*ramclustObj$clrtsd[cl]*rt.sd.factor)
    tar.ms2 <- which(rts > rt.range[1] & rts < rt.range[2])
    tar.ms2.r <- cor(ramclustObj$MSdata[, cl], msms.data[, tar.ms2])
    keep <- which(tar.ms2.r > min.cor)
    if(length(keep) == 0) next
    pol <- if(grepl("p", ramclustObj$params$findmain['mode'])) {1L} else {0L}
    spd.tmp <- DataFrame(
      msLevel = rep(2L, 1),
      polarity = rep(pol, 1),
      id = rep(ramclustObj$cmpd[cl], 1),
      name = rep(ramclustObj$cmpd[cl], 1),
      cmpd = rep(ramclustObj$cmpd[cl], 1),
      rtime = rep(ramclustObj$clrt[cl], 1))
    
    ## Assign m/z and intensity values.
    spd.tmp$mz <- list(
      as.vector(mzs[tar.ms2[keep]])
    )
    spd.tmp$intensity <- list(
      as.vector(median.intensity[tar.ms2[keep]])
    )
    
    if(!is.null(ramclustObj$clri)) {
      spd.tmp$ri = ramclustObj$clri[cl]
    }
    
    spd <- rbind(spd, spd.tmp)
    
  }
  
  ramclustObj$mse.spectra <- Spectra::Spectra(spd)
  return(ramclustObj)
}
