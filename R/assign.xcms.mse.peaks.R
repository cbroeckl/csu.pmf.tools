#' assign.xcms.mse.peaks
#'
#' 
#'
#' @param ramclustObj ramclustR object to annotate. 
#' @param min.cor numeric: minimum dataset-wide correlational r-value to be considered for assignment. 
#' @details This function is developed to assign fragment ions to ramclustR compounds after performing peak finding in the MSe (DIA MS/MS) data from centWave XCMS output.
#' @return    $M:  The inferred molecular weight of the compound giving rise to the each spectrum
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
    min.cor = 0.7,
    rt.sd = 2,
    min.rt.range = 0.5
) {
  
  if(is.null(ramclustObj)) {
    stop("must supply ramclustObj as input.  i.e. ramclustObj = RC", '\n')
  }
  
  mse.spectra <- as.list(rep(NA, length(ramclustObj$clrt)))
  for(i in 1:length(mse.spectra)) {
    mse.spectra[[i]] <- as.list(rep(NA, 0))
  }
  mse.intensities <- colSums(ramclustObj$MSMSpeak.data, na.rm = TRUE)/nrow(ramclustObj$MSMSpeak.data)
  assigned.fragments <- rep(0, length(mse.spectra))
  for(i in 1:max(ramclustObj$featclus)) {
    tar <- ramclustObj$ms1.isotopes[[i]][[1]]
    ms1 <- ramclustObj$MSdata[,i]
    ms2.use <- which(abs(ramclustObj$frt.2 - ramclustObj$clrt[i]) <= max(min.rt.range, rt.sd*ramclustObj$clrtsd[i]))
    ms2.use <- ms2.use[which(ramclustObj$fmz.2[ms2.use] <= (1.1+ramclustObj$ms1.isotopes[[i]][[1]]$precursor.m))]
    ms2.cor <- cor(ms1, ramclustObj$MSMSpeak.data[,ms2.use])
    ms2.cor.use <- ms2.use[ms2.cor >= min.cor]
    
    if(length(ms2.cor.use) > 0) {
      s <- data.frame(
        mz = ramclustObj$fmz.2[ms2.cor.use], 
        int = mse.intensities[ms2.cor.use]
      )
      row.names(s) <- ramclustObj$feature_names.2[ms2.cor.use]
      tar$spectrum.type <- "mse"
      tar$spectrum <- s
      mse.spectra[[i]][[1]] <- tar
      }
  }
  
  ramclustObj$mse.spectra <- mse.spectra
  return(ramclustObj)
}