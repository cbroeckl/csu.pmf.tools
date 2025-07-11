#' assign.xcms.dda.peaks
#'
#' 
#'
#' @param ramclustObj ramclustR object to annotate. 
#' @param cmpd integer: vector defining compound numbers to annotated.  if NULL (default), all compounds
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
assign.xcms.dda.peaks <- function(
    ramclustObj = NULL,
    xcms.feature.msmsObj = NULL,
    cmpd = NULL
) {
  
  if(is.null(ramclustObj)) {
    stop("must supply ramclustObj as input.  i.e. ramclustObj = RC", '\n')
  }
  
  msms.feat.names <- xcms.feature.msmsObj$feature_id
  msms.intensity <- xcms.feature.msmsObj$totIonCurrent
  
  # add.priority <- unlist(strsplit(ramclustObj$params$findmain["ads"], ", "))
  ## change this to be ANY DDA spectrum for a compound
  dda.spectra <- as.list(rep(NA, length(ramclustObj$clrt)))
  for(i in 1:max(ramclustObj$featclus)) {
    dda.spectra[[i]] <- as.list(rep(NA, 0))
    for(j in 1:length(ramclustObj$ms1.isotopes[[i]])) {

      tar <- ramclustObj$ms1.isotopes[[i]][[j]]
      tar.feat <- tar$xcms.name
      do <- which(msms.feat.names == tar.feat)
      if(length(do) == 0) next
      if(length(do) > 1) {
        do <- do[which.max(msms.intensity[do])]
      }
      s <- data.frame(
        mz = round(unlist(mz(xcms.feature.msmsObj[do])), 5), 
        int = round(unlist(intensity(xcms.feature.msmsObj[do])), 2)
      )
      tar$spectrum.type <- "dda"
      tar$spectrum <- s
      dda.spectra[[i]][[length(dda.spectra[[i]])+1]] <- tar
    }
  }
  
  ramclustObj$dda.spectra <- dda.spectra
  return(ramclustObj)
}
