#' retrieve inchikeys GOLM msp 
#' 
#' @details convenience function for editing RC object.  matches compound name to those in msp file, returns compound (metabolite) inchikey when available.   
#' @param ramclustObj ramclustR object to edit
#' @return returns an updated ramclustR object with inchikeys when available. 
#' @concept RAMClustR
#' @author Corey Broeckling

#' @export 

golm.inchikey<-function(
  ramclustObj = RC,
  msp = "R:/RSTOR-PMF/Projects/Broeckling_Corey/GMD_20111121_VAR5_ALK_MSP.txt"
) {
  spec<-readLines(msp)
  names<-c(grep("Name: ", spec), length(spec))
  inchi <- rep(NA, length(names)) 
  for(i in 1:(length(names)-1)) {
    do<-spec[names[i]:(names[i+1])-1]
    keep<-grep("Synon: METB InChIKey:", do)
    if(length(keep)>0) {
      if(length(keep) > 1) {
        keep <- keep[1]
      } 
      inchi[i] <- do[keep]
    }
    rm(do); rm(keep)
  }
  names<-spec[names]
  names<-gsub("Name: ", "", names)
  inchi<-gsub("Synon: METB InChIKey: ", "", inchi)
  ramclustObj$inchikey<-rep(NA, length(ramclustObj$ann))
  for(i in 1:length(ramclustObj$inchikey)) {
    do<-grep(ramclustObj$ann[i], names, fixed = TRUE)
    if(length(do))
      if(length(do) > 0) {
        if(length(do) > 1) {
          do <- do[1]
        }
        ramclustObj$inchikey[i]<-inchi[do]
      }
  }
  return(ramclustObj)
}