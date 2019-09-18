#' convenient reporting of sample name and design
#' 
#' @details convenience function reporting of sample names.  writes text file with sample summary.   
#' @param ramclustObj ramclustR object summarize
#' @param which.data character; which dataset (SpecAbund or SpecAbundAve) to perform PCA on.  
#' @param delim  character; "-" by default - the delimiter for parsing sample names to factors  
#' @param cmpdlabel = "cmpd";  label the data with the annotation. can also be set to 'ann' for column names assigned as annotatins.
#' @return returns a ramclustR Object, with sample summary appended to 'sampleSummary'slot.
#' @concept RAMClustR
#' @author Corey Broeckling

#' @export 

reportSampleNames<-function(
  ramclustObj=RC,
  which.data="SpecAbund",
  delim="-",
  cmpdlabel="cmpd"
) {
  
  d <- getData(ramclustObj = ramclustObj, which.data = which.data, delim = delim, cmpdlabel = cmpdlabel)
  
  out <- paste("The sample set after all processing contains", nrow(ramclustObj[[which.data]]), "samples analyses.", '\n') 
  out <- paste0(out, "Factors are summarized as 'Factor Name' - 'factor level' : 'number of reps'", '\n')
  
  
  for(i in 1:ncol(d[[1]])) {
    tmp <- table(d[[1]][,i])
    if(max(tmp) < 2) {next}
    out <- paste(out, " ", names(d[[1]])[i], "  ",  
                 paste(names(tmp), tmp, collapse =  ';  ', sep = ":"),
                 '\n'
                 )
  }

  ramclustObj$sampleSummary <- out
  sink("sampleSummary.txt")
  cat(out)
  sink()
  cat(out)
  cat(paste0("File written to ", getwd(), "/sampleSummary.txt", '\n'))
  return(ramclustObj)
}	

