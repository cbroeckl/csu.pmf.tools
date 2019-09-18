#' getData
#'
#' retreive and parse sample names, retrieve metabolite data.  returns as list of two data frames
#' @details convenience function for parsing sample names and returning a dataset.   
#' @param ramclustObj ramclustR object to retrieve data from
#' @param which.data character; which dataset (SpecAbund or SpecAbundAve) to perform PCA on.  
#' @param delim  character; "-" by default - the delimiter for parsing sample names to factors  
#' @param cmpdlabel = "cmpd";  label the data with the annotation. can also be set to 'ann' for column names assigned as annotatins.
#' @return returns a list of length 3: $design is the experimental sample factors after parsing by the delim, $data is the dataset, $full.data is merged $des and $data data.frames.
#' @concept RAMClustR
#' @author Corey Broeckling

#' @export 

getData<-function(ramclustObj=RC,
                  which.data="SpecAbund",
                  delim="-",
                  cmpdlabel="cmpd"
) {
  
  dat <- as.data.frame(ramclustObj[[which.data]])
  if(length(ramclustObj[[cmpdlabel]]) == dim(ramclustObj[[which.data]])[2]) {
    names(dat) <- ramclustObj[[cmpdlabel]]
  } else {
    stop(paste("ramclustObj slot", cmpdlabel, "has length", length(ramclustObj[[cmpdlabel]]), "while the", which.data, "dataset has", dim(ramclustObj[[which.data]])[2], "columns", '\n'))
  }
  des <- data.frame(t(data.frame(strsplit(row.names(dat), "-"))), stringsAsFactors = FALSE)
  row.names(des) <- row.names(dat)

  factors<-sapply(1:nrow(dat), FUN=function(x) length(strsplit(as.character(dimnames(dat)[[1]]), delim)[[x]]))
  maxfact<-max(factors)
  factnames<-c(ramclustObj$ExpDes$design[which(row.names(ramclustObj$ExpDes$design)=="fact1name"): 
                                           (which(row.names(ramclustObj$ExpDes$design)=="fact1name")+(maxfact-1)), 1])
  
  names(des)<-factnames
  dat<-list("design" = des, "data" = dat, "full.data" = cbind(des, dat))
  return(dat)
}	

