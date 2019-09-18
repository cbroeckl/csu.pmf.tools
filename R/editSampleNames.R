#' convenient editing of ramclustObj sample names
#' 
#' @details convenience function editing of sample names within R to correct errors or add additional factors   
#' @param ramclustObj ramclustR object to edit
#' @param delim  character; "-" by default - the delimiter for parsing sample names to factors  
#' @param cmpdlabel = "cmpd";  label the data with the annotation. can also be set to 'ann' for column names assigned as annotatins.
#' @return returns a list of length 3: $design is the experimental sample factors after parsing by the delim, $data is the dataset, $full.data is merged $des and $data data.frames.
#' @concept RAMClustR
#' @author Corey Broeckling

#' @export 

editSampleNames<-function(
  ramclustObj=RC,
  delim="-",
  cmpdlabel="cmpd"
) {
  
  if(is.null(ramclustObj$SpecAbundAve)) {
    which.data <- "SpecAbund"
    alt <- NULL
  } else {
    which.data <- "SpecAbundAve"
    alt <- "SpecAbund"
  }
  
  d <- getData(ramclustObj = ramclustObj, which.data = which.data, delim = delim, cmpdlabel = cmpdlabel)
  old.names <- row.names(d[[1]])
  
  d[[1]] <- edit(d[[1]])
  new.names <- sapply(1:nrow(d[[1]]), FUN = function(x) {paste0(d[[1]][x,], collapse = delim)})
  
  tmp.names <- old.names
  for(i in 1:length(old.names)) {
    tmp.names <- gsub(old.names[i], new.names[i], tmp.names)
  }
  row.names(ramclustObj[[which.data]]) <- tmp.names

  if(!is.null(alt)) {
    tmp.names <- row.names(ramclustObj[["SpecAbund"]])
    for(i in 1:length(old.names)) {
      tmp.names <- gsub(old.names[i], new.names[i], tmp.names)
    }
    row.names(ramclustObj[["SpecAbund"]]) <- tmp.names
  }
    
  factnames <- names(d[[1]])
  
  st <- which(row.names(ramclustObj$ExpDes$design) == "fact1name")
  ramclustObj$ExpDes$design[st:(st+length(factnames)-1),1] <- factnames
  
  return(ramclustObj)
}	

