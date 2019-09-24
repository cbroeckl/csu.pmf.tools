#' convenient editing of ramclustObj sample names
#' 
#' @details convenience function editing of sample names within R to correct errors or add additional factors. outputs a pdf plot and table in the 'spectra' directory which can be used to refine annotations.   
#' @param ramclustObj ramclustR object to edit
#' @return returns an updated ramclustR object, with truncated GLM names, in addition to inchikey when available. 
#' @concept RAMClustR
#' @author Corey Broeckling

#' @export 

degolm<-function(
  ramclustObj=RC
  ) { 
  
  load(paste0(find.package("csu.pmf.tools"), "/glm.inchi.Rdata"))
  
  anns<-ramclustObj$ann
  glm<-grep("VAR5", anns)
  glmnames<-strsplit(anns[glm], "_")
  glmlen<-unlist(lapply(glmnames, FUN="length"))
  fix<-which(glmlen>8)
  if(length(fix)>0) {for(i in 1:length(fix)) {
    glmnames[[fix[i]]][8]<-paste(glmnames[[fix[i]]][8:glmlen[fix[i]]], collapse="-")
    glmnames[[fix[i]]]<-glmnames[[fix[i]]][1:8]
  }
  }
  glmlen<-unlist(lapply(glmnames, FUN="length"))
  fix<-which(glmlen>8)
  if(length(fix)>0) {stop(paste("remove extra '_' from compound(s)", paste(glm[fix], collapse= " ")) )}
  nr<-glmlen[1]
  glmdat<-matrix(unlist(glmnames), nrow=8)  
  glmdat<-rbind(glm, glmdat)
  ramclustObj$ann[glm]<-glmdat[9,]
  glmdat[5,]<-as.numeric(gsub(",", ".", glmdat[5,]))
  ramclustObj$glmdat<-glmdat
  pdf(file="spectra/GOLMRIvsRT.pdf")
  plot(ramclustObj$clrt[glm], as.numeric(glmdat[5,]), pch="", xlab='retention time', ylab='retention index')
  grid()
  text(ramclustObj$clrt[glm], as.numeric(glmdat[5,]), labels=ramclustObj$cmpd[glm], cex=0.5)
  dev.off()
  write.csv(file="spectra/GOMRIvsRT.csv", data.frame(ramclustObj$cmpd[glm], ramclustObj$clrt[glm], as.numeric(glmdat[5,])))
  
  if(is.null(ramclustObj$inchikey)) {
    ramclustObj$inchikey<-rep(NA, length(ramclustObj$ann))
  }
  for(i in 1:length(ramclustObj$inchikey)) {
    do<-grep(ramclustObj$ann[i], as.character(glm.inchi$name), fixed = TRUE)
    if(length(do))
      if(length(do) > 0) {
        if(length(do) > 1) {
          do <- do[1]
        }
        ramclustObj$inchikey[i]<- as.character(glm.inchi$inchikey)[do]
      }
  }
  
  return(ramclustObj)
}

