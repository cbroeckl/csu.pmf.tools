#' export.ms.formatted.spectra
#'
#' @param ramclustObj ramclustR object to annotate. 
#' @param output.dir character: path to write MS formatted spectra to.  a new directory called 'spectra' is created (if it does not exist), and within it a directory called 'ms'.  all spectra files are written to the 'spectra/ms' directory within the specified directory, or the working directory, if not specified. 
#' @details This function exports spectra in .ms format, suitable for importing into Sirius. Requires
#' @return  nothing - files written to disk
#' @concept RAMClustR
#' @concept metabolomics
#' @concept mass spectrometry
#' @concept xcms
#' @author Corey Broeckling
#' @export 
#' 
#' 
export.ms.formatted.spectra <- function(
    ramclustObj = NULL,
    output.dir = NULL
) {
  
  if(is.null(ramclustObj$mse.spectra) & is.null(ramclustObj$dda.spectra)){
    stop("neither mse nor dda spectra have been assigned. Please use one of 'assign.xcms.dda.peaks' or 'assign.xcms.mse.peaks' first.", '\n')
  }
  
  
  if(is.null(output.dir)) {
    output.dir <- getwd()
  }
  if(!dir.exists(output.dir)) {
    dir.create(output.dir)
  }
  if (!dir.exists(paste0(output.dir, "/spectra"))) {
    dir.create(paste0(output.dir, "/spectra"))
  }
  if (!dir.exists(paste0(output.dir, "/spectra/ms"))) {
    dir.create(paste0(output.dir, "/spectra/ms"))
  }
  
  if(!is.null(ramclustObj$dda.spectra)) {
    spec <- ramclustObj$dda.spectra
  }
  if(!is.null(ramclustObj$mse.spectra)) {
    if(any(ls() == 'spec')) {
      spec <- c(spec, ramclustObj$mse.spectra)
    } else {
      spec <- ramclustObj$mse.spectra
    }
    
  }
  
  
  
  for (i in 1:length(spec)) {
    if(!is.null(spec[[i]])) {
      if(length(spec[[i]]) == 0) next
      for(j in 1:length(spec[[i]])) {
        sp.name <- paste(
          spec[[i]][[j]]$ramclustr.cmpd, 
          spec[[i]][[j]]$xcms.name, 
          spec[[i]][[j]]$adduct.type,
          spec[[i]][[j]]$spectrum.type, sep = ".")
        out <- paste(">compound ", sp.name,
                     "\n", 
                     ">parentmass ", round(spec[[i]][[j]]$precursor.mz, 5), "\n", 
                     ">ionization ",   spec[[i]][[j]]$adduct.type, "\n", 
                     ">charge ", spec[[i]][[j]]$charge, '\n',
                     ">retention ", round(spec[[i]][[j]]$retention.time, 2), '\n',
                     "\n", sep = "")
        
        if(nrow(spec[[i]][[j]]$isotopes) > 0) {
          out <- paste(out, ">ms1", "\n", sep = "")
          for (k in 1:nrow(spec[[i]][[j]]$isotopes)) {
            out <- paste(out, round(spec[[i]][[j]]$isotopes$feature.mz[k], 5), " ", 
                         round(spec[[i]][[j]]$isotopes$feature.med.int[k], 2), "\n",
                         sep = "")
          }
        }
        
        out <- paste(out, '\n', 
                     ">collision ", spec[[i]][[j]]$collision.energy, '\n',
                     sep = "")
        for (k in 1:nrow(spec[[i]][[j]]$spectrum)) {
          out <- paste(out, round(spec[[i]][[j]]$spectrum$mz[k], 5), " ", round(spec[[i]][[j]]$spectrum$int[k], 2), "\n",
                       sep = "")
        }
        
        write(out, file = paste0(output.dir, "/spectra/ms/", sp.name,
                                 ".ms"))
        rm(out)
      }
    }
  }
}

