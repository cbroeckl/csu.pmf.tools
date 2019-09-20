#' zip selected directories and move to 'report' directory. 
#' 
#' @details convenience function for zipping raw data and processed data (two separate zip files)   
#' @param directories: character vector.  Which directories should be zipped. Only files in these directories will be chosen.
#' @param do.raw: logical - should a zip file of raw data files be generated?  
#' @params raw.extensions: character vector. which file extensions represent raw files.  Default: c(".cdf", ".mzML", ".mzXML", ".PRO", ".cmbx", ".raw").
#' @return NA 
#' @concept RAMClustR
#' @author Corey Broeckling

#' @export 

make.zip.files <- function(
  directories = c("datasets", "spectra", "QC", "stats"),
  do.raw = TRUE,
  raw.file.extensions = c(".cdf", ".mzML", ".mzXML", ".cmbx", ".PRO", ".raw")
) {
  
  require(zip)
  
  if(!dir.exists("report")) {
    dir.create("report")
  }
  cat('zipping data can take quite some time - please be patient', '\n')
  f <- vector(mode = "character", length = 0)
  for(i in 1:length(directories)) {
    f <- list.files(path = directories[i], recursive = TRUE, full.names = TRUE)
  }
  
  if(file.exists("seq.csv")) {
    f <- c(f, "seq.csv")
  }
  
  zipr("report/supplementalFiles.zip", files = f)
  rm(f)
  
  if(do.raw) {
    
    f <- vector(mode = "character", length = 0)
    for(i in 1:length(raw.file.extensions)) {
      f <- c(f, list.files(pattern = raw.extensions[i], recursive = TRUE, full.names = TRUE, ignore.case = TRUE, include.dirs = TRUE))
    }  # f2 <- f
    
    f <- f[order(nchar(f))]
    for(i in 1:(length(f)-1)) {
      r <- grep(f[i], f[(i):length(f)])
      r <- r[-1]
      if(length(r) > 0) {f <- f[-r]}
      rm(r)
      cat(i, ": length", length(f), '\n')
      if(i >= length(f)) {break}
    }
    
    zipr("report/rawData.zip", files = f)
  }
  cat('--finished', '\n')
}



    