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
  raw.extensions = c(".cdf", ".mzML", ".mzXML", ".PRO", ".cmbx")
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
  
  zipr("report/supplementalFiles.zip", files = f)
  rm(f)
  
  if(do.raw) {
    
    f <- vector(mode = "character", length = 0)
    for(i in 1:length(raw.extensions)) {
      f <- c(f, list.files(pattern = raw.extensions[i], recursive = TRUE, full.names = TRUE, ignore.case = TRUE))
    }
    
    zipr("report/rawData.zip", files = f)
  }
  cat('--finished', '\n')
}



    