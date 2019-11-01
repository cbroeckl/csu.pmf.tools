#' returns a dataframe of all directories and their size in megabytes and last accessed (in days).
#' 
#' @details convenience function determining which directories might be archived.   
#' @param dir directory to summarize.  default is getwd().  can also be set - i.e. "C:/Users/username/Desktop/temp"
#' @return returns an dataframe with three columns, including directory path, directory size in megabytes and the minimum number of days since any file within that directory was accessed.  'accessed' includes any new files being written or any old file being modified and saved.  
#' @author Corey Broeckling

#' @export 

pmf.directory.report <- function(dir = getwd()) {
  d <- list.dirs(dir, recursive = FALSE, full.names = TRUE)
  out <- data.frame('directory' = d, 
                    "size_Mb" = rep(NA, length(d)), 
                    "last.accessed_days" = rep(NA, length(d)))
    for(i in 1:length(d)) {
      f <- list.files(d[i], all.files = TRUE, full.names = TRUE, recursive = TRUE)
      if(length(f) == 0) {next}
      s <- sapply(1:length(f), FUN = function(x) {
        file.size(f[x])[1]
      })
      a <- sapply(1:length(f), FUN = function(x) {
        as.integer(round(difftime(Sys.time(), file.info(f[x])$atime, units = "days")))
      })
      out[i, 2] <- round(sum(s)/1000000)
      out[i, 3] <- min(a)
      
    }
  return(out)
}
