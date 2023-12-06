#' @export
#' 
rc.run.msfinder <- function(
    msfinder.dir = "",
    input.dir  =  "",
    param.file = ""
) {
  
  if(substring(msfinder.dir, nchar(msfinder.dir)) != "/") {
    msfinder.dir <- paste0(msfinder.dir, "/")
  }
  
  if(substring(input.dir, nchar(input.dir)) != "/") {
    input.dir <- paste0(input.dir, "/")
  }
  
  cat(" ----- pre_mssearch", '\n')
  #  MsfinderConsoleApp.exe mssearch -i .\Data\ -o .\ -m .\MsfinderConsoleApp-Param.txt
  system(
    paste0(
      msfinder.dir, "MsfinderConsoleApp",
      " mssearch ",
      " -i ", input.dir,
      " -o ", input.dir,
      " -m ", param.file
    )
  )
  
  cat(" ----- post_mssearch", '\n')
  
  ## rename directories containing spectral search results to keep them from being overwritten
  spec.results <- list.files(
    paste0(input.dir),
    pattern = "MsSearch result",
    recursive = FALSE)
  versions <- as.numeric(gsub(".txt", "", gsub("MsSearch result-", "", spec.results)))
  spec.results <- spec.results[which.max(versions)]
  res <- read.delim(paste0(input.dir, spec.results))
  from.name <- unique(res$File.name)
  to.name <- paste0(from.name, ".mssearch")
  
  for(i in 1:length(from.name)) {
    dir.create(paste0(input.dir,  to.name[i]))
    files.to.move <- list.files(paste0(input.dir, from.name[i]), recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
    file.copy(files.to.move, to = paste0(input.dir,  to.name[i]), recursive = TRUE)
  }
  cat(" ----- post_modify.directory.names", '\n')
  
  system(
    paste0(
      msfinder.dir, "MsfinderConsoleApp",
      " predict ",
      " -i ", input.dir,
      " -o ", input.dir,
      " -m ", param.file
    )
  )
  
  cat (" ----- post_predict", '\n')
}


rc.run.msfinder.gc <- function(
    msfinder.dir = "",
    input.dir  =  "",
    param.file = ""
) {
  
  if(substring(msfinder.dir, nchar(msfinder.dir)) != "/") {
    msfinder.dir <- paste0(msfinder.dir, "/")
  }
  
  if(substring(input.dir, nchar(input.dir)) != "/") {
    input.dir <- paste0(input.dir, "/")
  }
  
  cat(" ----- pre_mssearch", '\n')
  #  MsfinderConsoleApp.exe mssearch -i .\Data\ -o .\ -m .\MsfinderConsoleApp-Param.txt
  system(
    paste0(
      msfinder.dir, "MsfinderConsoleApp",
      " mssearch ",
      " -i ", input.dir,
      " -o ", input.dir,
      " -m ", param.file
    )
  )
  
  cat(" ----- post_mssearch", '\n')
  
}
