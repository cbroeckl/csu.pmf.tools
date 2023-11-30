#' rc.run.msfinder
#'
#' run MSFinder for LC-MS/MS annotation 
#' @param msfinder.dir directory containing the msfinder executable file
#' @param input.dir directory containing all .mat files
#' @param param.file file (with full path) to msfinder paramater file
#' @details function to call msfinder on linux server
#' @return nothing - output written to drive. 
#' @concept MSFinder
#' @author Corey Broeckling
#' @export


rc.run.msfinder <- function(
  msfinder.dir = "C:/MSFinder/msfinder",
  input.dir  =  "R:/RSTOR-PMF/Temp/mat",
  param.file = "R:/RSTOR-PMF/Software/MSFinder/MSFINDER.INI"
) {

  if(substring(msfinder.dir, nchar(msfinder.dir)) != "/") {
    msfinder.dir <- paste0(msfinder.dir, "/")
  }

  if(substring(input.dir, nchar(input.dir)) != "/") {
    input.dir <- paste0(input.dir, "/")
  }


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

  system(
    paste0(
      msfinder.dir, "MsfinderConsoleApp",
      " predict ",
      " -i ", input.dir,
      " -o ", input.dir,
      " -m ", param.file
    )
  )

}
