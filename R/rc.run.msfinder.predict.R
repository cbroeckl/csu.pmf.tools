rc.run.msfinder <- function(
  msfinder.dir = "/home/pmflab/apps/msfinder.3.52/",
  input.dir  =  "/home/pmflab/RStor/Projects/20210701-KWEBB-KW-1044/TOF_T3_Pos/spectra/mat",
  param.file = "/lab_data/pmf_lab/RStor/Software/MSFinder/MSFINDER.INI"
  output.dir = "out"
) {
  #  MsfinderConsoleApp.exe mssearch -i .\Data\ -o .\ -m .\MsfinderConsoleApp-Param.txt
  system(
    paste0(
      msfinder.dir, "/MsfinderConsoleApp.exe",
      " -i=",
      input.dir, 
      " -m ", param.file
    )
  )
  
}

