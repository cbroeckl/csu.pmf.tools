# proj.dir <- "20211118-ECARN-GDDC-1118/TOF_PH_Pos/oocytes/"
# 
# if(Sys.info()['sysname'] == "Linux") {
#   msfinder.dir = "/home/pmflab/apps/msfinder.3.52/"
#   sirius.dir = "/home/pmflab/sirius-gui/bin/sirius"
#   proj.dir = paste0("/lab_data/pmf_lab/RStor/Projects", proj.dir)
#   param.file = "/lab_data/pmf_lab/RStor/Software/MSFinder/MSFINDER.INI"
# }
# 
# if(Sys.info()['sysname'] == "Windows") {
#   msfinder.dir = "C:/MSFinder/current"
#   sirius.dir = "C:/MSFinder/current"
#   proj.dir = paste0("R:/RSTOR-PMF/", proj.dir)
#   param.file = "R:/RSTOR-PMF/Software/MSFinder/MSFINDER.INI"
# }
# 
# rc.run.msfinder <- function(
#   msfinder.dir = "",
#   input.dir  =  "",
#   param.file = ""
# ) {
#   
#   if(substring(msfinder.dir, nchar(msfinder.dir)) != "/") {
#     msfinder.dir <- paste0(msfinder.dir, "/")
#   }
#   
#   if(substring(input.dir, nchar(input.dir)) != "/") {
#     input.dir <- paste0(input.dir, "/")
#   }
#   
#   
#   #  MsfinderConsoleApp.exe mssearch -i .\Data\ -o .\ -m .\MsfinderConsoleApp-Param.txt
#   system(
#     paste0(
#       msfinder.dir, "MsfinderConsoleApp",
#       " mssearch ",
#       " -i ", input.dir, 
#       " -o ", input.dir, 
#       " -m ", param.file
#     )
#   )
#   
#   system(
#     paste0(
#       msfinder.dir, "MsfinderConsoleApp",
#       " predict ", 
#       " -i ", input.dir, 
#       " -o ", input.dir,
#       " -m ", param.file
#     )
#   )
#   
# }
# 
# 
# rc.run.sirius <- function(
#   sirius.dir = "",
#   input.dir  =  "",
#   output.dir = ""
# ) {
#   
#   system(
#     paste0(
#       sirius.dir, " ",
#       "-i=",
#       input.dir, 
#       " -o=",
#       input.dir, "/out",
#       " --recompute ", 
#       "--naming-convention=%compoundname ", 
#       "--cores 24 ",
#       "formula ",
#       "-d ", 
#       "bio ", 
#       "-p ", 
#       "qtof ", 
#       "zodiac ", 
#       "structure ", 
#       "canopus"
#     )
#   )
#   
# }
# 
# rc.run.msfinder(
#   msfinder.dir = msfinder.dir, 
#   input.dir = paste0(input.dir, "/spectra/mat/"),
#   param.file = param.file)
# 
# rc.run.msfinder(
#   sirius.dir = msfinder.dir, 
#   input.dir = paste0(input.dir, "/spectra/ms/"),
#   output.dir = paste0(input.dir, "/spectra/ms/out/"))
