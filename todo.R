## TO DO ITEMS for ramclustR/csu.pmf.tools

## correct overloaded peaks for saturation correction
library(CorrectOverloadedPeaks)
CorrectOverloadedPeaks::read.mzXML()


## functionalize sirius call
# sirius -i=R:\RSTOR-PMF\Projects\20201130-ABARA-MS-994\Apple\TOF_PH_pos\spectra\ms -o=R:\RSTOR-PMF\Projects\20201130-ABARA-MS-994\Apple\TOF_PH_pos\spectra\ms\out2 --recompute --naming-convention=%compoundname formula -d bio --compound-timeout 30 -p qtof zodiac structure canopus 



## functionalize MSFinder spectrum search call
# MsfinderConsoleApp mssearch -i R:\RSTOR-PMF\Projects\20201130-ABARA-MS-994\Apple\TOF_PH_pos\spectra\mat\ -o R:\RSTOR-PMF\Projects\20201130-ABARA-MS-994\Apple\TOF_PH_pos\spectra\mat\out -m MSFINDER.INI
# best to store .INI file internally and write temp file each time?

## test one file which should return 5 spectra above 0.5, 9 above 0.4
msfinder.dir <- "C:/MSFinder/msfinder"
in.dir       <- "R:/RSTOR-PMF/Projects/20201130-ABARA-MS-994/test/mat/"
out.dir      <- paste0(in.dir, "out/")
ini.filename <- "MSFINDER.INI"
#set the score threshold in the ini file very low - i.e. 0.05

## create path strings
msfinder <- paste0(msfinder.dir, "/", "MsfinderConsoleApp.exe")
ini <- paste0(msfinder.dir, "/", ini.filename)

## create full command for cmd prompt
cmd.string <- paste(
  normalizePath(msfinder), 
  "mssearch -i",
  normalizePath(in.dir), 
  "-o", normalizePath(out.dir), 
  "-m", normalizePath(ini)
)

## submit command to cmd
system(cmd.string)






