
## DDA no precursor refinement
# system(
#   paste(
#     '"C:/Program Files/ProteoWizard/msconvert.exe"',
#     "-z", "C:/Users/cbroeckl/Documents/temp/DDA.raw",
#     "-o", "C:/Users/cbroeckl/Documents/temp/",
#     "--filter", '"scanTime [120,300]"'
#   )
# )
# 
# 
# library(MSnbase)
# 
# d <- MSnbase::readMSData("C:/Users/cbroeckl/Documents/temp/20210524_HeLa_001.mzML", centroided. = FALSE, mode="onDisk")
# dh <- header(d)
# summary(dh)