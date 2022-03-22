system(
  paste0(
    "/home/pmflab/sirius-gui/bin/sirius ", 
    "-i=",
    "/home/pmflab/RStor/Projects/20210701-KWEBB-KW-1044/TOF_T3_Pos/spectra/ms", 
    " -o=",
    "/home/pmflab/RStor/Projects/20210701-KWEBB-KW-1044/TOF_T3_Pos/spectra/ms/out",
    " --recompute ", 
    "--naming-convention=%compoundname ", 
    "--cores 24 ",
    # "--compound-timeout=20 ", 
    "formula ",
    "-d ", 
    "bio ", 
    "-p ", 
    "qtof ", 
    "zodiac ", 
    "structure ", 
    "canopus"
  )
)

