rc.run.sirius <- function(
  sirius.dir = "/home/pmflab/sirius-gui/bin/sirius",
  input.dir  =  "/home/pmflab/RStor/Projects/20210701-KWEBB-KW-1044/TOF_T3_Pos/spectra/ms",
  output.dir = "out"
) {
  
  system(
    paste0(
      sirius.dir, " ",
      "-i=",
      input.dir, 
      " -o=",
      input.dir, "/out",
      " --recompute ", 
      "--naming-convention=%compoundname ", 
      "--cores 24 ",
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
  
}

