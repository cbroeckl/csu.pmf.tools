## ultimately, have an mwtab slot in the ramclustObj at the earliest step, right after getting xcms features. 
## this would involve reading in the mwtab template and converting all the relevent data into a
## list of spreadsheet tabs.  all tabs would have a two column
## data frame except for the 'treatment' tab, which could have more columns

add.mwtab.metadata <- function(
    mwtab.spreadsheet = "mwTab.input.file.xlsx",  
    ramclustObj = RC, 
    study.id = NULL,
    analysis.id = NULL
) {
  
  if(!file.exists(mwtab.spreadsheet)) {
    error("mwtab.spreadsheet file:", mwtab.spreadsheet, "  does not exist", '\n')
  }

  spreadsheet.format <- data.frame(
    tabs = c("PROJECT", "STUDY", "SUBJECT", "COLLECTION", "TREATMENT", "SAMPLEPREP", "CHROMATOGRAPHY", "ANALYSIS", "MS", "NMR"),
    headers = c("PROJECT", "STUDY", "SUBJECT", "COLLECTION", "TREATMENT", "SAMPLEPREP", "CHROMATOGRAPHY", "ANALYSIS", "MS", "NMR"),
    leads = c("PR", "ST", "SU", "CO", "TR", "SP", "CH", "AN", "MS", "NM"),
    char.width = rep(33, 10),
    max.width = rep(80, 10)
  )
  
  required <- data.frame(
    block = c(
      rep("PROJECT", 8),
      rep("STUDY", 8)
      ),
    variable = c(
      "PROJECT_TITLE",
      "PROJECT_SUMMARY",
      "INSTITUTE",
      "LAST_NAME",
      "FIRST_NAME",
      "ADDRESS",
      "EMAIL",
      "PHONE",
      "STUDY_TITLE",
      "STUDY_SUMMARY",
      "INSTITUTE",
      "LAST_NAME",
      "FIRST_NAME",
      "ADDRESS",
      "EMAIL",
      "PHONE"
    )
    
  )
  
  mwtab <- list()
  for(i in 1:nrow(spreadsheet.format)) {
    d <- readxl::read_xlsx(mwtab.spreadsheet, sheet = spreadsheet.format$tabs[i])
    if(spreadsheet.format$tabs[i] == "TREATMENT") {
      factor.names <- names(d)
      ## replace missing values with either "NA" if character or NA if numeric? 
      mwtab[[i]] <- d
      names(mwtab)[i] <- spreadsheet.format$tabs[i]
    } else {
      if()
    }
  }
  
  return(ramclustObj)
}


make.mwtab.file <- function(
    mwtab.spreadsheet = "mwTab.input.file.xlsx",  
    ramclustObj = RC, 
    study.id = NULL,
    analysis.id = NULL
) {
  
  

  
  
  out <- vector(mode = "character", length = 0)
  
  ## create three line header
  out <- c(
    out, '\n',
    paste("#METABOLOMICS WORKBENCH", study.id, analysis.id), '\n',
    paste0("VERSION", "             ", '\t', "1.7"), '\n',
    paste0("CREATED_ON", "          ", '\t', format(Sys.Date(), '%Y-%m-%d')), '\n'
  )
  
  ## Project Block
  out <- c(
    out, 
    "#PROJECT", '\n',
    "PR:PROJECT_TITLE", '\t', 
  )
  
}