## ultimately, have an mwtab slot in the ramclustObj at the earliest step, right after getting xcms features. 
## this would involve reading in the mwtab template and converting all the relevent data into a
## list of spreadsheet tabs.  all tabs would have a two column
## data frame except for the 'treatment' tab, which could have more columns

get.mwtab.format <- function() {
  return(
    data.frame(
      tabs = c("PROJECT", "STUDY", "SUBJECT", "COLLECTION", "TREATMENT", "SAMPLEPREP", "CHROMATOGRAPHY", "ANALYSIS", "MS", "NMR"),
      headers = c("PROJECT", "STUDY", "SUBJECT", "COLLECTION", "TREATMENT", "SAMPLEPREP", "CHROMATOGRAPHY", "ANALYSIS", "MS", "NMR"),
      leads = c("PR", "ST", "SU", "CO", "TR", "SP", "CH", "AN", "MS", "NM"),
      char.width = rep(33, 10),
      max.width = rep(80, 10)
    )
  )
}

string.to.lines <- function(
    string = "test that this is broken into multiple lines by max.length",
    max.length = 80) {
  
  if(nchar(string) <= max.length) {
    out <- string
  } else {
    spaces <- c(1, unlist(gregexpr(pattern =' ', string)), nchar(string))
    keep <- rep(FALSE, length(spaces))
    start <- 1
    for(i in 1:(length(spaces)-1)) {
      if(spaces[i+1] - start >= max.length) {
        keep[i] <- TRUE
        start <- spaces[i]
      }
    }
    
    out <- (substring(
      string,
      c(1, spaces[keep] + 1),
      c(spaces[keep]-1, nchar(string))
    )
    )
    
  }
  return(out)
}


add.mwtab.metadata <- function(
    ramclustObj = RC,
    mwtab.spreadsheet = "mwTab.input.file.xlsx",  
    study.id = NULL,
    analysis.id = NULL
) {
  
  if(!file.exists(mwtab.spreadsheet)) {
    error("mwtab.spreadsheet file:", mwtab.spreadsheet, "  does not exist", '\n')
  }
  mwtab.format <- get.mwtab.format()
  mwtab <- list()
  for(i in 1:nrow(mwtab.format)) {
    d <- readxl::read_xlsx(mwtab.spreadsheet, sheet = mwtab.format$tabs[i], skip = 1)
    ## replace missing values with either "NA" if character or NA if numeric? 
    mwtab[[i]] <- d
    names(mwtab)[i] <- mwtab.format$tabs[i]
  }
  
  ramclustObj$mwTab <- mwtab
  return(ramclustObj)
}

# RC <- list()
# RC <- add.mwtab.metadata()

make.mwtab.file <- function(
    ramclustObj = RC, 
    study.id = NULL,
    analysis.id = NULL,
    testing = TRUE
) {
  
  if(any(is.null(c(study.id, analysis.id)))) {
    stop("both study.id and analysis.id must be assigned before created mwTab file", '\n')
  }
  
  mwtab.format <- get.mwtab.format()
  
  arc.extra.tables <- c("REAGENTS", "INTERNAL_STANDARDS", "ANALYTICAL_STANDARDS")
  
  out <- vector(mode = "character", length = 0)
  
  ## create three line header
  out <- c(
    out, '\n',
    paste0("#METABOLOMICS WORKBENCH ", "STUDY_ID:", study.id, " ANALYSIS_ID:", analysis.id), '\n',
    paste0("VERSION", "             ", '\t', "1.7"), '\n',
    paste0("CREATED_ON", "          ", '\t', format(Sys.Date(), '%Y-%m-%d')), '\n'
  )
  
  ## for all blocks
  for(i in 1:nrow(mwtab.format)) {
    d <- ramclustObj$mwTab[[mwtab.format[i,"tabs"]]]
    do <- which(d$Use == "Workbench" & d$Required == "Y")
    if(length(do) == 0) next
    out <- c(
      out, 
      paste0("#", mwtab.format[i,"headers"], '\n')
    )
    for(j in 1:length(do)) {
      lab <- paste0(mwtab.format$leads[i], ":", d$Header[j])
      lab.nchar <- nchar(lab)
      val <- d$Value[i]
      if(is.na(val)) {
        if(testing) {
          val <- 'test'
        } else {
          stop('required infomration is missing in the', mwtab.format[i,"tabs"], 'at least', '\n')
        }
      }
      val <- string.to.lines(val, max.length = mwtab.format$max.width[i])
      ## break string into individual lines for mwTab output
      for(k in 1:length(val)) {
        out <- c(
          out, 
          paste0(lab, paste0(rep(" ", mwtab.format$char.width[i]-lab.nchar), collapse = ""), '\t', val[i], '\n')
        )
      }
    }
  }
  return(out)
}

test <- make.mwtab.file(ramclustObj = RC, study.id = 'test.study.id', analysis.id = 'test.analysis.id')
