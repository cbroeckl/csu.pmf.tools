## ultimately, have an mwtab slot in the ramclustObj at the earliest step, right after getting xcms features. 
## this would involve reading in the mwtab template and converting all the relevent data into a
## list of spreadsheet tabs.  all tabs would have a two column
## data frame except for the 'treatment' tab, which could have more columns

get.mwtab.format <- function() {
  return(
    data.frame(
      tabs = c("PROJECT", "STUDY", "SUBJECT", "SUBJECT_SAMPLE_FACTORS", "COLLECTION", "TREATMENT", "SAMPLEPREP", "CHROMATOGRAPHY", "ANALYSIS", "MS"),
      headers = c("PROJECT", "STUDY", "SUBJECT", "SUBJECT_SAMPLE_FACTORS", "COLLECTION", "TREATMENT", "SAMPLEPREP", "CHROMATOGRAPHY", "ANALYSIS", "MS"),
      leads = c("PR", "ST", "SU", "TR", "CO", "TR", "SP", "CH", "AN", "MS"),
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


check.mwtab.spreadsheet <- function(
    mwtab.spreadsheet = "mwTab.input.file.xlsx"
) {
  ## check that: 
  ## - 'sample' is defined in the subject-sample-factors tab
  ## - and all samples are in the 'sequence' tab as samples
  ## - that sample names are unique and not missing
  ## - that all required values are defined
  ## - check for ARC extra tabs, 'SEQUENCE' is critical
  ## 
}


add.mwtab.metadata <- function(
    ramclustObj = RC,
    mwtab.spreadsheet = "mwTab.input.file.xlsx"
) {
  
  if(!file.exists(mwtab.spreadsheet)) {
    stop("mwtab.spreadsheet file:", mwtab.spreadsheet, "  does not exist", '\n')
  }
  mwtab.format <- get.mwtab.format()
  mwtab <- list()
  for(i in 1:nrow(mwtab.format)) {
    d <- readxl::read_xlsx(mwtab.spreadsheet, sheet = mwtab.format$tabs[i], skip = 1)
    ## replace missing values with either "NA" if character or NA if numeric? 
    mwtab[[i]] <- d
    names(mwtab)[i] <- mwtab.format$tabs[i]
  }
  
  ## add sequence tab for ARC
  d <- readxl::read_xlsx(mwtab.spreadsheet, sheet = "SEQUENCE", skip = 1)
  mwtab$SEQUENCE <- d
  
  ## add as slot to RC object
  ramclustObj$mwTab <- mwtab
  return(ramclustObj)
}

RC <- list()
RC <- add.mwtab.metadata(mwtab.spreadsheet = 'C:/Users/cbroe/OneDrive/Documents/GitHub/csu.pmf.tools/inst/params/mwTab.input.file.xlsx')
RC$mwTab$MS_METABOLITE_DATA <- read.csv('C:/Users/cbroe/OneDrive/Documents/GitHub/csu.pmf.tools/inst/params/SpecAbund.csv', row.names = 1)
RC$mwTab$METABOLITES <- read.csv('C:/Users/cbroe/OneDrive/Documents/GitHub/csu.pmf.tools/inst/params/annotations.csv', row.names = 1)

make.mwtab.file <- function(
    ramclustObj = RC, 
    testing = FALSE
) {
  
  mwtab.format <- get.mwtab.format()
  
  arc.extra.tables <- c("REAGENTS", "INTERNAL_STANDARDS", "ANALYTICAL_STANDARDS", "SEQUENCE")
  
  ## create three line header
  out <- paste0("#METABOLOMICS WORKBENCH ", '\n')
  out <- c(
    out, '\n',
    paste0("VERSION", "             ", '\t', "1.7"), '\n',
    paste0("CREATED_ON", "          ", '\t', format(Sys.Date(), '%Y-%m-%d')), '\n'
  )
  
  ## for all blocks
  for(i in 1:nrow(mwtab.format)) {
    d <- ramclustObj$mwTab[[mwtab.format[i,"tabs"]]]
    
    if(mwtab.format[i,"tabs"] != "SUBJECT_SAMPLE_FACTORS") {
      do <- which(d$Use == "Workbench" & d$Required == "Y")
      if(length(do) == 0) next
      out <- c(
        out, 
        paste0("#", mwtab.format[i,"headers"], '\n')
      )
      for(j in 1:length(do)) {
        lab <- paste0(mwtab.format$leads[i], ":", d$Header[j])
        lab.nchar <- nchar(lab)
        val <- d$Value[j]
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
            paste0(lab, paste0(rep(" ", mwtab.format$char.width[i]-lab.nchar), collapse = ""), '\t', val[k], '\n')
          )
        }
      }
    }
    if(mwtab.format[i,"tabs"] == "SUBJECT_SAMPLE_FACTORS") {
      ## SUBJECT_SAMPLE_FACTORS contains user supplied factor data
      ## SEQUENCE provides ARC provided prep and run order and batch, sample type, and raw file name(s)
      ## merge the two by SAMPLE column in preparation for output
      ## QC/blank samples will have empty (NA) cells for all factors in the user document, replace them with the row value in 'SAMPLE_TYPE'
      
      ## make sure subject is present, and if it is, replace missing values with a '-'
      if(!any(names(d) == "SUBJECT")) d$SUBJECT <- rep(NA, nrow(d))
      d$SUBJECT[which(is.na(d$SUBJECT))] <- '-'
      
      ## remove any columns that are all NA values
      for(col in ncol(d):1) {
        if(all(is.na(d[,col]))) {
          d <- d[,-col]
        }
      }
      
      ## merge and replace missing values with 'NA' character string
      seq <- ramclustObj$mwTab[["SEQUENCE"]]
      d <- merge(d, seq, by = "SAMPLE", all = TRUE)
      d$SUBJECT[which(is.na(d$SUBJECT))] <- '-'
      repl.na <- which(is.na(d[]), arr.ind = TRUE)
      if(length(repl.na)>0) d[repl.na] <- d[repl.na[,2], "SAMPLE_TYPE"]
      
      ## format output
      out <- c(
        out,
        paste0("#", mwtab.format[i,"headers"], ':',
               paste0(rep(" ", mwtab.format[i,"char.width"] - nchar(mwtab.format[i,"headers"])), collapse = ""), 
               '\t', "SUBJECT(optional)[tab]SAMPLE[tab]FACTORS(NAME:VALUE pairs separated by |)[tab]Raw file names and additional sample data", '\n')
      )
      
      fact.names <- names(d)
      fact.names <- fact.names[!(fact.names %in% c("SUBJECT", "SAMPLE", "RAW_FILE_NAME"))]
      
      ##
      for(sample in 1:nrow(d)) {
        val <- mwtab.format[i,"headers"]
        val <- paste0(val, paste0(rep(" ", (mwtab.format$char.width[i]-nchar(mwtab.format[i,"headers"]))), collapse = ""), '\t')
        val <- paste0(val, d$SUBJECT[sample], '\t')
        val <- paste0(val, d$SAMPLE[sample], '\t')
        if(length(fact.names) > 0) {
          fact.string <- vector(length = 0, mode = "character")
          for(fact in fact.names) {
            fact.string <- c(fact.string, paste0(fact, ":", d[sample, fact]))
          }
          val <- paste0(val, paste(fact.string, collapse = " | "), '\t')
        }
        val <- paste0(val, "RAW_FILE_NAME(Raw file name)=", d$RAW_FILE_NAME[sample], '\n')
        out <- c(
          out, 
          val
        )
      }
    }
  }
  
  ## MS_METABOLITE_DATA block
  out <- c(
    out, 
    "#MS_METABOLITE_DATA", '\n',
    "MS_METABOLITE_DATA:UNITS", " ", "peak area", '\n',
    "MS_METABOLITE_DATA_START", '\n',
    paste0(c("Samples", dimnames(ramclustObj$mwTab$MS_METABOLITE_DATA)[[2]]), collapse = '\t'), '\n'
  )
  for(mets in 1:nrow(ramclustObj$mwTab$MS_METABOLITE_DATA)) {
    out <- c(
      out,
      paste0(c(dimnames(ramclustObj$mwTab$MS_METABOLITE_DATA)[[1]][mets], ramclustObj$mwTab$MS_METABOLITE_DATA[mets,]), collapse = '\t'), '\n'
    )
  }
  out <- c(
    out,
    "MS_METABOLITE_DATA_END", '\n'
  )
  
  ## METABOLITES block
  out <- c(
    out,
    "#METABOLITES",'\n',
    "METABOLITES_START", '\n',
    paste0(c("metabolite_name", dimnames(ramclustObj$mwTab$METABOLITES)[[2]]), collapse = '\t'), '\n'
  )
  for(mets in 1:nrow(ramclustObj$mwTab$METABOLITES)) {
    out <- c(
      out,
      paste0(c(dimnames(ramclustObj$mwTab$METABOLITES)[[1]][mets], ramclustObj$mwTab$METABOLITES[mets,]), collapse = '\t'), '\n'
    )
  }
  return(out)
}

sink('mwtab.txt')
test <- make.mwtab.file(ramclustObj = RC)
cat(test, sep = "")
sink()
