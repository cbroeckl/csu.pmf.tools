#' annotate.msfinder
#'
#' After running MSFinder on .mat or .msp files, import the formulas that were predicted and their scores 
#' @param ramclustObj R object - the ramclustR object which was used to write the .mat or .msp files
#' @param mat.dir optional path to .mat directory
#' @param msp.dir optional path to .msp directory
#' @details this function imports the output from the MSFinder program to support annotation of the ramclustR object
#' @return new slot at $msfinder.formula.details
#' @references Broeckling CD, Afsar FA, Neumann S, Ben-Hur A, Prenni JE. RAMClust: a novel feature clustering method enables spectral-matching-based annotation for metabolomics data. Anal Chem. 2014 Jul 15;86(14):6812-7. doi: 10.1021/ac501530d.  Epub 2014 Jun 26. PubMed PMID: 24927477.
#' @references Broeckling CD, Ganna A, Layer M, Brown K, Sutton B, Ingelsson E, Peers G, Prenni JE. Enabling Efficient and Confident Annotation of LC-MS Metabolomics Data through MS1 Spectrum and Time Prediction. Anal Chem. 2016 Sep 20;88(18):9226-34. doi: 10.1021/acs.analchem.6b02479. Epub 2016 Sep 8. PubMed PMID: 7560453.
#' @references Tsugawa H, Kind T, Nakabayashi R, Yukihira D, Tanaka W, Cajka T, Saito K, Fiehn O, Arita M. Hydrogen Rearrangement Rules: Computational MS/MS Fragmentation and Structure Elucidation Using MS-FINDER Software. Anal Chem. 2016 Aug 16;88(16):7946-58. doi: 10.1021/acs.analchem.6b00770. Epub 2016 Aug 4. PubMed PMID: 27419259.
#' @concept ramclustR
#' @concept RAMClustR
#' @concept metabolomics
#' @concept mass spectrometry
#' @concept clustering
#' @concept feature
#' @concept MSFinder
#' @concept xcms
#' @author Corey Broeckling
#' @export

annotate.msfinder <- function (ramclustObj = NULL, 
                               mat.dir = NULL,
                               priority.db = NULL,
                               priority.inchikey = NULL
                                      ) 
{
  
  if(is.null(ramclustObj)) {
    stop("must supply ramclustObj as input.  i.e. ramclustObj = RC", '\n')
  }
  
  home.dir <- getwd()
  
  r <- grep("msfinder.formula", names(ramclustObj))
  if (length(r) > 0) {
    warning("removed previously assigned MSFinder formulas and structures", 
            "\n")
    ramclustObj <- ramclustObj[-r]
    r <- grep("msfinder.structure", names(ramclustObj))
    if(length(r)>0) {
      ramclustObj <- ramclustObj[-r]
    }
    rm(r)
  }
  if (is.null(mat.dir)) {
    mat.dir = paste0(getwd(), "/spectra/mat")
  }
  
  if(is.null(ramclustObj$history)) {
    ramclustObj$history <- ""
  }
  
  ## determine most recent batchparam file
  msf.form.param.file <- list.files(mat.dir, pattern = "batchparam", full.names = TRUE)
  msf.form.param.file <- msf.form.param.file[which.max(file.info(msf.form.param.file)$ctime)]
  
  ## determine most recent formula summary file
  msf.form.summary.file <- list.files(mat.dir, pattern = "Formula result", full.names = TRUE)
  msf.form.summary.file <- msf.form.summary.file[which.max(file.info(msf.form.summary.file)$ctime)]
  
  ## determine most recent structure file
  msf.struc.summary.file <- list.files(mat.dir, pattern = "Structure result", full.names = TRUE)
  msf.struc.summary.file <- msf.struc.summary.file[which.max(file.info(msf.struc.summary.file)$ctime)]
  
  ## read in results
  msf.params <- readLines(msf.form.param.file)
  msf.form.summary <- read.delim(msf.form.summary.file, check.names = FALSE)
  msf.struc.summary <- read.delim(msf.struc.summary.file, check.names = FALSE)
  
  formula.rank.columns <- grep("Formula rank", names(msf.form.summary))
  n.cols.per.rank = diff(formula.rank.columns)
  if(all.equal(n.cols.per.rank)) {
    n.cols.per.rank <- n.cols.per.rank[1]
  } else {
    stop('MSFinder formula summary file is oddly formatted', '\n')
  }
  
  base.cols <- 1:(min(formula.rank.columns)-1)
  form.result.names <- names(msf.form.summary)[formula.rank.columns[1]:(formula.rank.columns[2]-1)]
  form.result.names[1] <- "Formula rank" 
  final.df <- data.frame(matrix(ncol = (length(base.cols)+n.cols.per.rank), nrow = 0))
  names(final.df) <- c(names(msf.form.summary)[base.cols], form.result.names) 
  for(i in 1:length(formula.rank.columns)) {
    sub.df <- data.frame(
      msf.form.summary[,base.cols],
      msf.form.summary[, formula.rank.columns[i]:(formula.rank.columns[i]+n.cols.per.rank-1)]
    )
    names(sub.df) <- names(final.df)
    is.spec.search <- as.character(sub.df$'Formula rank'[1]) == "Spectral DB search"
    if(is.na(is.spec.search)) is.spec.search = FALSE
    if(is.spec.search) next
    keep <- which(!is.na(sub.df$`Formula rank`))
    if(length(keep)>0) sub.df <- sub.df[keep,]
    if(nrow(sub.df)>0) final.df <- rbind(final.df, sub.df)
  }
  
  final.df <- as.data.frame(final.df, check.names = FALSE)
  msf.formula.results <- final.df
  rm(final.df)
  
  
  ## structure results
  structure.rank.columns <- grep("Structure rank", names(msf.struc.summary))
  n.cols.per.rank = diff(structure.rank.columns)
  if(length(table(n.cols.per.rank)) == 1) {
    n.cols.per.rank <- n.cols.per.rank[1]
  } else {
    stop('MSFinder structure summary file is oddly formatted', '\n')
  }
  
  base.cols <- 1:(min(structure.rank.columns)-1)
  struc.result.names <- names(msf.struc.summary)[structure.rank.columns[1]:(structure.rank.columns[2]-1)]
  struc.result.names[1] <- "Structure rank" 
  final.df <- data.frame(matrix(ncol = (length(base.cols)+n.cols.per.rank), nrow = 0))
  names(final.df) <- c(names(msf.struc.summary)[base.cols], struc.result.names) 
  for(i in 1:length(structure.rank.columns)) {
    sub.df <- data.frame(
      msf.struc.summary[,base.cols],
      msf.struc.summary[, structure.rank.columns[i]:(structure.rank.columns[i]+n.cols.per.rank-1)]
    )
    names(sub.df) <- names(final.df)
    keep <- which(!is.na(sub.df$`Structure rank`))
    if(length(keep)>0) sub.df <- sub.df[keep,]
    if(nrow(sub.df)>0) final.df <- rbind(final.df, sub.df)
  }
  
  final.df <- as.data.frame(final.df, check.names = FALSE)
  
  
  
  if(is.null(ramclustObj$history)) {
    ramclustObj$history <- ""
  }
  
  ramclustObj$history$msfinder <- paste(
    "MSFinder (Tsugawa 2016) was used for spectral matching,",
    "formula inference, and tentative structure assignment,",
    "and results were imported into the RAMClustR object.")
  
  if(is.null(ramclustObj$msfinder.dbs)) {
    dbs <- sapply(1:length(ramclustObj$ann), 
                          FUN = function(x) {
                            tmp <- paste(ramclustObj$msfinder.formula.details[[x]]$resourcenames, collapse = ",")
                            tmp <- strsplit(tmp, ",", fixed = TRUE)
                            tmp <- tmp[which(nchar(tmp)>0)]
                            return(tmp)
                          }
    )
    dbs <- unique(unlist(dbs))
    dbs <- dbs[which(nchar(dbs)>0)]
    ramclustObj$msfinder.dbs <- dbs
  }
  
  return(ramclustObj)
}
