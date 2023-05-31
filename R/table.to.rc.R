#' table.to.rc
#'
#' import preprocessed protein/metabolite data from csv file to convert to ramclustR object.  
#'
#' @param import.table filepath: csv, xls, or xlsx input. Molecules as columns, rows as samples. Column header is molecule name.
#' @param factor.columns integer: which column from the csv file contains sample names. i.e. 1, or 1:3
#' @details This function creates a ramclustObj which can be used to enable downstream statistical analysis.
#' @return  an empty ramclustR object.  this object is formatted as an hclust object with additional slots for holding feature and compound data. details on these found below.
#' @return   $frt: feature retention time, in whatever units were fed in
#' @return   $fmz: feature retention time, reported in number of decimal points selected in ramclustR function
#' @return   $ExpDes: the experimental design object used when running ramclustR.  List of two dataframes.
#' @return   $MSdata:  the MSdataset provided by either xcms or csv input
#' @return   $MSMSdata: the (optional) DIA(MSe, MSall, AIF etc) dataset
#' @return   $xcmsOrd: original xcms order of features, for back-referencing when necessary
#' @return   $msint: weighted.mean intensity of feature in ms level data
#' @return   $msmsint:weighted.mean intensity of feature in msms level data
#'
#' @references Broeckling CD, Afsar FA, Neumann S, Ben-Hur A, Prenni JE. RAMClust: a novel feature clustering method enables spectral-matching-based annotation for metabolomics data. Anal Chem. 2014 Jul 15;86(14):6812-7. doi: 10.1021/ac501530d.  Epub 2014 Jun 26. PubMed PMID: 24927477.
#' @references Broeckling CD, Ganna A, Layer M, Brown K, Sutton B, Ingelsson E, Peers G, Prenni JE. Enabling Efficient and Confident Annotation of LC-MS Metabolomics Data through MS1 Spectrum and Time Prediction. Anal Chem. 2016 Sep 20;88(18):9226-34. doi: 10.1021/acs.analchem.6b02479. Epub 2016 Sep 8. PubMed PMID: 7560453.
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
#' @examples
#' ## Choose csv input file. Features as columns, rows as samples
#' ## Choose csv input file phenoData 
#' filename <- system.file("extdata", "peaks.csv", package = "RAMClustR", mustWork = TRUE)
#' phenoData <- system.file("extdata", "phenoData.csv", package = "RAMClustR", mustWork = TRUE)
#'
#' ramclustobj <- rc.get.csv.data(csv = filename, phenoData = phenoData, st = 5)
#'

table.to.rc <- function(table.file = NULL,
                      factor.columns = NULL,
                      sample.name.column = NULL) {

  if(grepl(".csv", table.file, ignore.case = TRUE)) {
    d <- read.csv(file = table.file, header = TRUE, check.names = FALSE)
  }
  
  if(grepl(".xls", table.file, ignore.case = TRUE)) {
    if(grepl(".xlsx", table.file, ignore.case = TRUE)) {
      d <- readxl::read_xlsx(path = table.file)
    } else {
      d <- readxl::read_xls(path = table.file)
    }
  }
  
  if(is.null(d)) {
    stop("data must be in .csv, .xls, or .xlsx format", '\n')
  }
  
  d <- data.frame(d)
  
  if(is.null(factor.columns)) {
    warning("no factor columns specified", '\n')
  }
  if(is.null(sample.name.column)) {
    warning("no sample name column specified", '\n')
  }
  
  phenoData <- d[,(unique(c(sample.name.column, factor.columns)))]
  
  row.names(d) <- d[,sample.name.column]
  
  d <- d[,-(unique(c(sample.name.column, factor.columns)))]

  
  
  ramclustObj <- list(
    'cmpd' = names(d),
    'ann' = names(d),
    'phenoData' = phenoData,
    'SpecAbund' = d
  )
  
  return(ramclustObj)
}
