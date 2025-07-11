#' rc.get.xcms.data2
#'
#' extractor for xcms objects in preparation for normalization and clustering
#'
#' @param xcmsObj xcmsObject: containing grouped feature data for clustering by ramclustR
#' @param MStag character: character string in 'taglocation' to designate files as either MS / DIA(MSe, MSall, AIF, etc) e.g. "01.mzML"
#' @param MSMStag character: character string in 'taglocation' to designate files as either MS / DIA(MSe, MSall, AIF, etc) e.g. "02.mzML"
#' @param taglocation character: "filepaths" by default, "phenoData[,1]" is another option. refers to xcms slot
#' @param ExpDes either an R object created by R ExpDes object: data used for record keeping and labelling msp spectral output
#' @param mzdec integer: number of decimal places for storing m/z values
#' @param ensure.no.na logical: if TRUE, any 'NA' values in msint and/or msmsint are replaced with numerical values based on 10 percent of feature min plus noise.  Used to ensure that spectra are not written with NA values.
#' @details This function creates a ramclustObj which will be used as input for clustering.
#' @return  an empty ramclustR object.  this object is formatted as an hclust object with additional slots for holding feature and compound data. details on these found below.
#' @return   $frt: feature retention time, in whatever units were fed in (xcms uses seconds, by default)
#' @return   $fmz: feature retention time, reported in number of decimal points selected in ramclustR function
#' @return   $ExpDes: the experimental design object used when running ramclustR.  List of two dataframes.
#' @return   $MSdata:  the MSdataset provided by either xcms or csv input
#' @return   $MSMSdata: the (optional) DIA(MSe, MSall, AIF etc) dataset provided be either xcms or csv input
#' @return   $xcmsOrd: original xcms order of features, for back-referencing when necessary
#' @return   $msint: weighted.mean intensity of feature in ms level data
#' @return   $msmsint:weighted.mean intensity of feature in msms level data
#'
#' @references Broeckling CD, Afsar FA, Neumann S, Ben-Hur A, Prenni JE. RAMClust: a novel feature clustering method enables spectral-matching-based annotation for metabolomics data. Anal Chem. 2014 Jul 15;86(14):6812-7. doi: 10.1021/ac501530d.  Epub 2014 Jun 26. PubMed PMID: 24927477.
#' @references Broeckling CD, Ganna A, Layer M, Brown K, Sutton B, Ingelsson E, Peers G, Prenni JE. Enabling Efficient and Confident Annotation of LC-MS Metabolomics Data through MS1 Spectrum and Time Prediction. Anal Chem. 2016 Sep 20;88(18):9226-34. doi: 10.1021/acs.analchem.6b02479. Epub 2016 Sep 8. PubMed PMID: 7560453.
#' @concept ramclustR
#' @concept RAMClustRs
#' @concept metabolomics
#' @concept mass spectrometry
#' @concept clustering
#' @concept feature
#' @concept MSFinder
#' @concept xcms
#' @author Corey Broeckling
#' @export

rc.get.xcms.data2 <- function(xcmsObj = NULL,
                              ExpDes = NULL,
                              mzdec = 5,
                              use.filled = TRUE,
                              fill.na = TRUE,
                              pheno.file.header = "filename"
) {
  
  require(xcms)
  
  ########
  # If experimental design is NULL:
  if (is.null(ExpDes)) {
    ExpDes <- RAMClustR:: defineExperiment(force.skip = TRUE)
  }
  
  params <- c(
    "use.filled" = use.filled,
    "mzdec" = mzdec
  )
  
  ## add xcms processing history narrative here
  
  ## check xcms object presence
  if (is.null(xcmsObj)) {
    stop("please supply an xcms object as input", "\n")
  }
  
  ## check xcms format
  xcmsObj.class <- class(xcmsObj)[grepl("xcms", class(xcmsObj), ignore.case = TRUE)]
  
  if(xcmsObj.class == "XcmsExperiment") {
    feature.definitions <- featureDefinitions(xcmsObj, msLevel = 1L)
    chrom.peaks <- chromPeaks(xcmsObj)
    chrom.peaks.data <- chromPeakData(xcmsObj)
    if(use.filled) {
      use <- which(!chrom.peaks.data$is_filled)
    } else {
      use <- 1:nrow(chrom.peaks)
    }
    chrom.peaks <- chrom.peaks[use,]
    chrom.peaks.data <- chrom.peaks.data[use,]
    phenotype <- sampleData(xcmsObj)
    filepaths <- phenotype[,1]
    filenames <- basename(filepaths)
    nfiles <- length(filenames)
    data <- t(featureValues(xcmsObj, msLevel = 1L, filled = use.filled))
    st <- round(median(chrom.peaks[use, "rtmax"] - chrom.peaks[use, "rtmin"]) / 2, digits = 2)
    times <- feature.definitions[,"rtmed"]
    mzs <- feature.definitions[,"mzmed"]
    featnames <- row.names(feature.definitions)
    if(max(chrom.peaks.data[,"ms_level"]) == 2) {
      feature.definitions.2 <- featureDefinitions(xcmsObj, msLevel = 2L)
      data.2 <- t(featureValues(xcmsObj, msLevel = 2L, filled = use.filled))
      if (nrow(data) != nrow(data.2)) {
        stop("detected ", nrow(data), " ms files and ", nrow(data.2), " msms files - ", "\n", "       number of MSMS files MUST be identical to number of MS files")
      }
      times.2 <- feature.definitions.2[,"rtmed"]
      mzs.2 <- feature.definitions.2[,"mzmed"]
      featnames.2 <- row.names(feature.definitions.2)
    }
    if(fill.na) {
      data[which(is.na(data))] <- rnorm(length(which(is.na(data))), 0.5*min(data, na.rm = TRUE), sd = 0.05*min(data, na.rm = TRUE))
      if(any(ls() == "data.2")) {
        data.2[which(is.na(data.2))] <- rnorm(length(which(is.na(data.2))), 0.5*min(data.2, na.rm = TRUE), sd = 0.05*min(data.2, na.rm = TRUE))
      }
    }
    history <- paste0(
      "RAMClustR version ", utils::packageDescription("RAMClustR")$Version, " in ", R.Version()$version.string,
      ") was used to normalize, filter, and group features into spectra. ",
      "XCMS (Smith 2006)(Tautenhahn 2008) output data was transferred to a ramclustR object using the rc.get.xcms.data function. ",
      "Feature data was extracted using the xcms featureValues function."
    )
  }
  
  # reorder feature data by RT, record original xcmsOrder
  xcmsOrd <- order(times)
  data <- data[, xcmsOrd]
  mzs <- mzs[xcmsOrd]
  times <- times[xcmsOrd]
  featnames <- featnames[xcmsOrd]
  dimnames(data)[[2]] <- featnames
  dimnames(data)[[1]] <- filenames
  
  ramclustObj <- RAMClustR::create_ramclustObj(
    ExpDes = ExpDes,
    MSdata = data,
    MSMSdata = data,
    frt = times,
    fmz = mzs,
    st = st,
    input_history = history,
    phenoData = phenotype,
    feature_names = featnames,
    xcmsOrd = xcmsOrd,
    sample_names = phenotype[,1]
  )
  
  if(any(ls() == "data.2")) {
    ramclustObj$MSMSpeak.data <- data.2
    ramclustObj$frt.2 <- times.2
    ramclustObj$fmz.2 <- mzs.2
    ramclustObj$feature_names.2 <- featnames.2
  }
  
  if (is.null(ramclustObj$params)) {
    ramclustObj$params <- list()
  }
  ramclustObj$params$rc.get.xcms.data <- params
  
  return(ramclustObj)
}
