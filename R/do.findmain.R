#' doFindmain
#'
#' Cluster annotation function: inference of 'M' - molecular weight of the compound giving rise to each spectrum - using the InterpretMSSpectrum::findMain function
#'
#' @param ramclustObj ramclustR object to annotate. 
#' @param cmpd integer: vector defining compound numbers to annotated.  if NULL (default), all compounds
#' @param mode character: "positive" or "negative"
#' @param mzabs.error numeric: absolute mass deviation allowd, default = 0.01
#' @param ppm.error numeric: ppm mass error _added_ to mzabs.error, default = 10
#' @param ads character: vector of allowed adducts, i.e. c("[M+H]+"). if NULL, default positive mode values of H+, Na+, K+, and NH4+, as monomer, dimer, and trimer, are assigned. Negative mode include "[M-H]-", "[M+Na-2H]-", "[M+K-2H]-", "[M+CH2O2-H]-" as monomer, dimer, and trimer.
#' @param nls  character: vector of allowed neutral losses, i.e. c("[M+H-H2O]+").  if NULL, an extensive list derived from CAMERA's will be used. 
#' @param plot.findmain logical: should pdf polts be generated for evaluation? detfault = TRUE. PDF saved to working.directory/spectra
#' @param writeMat logical: should individual .mat files (for MSFinder) be generated in a 'mat' subdirectory in the 'spectra' folder? default = TRUE.
#' @details a partially annotated ramclustR object.  base structure is that of a standard R heirarchical clustering output, with additional slots described in ramclustR documentation (?ramclustR).  New slots added after using the interpretMSSpectrum functionality include those described below. 
#' @return    $M:  The inferred molecular weight of the compound giving rise to the each spectrum
#' @return    $M.ppm:  The ppm error of all the MS signals annotated, high error values should be considered 'red flags'. 
#' @return    $M.ann:  The annotated spectrum supporting the interpretation of M
#' @return    $use.findmain:  Logical vector indicating whether findmain scoring (TRUE) or ramclustR scoring (FALSE) was used to support inference of M.  By default, findmain scoring is used.  When ramclustR scoring differs from findmain scoring, the scoring metric which predicts higher M is selected. 
#' @return    $M.ramclustr:  M selected using ramclustR scoring
#' @return    $M.ppm.ramclustr:  ppm error of M selected using ramclustR scoring. Used to resolve concflicts between ramclustR and findmain M assignment when scoring = auto. 
#' @return    $M.ann.ramclustr:  annotated spectrum supporting M using ramclustR scoring
#' @return    $M.nann.ramclustr:  number of masses annotated using ramclustR scoring. Used to resolve concflicts between ramclustR and findmain M assignment when scoring = auto. 
#' @return    $M.space.ramclustr:  the 'space' of scores between the best and second best ramclustR scores. Calculated as a ratio. Used to resolve concflicts between ramclustR and findmain M assignment when scoring = auto. 
#' @return    $M.findmain:  M selected using findmain scoring
#' @return    $M.ppm.findmain:  ppm error of M selected using findmain scoring. Used to resolve concflicts between ramclustR and findmain M assignment when scoring = auto. 
#' @return    $M.ann.findmain:  annotated spectrum supporting M using findmain scoring
#' @return    $M.nann.findmain:  number of masses annotated using findmain scoring. Used to resolve concflicts between ramclustR and findmain M assignment when scoring = auto. 
#' @return    $M.space.findmain:  the 'space' of scores between the best and second best findmain scores. Calculated as a ratio. Used to resolve concflicts between ramclustR and findmain M assignment when scoring = auto. 
#' @references Jaeger C, ... Lisec J. Compound annotation in liquid chromatography/high-resolution mass spectrometry based metabolomics: robust adduct ion determination as a prerequisite to structure prediction in electrospray ionization mass spectra. Rapid Commun Mass Spectrom. 2017 Aug 15;31(15):1261-1266. doi: 10.1002/rcm.7905. PubMed PMID: 28499062.
#' @references Broeckling CD, Afsar FA, Neumann S, Ben-Hur A, Prenni JE. RAMClust: a novel feature clustering method enables spectral-matching-based annotation for metabolomics data. Anal Chem. 2014 Jul 15;86(14):6812-7. doi: 10.1021/ac501530d.  Epub 2014 Jun 26. PubMed PMID: 24927477.
#' @references Broeckling CD, Ganna A, Layer M, Brown K, Sutton B, Ingelsson E, Peers G, Prenni JE. Enabling Efficient and Confident Annotation of LC-MS Metabolomics Data through MS1 Spectrum and Time Prediction. Anal Chem. 2016 Sep 20;88(18):9226-34. doi: 10.1021/acs.analchem.6b02479. Epub 2016 Sep 8. PubMed PMID: 7560453.
#' @concept ramclustR
#' @concept RAMClustR
#' @concept metabolomics
#' @concept mass spectrometry
#' @concept clustering
#' @concept feature
#' @concept findMain
#' @concept interpretMSSpectrum
#' @concept xcms
#' @author Corey Broeckling
#' @export 


doFindmain <- function (
    ramclustObj = NULL, 
    cmpd = NULL, 
    mode = "positive", 
    mzabs.error = 0.005, 
    ppm.error = 10,
    mainpkthr = 0.15,
    ads = NULL, 
    nls = NULL,
    plot.findmain = TRUE, 
    writeMat = TRUE,
    writeMS = FALSE,
    writeMGF = FALSE,
    writeMSP = FALSE
) 
{
  
  if (!requireNamespace("InterpretMSSpectrum", quietly = TRUE)) {
    stop("The use of this function requires package 'InterpretMSSpectrum'.")
  }
  
  if(is.null(ramclustObj)) {
    stop("must supply ramclustObj as input.  i.e. ramclustObj = RC", '\n')
  }
  
  
  if (is.null(ads)) {
    if (grepl("p", mode)) {
      ads <- c("[M+H]+", "[M+Na]+", "[M+K]+", "[M+NH4]+", 
               "[2M+H]+", "[2M+Na]+", "[2M+K]+", "[2M+NH4]+",
               "[3M+H]+", "[3M+Na]+", "[3M+K]+", "[3M+NH4]+")
    }
    if (grepl("n", mode)) {
      ads <- c("[M-H]-", "[M+Na-2H]-", "[M+K-2H]-", "[M+CH2O2-H]-", 
               "[2M-H]-", "[2M+Na-2H]-", "[2M+K-2H]-", "[2M+CH2O2- H]-",
               "[3M-H]-", "[3M+Na-2H]-", "[3M+K-2H]-", "[3M+CH2O2- H]-")
    }
    if (is.null(ads)) {
      stop("please define adducts using 'ads' or set mode to either'positive' or 'negative'")
    }
  }
  if (is.null(nls)) {
    if (grepl("p", mode)) {
      nls <- c("[M+H-COCH2]+", "[M+H-C2H3NO]+", "[M+H-H2O]+", 
               "[M+H-NH3]+", "[M+H-HCOOH]-", "[M+H-C6H12O6]+", "[M+H-C5H10O5]+", 
               "[M+H-C12H22O11]+")
    }
    if (grepl("n", mode)) {
      nls <- c("[M-H-NH3]-", "[M-H-H2O]-", "[M-H-COCH2]-", 
               "[M-H-CO2]-", "[M-H-NH3-CO2]-", "[M-H-HCOOH]-", "[M-H-C6H12O6]-", 
               "[M-H-C5H10O5]-", "[M-H-C12H22O11]-")
    }
    if (is.null(nls)) {
      stop("please define neutral losses using 'nls' or set mode to either'positive' or 'negative'")
    }
  }
  
  
  findmain <- as.list(rep(NA, length(ramclustObj$cmpd)))
  names(findmain) <-  ramclustObj$cmpd
  
  if(is.null(cmpd)) {
    cmpd <- (1:max(ramclustObj$featclus))
  }
  
  ramclustObj$ms1.spectrum <- as.list(rep(NA, length(ramclustObj$ann)))
  ramclustObj$ms2.spectrum <- as.list(rep(NA, length(ramclustObj$ann)))
  ramclustObj$ms2.precursor <- rep(NA, length(ramclustObj$ann))
  ramclustObj$ms2.precursor.iontype <- rep(NA, length(ramclustObj$ann))
  
  for (cl in cmpd) {
    s <- data.frame(
      mz = ramclustObj$fm[which(ramclustObj$featclus == cl)], 
      int = ramclustObj$msint[which(ramclustObj$featclus == cl)])
    
    
    s2 <- data.frame(
      mz = ramclustObj$fm[which(ramclustObj$featclus == cl)], 
      int = ramclustObj$msmsint[which(ramclustObj$featclus == cl)])
    
    s <- s[order(s$mz),]
    s2 <- s2[order(s2$mz),]
    
    ramclustObj$ms1.spectrum[[cl]] <- s
    ramclustObj$ms2.spectrum[[cl]] <- s2
    
    out.fm <- InterpretMSSpectrum::findMAIN(
      s, 
      rules = c(ads), 
      adducthyp = ads[grep("[M",ads, fixed = TRUE)], 
      ionmode = mode, 
      mzabs = mzabs.error, 
      ppm = ppm.error,
      mainpkthr = mainpkthr
    )
    
    fm.out <- summary(out.fm)
    fm.max <- max(fm.out$total_score)
    
    out.rc <- InterpretMSSpectrum::findMAIN(
      s, 
      adductmz = NULL, 
      ionmode = mode, 
      rules = c(ads, nls), 
      adducthyp = ads[grep("[M", ads, fixed = TRUE)], 
      # ms2spec = s2, 
      mzabs = mzabs.error,
      ppm = ppm.error, 
      mainpkthr = mainpkthr, 
      collapseResults = FALSE)
    rc.out <- summary(out.rc)
    rc.max <- max(rc.out$total_score)
    
    if(rc.max >= fm.max) {
      sum.score <- rc.out
      sum.score$score.method <- rep("rc", nrow(sum.score))
    } else {
      sum.score <- fm.out
      sum.score$score.method <- rep("fm", nrow(sum.score))
    }
    
    
    # sum.score <- merge(rc.out, fm.out, by = 'neutral_mass')
    # sum.score <- data.frame(sum.score, 'total.score' = (sum.score$total_score.x + sum.score$total_score.y)/2)
    # if(nrow(sum.score) == 0) {
    #   sum.score <- rc.out
    # }
    sum.score <- sum.score[order(sum.score$total_score, decreasing = TRUE),]
    sum.score <- sum.score[which(sum.score$total_score >= (max(sum.score$total_score * 0.90))),]
    redund <- which(sum.score$total_score == max(sum.score$total_score))
    if(length(redund) > 1) {
      if(max(sum.score$adducts_explained[redund]) >1) {
        rem.redund <- redund[-which.max(sum.score$adducts_explained[redund]/sum.score$medppm[redund])]
        sum.score <- sum.score[-rem.redund,]
      } 
    }
    
    
    out.list <- as.list(rep(NA, 2))
    names(out.list) <- c("summary", "details")
    out.list[[1]] <- sum.score
    if(sum.score$score.method[1] == "rc") {
      out.list[[2]] <- out.rc[as.numeric(row.names(sum.score))]
    } else {
      out.list[[2]] <- out.fm[as.numeric(row.names(sum.score))]
    }
    
    if (100 * round(cl/100, digits = 0) == cl) {
      cat(cl, "of", max(ramclustObj$featclus), "\n")
    }
    
    findmain[[cl]] <- out.list
  }
  
  ramclustObj$findmain <- findmain
  
  # generate master findmain summary table
  sum.table <- data.frame(
    'cmpd' = vector(mode = 'character', length = 0),
    'rt' = vector(mode = 'numeric', length = 0),
    'fm.hypothesis' = vector(mode = 'character', length = 0),
    'fm.mz' = vector(mode = 'numeric', length = 0),
    'fm.adduct' = vector(mode = 'character', length = 0),
    'fm.m' = vector(mode = 'numeric', length = 0),
    'fm.score' = vector(mode = 'numeric', length = 0),
    'fm.masses.explained' = vector(mode = 'integer', length = 0),
    'fm.median.ppm' = vector(mode = 'numeric', length = 0)
  )
  for (cl in cmpd) {
    tmp <- findmain[[cl]]$summary
    sub.sum.table <- data.frame(
      'cmpd' = rep(ramclustObj$cmpd[cl], nrow(tmp)),
      'rt' = rep(ramclustObj$clrt[cl], nrow(tmp)),
      'fm.hypothesis' = paste0(ramclustObj$cmpd[cl], ".", formatC((1:nrow(tmp)), width = 2, flag = 0)),
      'fm.mz' = tmp$adductmz,
      'fm.adduct' = tmp$adducthyp,
      'fm.m' = tmp$neutral_mass,
      'fm.score' = tmp$total_score,
      'fm.masses.explained' = tmp$adducts_explained,
      'fm.median.ppm' = tmp$medppm
    )
    sum.table <- rbind(sum.table, sub.sum.table)
  }
  
  ramclustObj$findmain.summary <- sum.table
  
  if (writeMat) {
    if (!dir.exists("spectra")) {
      dir.create("spectra")
    }
    dir.create("spectra/mat")  # cmpd <- 1:50
    for (cl in cmpd) {
      for(i in 1:length(findmain[[cl]]$details)) {
        ms <- findmain[[cl]]$details[[i]]
        ms$int <- round(ms$int, digits = 3)
        prcr <- which(ms[, "adduct"] %in% ads)
        prcr <- prcr[which.max(ms[prcr, "int"])]
        prcmz <- ms[prcr, "mz"]
        prctype <- ms[prcr, "adduct"]
        out <- paste("NAME: ", ramclustObj$cmpd[cl], "\n", 
                     "RETENTIONTIME: ", round(ramclustObj$clrt[cl], 
                                              digits = 2), "\n", "PRECURSORMZ: ", prcmz, 
                     "\n", "PRECURSORTYPE: ", prctype, "\n", "IONTYPE: ", 
                     mode, "\n", "SPECTRUMTYPE: Centroid", "\n", 
                     if ((!is.null(ramclustObj$msmsint))) {
                       paste("COLLISIONENERGY: ", as.character(ramclustObj$ExpDes[[2]][which(row.names(ramclustObj$ExpDes[[2]]) == 
                                                                                               "CE2"), 1]), "\n", sep = "")
                     }, "MSTYPE: ", "MS1", "\n", "Num Peaks: ", nrow(ms), 
                     "\n", sep = "")
        for (j in 1:nrow(ms)) {
          
          out <- paste(out, ms[j, 1], " ", ms[j, 2], "\n", 
                       sep = "")
        }
        if (!is.null(ramclustObj$msmsint)) {
          feats <- which(ramclustObj$featclus == cl)
          if (length(feats) > 0) {
            msms <- data.frame(mz = ramclustObj$fm[feats], 
                               int = ramclustObj$msmsint[feats])
            msms <- msms[which(msms[, "mz"] <= (prcmz + 
                                                  3)), , drop = FALSE]
            msms <- msms[order(msms[, "int"], decreasing = TRUE), 
                         , drop = FALSE]
            msms$int <- round(1000*(msms$int/max(msms$int)), digits = 3)
            if (nrow(msms) > 0) {
              out <- paste(out, '\n', "MSTYPE:", " MS2", "\n", 
                           "Num Peaks: ", nrow(msms), "\n", sep = "")
              for (k in 1:nrow(msms)) {
                out <- paste(out, msms[k, 1], " ", msms[k, 
                                                        2], "\n", sep = "")
              }
            }
          }
        }
        else {
          do <- which(ramclustObj$featclus == cl)
          if (length(do) > 0) {
            msms <- data.frame(mz = ramclustObj$fm[do], 
                               int = ramclustObj$msint[do])
            msms <- msms[which(msms[, "mz"] <= (prcmz + 
                                                  3)), , drop = FALSE]
            msms <- msms[order(msms[, "int"], decreasing = TRUE), 
                         , drop = FALSE]
            msms$int <- round(1000*(msms$int/max(msms$int)), digits = 3)
            if (nrow(msms) > 0) {
              out <- paste(out, '\n', "MSTYPE:", " MS2", "\n", 
                           "Num Peaks: ", nrow(msms), "\n", sep = "")
              for (k in 1:nrow(msms)) {
                out <- paste(out, msms[k, 1], " ", msms[k, 
                                                        2], "\n", sep = "")
              }
            }
          }
        }
        write(out, file = paste0("spectra/mat/", ramclustObj$cmpd[cl], ".", formatC(i, width = 2, flag = 0),
                                 ".mat"))
      }
    }
  }
  
  
  ramclustObj$history$do.findmain <- paste(
    " Molecular weight was inferred from in-source spectra (Broeckling 2016) using the do.findmain function, which calls the ", 
    "interpretMSSpectrum package (Jaeger 2016). ", 
    "Parameters for do.findmain were set to: ", 
    "mode = ", mode, ", mzabs.error = ", mzabs.error, ", ppm.error = ", 
    ppm.error, ", ads = ", paste(ads, collapse = " "), ", nls = ", 
    paste(nls, collapse = " "), ".", sep = "")
  cat("finished", "\n")
  return(ramclustObj)
  
  
}
