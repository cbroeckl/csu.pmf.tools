#' output file descriptions for reporting
#' 
#' @details convenience function providing a brief description of the common files delivered with the report. Looks in the working directory for the standard directory and file names and writes a new file to the report directory called 'file.descriptions.txt'.    
#' @return NA 
#' @concept RAMClustR
#' @author Corey Broeckling

#' @export 

write.file.summary <- function() {
  
  if(!dir.exists("tmp.report")) {
    dir.create("tmp.report")
  }
  
  f <- list.files(recursive = TRUE)
  
  file.descriptions <- data.frame(
    c("datasets/ExpDes.Rdata", "An R object holding instrument and sample information"), 
    c("datasets/RCObject.Rdata", "The R RAMClustR Object - contains detailed data on features, compounds, spectra, annotation, and stats.  The most relevent of this data is found in other more easily accessible documents, but you can open this file in R if you wish.  QC will have been removed from this file"),
    c("datasets/RCObject_withQC.Rdata", "The R RAMCLustR Object with QC samples still in it."), 
    c("datasets/SpecAbund.csv", "CSV file (excel compatible) containing signal intensity values for all compounds and samples in set.  This is the data on which all statistical analyses are based. Each coloumn is a compound, each row a sample, each cell a compound (group of features) signal intensity."),
    c("datasets/xcmsFillPeaks.Rdata", "The R formatted XCMS object containing all feature data.  This is used as input to RAMClustR."),
    c("QC/ramclustQC2.pdf", "A summary document describing (pages 1 thought 2) diagnostic plots for ramclustR behavior, (pages 3-5) PCA overview of analytical variance, as astimated by QC samples, relative to total dataset variance, and (page 6) a histogram of all compound coefficients of variation."),
    c("QC/RCpcascores.csv", "Tabular version of PCA scores depicted in ramclustQC2.pdf document.  Can be used for determing which samples may need to be removed."),
    c("spectra/*.mspLib", "msp formatted spectra from RAMClustR output. All compounds in one file."),
    c("spectra/mat/", "Spectra in mat format for MSFinder. Output files *.fgt and directories with *.sfd extensions can be found for each file using compound name as file or directory name. The contents of this directory can be imported into MSFinder for viewing on your computer."),
    c("spectra/ms/", "Spectra in .ms format suitable for the program 'Sirius'. No results are presented from Sirius, typically - files are provided for your convenience."),
    c("spectra/findMainPlots.pdf", "PDF plots of spectra with inferred molecular weight and ion annotations."),
    c("spectra/project.rsf", "RAMSearch file - can be viewed using the freely available RAMSearch Program (https://osf.io/x8bw5/).  Contains evidence for annotations with library spectral matching results."),
    c("spectra/results.rse", "RAMSearch export format - used to bring RAMSearch data into R to annotate compounds in RAMClustR object."),
    c("spectra/GOLMRIvsRT.pdf", "PDF file plotting the GOLM database retention index vs the retention time for your compounds. A diagnostic plot used to support annotation assignments."),
    c("spectra/GOLMRIvsRT.csv", "CSV file providing the GOLM database retention index vs the retention time for your compounds. A diagnostic plot used to support annotation assignments."),
    c("annotationSummary.csv", "CSV file providing an overview summary of the annotation results for each compound."),
    c("pca/loadings.outliers.csv", "CSV file containing p-values for a statistical test determing the most extreme (outlier) metabolites for each PC. Non-outlier values contains 'NA'."),
    c("pca/loadings.values.allPCs.csv", "PCA loadings values for all PCs for replotting on your own, if you wish.  This includes PCs past that selected using automated PC number optimization."),
    c("pca/loadings.values.csv","PCA loadings values for all PCs and metabolites, for replotting on your own, if you wish."),
    c("pca/plots.pdf", "PCA plots, with points representing samples, colored by factors of interest. Only PCs selected as meaningful (to prevent overfitting) are plotted. Quantitaive factors will be colored using a color gradient."),
    c("pca/scores_allPCs.csv", "PCA scores values for all PCs for replotting on your own, if you wish.  This includes PCs past that selected using automated PC number optimization."),
    c("pca/scores.csv", "PCA scores values for autoselected PCs for replotting on your own, if you wish."),
    c("anova_pvalues.csv", "CSV file containing p-values resulting from analysis of variance for each compound."),
    c("anova_summary.pdf", "PDF file containing summary plots for pvalues reported in .csv file."),
    c("model_details.txt", "Text file with model summary in text format.  While not pleasant to navigate, it provides more details than a simple p-value."),
    c("models_r_objects.Rdata", "R object containing all model fit results from ANOVA results for each compound."),
    
  stringsAsFactors = FALSE
  )
  
  sink('tmp.report/file.descriptions.txt')
  for(i in 1:ncol(file.descriptions)) {
    mtch <- grep(file.descriptions[1,i], f)
    if(length(mtch)>0) {
      cat(paste(file.descriptions[1,i], file.descriptions[2,i], sep = ": "), '\n', '\n')
    }
  }
  sink()
  
}



    