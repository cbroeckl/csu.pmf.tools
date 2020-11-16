#' pmfpca
#'
#' perform principle compponent analysis on ramclustR object dataset, export plots
#' @details This function uses the native prcomp() function in R to perform PCA analysis. 
#' @details Automatic selection of the number of principle components using the AuerGervini method is enabled by the PCDimension and ClassDiscovery packages.
#' @details R PCA AuerGervini objects are attached to the ramclustR object, and summary plots and csv files are written to the stats/pca directory in the working directory
#' @details a methods narrative is also appended to the $history slot

#' @param ramclustObj ramclustR object to perform PCA on
#' @param which.data character; which dataset (SpecAbund or SpecAbundAve) to perform PCA on.  
#' @param subset character or integer vector; if character, must be even length.  If you wish to perform PCA only when your factor called 'treatment' is a 'trt' sample and when 'time' is '3', then you would use  i.e. c("treatment", "trt", "time", "3"). vector length must always be even, and with 'factor' followed by 'level'.  If an integer vector is provided, only row numbers matching those integers are retained. 
#' @param scale  character; default = 'pareto'.  will also accept 'uv' or 'none'   
#' @param which.factors  character vector; i.e. which.factors = c("treatment", "time").   which factors should be used for coloring PCA plots?  
#' @param num.factors which factors should be treated as numeric? must be subset of 'which.factors'. i.e. c("time")
#' @param label.by  how should metabolites columns be labelled? one of 'ann' or 'cmpd', typically. 
#' @param npc "auto" by default (recommended).  This will autoselect number of PCs to use.  Can also be set to any integer value to force more PCs.
#' @param filter logical, TRUE by default. when $cmpd.use slot is present (from rc.cmpd.filter.cv function), only cmpds that passed cv filtering are used. If you wish to change that behavior, rerun the rc.cmpd.filter.cv function with a really high CV threshold. 

#' @return returns a ramclustR object.  new R object in $pca slot. Optionally, new R object in $AuerGervini slot if npc = "auto".
#' @concept RAMClustR
#' @author Corey Broeckling

#' @export 


pmfpca<-function(ramclustObj=RC,
                 which.data="SpecAbund",
                 scale="pareto",
                 subset = c(""),
                 which.factors = NULL,
                 num.factors = NULL,
                 label.by = "cmpd", 
                 npc = "auto",
                 ag.summary.plot = FALSE,
                 filter = FALSE) {
  
  require(ggplot2)
  require(ggfortify)
  require(PCDimension)
  require(ClassDiscovery)
  
  if(is.null(ramclustObj)) {
    stop("must supply ramclustR object as input")
  }
  
  if(!dir.exists("stats")) {
    dir.create('stats')
  }
  if(!dir.exists("stats/pca")) {
    dir.create('stats/pca')
  }
  
  if(is.null(subset)) {
    subset <- c("")
  }
  
  ramclustObj$history$PCA.main <- paste0(
    "Principle Component Analysis was performed in R. The ", 
    which.data, 
    " dataset was used as input with scaling set to ", 
    scale, ".")
  
  
  d <- getData(
    ramclustObj = ramclustObj, 
    which.data = which.data, 
    filter = filter,
    cmpdlabel = label.by
  )
  
  if(length(subset) > 1) {
    if(is.integer(subset))  {
      d[[1]] <- d[[1]][subset,]
      d[[2]] <- d[[2]][subset,]
      d[[3]] <- d[[3]][subset,]
    } else {
      if(length(subset)/2 != round(length(subset)/2)) stop("'subset' length must be even", '\n')
      subset <- matrix(subset, nrow = 2)
      keep <- 1:nrow(d[[1]])
      for(i in 1:ncol(subset)) {
        if(!any(names(d[[1]]) == subset[1,i])) {
          stop(paste("factor", subset[1,i], "was not found", '\n'))
        }
        f <- as.character(d[[1]][,subset[1,i]])
        k <- which(f == subset[2,i])
        if(length(k) == 0) {
          stop(paste("level", subset[2,], "in factor", subset[1,i], "was not found", '\n'))
        }
        keep <- sort(intersect(keep, k))
      }
      d[[1]] <- d[[1]][keep,]
      d[[2]] <- d[[2]][keep,]
      d[[3]] <- d[[3]][keep,]
      # ramclustObj$history$anova3 <- paste(
      #   paste0("The dataset was subsetted to include only samples for which [") 
      # )
      # for(i in 1:ncol(subset)) {
      #   ramclustObj$history <- paste(ramclustObj$history, paste0(subset[1,i], "=", subset[2,i]))
      # }
      # ramclustObj$history <- paste0(ramclustObj$history, ".")
    }
    
  }
  
  
  if(!is.null(num.factors)) {
    for(i in 1:length(num.factors)) {
      d[[1]][,num.factors[i]] <- as.numeric(d[[1]][,num.factors[i]])
    }
  }
  
  if(length(ramclustObj[[label.by]]) == dim(d[[2]])[2]) {
    colnames(d[[2]]) <- ramclustObj[[label.by]]
  }
  
  ## scale data first. 
  
  if(scale == "pareto") {
    d[[2]] <- scale(d[[2]], center = T, scale = sqrt(apply(d[[2]], 2, FUN = "sd")))
  }
  if(scale == "uv") {
    d[[2]] <- scale(d[[2]], center = T, scale = T)
  }
  
  ## autoselect number of principle components using ClassDiscovery and PCDimensions Package tools
  ## AuerGervini method, in particular.  Use median of all AuerGervini Dimention Methods
  
  
  if(npc == "auto") {
    
    force.npc = FALSE
    spca <- ClassDiscovery::SamplePCA(t(d[[2]]), center = TRUE)
    ag.obj <- PCDimension::AuerGervini(spca)
    f <- PCDimension::makeAgCpmFun("Exponential")
    agfuns <- list(twice=agDimTwiceMean, specc=agDimSpectral,
                   km=agDimKmeans, km3=agDimKmeans3,
                   tt=agDimTtest, tt2=agDimTtest2,
                   cpt=agDimCPT, cpm=f)
    npc <- as.integer(ceiling(median(PCDimension::compareAgDimMethods(ag.obj, agfuns))))
    
    
    ramclustObj$history$PCA.npc <- paste0(
      "The number of principle components was selected using the ", 
      "AuerGervini method from the ClassDiscovery R package. ", 
      "The median value of all nPC values from the 'compareAgDimMethods' function ",
      "was used to set nPC to ", npc, ".")
    
    if(npc < 2)  {
      orig.npc <- npc
      npc <- 2
      
      force.npc = TRUE
      ramclustObj$history$PCA.npc <- paste(ramclustObj$history$PCA.npc, 
        "The nPC value was then manually set to '2' to enable two dimensional plotting. "
      )
    }
  } else {
    if(!is.integer(as.integer(npc))) {
      stop("please set npc to either an integer value or 'auto'", '\n')
    }
    ramclustObj$history <- paste(
      paste0("The nPC value was manually set to ", npc, ".")
    )
  }
  
  
  pc <- prcomp(d[[2]])
  
  if(length(npc) <= 5) {plot.pcs <- rep(TRUE, npc)} else {plot.pcs <- rep(FALSE, npc)}
  plot.pcs[1:2] <- TRUE
  sig.pcs <- rep(FALSE, npc)
  if(!is.null(which.factors)) {
    for(i in 1:length(which.factors)) {
      if(min(table(d[[1]][,which.factors[i]])) <2 & !is.numeric(d[[1]][,which.factors[i]])) {
        # warning(paste(which.factors[i], "Insufficient replication in factor:", "'", which.factors[i], "'", '\n'))
        next
      }
      for(j in 1:npc) {
        p <- as.numeric(anova(lm(pc$x[,j]~d[[1]][,which.factors[i]]))[1,"Pr(>F)"])
        
        if(p < 0.05) {
          plot.pcs[j] <- TRUE
          sig.pcs[j] <- TRUE
        }
      }
    }
  } else {
    sig.pcs <- rep(FALSE, npc)
  }
  
  
  ramclustObj$history$PCA.anova <- paste0(
    "Linear model ANOVA was performed for the factor(s) [",
    paste(which.factors, collapse = ", "), 
    "] to provide some guidance on which PCs appear responsive to factors of interest.", 
    " These are not meant to be rigorous statistical tests but to help guide interpretation of the data."
  )
  
  pdf('stats/pca/plots.pdf', width = 10, height = 6)
  if(any(ls() == ("ag.obj"))) {
    if(ag.summary.plot) {
      par(mfrow = c(1,2))
      # cat("before plotting", '\n')
      plot(ag.obj, agfuns, 
           main = "n PC selection: AuerGervini method",
           sub = "dashed line(s) indicate all possible nPC options, 
       blue dotted represents median of all", 
           cex.sub = 0.5)
      # cat("after plotting", '\n')
      abline(h = npc, col = "blue", lty = 3, lwd = 2)
      # cat("after abline", '\n')
      legend(x = "topright", legend = if(force.npc) {
        paste0("Orig npc = ",orig.npc, "; Forced npc = ", npc)
      } else {
        paste("npc =", npc)
      }, bty = "n")
      # cat("after legend", '\n')
      pt.cols <- rep("gray", length(pc$sdev)); pt.cols[1:npc] <- "darkgreen"
      rel.var <- round((spca@variances)/sum((spca@variances)), digits = 3)
      plot(1:(2*npc), rel.var[1:(2*npc)], col = pt.cols, mgp = c(3,1,0), ylim = c(0, 1.2*rel.var[1]), 
           ylab = "proportion variance explained", pch = 19,
           main = "screeplot", xlab = "PC", 
           sub = paste("green points represent PCs used, * (if present) indicates PCSs with response to factor(s)"), 
           cex.lab = 1, cex.sub = 0.5, type = "b")
      if(any(sig.pcs)) {
        points((1:npc)[which(sig.pcs)], (rel.var + (rel.var[1]/20))[which(sig.pcs)], 
               pch = "*", cex = 1)
      }
      
    }
    
    }
    
  par(mfrow = c(1,1))
  xy <- combn(x = which(plot.pcs),  2)
  
  for(i in 1:ncol(xy)) {
    for(j in 1:length(which.factors)) {
      print(autoplot(pc, x = xy[1,i], y = xy[2,i], 
                     data = cbind(d[[1]], d[[2]]), 
                     colour = which.factors[j], frame = !any(which.factors[j] %in% num.factors), 
                     width = 5) + theme_bw()) 
    }
  }
  dev.off()
  
  loadings.out <- (pc$rotation)[,1:npc, drop = FALSE]
  write.csv(loadings.out, file = "stats/pca/loadings.values.csv")
  for(i in 1:ncol(loadings.out)) {
    tmp <-p.adjust(2*pnorm(abs(pc$rotation[,1]), lower.tail=FALSE), method="BH")
    loadings.out[which(tmp > 0.05),i] <- "NA"
  }
  
  ramclustObj$history$loadings <- paste(
    "Outlier tests are performed on PC loadings to serve as a guide in interpreting which compounds contribute most to the observed separation.", 
    "This is performed using the R pnorm function. Returned p-values are false discovery rate corrected.", 
    "These p-values are not used to conclude that a compound is significantly changing, but rather to indicate that a compound disproportionately contributes to the multivariate sample separation observed for that PC."
  )
  
  write.csv(pc$rotation, file = "stats/pca/loadings.values.allPCs.csv")
  write.csv(loadings.out, file = "stats/pca/loadings.outliers.csv")
  write.csv(pc$x[,1:npc], file = "stats/pca/scores.csv")
  write.csv(pc$x, file = "stats/pca/scores_allPCs.csv")
  
  ramclustObj$pca <- pc
  if(any(ls() == "ag.obj")) ramclustObj$AuerGervini <- ag.obj
  
  return(ramclustObj)
}

