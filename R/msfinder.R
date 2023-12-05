
#' #' annotate.msfinder
#' #'
#' #' After running MSFinder on .mat or .msp files, import the formulas that were predicted and their scores 
#' #' @param ramclustObj R object - the ramclustR object which was used to write the .mat or .msp files
#' #' @param mat.dir optional path to .mat directory
#' #' @param msp.dir optional path to .msp directory
#' #' @details this function imports the output from the MSFinder program to support annotation of the ramclustR object
#' #' @return new slot at $msfinder.formula.details
#' #' @references Broeckling CD, Afsar FA, Neumann S, Ben-Hur A, Prenni JE. RAMClust: a novel feature clustering method enables spectral-matching-based annotation for metabolomics data. Anal Chem. 2014 Jul 15;86(14):6812-7. doi: 10.1021/ac501530d.  Epub 2014 Jun 26. PubMed PMID: 24927477.
#' #' @references Broeckling CD, Ganna A, Layer M, Brown K, Sutton B, Ingelsson E, Peers G, Prenni JE. Enabling Efficient and Confident Annotation of LC-MS Metabolomics Data through MS1 Spectrum and Time Prediction. Anal Chem. 2016 Sep 20;88(18):9226-34. doi: 10.1021/acs.analchem.6b02479. Epub 2016 Sep 8. PubMed PMID: 7560453.
#' #' @references Tsugawa H, Kind T, Nakabayashi R, Yukihira D, Tanaka W, Cajka T, Saito K, Fiehn O, Arita M. Hydrogen Rearrangement Rules: Computational MS/MS Fragmentation and Structure Elucidation Using MS-FINDER Software. Anal Chem. 2016 Aug 16;88(16):7946-58. doi: 10.1021/acs.analchem.6b00770. Epub 2016 Aug 4. PubMed PMID: 27419259.
#' #' @concept ramclustR
#' #' @concept RAMClustR
#' #' @concept metabolomics
#' #' @concept mass spectrometry
#' #' @concept clustering
#' #' @concept feature
#' #' @concept MSFinder
#' #' @concept xcms
#' #' @author Corey Broeckling
#' #' @export
#' 
#' annotate.msfinder <- function (ramclustObj = NULL, 
#'                                mat.dir = NULL,
#'                                priority.db = NULL,
#'                                priority.inchikey = NULL,
#'                                priority.db.factor = 0.95,
#'                                priority.inchikey.factor = 0.95
#' ) 
#' {
#'   
#'   if(is.null(ramclustObj)) {
#'     stop("must supply ramclustObj as input.  i.e. ramclustObj = RC", '\n')
#'   }
#'   
#'   home.dir <- getwd()
#'   
#'   if(!is.null(priority.db) & !is.null(priority.inchikey)) {
#'     warning("both inchikey and database priority set - ensure they are independent. ", '\n')
#'   }
#'   
#'   r <- grep("msfinder.formula", names(ramclustObj))
#'   if (length(r) > 0) {
#'     warning("removed previously assigned MSFinder formulas and structures", 
#'             "\n")
#'     ramclustObj <- ramclustObj[-r]
#'     r <- grep("msfinder.structure", names(ramclustObj))
#'     if(length(r)>0) {
#'       ramclustObj <- ramclustObj[-r]
#'     }
#'     rm(r)
#'   }
#'   if (is.null(mat.dir)) {
#'     mat.dir = paste0(getwd(), "/spectra/mat")
#'   }
#'   
#'   if(is.null(ramclustObj$history)) {
#'     ramclustObj$history <- ""
#'   }
#'   
#'   findmain.summary <- ramclustObj$findmain.summary
#'   
#'   ## determine most recent batchparam file
#'   msf.form.param.file <- list.files(mat.dir, pattern = "batchparam", full.names = TRUE)
#'   if(length(msf.form.param.file) > 0) {
#'     msf.form.param.file <- msf.form.param.file[which.max(file.info(msf.form.param.file)$ctime)]
#'     msf.params <- readLines(msf.form.param.file)
#'   }
#'   
#'   
#'   ## find all formula output files: 
#'   form.files <- list.files(mat.dir, pattern = '.fgt', recursive = FALSE, full.names = TRUE)
#'   
#'   ## find all structure output files: 
#'   struc.files <- list.files(mat.dir, pattern = '.sfd', recursive = TRUE, full.names = TRUE)
#'   
#'   ## separate out spectal search results
#'   is.spec.db <- grep("Spectral", struc.files)
#'   spec.files <- struc.files[is.spec.db]
#'   struc.files <- struc.files[-is.spec.db]
#'   
#'   
#'   ###############################
#'   ## read in formula results
#'   cat(" -- importing formula results", '\n')
#'   hypothesis <- gsub(".fgt", "", basename(form.files), ignore.case = TRUE)
#'   
#'   form.results <- data.frame(
#'     'hypothesis' = vector(mode = 'character', length = 0),
#'     'formula' = vector(mode = 'character', length = 0),
#'     'formula.score'   = vector(mode =  'numeric', length = 0)
#'   )
#'   # for(i in 1:10) {
#'   for(i in 1:length(form.files)) {
#'     tmp <- readLines(form.files[i])
#'     tmp.names <- grep("NAME: ", tmp)
#'     if(length(tmp.names) > 0) {
#'       tmp.scores <- grep("TOTALSCORE:", tmp)
#'       if((length(tmp.names)>0) & (length(tmp.names) == length(tmp.scores))) {
#'         tmp.out <- data.frame(
#'           'hypothesis' = rep(hypothesis[i], length(tmp.names)),
#'           'formula' = gsub("NAME: ", "", tmp[tmp.names]),
#'           'formula.score'   = as.numeric(gsub("TOTALSCORE: ", "", tmp[tmp.scores]))
#'         )
#'       }
#'       tmp.rm <- grep("Spectral", tmp.out$formula)
#'       if(length(tmp.rm)>0) {
#'         tmp.out <- tmp.out[-tmp.rm,]
#'       }
#'       if(nrow(tmp.out)>0) {
#'         form.results <- rbind(form.results, tmp.out)
#'       }
#'     }
#'     
#'   }
#'   
#'   ## read in structure results
#'   cat(" -- importing structure results", '\n')
#'   struc.path <- lapply(1:length(struc.files), FUN = function(x) {
#'     unlist(strsplit(struc.files[x], "/", fixed = TRUE))
#'   })
#'   struc.path <- t(data.frame(struc.path))
#'   hypothesis <- as.character(struc.path[,(ncol(struc.path)-1)])
#'   hypothesis.n <- as.numeric(sapply(1:length(hypothesis), FUN = function(x) unlist(strsplit(hypothesis[x], '.', fixed = TRUE))[2]))
#'   formula    <- as.character(gsub('.sfd', '', struc.path[,ncol(struc.path)]))
#'   cmpd       <- sapply(1:length(hypothesis), FUN = function(x) {
#'     strsplit(hypothesis[x], ".", fixed = TRUE)[[1]][1]
#'   }
#'   )
#'   
#'   struc.results <- data.frame(
#'     'cmpd' = vector(mode = 'character', length = 0),
#'     'hypothesis' = vector(mode = 'character', length = 0),
#'     'retention.time' = vector(mode = 'character', length = 0),
#'     'assigned.M' = vector(mode = 'character', length = 0),
#'     'formula'    = vector(mode = 'character', length = 0),
#'     'compound.name' = vector(mode = 'character', length = 0),
#'     'inchikey'   = vector(mode = 'character', length = 0),
#'     'smiles'     = vector(mode = 'character', length = 0),
#'     'dbs'        = vector(mode = 'character', length = 0),
#'     'direct.parent.class'   = vector(mode = "character", length = 0),
#'     'chemontid'  = vector(mode = 'character', length = 0),
#'     'structure.score'  = vector(mode =  'numeric', length = 0)
#'   )
#'   cmpd.rts <- round(ramclustObj$clrt, digits = 1)
#'   names(cmpd.rts) <- ramclustObj$cmpd
#'   
#'   #for(i in 1:10) {
#'   for(i in 1:length(struc.files)) {
#'     
#'     tmp <- readLines(struc.files[i])
#'     tmp.names <- grep("NAME: ", tmp)
#'     if(length(tmp.names) > 0) {
#'       # error()
#'       tmp.scores <- grep("TotalScore:", tmp)
#'       tmp.inchikey <- grep("INCHIKEY: ", tmp)
#'       tmp.smiles <- grep("SMILES: ", tmp)
#'       tmp.dbs     <- grep("RESOURCES: ", tmp)
#'       tmp.subclass<- grep("Ontology: ", tmp)
#'       tmp.chemontid<- grep("OntologyID: ", tmp)
#'       same.length <- all.equal(
#'         length(tmp.names),
#'         length(tmp.scores),
#'         length(tmp.inchikey),
#'         length(tmp.smiles),
#'         length(tmp.dbs),
#'         length(tmp.subclass),
#'         length(tmp.chemontid)
#'       )
#'       if((length(tmp.names)>0) & same.length) {
#'         tmp.out <- data.frame(
#'           'cmpd'                  = rep(cmpd[i], length(tmp.names)),
#'           'hypothesis'            = rep(hypothesis[i], length(tmp.names)),
#'           'retention.time'        = as.vector(rep(cmpd.rts[cmpd[i]], length(tmp.names))),
#'           'assigned.M'            = rep(ramclustObj$findmain[[cmpd[i]]]$summary$neutral_mass[hypothesis.n[i]], length(tmp.names)),
#'           'formula'               = rep(formula[i], length(tmp.names)),
#'           'compound.name'         = gsub("NAME: ", "", tmp[tmp.names]),
#'           'inchikey'              = gsub("INCHIKEY: ", "", tmp[tmp.inchikey]),
#'           'smiles'                = gsub("SMILES: ", "", tmp[tmp.smiles]),
#'           'dbs'                   = gsub("RESOURCES: ", "", tmp[tmp.dbs]),
#'           'direct.parent.class'   = gsub("Ontology: ", "", tmp[tmp.subclass]),
#'           'chemontid'             = gsub("OntologyID: ", "", tmp[tmp.chemontid]),
#'           'structure.score'       = as.numeric(gsub("TotalScore: ", "", tmp[tmp.scores]))
#'         )
#'       }
#'       tmp.rm <- grep("Spectral", tmp.out$formula)
#'       if(length(tmp.rm)>0) {
#'         tmp.out <- tmp.out[-tmp.rm,]
#'       }
#'       if(nrow(tmp.out)>0) {
#'         struc.results <- rbind(struc.results, tmp.out)
#'       }
#'     }
#'   }
#'   
#'   ## bring formula score into structure table 
#'   cat(" -- associating formula scores to structures", '\n')
#'   struct.names <- paste(struc.results$hypothesis, struc.results$formula)
#'   form.names   <- paste(form.results$hypothesis, form.results$formula)
#'   struc.to.form <- match(
#'     struct.names,
#'     form.names
#'   )
#'   struc.results$formula.score <- form.results$formula.score[struc.to.form]
#'   
#'   ## bring findmain score into structure table 
#'   struc.to.findmain <- match(
#'     struc.results$hypothesis ,
#'     findmain.summary$hypothesis
#'   )
#'   struc.results$findmain.score <- findmain.summary$findmain.score[struc.to.findmain]
#'   
#'   
#'   ## calculate a total score for each annotation hypothesis
#'   cat(" -- assigning total score", '\n')
#'   struc.results$total.score <- round(
#'     2*struc.results$structure.score * struc.results$formula.score * struc.results$findmain.score, 
#'     digits = 2
#'   )
#'   
#'   
#'   ## read in spectral match results
#'   cat(" -- importing spectral search results", '\n')
#'   spec.path <- lapply(1:length(spec.files), FUN = function(x) {
#'     unlist(strsplit(spec.files[x], "/", fixed = TRUE))
#'   })
#'   spec.path <- t(data.frame(spec.path))
#'   hypothesis <- as.character(spec.path[,(ncol(spec.path)-1)])
#'   cmpd       <- sapply(1:length(hypothesis), FUN = function(x) {
#'     strsplit(hypothesis[x], ".", fixed = TRUE)[[1]][1]
#'   }
#'   )
#'   
#'   spec.results <- data.frame(
#'     'cmpd' = vector(mode = 'character', length = 0),
#'     'hypothesis' = vector(mode = 'character', length = 0),
#'     'compound.name' = vector(mode = 'character', length = 0),
#'     'inchikey'   = vector(mode = 'character', length = 0),
#'     'dbs'        = vector(mode = 'character', length = 0),
#'     'spectral.match.score'  = vector(mode =  'numeric', length = 0),
#'     'direct.parent.class'   = vector(mode = "character", length = 0),
#'     'chemontid'  = vector(mode = 'character', length = 0)
#'   )
#'   
#'   for(i in 1:length(spec.files)) {
#'     tmp <- readLines(spec.files[i])
#'     tmp.names <- grep("NAME: ", tmp)
#'     if(length(tmp.names) > 0) {
#'       tmp.scores <- grep("TotalScore:", tmp)
#'       tmp.inchikey <- grep("INCHIKEY: ", tmp)
#'       tmp.dbs     <- grep("RESOURCES: ", tmp)
#'       tmp.subclass<- grep("Ontology: ", tmp)
#'       tmp.chemontid<- grep("OntologyID: ", tmp)
#'       same.length <- all.equal(
#'         length(tmp.names),
#'         length(tmp.scores),
#'         length(tmp.inchikey),
#'         length(tmp.dbs),
#'         length(tmp.subclass),
#'         length(tmp.chemontid)
#'       )
#'       if((length(tmp.names)>0) & same.length) {
#'         tmp.out <- data.frame(
#'           'cmpd'       = rep(cmpd[i], length(tmp.names)),
#'           'hypothesis' = rep(hypothesis[i], length(tmp.names)),
#'           'compound.name'   = gsub("NAME: ", "", tmp[tmp.names]),
#'           'inchikey'   = gsub("INCHIKEY: ", "", tmp[tmp.inchikey]),
#'           'dbs'        = gsub("RESOURCES: ", "", tmp[tmp.dbs]),
#'           'spectral.match.score'      = as.numeric(gsub("TotalScore: ", "", tmp[tmp.scores])),
#'           'direct.parent.class'   = gsub("Ontology: ", "", tmp[tmp.subclass]),
#'           'chemontid'  = gsub("OntologyID: ", "", tmp[tmp.chemontid])
#'         )
#'       }
#'       if(nrow(tmp.out)>0) {
#'         spec.results <- rbind(spec.results, tmp.out)
#'       }
#'     }
#'   }
#'   
#'   struc.results <- data.frame(
#'     'assigned' = rep(FALSE, nrow(struc.results)),
#'     struc.results,
#'     'spectral.match.score' = rep(NA, nrow(struc.results)),
#'     'spectral.match.name'  = rep(NA, nrow(struc.results)),
#'     'spectral.match.inchikey' = rep(NA, nrow(struc.results))
#'   )
#'   
#'   ## update spectral match results table
#'   if(length(spec.results) > 0) {
#'     cmpds <- unique(spec.results$cmpd)
#'     if(length(cmpds) > 0) {
#'       for(i in 1:length(cmpds)) {
#'         tmp <- spec.results[which(spec.results$cmpd == cmpds[i]),]
#'         tmp <- tmp[which.max(tmp$spectral.match.score),]
#'         cmpd.match <- which(struc.results$cmpd == tmp$cmpd[1])
#'         if(length(cmpd.match)>0) {
#'           struc.results[cmpd.match, "spectral.match.score"] <- as.numeric(tmp$spectral.match.score[1])
#'           struc.results[cmpd.match, "spectral.match.name"] <- as.character(tmp$compound.name[1])
#'           struc.results[cmpd.match, "spectral.match.inchikey"] <- as.character(tmp$inchikey[1])
#'         }
#'       }
#'     }
#'   }
#'   
#'   ## assign database priority score, if applicable
#'   
#'   if(!is.null(priority.db)) {
#'     cat(" -- assinging database priority score", '\n')
#'     priority.factor.v <- rep(priority.db.factor, nrow(struc.results))
#'     for(i in 1:length(priority.db)) {
#'       db.match <- grep(priority.db[i], struc.results$dbs, ignore.case = TRUE)
#'       priority.factor.v[db.match] <- 1
#'     }
#'     struc.results$db.priority.factor <- priority.factor.v
#'     struc.results$total.score <- struc.results$total.score * priority.factor.v
#'   }
#'   
#'   
#'   ## assign inchikey priority score, if applicable
#'   if(!is.null(priority.inchikey)) {
#'     cat(" -- assinging inchikey priority score", '\n')
#'     priority.factor.v <- rep(priority.inchikey.factor, nrow(struc.results))
#'     short.inchikey.priority <- sapply(priority.inchikey, FUN = function(x) {
#'       unlist(strsplit(x, "-"))[1]
#'     })
#'     short.inchikey.struc <- sapply(struc.results$inchikey, FUN = function(x) {
#'       unlist(strsplit(x, "-"))[1]
#'     })
#'     inchi.match <- short.inchikey.struc %in% short.inchikey.priority
#'     priority.factor.v[inchi.match] <- 1
#'     struc.results$inchikey.priority.factor <- priority.factor.v
#'     struc.results$total.score <- struc.results$total.score * priority.factor.v
#'   }
#'   
#'   ## remove any results which have no findmain score (and therefore no total score)
#'   struc.results <- struc.results[which(!is.na(struc.results$findmain.score)),]
#'   
#'   ## reset annotations
#'   ramclustObj$ann  <- ramclustObj$cmpd
#'   
#'   ## update structure table
#'   ramclustObj$use.spectral.match <- rep(FALSE, length(ramclustObj$cmpd))
#'   
#'   cat(" -- annotating", '\n')
#'   for(i in 1:length(ramclustObj$ann)) {
#'     if(ramclustObj$ann[i] != ramclustObj$cmpd[i]) next
#'     cmpd.match <- which(struc.results$cmpd == ramclustObj$cmpd[i])
#'     if(length(cmpd.match)==0) next
#'     ann.sub <- struc.results[cmpd.match,]
#'     
#'     use.spectral.match <- any(!is.na(ann.sub$spectral.match.score))
#'     if(use.spectral.match) {
#'       ramclustObj$use.spectral.match[i] <- TRUE
#'       sp.m.index <- which.max(ann.sub$spectral.match.score)
#'       struc.results$assigned[cmpd.match[sp.m.index]] <- TRUE
#'       ramclustObj$ann[i] <-  ann.sub$spectral.match.name[sp.m.index]
#'       ramclustObj$inchikey <- ann.sub$spectral.match.inchikey[sp.m.index]
#'       ramclustObj$spectral.match.score[i] <- ann.sub$spectral.match.score[sp.m.index]
#'     } else {
#'       best.score <- which.max(ann.sub$total.score)
#'       sel.hyp <- as.numeric(gsub(paste0(ramclustObj$cmpd[i], "."), "", ann.sub$hypothesis[best.score]))
#'       fm <- ramclustObj$findmain[[i]]$details[[sel.hyp]]
#'       fm <- fm[order(fm[,1]),]
#'       fm.sum <- ramclustObj$findmain[[i]]$summary[sel.hyp,]
#'       struc.results$assigned[cmpd.match[best.score]] <- TRUE
#'       ramclustObj$ms1.spectrum[[i]] <- fm
#'       ramclustObj$ms2.spectrum[[i]] <- data.frame(
#'         ramclustObj$ms2.spectrum[[i]][order(ramclustObj$ms2.spectrum[[i]][,1]),],
#'         fm[3:ncol(fm)]
#'       )
#'       ramclustObj$ann[i] <- ann.sub$compound.name[best.score]
#'       ramclustObj$M[i] <- fm.sum$neutral_mass[1]
#'       ramclustObj$formula[i] <- ann.sub$formula[best.score]
#'       ramclustObj$inchikey[i] <- ann.sub$inchikey[best.score]
#'       ramclustObj$findmain.score[i] <- ann.sub$findmain.score[best.score]
#'       ramclustObj$formula.score[i] <- ann.sub$formula.score[best.score]
#'       ramclustObj$structure.score[i] <- ann.sub$structure.score[best.score]
#'       ramclustObj$total.score[i] <- ann.sub$total.score[best.score]
#'     }
#'   }
#'   
#'   find.cid <- function(x) {
#'     if(any(grepl("PubChem", struc.results$dbs[x], ignore.case = TRUE))) {
#'       tmp <- unlist(strsplit(struc.results$dbs[x], ","))
#'       tmp <- tmp[grep("PubChem", tmp, ignore.case = TRUE)]
#'       tmp <- gsub("PubChem=", "", tmp, ignore.case = TRUE)
#'       tmp <- min(as.numeric(unlist(strsplit(tmp, ";"))))
#'       tmp
#'     } else {
#'       NA
#'     }
#'   } 
#'   
#'   cid <- sapply(1:nrow(struc.results), FUN = find.cid)
#'   struc.results$cid <- cid
#'   
#'   ramclustObj$annotations.full <- struc.results
#'   ramclustObj$annotations.selected <- struc.results[which(struc.results$assigned),2:ncol(struc.results)]
#'   
#'   ramclustObj$history$msfinder <- paste(
#'     "MSFinder (Tsugawa 2016) was used for spectral matching,",
#'     "formula inference, and tentative structure assignment.",
#'     "Results were imported into the RAMClustR object.", 
#'     "A total score was calculated based on the product scores from the findmain function",
#'     "and the MSfinder formula and structure scores.", 
#'     "A total of", length(unique(ramclustObj$findmain.summary$hypothesis)), "annotation hypotheses were tested", 
#'     "for", length(ramclustObj$ann), "compounds.", 
#'     "A complete spreadsheet of all annotation hypothesis and scores can be found in the 'spectra/all.annotations.csv' file,",
#'     "and a subset of only those selected for annotation can be found in the 'spectra/assigned.annotations.csv' file.",
#'     "Spectra matches took precedence over computational inference based annotations.")
#'   
#'   
#'   
#'   if(!is.null(priority.db)) {
#'     ramclustObj$history$msfinder <-paste(
#'       as.character(ramclustObj$history$msfinder),
#'       "The following database(s) were assigned as 'priority': ", paste0(paste(priority.db, collapse = ', '), "."),
#'       "The database priority.factor was set to", priority.db.factor, "to decrease scores for compounds which tailed to match priority database(s)."
#'     )
#'   }
#'   
#'   if(!is.null(priority.inchikey)) {
#'     ramclustObj$history$msfinder <-paste(
#'       as.character(ramclustObj$history$msfinder),
#'       "The a list of",  length(priority.inchikey), "inchikeys was provided.", 
#'       "The inchikey priority.factor was set to", priority.inchikey.factor, "to decrease scores for compounds with non-matching inchikey(s)."
#'     )
#'   }
#'   ramclustObj$history$msfinder <-paste(
#'     as.character(ramclustObj$history$msfinder),
#'     "The highest total score was selected for each compound, considering all hypotheses."
#'   )
#'   
#'   ## write annotaion tables to 'spectra' directory
#'   out.dir <- gsub(basename(mat.dir), "",  mat.dir)
#'   write.csv(struc.results, file = paste0(out.dir, "all.annotations.csv"), row.names = FALSE)
#'   write.csv(ramclustObj$annotations.selected, file = paste0(out.dir, "assigned.annotations.csv"), row.names = FALSE)
#'   return(ramclustObj)
#' }

rc.run.msfinder <- function(
    msfinder.dir = "",
    input.dir  =  "",
    param.file = ""
) {
  
  if(substring(msfinder.dir, nchar(msfinder.dir)) != "/") {
    msfinder.dir <- paste0(msfinder.dir, "/")
  }
  
  if(substring(input.dir, nchar(input.dir)) != "/") {
    input.dir <- paste0(input.dir, "/")
  }
  
  cat(" ----- pre_mssearch", '\n')
  #  MsfinderConsoleApp.exe mssearch -i .\Data\ -o .\ -m .\MsfinderConsoleApp-Param.txt
  system(
    paste0(
      msfinder.dir, "MsfinderConsoleApp",
      " mssearch ",
      " -i ", input.dir,
      " -o ", input.dir,
      " -m ", param.file
    )
  )
  
  cat(" ----- post_mssearch", '\n')
  
  ## rename directories containing spectral search results to keep them from being overwritten
  spec.results <- list.files(
    paste0(input.dir),
    pattern = "MsSearch result",
    recursive = FALSE)
  versions <- as.numeric(gsub(".txt", "", gsub("MsSearch result-", "", spec.results)))
  spec.results <- spec.results[which.max(versions)]
  res <- read.delim(paste0(input.dir, spec.results))
  from.name <- unique(res$File.name)
  to.name <- paste0(from.name, ".mssearch")
  
  for(i in 1:length(from.name)) {
    dir.create(paste0(input.dir,  to.name[i]))
    files.to.move <- list.files(paste0(input.dir, from.name[i]), recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
    file.copy(files.to.move, to = paste0(input.dir,  to.name[i]), recursive = TRUE)
  }
  cat(" ----- post_modify.directory.names", '\n')
  
  system(
    paste0(
      msfinder.dir, "MsfinderConsoleApp",
      " predict ",
      " -i ", input.dir,
      " -o ", input.dir,
      " -m ", param.file
    )
  )
  
  cat (" ----- post_predict", '\n')
}


rc.run.msfinder.gc <- function(
    msfinder.dir = "",
    input.dir  =  "",
    param.file = ""
) {
  
  if(substring(msfinder.dir, nchar(msfinder.dir)) != "/") {
    msfinder.dir <- paste0(msfinder.dir, "/")
  }
  
  if(substring(input.dir, nchar(input.dir)) != "/") {
    input.dir <- paste0(input.dir, "/")
  }
  
  cat(" ----- pre_mssearch", '\n')
  #  MsfinderConsoleApp.exe mssearch -i .\Data\ -o .\ -m .\MsfinderConsoleApp-Param.txt
  system(
    paste0(
      msfinder.dir, "MsfinderConsoleApp",
      " mssearch ",
      " -i ", input.dir,
      " -o ", input.dir,
      " -m ", param.file
    )
  )
  
  cat(" ----- post_mssearch", '\n')
  
}
