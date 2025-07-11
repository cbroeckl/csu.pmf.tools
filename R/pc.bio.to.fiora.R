#' pc.bio.to.fiora
#'
#' utilizes downloaded and properly formatted local pubchem data created by 'get.pubchem.ftp' function
#' @details utilizes downloaded and properly formatted local pubchem data created by 'get.pubchem.ftp' function
#' @param cid vector of integers.  should be subset of all CIDs found in the $cid slot of the pc.data.frame.
#' @param pc.directory directory from which to load pubchem .Rdata files
#' @param bio.sources vector of source names from which to extract pubchem CIDs.  all can be found here: https://pubchem.ncbi.nlm.nih.gov/sources/.  deafults to c("Metabolomics Workbench", "Human Metabolome Database (HMDB)", "ChEBI", "LIPID MAPS",  "MassBank of North America (MoNA)")
#' @param use.pathways logical.  should all CIDs from any biological pathway data be used? 
#' @param use.taxid logical.  should all CIDs associated with a taxonomic identifier (taxid) be used?  
#' @param use.parent.cid logical. should CIDs be replaced with parent CIDs? 
#' @param get.properties logical. if TRUE, will return pubchem properties.  Either a short list of readily available properties including by default XLogP, TPSA, HBondDonorCount and HBondAcceptorCount, or a comprehensive list, if all.props = TRUE.
#' @param threads integer. how many threads to use when calculating rcdk properties.  parallel processing via DoParallel and foreach packages.  
#' @return a data frame containing pubchem CID, title, formula, monoisotopic molecular weight, inchikey, smiles, cas, optionally pubchem properties
#' @author Corey Broeckling
#' 
#' @export 
#' 
#' @param remove.inorganics logical.  if TRUE, removes all molecules which have exactly one element, or have two or more and have zero carbon or oxygen. In this way, phosphorus is removed, but phosphate stays.


pc.bio.to.fiora <- function(
    cid = vector(length = 0, mode = 'integer'),
    pc.data.frame = NULL,
    pc.directory = "R:/RSTOR-PMF/Software/db/met.db/20241216/",
    use.parent.cid = FALSE,
    precursor.type = c("[M+H]+"),
    ce = c(10, 20, 40),
    instrument.type = 'HCD',
    remove.inorganics = TRUE,
    remove.salts = TRUE,
    remove.redundant = TRUE,
    remove.invalid.smiles = FALSE,
    mass.range = c(50, 2000),
    subset.cmpds = 1:nrow(out),
    batch.output.size = 5000, ## in number of rows per output file
    output.filename = paste0(pc.directory, "fiora.input.csv")
    
) {
  
  if(!is.null(pc.data.frame)) {
    pc.data.frame <- data.table::data.table(pc.data.frame)
    if(!is.null(cid)) {
      m <- match(cid, pc.data.frame$cid)
      pc.data.frame <- pc.data.frame[m,]
    }
  } else {
    if(!file.exists(paste0(pc.directory, "/pc.bio.Rdata"))) {stop("file ", paste0(pc.directory, "/pc.bio.Rdata"), "missing, please run 'build.pubchgem.bio' function first", '\n')}
    objs <- list()
    load(paste0(pc.directory, "/pc.bio.Rdata"))
    pc.data.frame <- pc.bio
    rm(pc.bio)
    gc()
    
  }
  
  pc.data.frame <- unique(pc.data.frame[,1:ncol(pc.data.frame)])
  
  if(remove.inorganics) {
    keep <- sapply(1:nrow(pc.data.frame), FUN = function(x) {
      tmp <- CHNOSZ::makeup(pc.data.frame$formula[x])
      ret <- TRUE
      if(length(tmp) == 1) {
        ret <- FALSE
      }
      if(all(!(c("C", "O") %in% names(tmp)))){
        ret <- FALSE
      }
      return(ret)
    }
    )
    pc.data.frame <- pc.data.frame[keep,]
  }

  if(remove.salts) {
    keep <- !grepl(".", pc.data.frame$smiles, fixed = TRUE)
    pc.data.frame <- pc.data.frame[keep,]
  }
  
  if(!is.null(mass.range)) {
    pc.data.frame <- pc.data.frame[which(pc.data.frame$monoisotopic.mass >= mass.range[1] & pc.data.frame$monoisotopic.mass <= mass.range[2]),]
  }
  
  if(remove.invalid.smiles) {
    is.valid <- sapply(1:nrow(pc.data.frame), FUN = function(x) {
      webchem::is.smiles(pc.data.frame$smiles[x])
    }
    )
    pc.data.frame <- pc.data.frame[is.valid,]
    cat("removed", (length(which(!is.valid))), "invalid smiles", '\n') 
  }

  out <- data.frame(
    "Name" = vector(length = 0, mode = 'character'),
    "SMILES" = vector(length = 0, mode = 'character'),
    "Precursor_type" = vector(length = 0, mode = 'character'),
    "CE" = vector(length = 0, mode = 'numeric'),
    "Instrument_type" = vector(length = 0, mode = 'character')
  )
  
  for(i in 1:length(precursor.type)) {
    for(j in 1:length(ce)) {
      for(k in 1:length(instrument.type)) {
        tmp <- data.frame(
          "Name" = pc.data.frame$name,
          "SMILES" = pc.data.frame$smiles,
          "Precursor_type" = precursor.type[i],
          "CE" = ce[j],
          "Instrument_type" = instrument.type[k]
        )
        out <- rbind(out, tmp)
      }
    }
  }
  
  if(remove.redundant) {
    out <- unique(out[,1:ncol(out)])
  }

  if(!is.null(batch.output.size)) {write.csv(out[subset.cmpds,], file = output.filename, row.names = FALSE)}
  if(is.numeric(batch.output.size)) {
    spl <- split(1:nrow(out), ceiling(seq_along((1:nrow(out)))/batch.output.size))
    app <- formatC(1:length(spl), digits = nchar(length(spl)), flag = 0)
    for(i in 1:length(spl)) {
      write.csv(out[spl[[i]],], file = gsub(".csv", paste0("_", app[i], ".csv"), output.filename), row.names = FALSE)
    }
  }
  
  # bat <- paste0("fiora-predict -i fiora.input_", app, ".csv", " -o fiora.output_", app, ".mgf --annotation", sep = "")
  # sink(paste0(pc.directory, "script.bat"))
  # cat(bat, sep = '\n')
  # sink()
  
  
  cat("completed fiora ouptut with ", nrow(out), "rows: ", output.filename, '\n')
}

# pc.bio.to.fiora()

# create bat file


# pc.directory = "R:/RSTOR-PMF/Software/db/met.db/20241216/fiora/"
# do <- formatC(c(141,159,207:377), flag = 0, width = 4)
# out <- vector(length = 0, mode = "character")
# for(i in 1:length(do)) {
#   d <- read.csv(paste0(pc.directory, "/fiora.input_", do[i], ".csv", sep = ""))
#   half <- round(nrow(d)/2)
#   da <- d[1:half,]
#   db <- d[(half+1):nrow(d),]
#   write.csv(da, paste0(pc.directory, "/fiora.input_", do[i], "a.csv", sep = ""), row.names = FALSE)
#   write.csv(db, paste0(pc.directory, "/fiora.input_", do[i], "b.csv", sep = ""), row.names = FALSE)
#   out <- c(out, paste0("fiora-predict -i fiora/data/fiora.input_", do[i], "a.csv", " -o fiora/data/fiora.output_", do[i], "a.mgf --annotation", sep = ""))
#   out <- c(out, paste0("fiora-predict -i fiora/data/fiora.input_", do[i], "b.csv", " -o fiora/data/fiora.output_", do[i], "b.mgf --annotation", sep = ""))
# }
# 
# sink(paste0(pc.directory, "/script3.bat"))
# cat(out, sep = '\n')
# sink()
