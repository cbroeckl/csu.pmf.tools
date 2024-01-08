
get.pubchem.sources <- function(url = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/sourcetable/all/CSV/?response_type=save&response_basename=PubChemDataSources_all') {
  sources <- read.csv(url)
}

get.pathway.sources <- function(url = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/sourcetable/all/CSV/?response_type=save&response_basename=PubChemDataSources_all') {
  sources <- read.csv(url)
  pathways <- sources[which(sources$Pathway.Count >0),]
  pw.sources <- unique(pathways$Source.Name)
  return(pw.sources)
}

get.pathways.from.sources <- function(
    sources = NULL) {
  
  for(i in 1:length(sources)) {
    url <- paste0(
      "https://pubchem.ncbi.nlm.nih.gov/sdq/sdqagent.cgi?infmt=json&outfmt=csv&query={%22download%22:%22*%22,%22collection%22:%22pathway%22,%22order%22:[%22relevancescore,desc%22],%22start%22:1,%22limit%22:10000000,%22downloadfilename%22:%22PubChem_pathway_text_", 
      gsub(" ", "%20", sources[i]),
      "%22,%22where%22:{%22ands%22:[{%22*%22:%22",
      gsub(" ", "%22},{%22*%22:%22", sources[i]),
      "%22},{%22source%22:%22^",
      gsub(" ", "%20", sources[i]),
      "$%22}]}}"
    )
    tmp <- read.csv(url)
    if(i == 1) {
      out <- tmp
    } else {
      out <- rbind(out, tmp)
    }
  }
  cat(" - retreived", 
      nrow(out), 
      "pathways from", 
      length(unique(out$source)), 
      "sources representing", 
      length(unique(out$taxid)), 
      "taxa.",
      '\n')
  return(out)
}

## remarkably slow and unlikey worth the effort
# reshape.pathway.table <- function(
    #     pathway.table = NULL
# ) {
#   all.cids <- strsplit(pathway.table$cids, "|", fixed = TRUE)
#   out <- data.frame(
#     'source' = vector(mode = "character", length = 0),
#     'accession' = vector(mode = "character", length = 0),
#     'name' = vector(mode = "character", length = 0),
#     'type' = vector(mode = "character", length = 0),
#     'url' = vector(mode = "character", length = 0),
#     'taxid' = vector(mode = "numeric", length = 0),
#     'taxname' = vector(mode = "character", length = 0),
#     'cid' = vector(mode = "numeric", length = 0)
#   )
#   
#   for(i in 1:length(all.cids)) {
#     
#     tmp <- data.frame(
#       'source' = rep(pathway.table$source[i], length(all.cids[[i]])),
#       'accession' = rep(pathway.table$pwacc[i], length(all.cids[[i]])),
#       'name' = rep(pathway.table$name[i], length(all.cids[[i]])),
#       'type' = rep(pathway.table$pwtype[i], length(all.cids[[i]])),
#       'url' = rep(pathway.table$url[i], length(all.cids[[i]])),
#       'taxid' = rep(as.numeric(pathway.table$taxid[i]), length(all.cids[[i]])),
#       'taxname' = rep(pathway.table$taxname[i], length(all.cids[[i]])),
#       'cid' = as.numeric(unlist(all.cids[[i]]))
#     )
#     
#     if(i == 1) {
#       out <- tmp
#     } else {
#       out <- rbind(out, tmp)
#     }
#   }
#   
#   return(out)
# }
# 
# pw.cids <- reshape.pathway.table(pathway.table = pws)

filter.pathway.taxonomy <- function(
    taxid = NULL,
    pathway.table = NULL,
    single.source = TRUE
) {
  
  keep <- which(pathway.table$taxid %in% taxid)
  if(length(keep) > 0) {
    out <- pathway.table[keep,]
  } else {
    out <- pathway.table[0,]
    warning('no pathways matched: available species include:', '\n')
    spp <- sort(unique(paste(pathway.table$taxname, "taxid =", pathway.table$taxid)))
    for(i in 1:length(spp)) {
      cat(spp[i], '\n')
    }
  }
  
  if(single.source) {
    sources <- sort(table(out$source), decreasing = TRUE)
    use <- names(sources)[1]
    out <- out[which(out$source == use),]
  }
  
  cat(" - retained", 
      nrow(out), 
      "pathways from", 
      length(unique(out$source)), 
      if(length(unique(out$source)) == 1) {"source:"} else {"sources:"}, 
      paste(unique(out$source)), 
      '\n'
  )
  
  return(out)
}


filter.pathway.conserved <- function(
    pathway.table = NULL,
    source = c("BioCyc", "PlantCyc")
) {
  
  keep <- which(pathway.table$pwtype == 'conserved')
  if(length(keep) > 0) {
    out <- pathway.table[keep,]
  } else {
    out <- pathway.table[0,]
    warning('no conserved pathways.', '\n')
  }
  
  if(!is.null(source)) {
    use <- which(out$source %in% source)
    if(length(use) > 0) {
      out <- out[use,]
      cat(" -", length(use), "conserved pathways retained", '\n')
    } else {
      out <- out[0,]
      cat(" - no conserved pathways retained.", '\n')
    }
  }
  
  cat(" - retained", 
      nrow(out), 
      "pathways from", 
      length(unique(out$source)), 
      if(length(unique(out$source)) == 1) {"source:"} else {"sources:"}, 
      paste(unique(out$source)), 
      '\n'
  )
  
  return(out)
}



# cid.pathway.lookup <- function(
    #     pathway.table = NULL,
#     cids = NULL
# ) {
#   
#   all.cids <- strsplit(pathway.table$cids, "|", fixed = TRUE)
#   for(i in 1:length(all.cids)) {
#     cid <- cids[i]
#     pw.match <- which(sapply(1:length(all.cids), FUN = function(x) {any(all.cids[[x]] == cid )}))
#     tmp <- data.frame(
#       cid = rep(cid, length(pw.match))
#     )
#     if(i == 1) {
#       # out <- 
#     }
#   }
#   
# }

get.cids.from.pathways <- function(
    pathway.table = NULL
) {
  suppressWarnings(
    out <- sort(as.numeric(unique(unlist(strsplit(pathway.table$cids, "|", fixed = TRUE)))))
  )
  return(out)
}

#' get.taxon.cids
#'
#' use pubchem rest to retreive pubchem CIDS known to be found in a given species.  NCBI taxid should be used as input.  i.e. Homo sapiens subsp. 'Denisova' is taxid 741158 
#' @details this function enables return of a list of pubchem CIDs which can be used for prioritizing annotations.  If a genus level taxid is selected, setting the sub.taxa.n option > 0 will return metabolites associated with that taxid and all (assuming n is large enough) subtaxa.  i.e. seting taxid to 9605 (genus = 'Homo') will return metabolites associated with Homo sapiens, Homo heidelbergensis, Homo sapiens subsp. 'Denisova', etc. 
#' @param taxid integer NCBI taxid for the taxon to search.  Must supply one of taxid OR taxstring - not both. can be a vector of integers. 
#' @param taxstring character.  Taxonomy string to search.  tested with Genus species and or Genus alone.   Must supply one of taxid OR taxstring - not both. can be a vector of character strings (genus names). 
#' @param sub.taxa.n integer value for the number of subtaxa to consider.  Note that if the sub.taxa.n value is less the the availabe number of subtaxa, only the first sub.taxa.n values, as reported by rentrez, are returned.  If you require specific subtaxa, you should call those taxids explicitly to ensure those results are returned.  
#' @param ncbi.api.key character. optional NCBI API key to enable 10 calls per second (3 by default).  Note that this function is not parallelized, but if you wish to parallelize you may require use of a key to take full advantage. 
#' @return returns a vector of integer pubchem cids.  
#' @author Corey Broeckling
#' 
#' @export 
#' 

pubchem.taxon.metabolome <- function(taxid = NULL, taxstring = NULL, sub.taxa.n = 100, ncbi.api.key = NULL) {
  
  ## confirm rentrez
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    stop("The use of this function requires package 'rentrez'.")
  }
  
  ## ensure proper input
  if(is.null(taxid) & is.null(taxstring)) {
    stop("you must submit a valid inter NCBI Taxonomy ID value (taxid) or taxonomy string (taxstring) for the taxon of interest.", '\n',
         " - i.e. taxid = 9606 for Homo sapiens ", '\n',
         " - or taxstring = 'Homo sapiens'", '\n',
         " - or taxstring = 'Homo'", '\n')
  }
  
  ## ensure no ambiguous request
  if(!is.null(taxid) & !is.null(taxstring)) {
    message("you must submit either a valid inter NCBI Taxonomy ID value (taxid) or taxonomy string (taxstring) for the taxon of interest.", '\n',
            " -  taxid will be used in the case that both are submitted.", '\n')
    taxstring <- NULL
  }
  
  ## convert taxstring to taxid
  if(is.null(taxid)) {
    taxid <- rentrez::entrez_search(db = "taxonomy", term = paste0(taxstring, "[All names]"), api_key = ncbi.api.key)
    if(length(taxid$ids) == 0) warning("No taxon matched: returning empty dataframe", '\n')
    if(length(taxid$ids) > 1) warning("More than one taxon matched - only the smallest taxid will be used", '\n')
    taxid <- taxid$ids[which.min(taxid$ids)]
  }
  
  ## if sub.taxa.n >0, get subtaxa taxid values
  if(sub.taxa.n > 0) {
    sub.taxid <- rentrez::entrez_search(db = "taxonomy", term = paste0("txid", taxid, "[Subtree]"), api_key = ncbi.api.key)
    sub.taxid <- as.integer(sub.taxid$ids)
    if(length(sub.taxid)> 0) {
      taxid <- sort(unique(c(taxid, sub.taxid)))
    }
  }
  
  ## cleanup connections to ensure no connections remain open
  closePubchemConnections <- function (desc.rem = "pubchem") {
    d <- showConnections(all = TRUE)
    desc <- d[,"description"]
    desc <- desc[grepl(desc.rem, desc)]
    set <- as.integer(as.numeric(names(desc)))
    if(length(set) > 0) {
      for (i in seq_along(set)) close(getConnection(set[i]))
    }
    gc()
    invisible()
  }
  closePubchemConnections()
  
  
  ## collect CIDS for all taxid values
  if(length(taxid) == 0)  {
    out <- data.frame(
      'cid' = vector(mode = 'integer', length = 0)
    )
  } else {
    all.cids <- vector(length = 0, mode = "integer")
    
    cat("retrieving metabolites for taxid: ", '\n')
    
    for(i in 1:length(taxid)) {
      cat(taxid[i], " ")
      cids <- vector(length = 0, mode = "integer")
      ## metabolites
      url.csv1 <-  paste0(
        "https://pubchem.ncbi.nlm.nih.gov/sdq/sdqagent.cgi?infmt=json&outfmt=csv&query={%22download%22:%22*%22,%22collection%22:%22consolidatedcompoundtaxonomy%22,%22where%22:{%22ands%22:[{%22taxid%22:%22",
        taxid[i],
        "%22},{%22srccmpdkind%22:%22Metabolite%22}]},%22order%22:[%22cid,asc%22],%22start%22:1,%22limit%22:10000000,%22downloadfilename%22:%22TaxID_",
        taxid[i],
        "_consolidatedcompoundtaxonomy%22}")
      d1 <- read.csv(url.csv1)
      if(any(colnames(d1) == "cid")) {
        cids <- c(cids, as.numeric(d1[which(!(d1[,"cid"] == "NULL")),"cid"]))
      }
      
      ## natural products
      url.csv2 <- paste0(
        "https://pubchem.ncbi.nlm.nih.gov/sdq/sdqagent.cgi?infmt=json&outfmt=csv&query={%22download%22:%22*%22,%22collection%22:%22consolidatedcompoundtaxonomy%22,%22where%22:{%22ands%22:[{%22taxid%22:%22",
        taxid[i],
        "%22},{%22srccmpdkind%22:%22Natural%20Product%22}]},%22order%22:[%22cid,asc%22],%22start%22:1,%22limit%22:10000000,%22downloadfilename%22:%22TaxID_",
        taxid[i],
        "_consolidatedcompoundtaxonomy%22}"
      )
      d2 <- read.csv(url.csv2)
      if(any(colnames(d2) == "cid")) {
        cids <- c(cids, as.numeric(d2[which(!(d2[,"cid"] == "NULL")),"cid"]))
      }
      
      ## metabolic pathway metabolites
      url.csv3 <- paste0(
        "https://pubchem.ncbi.nlm.nih.gov/assay/pcget.cgi?task=pathway_chemical&taxid=",
        taxid[i],
        "&start=1&limit=10000000&download=true&downloadfilename=TaxID_",
        taxid[i],
        "_pcget_pathway_chemical&infmt=json&outfmt=csv"
      )
      d3 <- read.csv(url.csv3)
      if(any(colnames(d3) == "cid")) {
        cids <- c(cids, as.numeric(d3[which(!(d3[,"cid"] == "NULL")),"cid"]))
      }
      
      ## glycans
      url.csv4 <- paste0( 
        "https://pubchem.ncbi.nlm.nih.gov/sdq/sdqagent.cgi?infmt=json&outfmt=csv&query={%22download%22:%22*%22,%22collection%22:%22glycosmos_glycan%22,%22where%22:{%22ands%22:[{%22taxid%22:%22",
        taxid[i],
        "%22}]},%22order%22:[%22relevancescore,desc%22],%22start%22:1,%22limit%22:10000000,%22downloadfilename%22:%22TaxID_",
        taxid[i],
        "_glycosmos_glycan%22}"
      )
      d4 <- read.csv(url.csv4)
      if(any(colnames(d4) == "cid")) {
        cids <- c(cids, as.numeric(d4[which(!(d4[,"cid"] == "NULL")),"cid"]))
      }
      
      cids <- unique(cids)
      all.cids <- sort(unique(c(all.cids, cids)))
    }
    
    out <-  all.cids
    
  }
  
  message(paste("retreived", length(out), "structures"), '\n')
  closePubchemConnections()
  return(out)
}

get.pubchem.plant.lipids <- function(
    mgdg = TRUE,
    dgdg = TRUE,
    sqdg = TRUE
) {
  
  cids <- vector(length = 0, mode = 'numeric')
  if(mgdg) {
    mgdgs <- read.csv(
      "https://pubchem.ncbi.nlm.nih.gov/sdq/sdqagent.cgi?infmt=json&outfmt=csv&query={%22download%22:%22*%22,%22collection%22:%22compound%22,%22order%22:[%22relevancescore,desc%22],%22start%22:1,%22limit%22:10000000,%22downloadfilename%22:%22PubChem_compound_text_MGDG(%22,%22where%22:{%22ands%22:[{%22*%22:%22MGDG(%22}]}}",
      header = TRUE, check.names = FALSE
    )
    mgdgs <- unique(mgdgs[,"cid"])
    cat(" - found", length(mgdgs), "MGDGs", '\n')
    cids <- c(cids, mgdgs)
  }
  
  if(dgdg) {
    dgdgs <- read.csv(
      "https://pubchem.ncbi.nlm.nih.gov/sdq/sdqagent.cgi?infmt=json&outfmt=csv&query={%22download%22:%22*%22,%22collection%22:%22compound%22,%22order%22:[%22relevancescore,desc%22],%22start%22:1,%22limit%22:10000000,%22downloadfilename%22:%22PubChem_compound_text_DGDG(%22,%22where%22:{%22ands%22:[{%22*%22:%22DGDG(%22}]}}",
      header = TRUE, check.names = FALSE
    )
    dgdgs <- unique(dgdgs[,"cid"])
    cat(" - found", length(dgdgs), "DGDGs", '\n')
    cids <- c(cids, dgdgs)
  }
  
  if(sqdg) {
    sqdgs <- read.csv(
      "https://pubchem.ncbi.nlm.nih.gov/sdq/sdqagent.cgi?infmt=json&outfmt=csv&query={%22download%22:%22*%22,%22collection%22:%22compound%22,%22order%22:[%22relevancescore,desc%22],%22start%22:1,%22limit%22:10000000,%22downloadfilename%22:%22PubChem_compound_text_SQDG(%22,%22where%22:{%22ands%22:[{%22*%22:%22SQDG(%22}]}}",
      header = TRUE, check.names = FALSE
    )
    sqdgs <- unique(sqdgs[,"cid"])
    cat(" - found", length(sqdgs), "SQDGs", '\n')
    cids <- c(cids, sqdgs)
  }
  
  cids <- sort(unique(cids))
  cat(" - found", length(cids), "plant lipids", '\n')
  return(cids)
}

get.pubchem.lipidmaps.cids <- function(
    class = NULL,
    subclass = NULL
) {
  
  # retreive from lipid maps directly
  # out <- read.delim("https://www.lipidmaps.org/rest/compound/lm_id/LM/all/download", skip = 1)
  # if(!is.null(class)) {
  #   keep <- vector(length = 0, mode = )
  #   
  # }
  
  
  ## can get from pubchem
  out <- read.csv(
    'https://pubchem.ncbi.nlm.nih.gov/sdq/sdqagent.cgi?infmt=json&outfmt=csv&query={%22download%22:%22*%22,%22collection%22:%22substance%22,%22order%22:[%22relevancescore,desc%22],%22start%22:1,%22limit%22:10000000,%22downloadfilename%22:%22PubChem_substance_cache_yY5uPf9Hmvut0ZjIGrDR42be1L6KMirYUP0xlEvsI5VL9R8%22,%22where%22:{%22ands%22:[{%22input%22:{%22type%22:%22netcachekey%22,%22idtype%22:%22sid%22,%22key%22:%22yY5uPf9Hmvut0ZjIGrDR42be1L6KMirYUP0xlEvsI5VL9R8%22}}]}}'
  )
  
  if(!is.null(class)) {
    keep <- vector(length = 0, mode = 'numeric')
    for(i in 1:length(class)) {
      keep <- c(keep, grep(class[i], out$sidextid))
    }
    out <- out[keep,]
  }
  
  
  if(!is.null(subclass)) {
    keep <- vector(length = 0, mode = 'numeric')
    for(i in 1:length(subclass)) {
      keep <- c(keep, grep(subclass[i], out$sidextid))
    }
    out <- out[keep,]
  }
  
  return(out$cid)
}

# all.sources <- get.pubchem.sources()
# pw.sources <- get.pathway.sources()
# pws <- get.pathways.from.sources(sources = pw.sources)
# 
# a.thaliana.pathways <- filter.pathway.taxonomy(taxid = 3702, pathway.table = pws, single.source = TRUE)
# a.thaliana.cids <- pubchem.taxon.metabolome(taxid = 3702)
# 
# conserved.pathways <- filter.pathway.conserved(pathway.table = pws)
# conserved.cids <- get.cids.from.pathways(pathway.table = conserved.pathways)
# 
# plant.lipids <- get.pubchem.plant.lipids()
# lipid.maps.cids <- get.pubchem.lipidmaps.cids(class = 'PR02', subclass = c("PR0201", "PR0202"))
# 
# all.cids <- sort(unique(c(a.thaliana.cids, conserved.cids)))

