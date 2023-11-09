#' getClassyFire
#'
#' use classyfire web API to look up full ClassyFire hirarchy for each inchikey
#' @details The $inchikey slot is used to look up the 
#' 
#' @param ramclustObj ramclustR object to ClassyFy.  Must supply one of either ramclustObj or inchikey (see below)
#' @param inchikey vector of text inchikeys to ClassyFy.  Must supply one of either ramclustObj or inchikey.
#' @param get.all logical; if TRUE, when inchikey classyfire lookup fails, submits for classyfication.  Can be slow. max.wait (below) sets max time to spend on each compound before moving on. default = FALSE.
#' @param max.wait  numeric; maximum time (seconds) to wait per compound when 'get.all' = TRUE.   
#' @param posts.per.minute  integer; a limit set when 'get.all' is true.  ClassyFire server accepts no more than 5 posts per minute when calculating new ClassyFire results.  Slows down submission process to keep server from denying access.  
#' @return returns a ramclustR object.  new dataframe in $classyfire slot with rows equal to number of compounds.  
#' @importFrom jsonlite fromJSON
#' @importFrom httr http_error
#' @concept ramclustR
#' @concept RAMClustR
#' @concept metabolomics
#' @concept mass spectrometry
#' @concept clustering
#' @concept feature
#' @concept MSFinder
#' @concept xcms
#' @concept classyFire
#' @author Corey Broeckling
#' @references Djoumbou Feunang Y, Eisner R, Knox C, Chepelev L, Hastings J, Owen G, Fahy E, Steinbeck C, Subramanian S, Bolton E, Greiner R, and Wishart DS. ClassyFire: Automated Chemical Classification With A Comprehensive, Computable Taxonomy. Journal of Cheminformatics, 2016, 8:61. DOI: 10.1186/s13321-016-0174-y

#' @export 

rc.cmpd.get.classyfire <- function (ramclustObj = NULL, inchikey = NULL, get.all = TRUE, 
                                    max.wait = 10, posts.per.minute = 5) 
{
  
  if(is.null(ramclustObj) & is.null(inchikey)) {
    stop("must supply ramclustObj or vector of inchikeys as input.  i.e. ramclustObj = RC; inchikey = c('MCPFEAJYKIXPQF-DXZAWUHFSA-N','GLZPCOQZEFWAFX-JXMROGBWSA-N')", '\n')
  }
  
  if(is.null(ramclustObj)) {
    ramclustObj <- list()
    ramclustObj$cmpd <- paste0("C", 1:length(inchikey))
    ramclustObj[['inchikey']] <- inchikey

  }

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The use of this function requires package 'httr'.")
  }
  
  params <- c(
    "ramclustObj" = ramclustObj, 
    "inchikey" = inchikey, 
    "get.all" = get.all, 
    "max.wait" = max.wait, 
    "posts.per.minute" = posts.per.minute
  )
  if (is.null(ramclustObj$inchikey)) {
    stop("no inchikey slot found, please 'annotate' first", 
         "\n")
  }
  if (get.all & is.null(ramclustObj$smiles)) {
    warning(" - getting smiles from inchikey", '\n')
    ramclustObj <- getSmilesInchi(ramclustObj = ramclustObj)
    # stop("obtaining new classyfication (get.all option) requires a smiles notation", 
    #      "\n", "and no smiles slot found in ramclustObj.  Please first run 'getSmilesInchi()'", 
    #      "\n")
  }
  if (any(names(ramclustObj) == "classyfire")) {
    redo <- TRUE} else {redo <- FALSE}
  if (!redo) {
    ramclustObj$classyfire <- data.frame(inchikey = rep(NA, 
                                                        length(ramclustObj$inchikey)), kingdom = rep(NA, 
                                                                                                     length(ramclustObj$inchikey)), superclass = rep(NA, 
                                                                                                                                                     length(ramclustObj$inchikey)), class = rep(NA, length(ramclustObj$inchikey)), 
                                         subclass = rep(NA, length(ramclustObj$inchikey)), 
                                         parent = rep(NA, length(ramclustObj$inchikey)), 
                                         description = rep(NA, length(ramclustObj$inchikey)))
  }
  
  
  url = "http://classyfire.wishartlab.com"
  
  ## check server
  out <- tryCatch(jsonlite::fromJSON(paste0(url, "/entities/", "RYYVLZVUVIJVGH-UHFFFAOYSA-N", 
                                            ".json")), error = function(y) {
                                              return(NA)
                                            })
  if(length(out) == 1) {
    if(is.na(out)) {
      stop("  classyfire server appears to be down", '\n')
    }
  }
                                              
  
  
  cat("performing inchikey based lookup:", '\n')
  for (i in 1:length(ramclustObj$inchikey)) {
    
    if (is.na(ramclustObj$inchikey[i])) {
      next
    }
    if (!is.na(ramclustObj$classyfire[i, "kingdom"])) {
      next
    }
    Sys.sleep(1/posts.per.minute)
    cat(i, " ")
    out <- tryCatch(jsonlite::fromJSON(paste0(url, "/entities/", ramclustObj$inchikey[i], 
                                              ".json")), error = function(y) {
                                                return(NA)
                                              })
    
    tryn <- 1 
    if(length(out) < 2 & tryn <= 3) {
      # cat("retry", i, '\n')
      tryn <- tryn + 1
      Sys.sleep(1)
      out <- tryCatch(jsonlite::fromJSON(paste0(url, "/entities/", ramclustObj$inchikey[i], 
                                                ".json")), error = function(y) {
                                                  return(NA)
                                                })
    }
    
    if (length(out) > 1) {
      a <- ramclustObj$inchikey[i]
      b <- out$kingdom$name
      if (is.null(b)) 
        b <- NA
      c <- out$superclass$name
      if (is.null(c)) 
        c <- NA
      d <- out$class$name
      if (is.null(d)) 
        d <- NA
      e <- out$subclass$name
      if (is.null(e)) 
        e <- NA
      f <- out$direct_parent$name
      if (is.null(f)) 
        f <- NA
      g <- out$description
      if (is.null(g)) 
        g <- NA
      ramclustObj$classyfire[i, ] <- c(a, b, c, d, e, 
                                       f, g)
      
      rm(out); rm(a); rm(b); rm(c); rm(d); rm(e); rm(f); rm(g); rm(tryn)
    }
  }
  if (get.all) {
    get.full <- which(
      !is.na(ramclustObj$inchikey) & is.na(ramclustObj$classyfire[,2])
    )
    cat("this will take some time - maximum of", posts.per.minute, "posts per minute", '\n')
    for (i in get.full) {
      
      cat(i, " ")
      if (!is.na(ramclustObj$smiles[i])) {
        params <- list(label = "ramclustR", query_input = ramclustObj$smiles[i], 
                       query_type = "STRUCTURE")
        submit <- httr::POST("http://classyfire.wishartlab.com/queries", 
                             body = params, encode = "json", httr::accept_json(), 
                             httr::add_headers(`Content-Type` = "application/json"))
        Sys.sleep(1)
        query_id <- jsonlite::fromJSON(httr::content(submit, 
                                                     "text"))
        
        if(any(names(query_id) == "error")) {
          if(query_id$error == "Limit exceeded") {
            Sys.sleep(60)
            params <- list(label = "ramclustR", query_input = ramclustObj$smiles[i], 
                           query_type = "STRUCTURE")
            submit <- httr::POST("http://classyfire.wishartlab.com/queries", 
                                 body = params, encode = "json", httr::accept_json(), 
                                 httr::add_headers(`Content-Type` = "application/json"))
            query_id <- jsonlite::fromJSON(httr::content(submit, 
                                                         "text"))
          }
        }
        
        
        if(any(names(query_id) == "status")) {
          if(query_id$status == "500") {
            cat(" failed", '\n')
            next
          }
        }
        
        out <- list()
        out$classification_status <- "not done"
        out$number_of_elements <- 0
        Sys.sleep(1)
        skiptonext <- FALSE
        time.a <- Sys.time()
        while (out$number_of_elements == 0) {
          Sys.sleep(1)
          
          if(httr::http_error(paste0("http://classyfire.wishartlab.com/queries/",  query = query_id$id, ".json"))) {
            cat(" not done", '\n')
            break
          }
          out <- tryCatch( 
            {
              out <- jsonlite::fromJSON(paste0("http://classyfire.wishartlab.com/queries/", query = query_id$id, ".json"))
            }, 
            error = function(y) {
              out <- list()
              out$classification_status <- "not done"
              out$number_of_elements <- 0
              out
            }
          )
          
          if (round(as.numeric(difftime(Sys.time(), 
                                        time.a, units = "secs")), 3) >= max.wait) {
            cat("timed out", "\n")
            skiptonext <- TRUE
            break
          }
        }
        if (out$number_of_elements == 0) {
          ramclustObj$classyfire[i, ] <- c(ramclustObj$inchikey[i], 
                                           rep(NA, 6))
          rm(out)
        } else {
          a <- out$entities$inchikey
          if(is.null(a)) {
            a <- ramclustObj$inchikey[i]
          } else {
            a <- gsub("InChIKey=", "", a)
          }
          b <- out$entities$kingdom$name
          if (is.null(b)) {
            b <- NA
            c <- NA
            d <- NA 
            e <- NA
            f <- NA
            g <- NA
          } else {
            c <- if(length(out$entities$superclass)>1) {out$entities$superclass$name} else {c <- NA}
            if (is.null(c)) {c <- NA}
            d <- if(length(out$entities$class)>1) {out$entities$class$name} else {d <- NA}
            if (is.null(d)) {d <- NA}
            e <- if(length(out$entities$subclass)>1) {out$entities$subclass$name} else {e <- NA}
            if (is.null(e)) {e <- NA}
            f <- if(length(out$entities$direct_parent)>1) {out$entities$direct_parent$name} else {f <- NA}
            if (is.null(f)) {f <- NA}
            g <- if(nchar(out$entities$description)>1) {out$entities$description} else {g <- NA}
            if (is.null(g)) {g <- NA}
          }
          cat(" done", '\n')
          ramclustObj$classyfire[i, ] <- c(a, b, c, 
                                           d, e, f, g)
          rm(out)
        }
      }
      Sys.sleep(ceiling(60/posts.per.minute))
    }
  }
  
  ramclustObj$history$classyfire <- paste(
    "Compounds were assigned to chemical ontogenies using the ClassyFire API (Djoumbou 2016)."
  )
  
  if(is.null(ramclustObj$params)) {ramclustObj$params <- list()}
  ramclustObj$params$rc.cmpd.get.classyfire <- params
  
  
  return(ramclustObj)
}



######################################################################
## get classyfire heirarchy for a given vector of classyfire chemonids

get.classyfire.local <- function(
    ramclustObj = NULL,
    chemontid = NULL,
    chemont.obo.location = NULL
) {
  
  if(!is.null(ramclustObj) & is.null(chemontid)) {
    chemontid <- ramclustObj$annotations.full$chemontid
  }
  
  if(is.null(chemontid)) {stop("no chemontids submitted", '\n')}
  if(is.null(chemont.obo.location)) {stop("no chemont.obo file specified", '\n')}
  
  classyfire <- ontologyIndex::get_ontology(
    file = chemont.obo.location,
    propagate_relationships = "is_a",
    extract_tags = "minimal",
    merge_equivalent_terms = TRUE
  )
  
  cmpd.classyfire <- data.frame(
    'superclass'     = rep(NA, 0),
    'class'          = rep(NA, 0),
    'subclass'       = rep(NA, 0),
    'direct.parent'  = rep(NA, 0)
  )
  
  for(i in 1:length(chemontid)) {
    
    if(
      nchar(ramclustObj$annotations.full[i,'chemontid'])==0 | ramclustObj$annotations.full[i,'chemontid'] == "NA"
    ) {
      out <- data.frame(
        'superclass'     = NA,
        'class'          = NA,
        'subclass'       = NA,
        'direct.parent'  = NA
      )
      cmpd.classyfire <- rbind(
        cmpd.classyfire,
        out
      )
    } else {
      classyfication <- as.vector(classyfire$name[unlist(classyfire$ancestors[chemontid[i]])][3:5])
      out <- data.frame(
        'superclass'     = classyfication[1],
        'class'          = classyfication[2],
        'subclass'       = classyfication[3],
        'direct.parent'  = classyfire$name[chemontid[i]]
      )
      cmpd.classyfire <- rbind(
        cmpd.classyfire,
        out
      )
      
    }
    if(nrow(cmpd.classyfire)>i) {stop("too many rows. On: ", i, '\n')}
  }
  
  cmpd.classyfire[which(is.na(cmpd.classyfire), arr.ind = TRUE)] <- 'unassigned'
  
  if(is.null(ramclustObj)) {
    return(cmpd.classyfire)} else {
      ramclustObj$annotations.full <- 
        data.frame(
          ramclustObj$annotations.full,
          cmpd.classyfire
        )
      ramclustObj$annotations.selected <- ramclustObj$annotations.full[which(ramclustObj$annotations.full$assigned),2:ncol(ramclustObj$annotations.full)]
      return(ramclustObj)
    }
  
  write.csv(ramclustObj$annotations.full, file = paste0("spectra/", "all.annotations.csv"), row.names = FALSE)
  write.csv(ramclustObj$annotations.selected, file = paste0("spectra/", "assigned.annotations.csv"), row.names = FALSE)
}




classyfire.sunburst.plot <-  function(
    ramclustObj = RC, 
    subset.cmpds = rep(TRUE, length(ramclustObj$ann)),
    out.file.name = NULL
){
  
  if(length(which(subset.cmpds)) == 0) {
    error("all subset.cmpds are set to FALSE", '\n')
  }
  
  require(dplyr)
  require(plotly)
  
  ## sunburst plot
  ## may be able to add more info with 'hover' function to add things like stats results, or compound description.
  ct <- ramclustObj$annotations.selected[,c("superclass", "class", "subclass", "direct.parent", "compound.name")]
  
  ct <- ct[subset.cmpds,]
  
  ct <- replace(ct, is.na(ct), "unassigned")
  for(i in 1:ncol(ct)) {
    ct[,i] <- gsub("-", "_", ct[,i])
  }
  d <- data.frame(
    "ids" = rep(NA, 0),
    "labels" = rep(NA, 0),
    "parents" = rep(NA, 0),
    "values"= rep(0, 0)
  )
  superclasses <- unique(ct[,"superclass"])
  for(i in 1:length(superclasses)) {
    superclass <- superclasses[i]
    ## add root node element
    ctsub <- ct[which(ct$superclass == superclass),]
    out <- data.frame(
      "ids" = superclass,
      "labels" = superclass,
      "parents" = "",
      "values" = nrow(ctsub)
    )
    d <- rbind(d, out)
    
    ## for all classes within superclass i
    classes <- unique(ctsub[,"class"])
    for(j in 1:length(classes)) {
      class <- classes[j]
      ctsub2 <- ctsub[which(ctsub$class == class),]
      if(nrow(ctsub2) == 0) next
      out <- data.frame(
        "ids" = paste0(c(superclass, class), collapse = "-"),
        "labels" = class,
        "parents" = superclass,
        "values" = nrow(ctsub2)
      )
      d <- rbind(d, out)
      
      ## for all subclasses within class j
      subclasses <- unique(ctsub2[,"subclass"])
      for(k in 1:length(subclasses)) {
        subclass <- subclasses[k]
        ctsub3 <- ctsub2[which(ctsub2$subclass == subclass),]
        if(nrow(ctsub3) == 0) next
        if(class == "unassigned" & subclass == "unassigned") next
        out <- data.frame(
          "ids" = paste0(c(class, subclass), collapse = "-"),
          "labels" = subclass,
          "parents" = paste0(c(superclass, class), collapse = "-"),
          "values" = nrow(ctsub3)
        )
        d <- rbind(d, out)
        
        # for all compounds within subclass k
        compounds <- unique(ctsub3[,"compound.name"])
        for(m in 1:length(compounds)) {
          compound <- compounds[m]
          ctsub5 <- ctsub3[which(ctsub3$compound.name == compound),]
          if(nrow(ctsub5) == 0) next
          if(subclass == "unassigned" & compound == "unassigned") next
          out <- data.frame(
            "ids" = paste0(c(subclass, compound), collapse = "-"),
            "labels" = compound,
            "parents" = paste0(c(class, subclass), collapse = "-"),
            "values" = nrow(ctsub5)
          )
          d <- rbind(d, out)
          
          # }
        }
      }
    }
  }
  fig2 <- plotly::plot_ly(d, 
                          ids = ~ids, 
                          labels = ~labels, 
                          parents = ~parents, 
                          values = ~values,
                          branchvalues = "total",
                          insidetextorientation='radial',
                          maxdepth=2,
                          type = 'sunburst')
  
  if(is.null(out.file.name)) {
    htmlwidgets::saveWidget(fig2, file=paste0( getwd(), "/spectra/annotation.classyfire.sunburst.html"), 
                            knitrOptions = list()) 
    write.csv(d, file = paste0( getwd(), "/spectra/annotation.classyfire.sunburst.input.csv"), row.names = FALSE)
  } else {
    htmlwidgets::saveWidget(fig2, file=paste0( getwd(), out.file.name, ".html"), 
                            knitrOptions = list()) 
    write.csv(d, file = paste0( getwd(), out.file.name, ".input.csv"), row.names = FALSE)
  }
}

