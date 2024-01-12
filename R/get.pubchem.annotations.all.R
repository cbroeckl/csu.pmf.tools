#' get.pubchem.annotations.all
#'
#' use PUG view to retrieve, for example, all pubchem values for 'Dissociation Constants', or other annotations.  all annotations found: https://pubchem.ncbi.nlm.nih.gov/rest/pug/annotations/headings/JSON
#' @details useful for moving from chemical name to digital structure representation. greek letters are assumed to be 'UTF-8' encoded, and are converted to latin text before searching.   if you are reading in your compound name list, do so with 'encoding' set to 'UTF-8'. 
#' @param annotation character.  must be an exact text match (case sensitive) to values as reported here: https://pubchem.ncbi.nlm.nih.gov/rest/pug/annotations/headings/JSON
#' @return returns a data frame with pubchem CID, compound name, data source, and returned value.  All data except CID are returned as character (text) as many values are a mix of text and numeric characters. 
#' @author Corey Broeckling
#' 
#' @export 
#' 

get.pubchem.annotations.all <- function(
    annotation = ""
) {
  
  all.annotations <- jsonlite::parse_json(readLines('https://pubchem.ncbi.nlm.nih.gov/rest/pug/annotations/headings/JSON'))
  Headings <- sapply(1:length(all.annotations$InformationList$Annotation), FUN = function(x) (all.annotations$InformationList$Annotation[[x]]$Heading))
  Type     <- sapply(1:length(all.annotations$InformationList$Annotation), FUN = function(x) (all.annotations$InformationList$Annotation[[x]]$Type ))
  
  if(!any(annotation == Headings)) {
    stop('This annotation heading unavailable. Available headings can be found at https://pubchem.ncbi.nlm.nih.gov/rest/pug/annotations/headings/JSON ')
  }
  
  annotation <- gsub(" ", "+", annotation)
  
  d <- jsonlite::parse_json(readLines(paste0('https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/annotations/heading/JSON?heading=', annotation)))
  total.pages <- d$Annotations$TotalPages
  out <- data.frame(
    'cid' = vector(length = 0, mode = 'numeric'),
    'name' = vector(length = 0, mode = 'character'),
    'source' = vector(length = 0, mode = 'character'),
    'value' = vector(length = 0, mode = 'character')
  )
  for(i in 1:length(d$Annotations$Annotation)) {
    for(j in 1:length(d$Annotations$Annotation[[i]]$Data)) {
      tmp <- data.frame(
        'cid' =     if(is.null(d$Annotations$Annotation[[i]]$LinkedRecords$CID[[1]][1])) {NA} else {as.numeric(d$Annotations$Annotation[[i]]$LinkedRecords$CID[[1]][1])} ,
        'name' =    if(is.null(d$Annotations$Annotation[[i]]$Name)) {NA} else {d$Annotations$Annotation[[i]]$Name},
        'source' =  if(is.null(d$Annotations$Annotation[[i]]$Data[[j]]$Reference[[1]][1])) {NA} else {d$Annotations$Annotation[[i]]$Data[[j]]$Reference[[1]][1]}, 
        'value' =   if(is.null(d$Annotations$Annotation[[i]]$Data[[j]]$Value$StringWithMarkup[[1]]$String[1])) {NA} else {d$Annotations$Annotation[[i]]$Data[[j]]$Value$StringWithMarkup[[1]]$String[1]}
      )
      out <- rbind(out, tmp)
      rm(tmp)
    }
  }
  
  if(total.pages > 1) {
    for(x in 2:total.pages) {
      d <- jsonlite::parse_json(readLines(paste0('https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/annotations/heading/JSON?heading=', annotation, "&page=", x)))
      for(i in 1:length(d$Annotations$Annotation)) {
        for(j in 1:length(d$Annotations$Annotation[[i]]$Data)) {
          tmp <- data.frame(
            'cid' =     if(is.null(d$Annotations$Annotation[[i]]$LinkedRecords$CID[[1]][1])) {NA} else {as.numeric(d$Annotations$Annotation[[i]]$LinkedRecords$CID[[1]][1])} ,
            'name' =    if(is.null(d$Annotations$Annotation[[i]]$Name)) {NA} else {d$Annotations$Annotation[[i]]$Name},
            'source' =  if(is.null(d$Annotations$Annotation[[i]]$Data[[j]]$Reference[[1]][1])) {NA} else {d$Annotations$Annotation[[i]]$Data[[j]]$Reference[[1]][1]}, 
            'value' =   if(is.null(d$Annotations$Annotation[[i]]$Data[[j]]$Value$StringWithMarkup[[1]]$String[1])) {NA} else {d$Annotations$Annotation[[i]]$Data[[j]]$Value$StringWithMarkup[[1]]$String[1]}
          )
          out <- rbind(out, tmp)
          rm(tmp)
        }
      }
    }
  }
  
  return(out)
}

# dis.constants <- get.pubchem.annotations.all(annotation = "Dissociation Constants")

