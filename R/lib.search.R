## Spectral searching of LC or GC MS data using R based tools
lib.search <- function(
    prod.tol.ppm = NULL,
    prec.tol.ppm = NULL,
    r.sigma = 50,
    exp.spectra = NULL,
    exp.rt = NULL,
    exp.ri = NULL,
    exp.precursor = NULL,
    lib.spectra = NULL,
    lib.rt = NULL,
    lib.ri = NULL,
    lib.precursor = NULL,
    n.cores = 2,
    block.size = 100,
    min.score = 0.5
) {
  
  library(Spectra)
  library(MsBackendMsp)
  library(xcms)
  library(doParallel)
  # library(MSnbase)
  
  
  
  ## check length of experimental spectra is equal to any other experimental lengths and is numeric
  sp.len <- length(exp.spectra)
  if(!is.null(exp.rt)) {
    if(length(exp.rt) != sp.len) stop('exp.rt must be equal to length of exp.spectra')
    if(!is.numeric(exp.rt)) stop('exp.rt must be numeric')
  }
  if(!is.null(exp.ri)) {
    if(length(exp.ri) != sp.len) stop('exp.ri must be equal to length of exp.spectra')
    if(!is.numeric(exp.ri)) stop('exp.ri must be numeric')
  }
  if(!is.null(exp.precursor)) {
    if(length(exp.precursor) != sp.len) stop('exp.precursor must be equal to length of exp.spectra')
    if(!is.numeric(exp.precursor)) stop('exp.precursor must be numeric')
  }
  
  ## and for library
  sp.len <- length(lib.spectra)
  if(!is.null(lib.rt)) {
    if(length(lib.rt) != sp.len) stop('lib.rt must be equal to length of lib.spectra')
    if(!is.numeric(lib.rt)) stop('lib.rt must be numeric')
  }
  if(!is.null(lib.ri)) {
    if(length(lib.ri) != sp.len) stop('lib.ri must be equal to length of lib.spectra')
    if(!is.numeric(lib.ri)) stop('lib.ri must be numeric')
  }
  if(!is.null(lib.precursor)) {
    if(length(lib.precursor) != sp.len) stop('lib.precursor must be equal to length of lib.spectra')
    if(!is.numeric(lib.precursor)) stop('lib.precursor must be numeric')
  }
  
  ## only one of rt and ri should be used, and if used, needs to be present for both library and experimental spectra
  if(!is.null(exp.rt) & !is.null(exp.ri)) stop('please provide only one of experimental retention time or index')
  if(!is.null(lib.rt) & !is.null(lib.ri)) stop('please provide only one of library retention time or index')
  if(is.null(exp.ri)  != is.null(lib.ri)) stop('please provide either both exp.ri and lib.ri or neither')
  if(is.null(exp.rt)  != is.null(lib.rt)) stop('please provide either both exp.rt and lib.rt or neither')
  
  ## and same for precursor
  if(is.null(exp.precursor)  != is.null(lib.precursor)) stop('please provide either both exp.precursor and lib.precursor or neither')
  
  ## set up parallel processing to expedite process
  ## benchmarking suggests optimal size of around 100 library spectra to search against
  ## for an experimental library of 1500 compounds
  ## register(bpstart(SnowParam(workers = n.cores)))
  ## using doParallel, but bioC parallel would also be a good option. 
  doParallel::registerDoParallel(n.cores)
  lib.n <- nist.n <- 1:length(lib.spectra)
  
  ## break library into chunks
  if(length(lib.n) > (2*block.size)) {
    chunks <- split(lib.n, cut(lib.n, round((length(lib.n)/block.size)), labels = FALSE))
  } else {
    chunks <- list()
    chunks[[1]] <- lib.n
  }
  
  ## perform spectral similarity calculations
  # pb = txtProgressBar(min = 1, max = length(chunks), initial = 1) 
  # pb.sim.fun <- function(y, ymax) {
  #   pb <- txtProgressBar(min = 1, max = y, style = 3)
  #   Spectra::compareSpectra(exp.spectra, lib.spectra[chunks[[y]]], ms2_tolerance_in_ppm = prod.tol.ppm)
  #   setTxtProgressBar(pb, y)
  #   flush.console()
  # }
  cat("spectral similarity calculations:", '\n')
  out.specsim <- foreach(y = 1:length(chunks)) %dopar% {
    # pb.sim.fun(y = y, ymax = length(chunks))
    Spectra::compareSpectra(exp.spectra, lib.spectra[chunks[[y]]], ms2_tolerance_in_ppm = prod.tol.ppm)
    # flush.console()
    # cat(y)
    # setTxtProgressBar(pb, y)
  }
  # cat('\n')
  # close(pb)
  
  out.specsim <- do.call(cbind, out.specsim)
  
  ## optionally perform retention index similarity
  if(!is.null(exp.ri)) {
    out.r <-  foreach(y = 1:length(chunks)) %dopar% {
      round(exp(
        -(
          abs(outer(exp.ri, 
                    lib.ri[chunks[[y]]], 
                    FUN = "-"
          )
          ) ^ 2) / (2 * (r.sigma ^ 2))), digits = 4)
    }
    out.r <- do.call(cbind, out.r)
    retention.index <- TRUE
  }

  ## optionally perform retention time similarity
  if(!is.null(exp.rt)) {
    out.r <-  foreach(y = 1:length(chunks)) %dopar% {
      round(exp(
        -(
          abs(outer(exp.rt, 
                    lib.rt[chunks[[y]]], 
                    FUN = "-"
          )
          ) ^ 2) / (2 * (r.sigma ^ 2))), digits = 4)
    }
    out.r <- do.call(cbind, out.r)
    retention.index <- FALSE
  }
  
  ## optionally perform precursor similarity acceptance criterion
  if(!is.null(exp.precursor)) {
    out.p <-  foreach(y = 1:length(chunks)) %dopar% {
      round(outer(exp.precursor, lib.precursor[chunks[[y]]], '-'), digits = 4)
    }
    out.p <- do.call(cbind, out.p)
  }

  
  ## prepare output
  ## get all spectral matches above min.score threshold
  out <- which(out.specsim >= min.score, arr.ind = TRUE)
  dimnames(out)[[2]] <- c("experimental.index", "library.index")
  out <- cbind(out, 
               "spectral.similarity" = out.specsim[out])
  
  ## if retention time/index data is compared, append this to above
  if(any(ls() == "out.r")) {
    out <- cbind(
      out, 
      out.r[out[,1:2]]
      )
    if(retention.index) {
      dimnames(out)[[2]][ncol(out)] <- "retention.index.similarity"
    } else {
      dimnames(out)[[2]][ncol(out)] <- "retention.time.similarity"
    }
    out <- cbind(
      out, 
      "total.score" = out[,"spectral.similarity"] * out[,ncol(out)]
    )
  }
  
  ## if precursor similarity
  if(any(ls() == "out.p")) {
    out <- cbind(
      out, 
      "exp.mass" = out.p[out[,1:2]], 
      "lib.mass" = exp.precursor[out[,1]],
      "precursor.error.ppm" = round((out.p[out[,1:2]] / exp.precursor[out[,1]]), digits = 2)
    )
  }
  
  return(out)
  
}

# my.scores <- lib.search(
#   prod.tol.ppm = 30,
#   prec.tol.ppm = 500,
#   r.sigma = 50,
#   exp.spectra = mona.lc.p[1:200],
#   exp.rt = NULL,
#   exp.ri = NULL,
#   exp.precursor = prec.mz[1:200],
#   lib.spectra = mona.lc.p[1:200],
#   lib.rt = NULL,
#   lib.ri = NULL,
#   lib.precursor = prec.mz[1:200],
#   n.cores = 2,
#   block.size = 100,
#   min.score = 0.5
# )
# head(my.scores)