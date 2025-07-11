get.dia.msms <- function(
    fmz = 1109.3293,
    ppm = 20,
    rtrange = 331.929 + c(-2, 3),
    raw.file = "20240205-AC-2996_014_01.mzML"
) {
  
  rd <- Spectra::Spectra(c(raw.file, gsub("01.mzML", "02.mzML", raw.file)), backend = MsBackendMemory())
  sub <- Spectra::filterRt(rd, rtrange)
  sub.rt <- unlist(rtime(sub))
  prec.eic <- Spectra::filterMzRange(sub, fmz+(c(-0.01, 0.01)))
  d <- data.frame(
    mslev = unlist(msLevel(prec.eic)),
    rt = unlist(rtime(prec.eic)),
    intensity = as.vector(unlist(intensity(prec.eic)))
  )
  
  ms1.rt <- d$rt[which(d$mslev == 1)]
  ms2.rt <- d$rt[which(d$mslev == 2)]
  ms1.rt.int <- d$intensity[which(d$mslev == 1)]
  ms1.rt.int.inferred <- approx(ms1.rt, ms1.rt.int, xout = ms2.rt)$y

  cmpd.rt <- weighted.mean(d$rt[which(d$mslev == 1)], d$intensity[which(d$mslev == 1)])
  cmpd.rt.2 <- weighted.mean(d$rt[which(d$mslev == 1)], log2(d$intensity[which(d$mslev == 1)]))
  
  prod.spec <- Spectra::filterMsLevel(sub, 2L)
  prod.spectrum <- combineSpectra(prod.spec)
  prod.spectrum <- combinePeaks(prod.spec)
  intensity <- unlist(intensity(prod.spectrum[1]))
  mz <- unlist(mz(prod.spectrum[1]))
  mz <- mz[which(mz <= (fmz+40))]
  mz.min <- mz - (mz*ppm/1000000)
  mz.max <- mz + (mz*ppm/1000000)
  mz.range <- cbind(mz.min, mz.max)
  eics <- matrix(ncol = length(mz), nrow = length(ms2.rt))
  for(i in 1:length(mz)) {
    eic <- Spectra::filterMzRange(prod.spec, c(mz.min[i], mz.max[i]), keep = TRUE)
    tmp <- as.list(intensity(eic, keep = TRUE))
    tmp <- sapply(1:length(tmp), FUN = function(x) max(0, tmp[[x]], na.rm = TRUE))
    eics[,i] <- tmp
    ints <- apply(eics, 2, 'sum', na.rm = TRUE)
  }
  cor.r <- cor(ms1.rt.int.inferred, eics[,1:ncol(eics)], use = 'pairwise.complete.obs')[1,]
  plot(mz[which(cor.r > 0.8^5)], ints[which(cor.r > 0.8^5)], type = 'h')
  
  
}
