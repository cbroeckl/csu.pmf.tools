normalize.intensity <- function(x, ...) {
  maxint <- max(x[, 2], na.rm = TRUE)
  x[, 2] <- 1000 * x[, 2] / maxint
  x
}

round.masses <- function(x, ...) {
  x[, 1] <- round(x[, 1])
  x
}

remove.low.intensity.signals <- function(x, ...) {
  x > max(x[,2], na.rm = TRUE) * 0.02
  x
}

filter.by.mass <- function(x, mz.min, mz.max, ...) {
  x.orig <- x
  x <- x[which(x[,1] >= mz.min & x[,1] <= mz.max),,drop = FALSE]
  if(nrow(x) < 2) {
    x.orig
  } else {
    x
  }
}
