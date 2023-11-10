#' lib.functions
#' @param mz.min numeric. lower m/z range limit for filter.by.mass function
#' @param mz.max numeric. upper m/z range limit for filter.by.mass function
#' @param min.int numeric. remove.low.intensity.signals function. value between 0 and 1 setting the threshold for the lower limit of intensity relative to the base peak intensity. 
#' @author Corey Broeckling
#' @concept spectral matching
#' @export  

normalize.intensity <- function(x, ...) {
  maxint <- max(x[, 2], na.rm = TRUE)
  x[, 2] <- 1000 * x[, 2] / maxint
  x
}

round.masses <- function(x, ...) {
  x[, 1] <- round(x[, 1])
  xdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
}

remove.low.intensity.signals <- function(x, min.int, ...) {
  x > max(x[,2], na.rm = TRUE) * min.int
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
