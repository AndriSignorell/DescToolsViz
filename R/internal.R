
# internal utils

.fm_num <- function(x, digits){
  
  format(
    x,
    digits = digits,
    nsmall = digits,
    scientific = FALSE,
    trim = TRUE
  )
  
}

.setDescToolsXOption <- function (...) {
  opts <- list(...)
  stopifnot(length(opts) > 0)
  names(opts) <- paste0("DescToolsX.", names(opts))
  options(opts)
}


.meanCI_raw <- function(x){
  
  x <- x[!is.na(x)]
  a <- qt(p = 0.025, df = length(x) - 1) * sd(x)/sqrt(length(x))
  
  return( c(est=mean(x), lci=mean(x) + a, uci=mean(x) - a) )
}


.binomCI_raw <- function(x, n, conf.level=0.95){
  setNamesX(c(est=x/n, prop.test(x, n, conf.level = conf.level, correct=FALSE)$conf.int),
             names=c("est", "lci", "uci"))
}




.linScale <- function (x, low = NULL, high = NULL, newlow = 0, newhigh = 1)  {
  
  x <- as.matrix(x)
  
  if(is.null(low)) {
    low <- apply(x, 2, min, na.rm=TRUE)
  } else {
    low <- rep(low, length.out=ncol(x))
  }
  if(is.null(high)) {
    high <- apply(x, 2, max, na.rm=TRUE)
  } else {
    high <- rep(high, length.out=ncol(x))
  }
  # do the recycling job
  newlow <- rep(newlow, length.out=ncol(x))
  newhigh <- rep(newhigh, length.out=ncol(x))
  
  xcntr <- (low * newhigh - high * newlow) / (newhigh - newlow)
  xscale <- (high - low) / (newhigh - newlow)
  
  return( scale(x, center = xcntr, scale = xscale))
  
}






