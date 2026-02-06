
# internal utils

.decToHex <- function (x) as.hexmode(as.numeric(x))

`%.nin%` <- function (x, table) match(x, table, nomatch = 0) == 0


.fm_num <- function(x, digits){
  
  format(
    x,
    digits = digits,
    nsmall = digits,
    scientific = FALSE,
    trim = TRUE
  )
  
}

.recycle <- function (...) {
  lst <- list(...)
  maxdim <- max(lengths(lst))
  res <- lapply(lst, rep, length.out = maxdim)
  attr(res, "maxdim") <- maxdim
  return(res)
}


.setDescToolsXOption <- function (...) {
  opts <- list(...)
  stopifnot(length(opts) > 0)
  names(opts) <- paste0("DescToolsX.", names(opts))
  options(opts)
  invisible(NULL)
}


.nDec <- function (x) {
  if (!inherits(x, "character")) 
    x <- as.character(x)
  res <- rep(0, length(x))
  x <- gsub(pattern = "[eE].+$", replacement = "", x = x)
  has_sep <- grep(gsub("1", "", format(1.1)), x, fixed = TRUE)
  res[has_sep] <- nchar(sub("^.+[.]", "", x))[has_sep]
  return(res)
}

.meanCI_raw <- function(x){
  
  x <- x[!is.na(x)]
  a <- qt(p = 0.025, df = length(x) - 1) * sd(x)/sqrt(length(x))
  
  return( c(est=mean(x), lci=mean(x) + a, uci=mean(x) - a) )
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


.combPairs <- function(x, y = NULL) {
  
  # Note: 
  # we have this in DescToolsX too, but DescToolsGraphics must stand on its own
  
  # returns a data.frame with all pairwise combinations of two variables
  if( missing(y)) {  # kein y vorhanden, use x only
    data.frame( t(combn(x, 2L)), stringsAsFactors=FALSE )
    
  } else {
    # if y is defined, all.x to all.y will be returned  
    expand.grid(x, y, stringsAsFactors=FALSE )
  }
}


