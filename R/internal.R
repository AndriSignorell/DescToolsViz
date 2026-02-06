
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


