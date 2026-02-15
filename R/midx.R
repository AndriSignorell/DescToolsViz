
#' Find the Midpoints of a Numeric Vector 
#' 
#' Calculate the midpoints of a sequence of numbers. This is e.g. useful for
#' labelling stacked barplots. 
#' 
#' 
#' @param x the numeric vector 
#' @param incl.zero should zero be appended to x before proceeding? If
#' \code{TRUE} the first value will be one half of the first value of x.
#' Default is \code{FALSE}.
#' @param cumulate should the result be calculated as cumulative sum? Default
#' is \code{FALSE}.
#' @return numeric vector with the calculated midpoins 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link[DescToolsX]{moveAvg}}
#' @keywords univar
#' @examples
#' 
#' x <- c(1, 3, 6, 7)
#' 
#' midx(x)
#' midx(x, incl.zero = TRUE)
#' midx(x, incl.zero = TRUE, cumulate = TRUE)
#' 
#' # see also as alternative:
#' # head(moveAvg(c(0, x), order = 2, align = "l"), n = -1)
#' 
#' tab <- matrix(c(401,216,221,254,259,169), nrow=2, byrow=TRUE)
#' b <- barplot(tab, beside = FALSE, horiz=TRUE)
#' 
#' x <- t(apply(tab, 2, midx, incl.zero=TRUE, cumulate=TRUE))
#' text(tab, x=x, y=b, col="red")
#' 


#' @export
midx <- function(x, incl.zero = FALSE, cumulate = FALSE){
  if(incl.zero) x <- c(0, x)
  res <- filter(x, rep(1/2,2))
  res <-  res[-length(res)]
  if(cumulate) res <- cumsum(res)
  return(res)
}

