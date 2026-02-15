
#' Plot Missing Data 
#' 
#' Takes a data frame and displays the location of missing data. The missings
#' can be clustered and be displayed together. 
#' 
#' A graphical display of the position of the missings can be help to detect
#' dependencies or patterns within the missings. 
#' 
#' @param x a data.frame to be analysed. 
#' @param col the colour of the missings. 
#' @param bg the background colour of the plot. 
#' @param clust logical, defining if the missings should be clustered. Default
#' is \code{FALSE}. 
#' @param main the main title. 
#' @param \dots the dots are passed to \code{\link{plot}}. 
#' @return if clust is set to TRUE, the new order will be returned invisibly.
#' @author Andri Signorell <andri@@signorell.net>, following an idea of Henk
#' Harmsen <henk@@carbonmetrics.com> 
#' @seealso \code{\link{hclust}}, \code{\link{countCompCases}} 
#' @keywords hplot
#' @examples
#' 
#' plotMiss(airquality, main="Missing data (in orignal order)")
#' plotMiss(airquality, main="Missing data (clustered)", clust=TRUE)
#'


#' @export 
plotMiss <- function(x, col = "deeppink4", bg=fade("navajowhite3", 0.3), clust=FALSE,
                     main = NULL, ...){
  
  if(is.null(main)) main <- deparse(substitute(x))
  
  x <- as.data.frame(x)
  if(ncol(x) > 1)
    x <- x[, ncol(x):1]
  n <- ncol(x)
  
  inches_to_lines <- (par("mar") / par("mai") )[1]  # 5
  lab.width <- max(strwidth(colnames(x), units="inches")) * inches_to_lines
  ymar <- lab.width + 3
  
  canvas(xlim=c(0, nrow(x)+1), ylim=c(0, n), asp=NA, xpd=TRUE, mar = c(5.1, ymar, 5.1, 5.1)
         , main=main, ...)
  
  usr <- par("usr") # set background color lightgrey
  rect(xleft=0, ybottom=usr[3], xright=nrow(x)+1, ytop=usr[4], col=bg, border=NA)
  axis(side = 1)
  
  missingIndex <- as.matrix(is.na(x))
  miss <- apply(missingIndex, 2, sum)
  
  if(clust){
    orderIndex <- order.dendrogram(as.dendrogram(hclust(dist(missingIndex * 1), method = "mcquitty")))
    missingIndex <- missingIndex[orderIndex, ]
    res <- orderIndex
  } else {
    res <- NULL
  }
  
  sapply(1:ncol(missingIndex), function(i){
    xl <- which(missingIndex[,i])
    if(length(xl) > 0)
      rect(xleft=xl, xright=xl+1, ybottom=i-1, ytop=i, col=col, border=NA)
  })
  
  abline(h=1:ncol(x), col="white")
  mtext(side = 2, text = colnames(x), at = (1:n)-0.5, las=1, adj = 1)
  mtext(side = 4, text = gettextf("%s (%s)", miss, 
                                  sprintf("%.1f%%", 
                                          100 * miss/nrow(missingIndex))),
        at = (1:n)-0.5, las=1, adj = 0)
  
  # text(x = -0.03 * nrow(x), y = (1:n)-0.5, labels = colnames(x), las=1, adj = 1)
  # text(x = nrow(x) * 1.04, y = (1:n)-0.5, labels = gettextf("%s (%s)", miss, Format(miss/nrow(missingIndex), fmt="%", digits=1)), las=1, adj=0)
  
  if(!is.null(.getOption("stamp")))
    stamp()
  
  invisible(res)
}


