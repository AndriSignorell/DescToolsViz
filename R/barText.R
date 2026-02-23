
#' Place Value Labels on a Barplot 
#' 
#' It can sometimes make sense to display data values directly on the bars in a
#' barplot. There are a handful of obvious alternatives for placing the labels,
#' either on top of the bars, right below the upper end, in the middle or at
#' the bottom. Determining the required geometry - although not difficult - is
#' cumbersome and the code is distractingly long within an analysis code. The
#' present function offers a short way to solve the task. It can place text
#' either in the middle of the stacked bars, on top or on the bottom of a
#' barplot (side by side or stacked).  
#' 
#' The x coordinates of the labels can be found by using
#' \code{\link{barplot}()} result, if they are to be centered at the top of
#' each bar. \code{barText()} calculates the rest.
#' 
#' \figure{barText.pngPositions for the text}
#' 
#' Notice that when the labels are placed on top of the bars, they may be
#' clipped. This can be avoided by setting \code{xpd=TRUE}.
#' 
#' @param height either a vector or matrix of values describing the bars which
#' make up the plot exactly as used for creating the barplot. 
#' @param b the returned mid points as returned by \code{b <- barplot(...)}. 
#' @param labels the labels to be placed on the bars. 
#' @param beside a logical value. If \code{FALSE}, the columns of height are
#' portrayed as stacked bars, and if \code{TRUE} the columns are portrayed as
#' juxtaposed bars. 
#' @param horiz a logical value. If \code{FALSE}, the bars are drawn vertically
#' with the first bar to the left. If \code{TRUE}, the bars are drawn
#' horizontally with the first at the bottom. 
#' @param cex numeric character expansion factor; multiplied by
#' \code{\link{par}}\code{("cex")} yields the final character size. \code{NULL}
#' and \code{NA} are equivalent to \code{1.0}.
#' @param adj one or two values in \verb{[0, 1]} which specify the x (and optionally
#' y) adjustment of the labels. On most devices values outside that interval
#' will also work.
#' @param pos one of \code{"topout"}, \code{"topin"}, \code{"mid"},
#' \code{"bottomin"}, \code{"bottomout"}, defining if the labels should be
#' placed on top of the bars (inside or outside) or at the bottom of the bars
#' (inside or outside).
#' @param offset a vector indicating how much the bars should be shifted
#' relative to the x axis.
#' @param col the color and to be used, possibly a vector (default in par("col")).
#' @param \dots the dots are passed to the \code{\link{boxedText}}. 
#' 
#' @return returns the geometry of the labels invisibly
#' @author Andri Signorell <andri@@signorell.net>
#' 
#' @seealso \code{\link{boxedText}} 
#' @keywords aplot
#' @examples
#' 
#' # simple vector
#' x <- c(353, 44, 56, 34)
#' b <- barplot(x)
#' barText(x, b, x)
#' 
#' # more complicated
#' b <- barplot(VADeaths, horiz = FALSE, col="steelblue", beside = TRUE)
#' barText(VADeaths, b=b, horiz = FALSE, beside = TRUE, cex=0.8)
#' barText(VADeaths, b=b, horiz = FALSE, beside = TRUE, cex=0.8, pos="bottomin",
#'         col="white", font=2)
#' 
#' b <- barplot(VADeaths, horiz = TRUE, col="steelblue", beside = TRUE)
#' barText(VADeaths, b=b, horiz = TRUE, beside = TRUE, cex=0.8)
#' 
#' b <- barplot(VADeaths)
#' barText(VADeaths, b=b)
#' 
#' b <- barplot(VADeaths, horiz = TRUE)
#' barText(VADeaths, b=b, horiz = TRUE, col="red", cex=1.5)
#' 
#' 
#' # position of the text
#' old <- par(mfrow=c(3,2), xpd=NA)
#' off <- c(10, 4, 1, 20, -15)
#' 
#' for(pos in eval(formals(barText)$pos)) {
#'   b <- barplot(x, offset=off, 
#'   main=gettextf("Textposition pos = '%s'", pos), horiz=TRUE)
#'   abline(h=0)
#'   barText(x, b, x, offset = off, pos=pos, cex=1.5, horiz=TRUE)
#' }
#' par(old)
#' 

#' @export
barText <- function(height, b, labels = height, beside = FALSE, horiz = FALSE,
                    cex = par("cex"),
                    adj = NULL,
                    pos = c("topout", "topin", "mid", "bottomin", "bottomout"),
                    offset = 0, col = NULL, ...) {
  
  pos <- match.arg(pos)
  
  ## ------------------------------------------------------------------
  ## helper: boxed text wrapper
  ## ------------------------------------------------------------------
  .btext <- function(x, y = NULL, labels, adj = NULL,
                     cex = 1, col = NULL, ...) {
    boxedText(x = x, y = y, labels = labels,
              adj = adj, cex = cex, col = col, ...)
  }
  
  ## ------------------------------------------------------------------
  ## normalize input
  ## ------------------------------------------------------------------
  if (is.vector(height) || (is.array(height) && length(dim(height)) == 1)) {
    height <- cbind(height)
    beside <- TRUE
  }
  
  if(!is.null(col))
    col    <- t(matrix(rep_len(col, length(height)), nrow = nrow(height)))
  # else leave NULL
  
  offset <- rep_len(as.vector(offset), length(height))
  
  char_w <- par("cxy")[1] * cex
  char_h <- par("cxy")[2] * cex
  
  ## ==================================================================
  ## BESIDE MODE
  ## ==================================================================
  if (beside) {
    
    if (horiz) {
      
      adjy <- 0.5
      if (is.null(adj)) adj <- 0
      
      shift <- 1.2 * sign(height) * char_w
      
      x <- switch(pos,
                  topout    = height + offset + shift,
                  topin     = height + offset - shift,
                  mid       = offset + height / 2,
                  bottomin  = offset + shift,
                  bottomout = offset - shift
      )
      
      adjx <- switch(pos,
                     topout    = -sign(height),
                     topin     =  sign(height),
                     mid       =  0.5,
                     bottomin  = -sign(height),
                     bottomout =  sign(height)
      )
      
      pp <- recycle(b = b, x = x, labels = labels,
                    adjx = adjx, adjy = adjy)
      
      for (i in seq_len(attr(pp, "maxdim"))) {
        with(pp,
             .btext(
               x = x[i], y = b[i],
               labels = labels[i],
               adj = c(adjx[i], adjy[i]),
               cex = cex, col = col, xpd = TRUE, ...
             )
        )
      }
      
      return(invisible(pp$x))
    }
    
    ## -------- vertical beside --------
    
    if (is.null(adj)) adj <- 0.5
    shift <- sign(height) * char_h
    
    y <- switch(pos,
                topout    = height + offset + shift,
                topin     = height + offset - shift,
                mid       = offset + height / 2,
                bottomin  = offset + shift,
                bottomout = offset - shift
    )
    
    .btext(x = b, y = y, labels = labels,
           adj = adj, cex = cex, col = col,
           xpd = TRUE, ...)
    
    return(invisible(y))
  }
  
  ## ==================================================================
  ## STACKED MODE
  ## ==================================================================
  
  shift <- if (horiz) char_w * 0.5 else char_h * 0.25
  
  cum_height <- apply(offset + height, 2, cumsum)
  
  x <- switch(pos,
              topout    = t(cum_height + sign(height) * shift),
              topin     = t(cum_height - sign(height) * shift),
              mid       = t(apply(offset + height, 2, midx,
                                  incl.zero = TRUE, cumulate = TRUE)),
              bottomin  = t(head(rbind(0, cum_height), -1) +
                              sign(height) * shift),
              bottomout = t(head(rbind(0, cum_height), -1) -
                              sign(height) * shift)
  )
  
  adj_base <- switch(pos,
                     topout    = 0,
                     topin     = 1,
                     mid       = 0.5,
                     bottomin  = 0,
                     bottomout = 1
  )
  
  if (is.null(adj)) adj <- 0.5
  
  if (horiz) {
    .btext(x = x, y = b,
           labels = t(labels),
           adj = c(adj_base, 0.5),
           cex = cex, col = col, ...)
  } else {
    .btext(x = b, y = x,
           labels = t(labels),
           adj = c(0.5, adj_base),
           cex = cex, col = col, ...)
  }
  
  invisible(x)
}


# barText <- function(height, b, labels=height, beside = FALSE, horiz = FALSE,
#                     cex=par("cex"), 
#                     adj=NULL, 
#                     pos=c("topout", "topin", "mid", "bottomin", "bottomout"), 
#                     offset=0, col=NULL, ...) {
#   
#   # allow to use the more flexible BoxedText instead of text here  
#   # redirection to be able to change defaults of BoxedText
#   .btext <- function (x, y = NULL, labels = seq_along(x), adj = NULL, pos = NULL, 
#                       offset = 0.5, vfont = NULL, cex = 1, font = NULL, col=NULL,
#                       srt = 0, xpad = 0.2, ypad = 0.2, density = NULL, angle = 45, 
#                       border = NA, lty = par("lty"), 
#                       lwd = par("lwd"), ...) {
#     
#     boxedText(x=x, y=y, labels = labels, adj = adj, pos = pos, 
#               offset = offset, vfont = vfont, cex = cex, col=col,
#               font = font, 
#               srt = srt, xpad = xpad, ypad = ypad, density = density, angle = angle, 
#               border = border, lty = lty, 
#               lwd = lwd, ...) 
#     
#   }
#   
#   
#   if (is.vector(height) || (is.array(height) && (length(dim(height)) == 1))) {
#     height <- cbind(height)
#     beside <- TRUE
#   }
#   
#   col <- t(matrix(rep_len(col, length(height)), nrow=nrow(height)))
#   
#   offset <- rep_len(as.vector(offset), length(height))
#   
#   pos <- match.arg(pos)
#   
#   
#   if(beside){
#     if(horiz){
#       if(is.null(adj)) adj <- 0
#       adjy <- 0.5
#       
#       if(pos=="topout"){
#         x <- height + offset + 1.2 * sign(height) * par("cxy")[1] * cex
#         adjx <- recodeX(x = factor(sign(x+offset)), "0"=1, "1"=-1, num = TRUE)
#       }
#       else if(pos=="topin") {
#         x <- height + offset - 1.2 * sign(height) * par("cxy")[1] * cex
#         adjx <- recodeX(x = factor(sign(x+offset)), "1"=1, "0"=-1, num = TRUE)
#       }
#       else if(pos=="mid"){
#         x <- offset + height / 2
#         adjx <- 0.5
#       }
#       else if(pos=="bottomout") {
#         x <- offset - 1.2 * sign(height) * par("cxy")[1] * cex
#         adjx <- recodeX(x = factor(sign(x+offset)), "1"=1, "0"=-1, num = TRUE)
#       }
#       else if(pos=="bottomin") {
#         x <- offset + 1.2 * sign(height) * par("cxy")[1] * cex
#         adjx <- recodeX(x = factor(sign(x+offset)), "0"=1, "1"=-1, num = TRUE)
#       }
#       
#       pp <- recycle(b=b, x=x, labels=labels, adjx=adjx, adjy=adjy)
#       
#       for(i in seq(attr(pp, "maxdim"))){
#         with(pp, 
#              .btext(y=b[i], x=x[i], labels=labels[i], 
#                         adj=c(adjx[i], adjy[i]), 
#                         cex=cex, xpd=TRUE, col=col, ...))    
#       } 
#       
#       res <- pp$x
#       
#       
#     } else {
#       
# 
#       if(is.null(adj)) adjx <- 0.5
#       
#       if(pos=="topout")
#         y <- height + offset + sign(height) * par("cxy")[2] * cex
#       else if(pos=="topin")
#         y <- height + offset - sign(height) * par("cxy")[2] * cex
#       else if(pos=="mid")
#         y <- offset + height/2
#       if(pos=="bottomin")
#         y <- offset + sign(height) * par("cxy")[2] * cex
#       if(pos=="bottomout")
#         y <- offset - sign(height) * par("cxy")[2] * cex
#       
#       .btext(x=b, y=y, labels=labels, xpd=TRUE, cex=cex, adj=adj, 
#              col=col, ...)  
#       
#       res <- y
#       
#     }
#     
#     # The xpd=TRUE means to not plot the text even if it is outside
#     # of the plot area and par("cxy") gives the size of a typical
#     # character in the current user coordinate system.
#     
#     
#     
#     
#   } else {
#     
#     if(horiz)
#       shift <- par("cxy")[1] * cex * .5
#     else 
#       shift <- par("cxy")[2] * cex * .25
#     
#     
#     if(pos=="topout"){
#       x <- t(apply(offset + height, 2, cumsum) + sign(height) * shift)
#       adjx <- 0
#       
#     } else if(pos=="topin") {
#       x <- t(apply(offset + height, 2, cumsum) - sign(height) * shift)
#       adjx <- 1
#       
#     } else if(pos=="mid"){
#       x <- t(apply(offset + height, 2, midx, incl.zero=TRUE, cumulate=TRUE))
#       adjx <- 0.5
#       
#     } else if(pos=="bottomin"){
#       x <- t(head(rbind(0, apply(offset + height, 2, cumsum)), -1) + sign(height) * shift)
#       adjx <- 0
#       
#     } else if(pos=="bottomout"){
#       x <- t(head(rbind(0, apply(offset + height, 2, cumsum)), -1) - sign(height) * shift)
#       adjx <- 1
#       
#     }
#     
#     if(horiz){
#       
#       if(is.null(adj)) adj <- 0.5
#       adjy <- 0.5
#       
#       .btext(labels=t(labels), x=x, y=b, cex=cex, adj=c(adjx, adjy), 
#              col=col, ...)
#       
#     } else {
#       if(is.null(adj)) adj <- 0.5
#       adjy <- adjx
#       adjx <- 0.5
#       
#       .btext(labels=t(labels), x=b, y=x, cex=cex, adj=c(adjx, adjy), 
#              col=col, ...)
#       
#     }
#     
#     res <- x
#     
#   }
#   
#   invisible(res)
#   
# }
# 
