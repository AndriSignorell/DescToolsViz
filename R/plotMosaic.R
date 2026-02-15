
#' Mosaic Plots 
#' 
#' Plots a mosaic on the current graphics device. 
#' 
#' The reason for this function to exist are the unsatisfying labels in base
#' mosaicplot. 
#' 
#' @param x a contingency table in array form, with optional category labels
#' specified in the dimnames(x) attribute. The table is best created by the
#' table() command. So far only 2-way tables are allowed. 
#' @param main character string for the mosaic title. 
#' @param horiz logical, defining the orientation of the mosaicplot.
#' \code{TRUE} (default) makes a horizontal plot. 
#' @param cols the colors of the plot. 
#' @param off the offset between the rectangles. Default is 0.02. 
#' @param mar the margin for the plot. 
#' @param xlab,ylab x- and y-axis labels used for the plot; by default, the
#' first and second element of names(dimnames(X)) (i.e., the name of the first
#' and second variable in X). 
#' @param cex numeric character expansion factor; multiplied by
#' \code{par("cex")} yields the final character size. \code{NULL} and \code{NA}
#' are equivalent to 1.0. 
#' @param las the style of axis labels. 0 - parallel to the axis, 1 -
#' horizontal, 2 - perpendicular, 3 - vertical. 
#' @param ... Graphical parameters passed to \code{par()}, such as
#'   \code{cex}, \code{las}, \code{mar}, or \code{oma}.
#'
#' 
#' @return list with the midpoints of the rectangles 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{mosaicplot}} 
#' 
#' @references Friendly, M. (1994) Mosaic displays for multi-way contingency
#' tables. \emph{Journal of the American Statistical Association}, \bold{89},
#' 190-200.
#' 
#' 
#' @keywords hplot
#' @examples
#' 
#' plotMosaic(HairEyeColor[,,1])
#' 


#' @export
plotMosaic <- function (x, main = deparse(substitute(x)), 
                        horiz = TRUE, cols = NULL,
                        off = 0.02, mar = NULL, 
                        xlab = NULL, ylab = NULL, 
                        cex=par("cex"), las=2, ...) {
  
  if(length(dim(x))>2){
    warning("plotMosaic is restricted to max. 2 dimensions")
    invisible()
  }
  
  .withGraphicsState({
    
    
  
  if (is.null(xlab))
    xlab <- names(dimnames(x)[2]) %||% "x"
  if (is.null(ylab))
    ylab <- names(dimnames(x)[1]) %||%  "y"
  
  if (is.null(mar)){
    # ymar <- 5.1
    # xmar <- 6.1
    
    inches_to_lines <- (par("mar") / par("mai") )[1]  # 5
    lab.width <- max(strwidth(colnames(x), units="inches")) * inches_to_lines
    xmar <- lab.width + 1
    lab.width <- max(strwidth(rownames(x), units="inches")) * inches_to_lines
    ymar <- lab.width + 1
    
    mar <- c(ifelse(is.na(xlab), 2.1, 5.1), ifelse(is.na(ylab), ymar, ymar+2),
             ifelse(is.na(main), xmar, xmar+4), 1.6)
    
    # par(mai = c(par("mai")[1], max(par("mai")[2], strwidth(levels(grp), "inch")) +
    #               0.5, par("mai")[3], par("mai")[4]))
    
  }
  
  canvas(xlim = c(0, 1), ylim = c(0, 1), asp = NA, mar = mar)
  
  col1 <- Pal()[1]
  col2 <- Pal()[2]
  
  oldpar <- par(xpd = TRUE)
  on.exit(par(oldpar))
  
  
  if(any(dim(x)==1)) {
    
    if (is.null(cols))
      cols <- colorRampPalette(c(col1, "white", col2), space = "rgb")(length(x))
    
    
    if(horiz){
      
      ptab <- prop.table(as.vector(x))
      pxt <- ptab * (1 - (length(ptab) - 1) * off)
      
      y_from <- c(0, cumsum(pxt) + (1:(length(ptab))) * off)[-length(ptab) - 1]
      y_to <- cumsum(pxt) + (0:(length(ptab) - 1)) * off
      
      if(nrow(x) > ncol(x))
        x <- t(x)
      
      x_from <- y_from
      x_to <- y_to
      
      y_from <- 0
      y_to <- 1
      
      
    } else {
      
      ptab <- rev(prop.table(as.vector(x)))
      pxt <- ptab * (1 - (length(ptab) - 1) * off)
      
      y_from <- c(0, cumsum(pxt) + (1:(length(ptab))) * off)[-length(ptab) - 1]
      y_to <- cumsum(pxt) + (0:(length(ptab) - 1)) * off
      
      
      x_from <- 0
      x_to <- 1
      
      if(ncol(x) > nrow(x))
        x <- t(x)
      
    }
    
    rect(xleft = x_from, ybottom = y_from, xright = x_to, ytop = y_to, col = cols)
    
    txt_y <- apply(cbind(y_from, y_to), 1, mean)
    txt_x <-  midx(c(x_from, 1))
    
  } else {
    
    if (horiz) {
      
      if (is.null(cols))
        cols <- colorRampPalette(c(col1, "white", col2), space = "rgb")(ncol(x))
      
      ptab <- prop.table(x, 1)[nrow(x):1, ]
      ptab <- ptab * (1 - (ncol(ptab) - 1) * off)
      pxt <- .rev(prop.table(margin.table(x, 1)) * (1 - (nrow(x) - 1) * off))
      
      y_from <- c(0, cumsum(pxt) + (1:(nrow(x))) * off)[-nrow(x) - 1]
      y_to <- cumsum(pxt) + (0:(nrow(x) - 1)) * off
      
      x_from <- t((apply(cbind(0, ptab), 1, cumsum) + (0:ncol(ptab)) * off)[-(ncol(ptab) + 1), ])
      x_to <- t((apply(ptab, 1, cumsum) + (0:(ncol(ptab) - 1) * off))[-(ncol(ptab) + 1), ])
      
      for (j in 1:nrow(ptab)) {
        rect(xleft = x_from[j,], ybottom = y_from[j],
             xright = x_to[j,], ytop = y_to[j], col = cols)
      }
      
      txt_y <- apply(cbind(y_from, y_to), 1, mean)
      txt_x <- apply(cbind(x_from[nrow(x_from),], x_to[nrow(x_from),]), 1, mean)
      
      # srt.x <- if (las > 1) 90  else 0
      # srt.y <- if (las == 0 || las == 3) 90 else 0
      #
      # text(labels = .rev(rownames(x)), y = txt_y, x = -0.04, adj = ifelse(srt.y==90, 0.5, 1), cex=cex, srt=srt.y)
      # text(labels = colnames(x), x = txt_x, y = 1.04, adj = ifelse(srt.x==90, 0, 0.5), cex=cex, srt=srt.x)
      
    } else {
      
      if (is.null(cols))
        cols <- colorRampPalette(c(col1, "white", col2), space = "rgb")(nrow(x))
      
      ptab <- .rev(prop.table(x, 2), margin = 1)
      ptab <- ptab * (1 - (nrow(ptab) - 1) * off)
      pxt <- (prop.table(margin.table(x, 2)) * (1 - (ncol(x) - 1) * off))
      
      x_from <- c(0, cumsum(pxt) + (1:(ncol(x))) * off)[-ncol(x) - 1]
      x_to <- cumsum(pxt) + (0:(ncol(x) - 1)) * off
      
      y_from <- (apply(rbind(0, ptab), 2, cumsum) + (0:nrow(ptab)) *
                   off)[-(nrow(ptab) + 1), ]
      y_to <- (apply(ptab, 2, cumsum) + (0:(nrow(ptab) - 1) *
                                           off))[-(nrow(ptab) + 1), ]
      
      for (j in 1:ncol(ptab)) {
        rect(xleft = x_from[j], ybottom = y_from[, j], xright = x_to[j],
             ytop = y_to[, j], col = cols)
      }
      
      txt_y <- apply(cbind(y_from[, 1], y_to[, 1]), 1, mean)
      txt_x <- apply(cbind(x_from, x_to), 1, mean)
      
      # srt.x <- if (las > 1) 90  else 0
      # srt.y <- if (las == 0 || las == 3) 90 else 0
      #
      # text(labels = .rev(rownames(x)), y = txt_y, x = -0.04, adj = ifelse(srt.y==90, 0.5, 1), cex=cex, srt=srt.y)
      # text(labels = colnames(x), x = txt_x, y = 1.04, adj = ifelse(srt.x==90, 0, 0.5), cex=cex, srt=srt.x)
      
    }
  }
  
  srt.x <- if (las > 1) 90  else 0
  srt.y <- if (las == 0 || las == 3) 90 else 0
  
  text(labels = .rev(rownames(x)), y = txt_y, x = -0.04, adj = ifelse(srt.y==90, 0.5, 1), cex=cex, srt=srt.y)
  text(labels = colnames(x), x = txt_x, y = 1.04, adj = ifelse(srt.x==90, 0, 0.5), cex=cex, srt=srt.x)
  
  
  if (!is.na(main)) {
    usr <- par("usr")
    plt <- par("plt")
    ym <- usr[4] + diff(usr[3:4])/diff(plt[3:4])*(plt[3]) + (1.2 + is.na(xlab)*4) * strheight('m', cex=1.2, font=2)
    
    text(x=0.5, y=ym, labels = main, cex=1.2, font=2)
  }
  
  
  if (!is.na(xlab)) title(xlab = xlab, line = 1)
  if (!is.na(ylab)) title(ylab = ylab)

  invisible(list(x = txt_x, y = txt_y))
  
  })
}



.rev <- function(x, margin, ...) {
  
  newdim <- rep("", length(dim(x)))
  newdim[margin] <- paste(dim(x), ":1", sep="")[margin]
  z <- eval(parse(text=gettextf("x[%s, drop = FALSE]", paste(newdim, sep="", collapse=","))))
  class(z) <- oldClass(x)
  return(z)
  
}
