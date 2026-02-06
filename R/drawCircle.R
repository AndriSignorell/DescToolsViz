
#' Draw a Circle 
#' 
#' Draw one or several circle on an existing plot. 
#' 
#' All geometric arguments will be recycled. 
#' 
#' @param x,y a vector (or scalar) of xy-coordinates for the center(s) of the
#' circle(s). 
#' @param r.out a vector (or scalar) of the outer radius of the circle. 
#' @param r.in a vector (or scalar) of a potential inner radius of an annulus.
#' @param theta.1 a vector (or scalar) of the starting angle(s). The sectors
#' are built counterclockwise. 
#' @param theta.2 a vector (or scalar) of the ending angle(s). 
#' @param nv number of vertices to draw the circle. 
#' @param border color for circle borders. The default is par("fg"). Use border
#' = \code{NA} to omit borders. 
#' @param col color(s) to fill or shade the circle(s) with. The default
#' \code{NA} (or also NULL) means do not fill, i.e., draw transparent circles,
#' unless density is specified. 
#' @param lty line type for borders and shading; defaults to \code{"solid"}. 
#' @param lwd line width for borders and shading. 
#' @param plot logical. If \code{TRUE} the structure will be plotted. If
#' \code{FALSE} only the points are calculated and returned. Use this option if
#' you want to combine several geometric structures to a polygon. 
#' @return The function invisibly returns a list of the calculated coordinates
#' for all shapes. 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{polygon}}, \code{\link{drawRegPolygon}},
#' \code{\link{drawEllipse}}, \code{\link{drawArc}} 
#' @keywords aplot
#' @examples
#' 
#' canvas(xlim = c(-5,5), xpd=TRUE)
#' cols <- Pal("Helsana")[1:4]
#' 
#' # Draw ring
#' drawCircle (r.in = 1, r.out = 5, border="darkgrey", 
#'             col=alpha("yellow", 0.2), lwd=2)
#' 
#' # Draw circle
#' drawCircle (r.in = 6, border="green", lwd=3)
#' 
#' # Draw sectors
#' geom <- rbind(c(-pi, 0, .25, .5), c(0, pi, 1, 2),
#'               c(-pi/2, pi/2, 2, 2.5), c(pi/2, 3 * pi/2, 3, 4),
#'               c(pi - pi/8, pi + pi/8, 1.5, 2.5))
#' 
#' drawCircle (r.in = geom[,3], r.out = geom[,4],
#'            theta.1 = geom[,1], theta.2 = geom[,2],
#'            col = alpha(cols, 0.6),
#'            border = cols, lwd=1)
#' 
#' 
#' # clipping
#' canvas(bg="lightgrey", main="Yin ~ Yang")
#' drawCircle (r.out = 1, col="white")
#' clip(0, 2, 2, -2)
#' drawCircle(col="black")
#' clip(-2, 2, 2, -2)
#' drawCircle (y = c(-0.5,0.5), r.out = 0.5, col=c("black", "white"), border=NA)
#' drawCircle (y = c(-0.5,0.5), r.out = 0.1, col=c("white", "black"), border=NA)
#' drawCircle ()
#' 
#' 
#' # overplotting circles
#' canvas(xlim=c(-5,5))
#' drawCircle (r.out=4:1, col=c("white", "steelblue2", "white", "red"), lwd=3, nv=300)
#' 
#' 
#' # rotation
#' x <- seq(-3, 3, length.out=10)
#' y <- rep(0, length.out=length(x))
#' 
#' canvas(xlim=c(-5,5), bg="black")
#' 
#' sapply( (0:11) * pi/6, function(theta) {
#'   xy <- rotate(x, y=y, theta=theta)
#'   drawCircle (x=xy$x, y=xy$y, r.in=2.4, border=alpha("white", 0.2))
#' } )



#' @export
drawCircle <- function (x = 0, y = x, r.out = 1, r.in = 0, theta.1 = 0,
                        theta.2 = 2 * pi, border = par("fg"), col = NA, 
                        lty = par("lty"),
                        lwd = par("lwd"), nv = 100, plot = TRUE) {
  
  drawSector <- function(x, y, r.in, r.out, theta.1,
                         theta.2, nv, border, col, lty, lwd, plot) {
    
    # get arc coordinates
    pts <- drawArc(x = x, y = y, rx = c(r.out, r.in), ry = c(r.out, r.in),
                   theta.1 = theta.1, theta.2 = theta.2, nv = nv,
                   col = border, lty = lty, lwd = lwd, plot = FALSE)
    
    is.ring <- (r.in != 0)
    is.sector <- any( ((theta.1-theta.2) %% (2*pi)) != 0)
    
    if(is.ring || is.sector) {
      # we have an inner and an outer circle
      ptx <- c(pts[[1]]$x, rev(pts[[2]]$x))
      pty <- c(pts[[1]]$y, rev(pts[[2]]$y))
      
    } else {
      # no inner circle
      ptx <- pts[[1]]$x
      pty <- pts[[1]]$y
    }
    
    if (plot) {
      if (is.ring & !is.sector) {
        # we have angles, so plot polygon for the area and lines for borders
        polygon(x = ptx, y = pty, col = col, border = NA,
                lty = lty, lwd = lwd)
        
        lines(x = pts[[1]]$x, y = pts[[1]]$y, col = border, lty = lty, lwd = lwd)
        lines(x = pts[[2]]$x, y = pts[[2]]$y, col = border, lty = lty, lwd = lwd)
        
      }
      else {
        polygon(x = ptx, y = pty, col = col, border = border,
                lty = lty, lwd = lwd)
      }
    }
    invisible(list(x = ptx, y = pty))
  }
  
  lgp <- .recycle(x=x, y=y, r.in = r.in, r.out = r.out,
                 theta.1 = theta.1, theta.2 = theta.2, border = border,
                 col = col, lty = lty, lwd = lwd, nv = nv)
  lst <- list()
  for (i in 1L:attr(lgp, "maxdim")) {
    pts <- with(lgp, drawSector(x=x[i], y=y[i], r.in=r.in[i],
                                r.out=r.out[i], theta.1=theta.1[i],
                                theta.2=theta.2[i], nv=nv[i], border=border[i],
                                col=col[i], lty=lty[i], lwd=lwd[i],
                                plot = plot))
    lst[[i]] <- pts
  }
  invisible(lst)
}

