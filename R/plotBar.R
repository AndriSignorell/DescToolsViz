
#' Themed Barplot with Optional Grid, Text and Connecting Lines
#'
#' Creates an enhanced wrapper around \code{\link[graphics]{barplot}} with
#' support for theming, background handling, optional grid lines,
#' value labels, and connecting lines for stacked barplots.
#'
#' The function first initializes the plotting region invisibly,
#' optionally adds grid lines, then draws the actual bars and
#' additional layers (zero line, connecting lines, text).
#'
#' @param height A vector or matrix of bar heights.
#'   Passed directly to \code{\link[graphics]{barplot}}.
#' @param bg Background color. Defaults to theme background or \code{par("bg")}.
#' @param col Bar fill colors. Defaults to theme color.
#' @param axes Logical. Should the numeric axis be drawn? Default is \code{TRUE}.
#' @param args.grid Optional list of arguments controlling grid lines.
#'   Supported elements include:
#'   \describe{
#'     \item{col}{Grid line color (default: \code{"grey85"})}
#'     \item{lty}{Line type (default: \code{1})}
#'     \item{lwd}{Line width (default: \code{1})}
#'   }
#'   Grid orientation is determined automatically.
#' @param args.text Optional list of arguments passed to \code{\link{barText}}
#'   to draw value labels. Common arguments include:
#'   \describe{
#'     \item{labels}{Text labels (default: \code{height})}
#'     \item{pos}{Position of labels (default: \code{"mid"})}
#'     \item{offset}{Offset from bar (default: \code{0})}
#'   }
#' @param args.connlines Optional list of arguments controlling connecting
#'   lines between stacked bars. Only supported when \code{beside = FALSE}.
#'   Typical elements include:
#'   \describe{
#'     \item{col}{Line color (default: \code{"grey40"})}
#'     \item{lwd}{Line width (default: \code{1})}
#'     \item{lty}{Line type (default: \code{2})}
#'   }
#' @param box logical, defining if a box should be drawn or not   
#' @param ... Additional arguments passed to \code{\link[graphics]{barplot}}
#'   and graphical parameters (via \code{par()}).
#'
#' @details
#' The function internally:
#' \enumerate{
#'   \item Adjusts margins for rotated or horizontal labels.
#'   \item Draws an invisible barplot to establish the coordinate system.
#'   \item Optionally adds grid lines.
#'   \item Draws the actual bars.
#'   \item Adds a zero reference line if appropriate.
#'   \item Optionally adds connecting lines (stacked bars only).
#'   \item Optionally adds value labels via \code{\link{barText}}.
#'   \item Draws the numeric axis (if enabled) and a box.
#' }
#'
#' @return Invisibly returns the midpoints of the bars
#'   (as returned by \code{\link[graphics]{barplot}}).
#'
#' @seealso \code{\link[graphics]{barplot}}, \code{\link{barText}}
#'
#' @examples
#' # Simple barplot
#' plotBar(1:5)
#'
#' # With grid lines
#' plotBar(1:5,
#'         args.grid = list(col = "grey90"))
#'
#' # Stacked barplot with labels and connecting lines
#' m <- matrix(c(3,2,4,1,5,2), nrow = 2)
#' plotBar(m,
#'         args.text = list(pos = "mid"),
#'         args.connlines = list(col = "black"))
#'
#' plotBar(VADeaths, ylim=c(0,250),
#'         args.grid=list(col = "grey", lty="dotted"), 
#'         las=1, main="MyTitle", 
#'         args.text = list(labels=VADeaths, 
#'         border = NA, srt=45, bg="navajowhite"))
#'
#' plotBar(VADeaths, ylim=c(0,80),
#'         las=1, main="MyTitle",
#'         box=FALSE, 
#'         col=gray.colors(nrow(VADeaths)),
#'         beside=TRUE, 
#'         args.text = list(col="red", bg=alpha("white", 0.7), border=NA))
#' 
#' ptab <- proportions(VADeaths, margin=2)
#' plotBar(ptab,
#'         las=1, main="VADeaths in %",
#'         box=FALSE, horiz=TRUE, 
#'         col=(cols <- gray.colors(nrow(VADeaths))),
#'         beside=FALSE, mar=c(right=10),
#'         args.text = list(labels=fm(ptab, fmt="%"), border=NA, 
#'                          col=contrastColor(cols)))
#' legend(x="right", fill=cols, legend=rownames(VADeaths))
#' 
#' plotBar(VADeaths, args.connlines = list(lwd=1, col="blue"), 
#' box=FALSE, las=1, main="Connecting Lines")
#' 



# Principle for the plot part of DescToolsViz:
# * User arguments override theme
# * Theme overrides base defaults
# .apply pars describe!!*********


#' @export
plotBar <- function(height,
                    bg = NULL,
                    col = NULL,
                    axes = TRUE, box=TRUE,
                    args.grid = NULL,
                    args.text = NULL,
                    args.connlines = NULL,
                    ...) {
  
  .withGraphicsState({
    
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    th <- .getTheme()
    
    bg  <- bg  %||% th$bg  %||% par("bg")
    col <- col %||% th$col
    
    # par() aus ...
    .applyParFromDots(...)
    
    dots  <- list(...)
    horiz <- isTRUE(dots$horiz)
    las   <- par("las")
    beside <- dots$beside %||% FALSE
    
    labels <- .getBarplotAxisLabels(height, dots)
    
    # --- Margin-Korrekturen ---
    if (horiz) {
      .adjustLeftMarginForLabels(labels)
    }
    
    if (!horiz && las == 2) {
      .adjustBottomMarginForLas2(labels)
    }
    
    par(bg = bg)
    
    # --- Setup (unsichtbar) ---
    mids <- barplot(height,
                    col = NA,
                    border = NA,
                    axes = FALSE,
                    ...)
    
    # --- GRID Layer ---
    if (!is.null(args.grid)) {
      
      args.grid <- .mergeArgs(
        defaults = list(
          horiz = horiz,
          col   = "grey85",
          lty   = 1,
          lwd   = 1
        ),
        user = args.grid,
        forbidden = c("horiz"),
        warn = TRUE
      )
      
      if (!horiz) {
        abline(h = axTicks(2),
               col = args.grid$col,
               lty = args.grid$lty,
               lwd = args.grid$lwd)
      } else {
        abline(v = axTicks(1),
               col = args.grid$col,
               lty = args.grid$lty,
               lwd = args.grid$lwd)
      }
    }
    
    # --- echte Balken ---
    b <- barplot(height,
                 col = col,
                 add = TRUE,
                 axes = FALSE,
                 ...)
    
    # --- Zero-Linie nach Balken ---
    usr <- par("usr")
    
    if (!horiz) {
      if (!par("ylog") && usr[3] <= 0 && usr[4] >= 0) {
        abline(h = 0, lwd = 1.5)
      }
    } else {
      if (!par("xlog") && usr[1] <= 0 && usr[2] >= 0) {
        abline(v = 0, lwd = 1.5)
      }
    }
    
    # --- Connecting Lines (nur stacked) ---
    if (!is.null(args.connlines)) {
      
      if (isTRUE(dots$beside)) {
        warning("Connecting lines only supported for stacked barplots.")
      } else {
        
        width <- dots$width %||% 1
        
        args.connlines <- .mergeArgs(
          defaults = list(
            height = height,
            b      = b,
            horiz  = horiz,
            width  = width,
            col    = "grey40",
            lwd    = 1,
            lty    = 2
          ),
          user = args.connlines,
          forbidden = c("height","b","horiz","width"),
          warn = TRUE
        )
        
        do.call(.barConnLines, args.connlines)
      }
    }
    
    # --- Text Layer ---
    if (!is.null(args.text)) {
      
      args.text <- .mergeArgs(
        defaults = list(
          height = height,
          b      = b,
          beside = beside,
          horiz  = horiz,
          labels = height,
          pos    = "mid",
          offset = 0
        ),
        user = args.text,
        forbidden = c("height","b","horiz"),
        warn = TRUE
      )
      
      do.call(barText, args.text)
    }
    
    # --- Numerische Achse ---
    if (isTRUE(axes)) {
      if (!horiz) axis(2) else axis(1)
    }
    
    if(box) 
      box()
    
    invisible(b)
    
  })
}




# == internal helper functions =======================================


.drawGridY <- function(horiz, col, lty, lwd, ...) {
  abline(h = axTicks(2), col = col, lty = lty, lwd = lwd, ...)
}

.drawGridX <- function(horiz, col, lty, lwd, ...) {
  abline(v = axTicks(1), col = col, lty = lty, lwd = lwd, ...)
}

.barConnLines <- function(height, b, horiz = FALSE,
                          width = 1,
                          col = 1, lwd = 1, lty = 2, ...) {
  
  if (!is.matrix(height)) {
    warning("Connecting lines only supported for stacked barplots.")
    return(invisible())
  }
  
  cumh <- apply(height, 2, cumsum)
  
  nc <- ncol(height)
  nr <- nrow(height)
  
  # width kann Vektor oder Skalar sein
  if (length(width) == 1)
    width <- rep(width, nc)
  
  left  <- b - width/2
  right <- b + width/2
  
  for (i in seq_len(nr)) {
    
    if (!horiz) {
      # vertikal: Linie von rechtem Rand zur linken nächsten Bar
      
      x0 <- right[-nc]
      x1 <- left[-1]
      y0 <- cumh[i, -nc]
      y1 <- cumh[i, -1]
      
      segments(x0, y0, x1, y1,
               col = col, lwd = lwd, lty = lty, ...)
      
    } else {
      # horizontal
      
      y0 <- right[-nc]
      y1 <- left[-1]
      x0 <- cumh[i, -nc]
      x1 <- cumh[i, -1]
      
      segments(x0, y0, x1, y1,
               col = col, lwd = lwd, lty = lty, ...)
    }
  }
  
  invisible()
}

# options(DescToolsX.theme = list(
#   bg        = "white",
#   col       = "grey70",
#   border    = "white",
#   grid      = TRUE,
#   grid.col  = "grey85",
#   grid.lty  = 1,
#   grid.lwd  = 1
# ))


.getTheme <- function() {
  th <- getOption("DescToolsX.theme")
  if (is.null(th))
    th <- list()
  th
}


.adjustLeftMarginForLabels <- function(labels) {
  
  if (is.null(labels) || !length(labels))
    return(invisible())
  
  w <- max(strwidth(labels,
                    units = "inches",
                    cex = par("cex.axis")))
  
  lineHeight <- par("csi") * par("mex")
  
  needed <- ceiling(w / lineHeight) + 1
  
  mar <- par("mar")
  
  if (needed > mar[2]) {
    mar[2] <- needed
    par(mar = mar)
  }
  
  invisible()
}



.adjustBottomMarginForLas2 <- function(labels) {
  
  if (is.null(labels) || !length(labels))
    return(invisible())
  
  w <- max(strwidth(labels,
                    units = "inches",
                    cex = par("cex.axis")))
  
  lineHeight <- par("csi") * par("mex")
  
  needed <- ceiling(w / lineHeight) + 1
  
  mar <- par("mar")
  
  if (needed > mar[1]) {
    mar[1] <- needed
    par(mar = mar)
  }
  
  invisible()
}


.getBarplotAxisLabels <- function(height, dots) {
  
  if (!is.null(dots$names.arg))
    return(dots$names.arg)
  
  if (is.matrix(height)) {
    labs <- colnames(height)
    if (!is.null(labs))
      return(labs)
  }
  
  names(height)
}
