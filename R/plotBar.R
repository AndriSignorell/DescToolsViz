
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
#' @param yax Optional list of arguments controlling the y-axis
#'   Supported elements include:
#'   \describe{
#'     \item{col}{Label color (default: \code{"black"})}
#'     \item{col.axis}{Grid line color (default: \code{"black"})}
#'     \item{lty}{Line type (default: \code{1})}
#'     \item{lwd}{Line width (default: \code{1})}
#'     \item{fm}{Format for the tickmark labels (default: \code{NULL})}
#'   }
#'   Grid orientation is determined automatically.
#' @param grid Optional list of arguments controlling grid lines.
#'   Supported elements include:
#'   \describe{
#'     \item{col}{Grid line color (default: \code{"grey85"})}
#'     \item{lty}{Line type (default: \code{1})}
#'     \item{lwd}{Line width (default: \code{1})}
#'   }
#'   Grid orientation is determined automatically.
#' @param text Optional list of arguments passed to \code{\link{barText}}
#'   to draw value labels. Common arguments include:
#'   \describe{
#'     \item{labels}{Text labels (default: \code{height})}
#'     \item{pos}{Position of labels (default: \code{"mid"})}
#'     \item{offset}{Offset from bar (default: \code{0})}
#'   }
#' @param connlines Optional list of arguments controlling connecting
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
#'         text = list(pos = "mid"),
#'         connlines = list(col = "black"))
#'
#' plotBar(VADeaths, ylim=c(0,250),
#'         grid=list(col = "grey", lty="dotted"), 
#'         las=1, main="MyTitle", 
#'         text = list(labels=VADeaths, 
#'         border = NA, srt=45, bg="navajowhite"))
#'
#' plotBar(VADeaths, ylim=c(0,80),
#'         las=1, main="MyTitle",
#'         box=FALSE, 
#'         col=gray.colors(nrow(VADeaths)),
#'         beside=TRUE, 
#'         text = list(col="red", bg=alpha("white", 0.7), border=NA))
#' 
#' ptab <- proportions(VADeaths, margin=2)
#' plotBar(ptab,
#'         las=1, main="VADeaths in %",
#'         box=FALSE, horiz=TRUE, 
#'         col=(cols <- gray.colors(nrow(VADeaths))),
#'         beside=FALSE, mar=c(right=10),
#'         text = list(labels=fm(ptab, fmt="%"), border=NA, 
#'                     col=contrastColor(cols)))
#' legend(x="right", fill=cols, legend=rownames(VADeaths))
#' 
#' plotBar(VADeaths, connlines = list(lwd=1, col="blue"), 
#'         box=FALSE, las=1, main="Connecting Lines")
#' 
#' plotBar(VADeaths/1e3,  box=FALSE, bg="lightyellow", main="VADeaths",
#'         horiz=TRUE, 
#'         text=list(border=FALSE, cex=0.8, col=c("blue", "green","orange")), 
#'         mar=c(right=8), 
#'         yax = list(fmt="%", d=0, big=",", 
#'                    col="red", col.axis="blue", lwd=2))
#' 



# Principle for the plot part of DescToolsViz:
# * User arguments override theme
# * Theme overrides base defaults
# .apply pars describe!!*********


#' @export
plotBar <- function(height,
                    bg = NULL,
                    col = NULL,
                    axes = TRUE, 
                    yax = NULL,
                    box=FALSE,
                    grid = NULL,
                    text = NULL,
                    connlines = NULL,
                    stamp = TRUE,
                    ...) {
  
  .withGraphicsState({
    
    # `%||%` <- function(a, b) if (!is.null(a)) a else b
    
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
    if (horiz && !(par("yaxt")=="n")) {
      .adjustLeftMarginForLabels(labels)
    }
    
    if (!horiz && las == 2 && !(par("xaxt")=="n")) {
      .adjustBottomMarginForLas2(labels)
    }
    
    par(bg = bg)
    
    # --- Setup (unsichtbar) ---
    dots[c("col","border","axes")] <- NULL
    b <- do.call(barplot, c(list(
                     height, 
                     col=NA, 
                     border=NA, 
                     axes=FALSE), 
                     dots))

    # --- GRID Layer ---
    if (!is.null(grid)) {
      
      if(isTRUE(grid))
        grid <- NULL
      
      grid <- .mergeArgs(
        defaults = list(
          horiz = horiz,
          col   = "grey85",
          lty   = 1,
          lwd   = 1
        ),
        user = grid,
        forbidden = c("horiz"),
        warn = TRUE
      )
      
      if (!horiz) {
        abline(h = axTicks(2),
               col = grid$col,
               lty = grid$lty,
               lwd = grid$lwd)
      } else {
        abline(v = axTicks(1),
               col = grid$col,
               lty = grid$lty,
               lwd = grid$lwd)
      }
    }
    
    # --- echte Balken ---
    barplot(height,
            col = col,
            add = TRUE,
            axes = FALSE,
            ...)
    
    # --- Zero-Linie nach Balken ---
    # ******* ???? do we need a zero line ???? ************
    
    # usr <- par("usr")
    # 
    # if (!horiz) {
    #   if (!par("ylog") && usr[3] <= 0 && usr[4] >= 0) {
    #     abline(h = 0, lwd = 1.5)
    #   }
    # } else {
    #   if (!par("xlog") && usr[1] <= 0 && usr[2] >= 0) {
    #     abline(v = 0, lwd = 1.5)
    #   }
    # }
    
    # --- Connecting Lines (nur stacked) ---
    if (!is.null(connlines)) {
      
      if (isTRUE(beside)) {
        warning("Connecting lines only supported for stacked barplots.")
        
      } else {
        
        # width <- dots$width %||% 1
        
        if(isTRUE(connlines))
          connlines <- NULL
        
        connlines <- .mergeArgs(
          defaults = list(
            height = height,
            b      = b,
            horiz  = horiz,
            width  = 1,
            col    = "grey40",
            lwd    = 1,
            lty    = 2
          ),
          user = connlines,
          forbidden = c("height","b","horiz","width"),
          warn = TRUE
        )
        
        do.call(.drawConnLines, connlines)
      }
    }
    
    # --- Text Layer ---
    .callIf(barText,
            text,
            defaults = list(
                height = height,
                b      = b,
                horiz  = horiz,
                beside = beside,
                labels = height,
                pos    = "mid",
                offset = 0
              ),
              forbidden = c("height","b","horiz", "beside"),
              warn = TRUE
            )

    
    # --- Numerische Achse ---
    if (isTRUE(axes)) {
      
      if (!horiz) {
        .drawAxis(2, yax)
      } else {
        .drawAxis(1, yax)
      }
    }    

        
    # draw box if box != FALSE || NA
    .callIf(graphics::box, 
            box,
            defaults=list(which="plot"))
    
    invisible(b)
    
  }, stamp=stamp)
  
  return(b)
  
}



# == internal helper functions =======================================

.applyFmt <- function(x, fmt) {
  
  if (is.null(fmt))
    return(x)
  
  if (isTRUE(fmt))
    return(fm(x))
  
  if (is.function(fmt))
    return(fmt(x))
  
  do.call(fm, modifyList(list(x = x), fmt))
}



.splitAxisArgs <- function(ax) {
  
  if (is.null(ax))
    return(list(fmt = NULL, axis = NULL))
  
  fm_names <- names(formals(fm))
  
  # axis + par-Achsenparameter erlauben
  axis_names <- unique(c(
    names(formals(graphics::axis)),
    grep("\\.axis$", names(par()), value = TRUE),
    "col", "lwd", "lty", "tck", "las", "cex", "font"
  ))
  
  fmt  <- ax[names(ax) %in% fm_names]
  axis <- ax[names(ax) %in% axis_names]
  
  axis <- axis[names(axis) != "labels"]
  
  # intuitive interpretation
  # col is the color of the axis labels, col.axis the one of the axis
  has_col      <- "col" %in% names(axis)
  has_col_axis <- "col.axis" %in% names(axis)

  if (has_col && has_col_axis){
    tmp <- axis$col.axis
    axis$col.axis <- axis$col
    axis$col <- tmp
  }

  
  if (has_col && !has_col_axis){
    axis$col.axis <- axis$col
    axis$col <- par("col.axis")
  }
  
  if (!has_col && has_col_axis){
    axis$col <- axis$col.axis
    axis$col.axis <- par("col")
  }
  
  if("cex" %in% names(axis))
    axis$cex.axis <- axis$cex
  
  if("font" %in% names(axis))
    axis$font.axis <- axis$font
  
  
  list(fmt = fmt, axis = axis)
}



.drawAxis <- function(side, ax) {
  
  at <- axTicks(side)
  
  sp <- .splitAxisArgs(ax)
  
  labs <- if (length(sp$fmt)) {
    do.call(fm, modifyList(list(x=at), sp$fmt))
  } else {
    at
  }
  
  do.call(
    graphics::axis,
    c(list(side=side, at=at, labels=labs), sp$axis)
  )
}


.drawGridY <- function(horiz, col, lty, lwd, ...) {
  abline(h = axTicks(2), col = col, lty = lty, lwd = lwd, ...)
}
.drawGridX <- function(horiz, col, lty, lwd, ...) {
  abline(v = axTicks(1), col = col, lty = lty, lwd = lwd, ...)
}


.drawConnLines <- function(height, b, horiz = FALSE,
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

