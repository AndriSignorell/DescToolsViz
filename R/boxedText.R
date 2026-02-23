
#' Add Text in a Box to a Plot 
#' 
#' boxedText draws the strings given in the vector labels at the coordinates
#' given by x and y, surrounded by a rectangle. 
#' 
#' @name boxedText
#' @aliases boxedText boxedText.default
#' @param x,y numeric vectors of coordinates where the text labels should be
#' written. If the length of x and y differs, the shorter one is recycled. 
#' @param labels a character vector or expression specifying the text to be
#' written.  An attempt is made to coerce other language objects (names and
#' calls) to expressions, and vectors and other classed objects to character
#' vectors by as.character. If labels is longer than x and y, the coordinates
#' are recycled to the length of labels. 
#' @param adj The value of adj determines the way in which text strings are
#' justified.  A value of 0 produces left-justified text, 0.5 (the default)
#' centered text and 1 right-justified text.  (Any value in \verb{[0, 1]} is allowed,
#' and on most devices values outside that interval will also work.)  Note that
#' the adj argument of text also allows adj = c(x, y) for different adjustment
#' in x- and y- directions.
#' 
#' @param pos a position specifier for the text. If specified this overrides
#' any adj value given. Values of 1, 2, 3 and 4, respectively indicate
#' positions below, to the left of, above and to the right of the specified
#' coordinates. 
#' @param offset when pos is specified, this value gives the offset of the
#' label from the specified coordinate in fractions of a character width. 
#' @param vfont \code{NULL} for the current font family, or a character vector
#' of length 2 for Hershey vector fonts. The first element of the vector
#' selects a typeface and the second element selects a style. Ignored if labels
#' is an expression. 
#' @param cex numeric character expansion factor; multiplied by
#' \code{par("cex")} yields the final character size. \code{NULL} and \code{NA}
#' are equivalent to 1.0. 
#' @param col,font the color and (if vfont = NULL) font to be used, possibly
#' vectors. These default to the values of the global graphical parameters in
#' \code{par()}. 
#' @param srt The string rotation in degrees.  
#' @param xpad,ypad The proportion of the rectangles to the extent of the text
#' within. 
#' @param density the density of shading lines, in lines per inch. The default
#' value of \code{NULL} means that no shading lines are drawn.  A zero value of
#' density means no shading lines whereas negative values (and NA) suppress
#' shading (and so allow color filling). 
#' @param angle angle (in degrees) of the shading lines. 
#' @param bg color(s) to fill or shade the rectangle(s) with. The default
#' \code{NA} (or also NULL) means do not fill, i.e., draw transparent
#' rectangles, unless density is specified. 
#' @param border color for rectangle border(s). The default is
#' \code{par("fg")}. Use \code{border = NA} to omit borders (this is the
#' default).  If there are shading lines, \code{border = TRUE} means use the
#' same colour for the border as for the shading lines. 
#' @param lty line type for borders and shading; defaults to \code{"solid"}. 
#' @param lwd line width for borders and shading. Note that the use of
#' \code{lwd = 0} (as in the examples) is device-dependent. 
#' @param \dots additional arguments are passed to the text function. 
#' 
#' @param formula A formula of the form \code{lhs ~ rhs}, where \code{lhs}
#'   gives the response values and \code{rhs} the corresponding groups
#'   or explanatory variables.
#'
#' @param data An optional matrix or data frame (or similar; see
#'   \code{\link[stats]{model.frame}}) containing the variables in the
#'   formula. By default the variables are taken from
#'   \code{environment(formula)}.
#'
#' @param subset An optional vector specifying a subset of observations
#'   to be used in the analysis.
#'

#' @author Andri Signorell <andri@@signorell.net> 
#' 
#' @seealso \code{\link{spreadOut}}, similar function in package \pkg{plotrix}
#' \code{\link[plotrix]{boxed.labels}} (lacking rotation option) 
#' @keywords aplot misc
#' @examples
#' 
#' canvas(xpd=TRUE)
#' 
#' boxedText(0, 0, adj=0, label="This is boxed text", srt=seq(0,360,20), 
#'           xpad=.3, ypad=.3)
#' points(0,0, pch=15)
#' 
#' plot(mpg ~ hp, data=mtcars, type="n", main="MT cars mpg/hp (log-log)", 
#'      panel.first=quote(grid()), las=1, log="xy")
#' boxedText(mpg ~ hp, data=mtcars, 
#'           labels=rownames(mtcars), cex=0.6, border=FALSE, bg="grey90")
#' 
#' 

#' @rdname boxedText
#' @export
boxedText <- function(x, ...) 
  UseMethod("boxedText")



#' @rdname boxedText
#' @export
boxedText.default <- function(
    x, y = NULL,
    labels = NULL,
    adj = NULL, pos = NULL, offset = 0.5,
    vfont = NULL, cex = 1, col = NULL, font = NULL,
    srt = 0, xpad = 0.2, ypad = 0.2,
    density = NULL, angle = 45,
    bg = NA, border = par("fg"),
    lty = par("lty"), lwd = par("lwd"),
    ...
) {
  
  # ------------------------------------------------------------
  # Koordinaten normalisieren (wie text.default)
  # ------------------------------------------------------------
  coords <- xy.coords(x, y, recycle = TRUE, setLab = FALSE)
  
  if (is.null(labels))
    labels <- seq_along(coords$x)
  
  # ------------------------------------------------------------
  # Recycling aller Parameter über DescToolsViz::recycle()
  # ------------------------------------------------------------
  pars <- recycle(
    x       = coords$x,
    y       = coords$y,
    labels  = labels,
    col     = if (is.null(col)) par("fg") else col,
    font    = if (is.null(font)) 1 else font,
    cex     = cex,
    srt     = srt,
    bg      = bg,
    border  = border,
    lty     = lty,
    lwd     = lwd,
    density = if (is.null(density)) NA else density,
    angle   = angle
  )
  
  n <- attr(pars, "maxdim")
  
  # adj wie text.default behandeln
  if (length(adj) == 1)
    adj <- c(adj, 0.5)
  if (is.null(adj))
    adj <- c(0.5, 0.5)
  adj <- rep_len(adj, 2)
  
  # ------------------------------------------------------------
  # Hauptschleife
  # ------------------------------------------------------------
  for (i in seq_len(n)) {
    
    xi  <- pars$x[i]
    yi  <- pars$y[i]
    lab <- pars$labels[i]
    
    # --- Mittelpunkt in Inches (log-stabil) ---
    xi_in <- grconvertX(xi, from = "user", to = "inches")
    yi_in <- grconvertY(yi, from = "user", to = "inches")
    
    # --- Textdimensionen in Inches ---
    w_in <- strwidth(lab,
                     cex = pars$cex[i],
                     font = pars$font[i],
                     vfont = vfont,
                     units = "inches")
    
    h_in <- strheight(lab,
                      cex = pars$cex[i],
                      font = pars$font[i],
                      vfont = vfont,
                      units = "inches")
    
    # --- symmetrisches Padding ---
    pad_x <- w_in * xpad
    pad_y <- h_in * ypad
    
    # --- Box relativ zu adj (wie text.default) ---
    xl <- xi_in - adj[1] * w_in - pad_x
    xr <- xl + w_in + 2 * pad_x
    
    yb <- yi_in - adj[2] * h_in - pad_y
    yt <- yb + h_in + 2 * pad_y
    
    # --- Rotation ---
    theta <- pars$srt[i] * pi / 180
    cx <- xi_in
    cy <- yi_in
    
    corners_x <- c(xl, xl, xr, xr)
    corners_y <- c(yb, yt, yt, yb)
    
    rot_x <- cx + (corners_x - cx) * cos(theta) -
      (corners_y - cy) * sin(theta)
    
    rot_y <- cy + (corners_x - cx) * sin(theta) +
      (corners_y - cy) * cos(theta)
    
    # --- zurück nach User-Koordinaten ---
    rot_x_user <- grconvertX(rot_x, from = "inches", to = "user")
    rot_y_user <- grconvertY(rot_y, from = "inches", to = "user")
    
    polygon(
      rot_x_user,
      rot_y_user,
      col     = pars$bg[i],
      density = if (is.na(pars$bg[i])) 0 else pars$density[i],
      angle   = pars$angle[i],
      border  = pars$border[i],
      lty     = pars$lty[i],
      lwd     = pars$lwd[i],
      ...
    )
    
    text(
      xi, yi,
      labels = lab,
      adj    = adj,
      pos    = pos,
      offset = offset,
      vfont  = vfont,
      cex    = pars$cex[i],
      col    = pars$col[i],
      font   = pars$font[i],
      srt    = pars$srt[i]
    )
  }
  
  invisible()
}


# == internal helper functions =========================================


.replace_text_calls <- function(expr) {
  
  if (is.call(expr)) {
    
    # Fall 1: direkter Funktionscall text(...)
    if (identical(expr[[1]], as.name("text"))) {
      expr[[1]] <- as.name("boxedText")
    }
    
    # Fall 2: do.call("text", ...)
    if (identical(expr[[1]], as.name("do.call"))) {
      if (is.character(expr[[2]]) && expr[[2]] == "text") {
        expr[[2]] <- "boxedText"
      }
    }
    
    expr[] <- lapply(expr, .replace_text_calls)
  }
  
  expr
}



#' @rdname boxedText
#' @export
boxedText.formula <- local({
  
  # super elegant formula implementation
  # in fact we need nothing other, than is already implemented in 
  # text.formula, besides the last call of boxedText() instead of text()
  
  tf <- getS3method("text", "formula")
  
  new_body <- .replace_text_calls(body(tf))
  
  new_fun <- tf
  body(new_fun) <- new_body
  
  new_fun
  
})






