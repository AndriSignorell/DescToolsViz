
#' Add a colorLegend to a Plot
#' 
#' Add a color legend, an image of a sequence of colors, to a plot.
#' 
#' The labels are placed at the right side of the colorlegend and are reparted
#' uniformly between y and y - height. \cr\cr The location may also be
#' specified by setting x to a single keyword from the list
#' \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"},
#' \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} and
#' \code{"center"}. This places the colorlegend on the inside of the plot frame
#' at the given location. Partial argument matching is used. The optional inset
#' argument specifies how far the colorlegend is inset from the plot margins.
#' If a single value is given, it is used for both margins; if two values are
#' given, the first is used for x- distance, the second for y-distance.
#' 
#' @param x the left x-coordinate to be used to position the colorlegend. See
#' 'Details'.
#' @param y the top y-coordinate to be used to position the colorlegend. See
#' 'Details'.
#' @param cols the color appearing in the colorlegend.
#' @param labels a vector of labels to be placed at the right side of the
#' colorlegend.
#' @param width the width of the colorlegend.
#' @param height the height of the colorlegend.
#' @param horiz logical indicating if the colorlegend should be horizontal;
#' default \code{FALSE} means vertical alignment.
#' @param xjust how the colorlegend is to be justified relative to the
#' colorlegend x location.  A value of 0 means left justified, 0.5 means
#' centered and 1 means right justified.
#' @param yjust the same as \code{xjust} for the legend y location.
#' @param inset inset distance(s) from the margins as a fraction of the plot
#' region when colorlegend is placed by keyword.
#' @param border defines the bordor color of each rectangle. Default is none
#' (\code{NA}).
#' @param frame defines the bordor color of the frame around the whole
#' colorlegend. Default is none (\code{NA}).
#' @param cntrlbl defines, whether the labels should be printed in the middle
#' of the color blocks or start at the edges of the colorlegend. Default is
#' \code{FALSE}, which will print the extreme labels centered on the edges.
#' @param adj text alignment, horizontal and vertical.
#' @param cex character extension for the labels, default 1.0.
#' @param title a character string or length-one expression giving a title to
#' be placed at the top of the legend.
#' @param title.adj horizontal adjustment for title: see the help for
#' \code{\link{par}("adj")}.
#' @param \dots further arguments are passed to the function \code{text}.
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{legend}}, \code{\link{findColor}}
#' @keywords aplot
#' @examples
#' 
#' plot(1:15,, xlim=c(0,10), type="n", xlab="", ylab="", main="Colorstrips")
#' 
#' # A
#' colorLegend(x="right", inset=0.1, labels=c(1:10))
#' 
#' # B: Center the labels
#' colorLegend(x=1, y=9, height=6, col=colorRampPalette(c("blue", "white", "red"),
#'   space = "rgb")(5), labels=1:5, cntrlbl = TRUE)
#' 
#' # C: Outer frame
#' colorLegend(x=3, y=9, height=6, col=colorRampPalette(c("blue", "white", "red"),
#'   space = "rgb")(5), labels=1:4, frame="grey")
#' 
#' # D
#' colorLegend(x=5, y=9, height=6, col=colorRampPalette(c("blue", "white", "red"),
#'   space = "rgb")(10), labels=sprintf("%.1f",seq(0,1,0.1)), cex=0.8)
#' 
#' # E: horizontal shape
#' colorLegend(x=1, y=2, width=6, height=0.2, col=rainbow(500), labels=1:5,horiz=TRUE)
#' 
#' # F
#' colorLegend(x=1, y=14, width=6, height=0.5, col=colorRampPalette(
#'   c("black","blue","green","yellow","red"), space = "rgb")(100), horiz=TRUE)
#' 
#' # G
#' colorLegend(x=1, y=12, width=6, height=1, col=colorRampPalette(c("black","blue",
#'             "green","yellow","red"), space = "rgb")(10), horiz=TRUE, 
#'             border="black", title="From black to red", title.adj=0)
#' 
#' text(x = c(8,0.5,2.5,4.5,0.5,0.5,0.5)+.2, y=c(14,9,9,9,2,14,12), LETTERS[1:7], cex=2)
#' 


#' @export
colorLegend <- function( x, y=NULL, cols=rev(heat.colors(100)), labels=NULL
                         , width=NULL, height=NULL, horiz=FALSE
                         , xjust=0, yjust=1, inset=0, border=NA, frame=NA
                         , cntrlbl = FALSE
                         , adj=ifelse(horiz,c(0.5,1), c(1,0.5)), cex=1.0
                         , title = NULL, title.adj=0.5, ...) {
  
  # positionierungscode aus legend
  auto <- if (is.character(x))
    match.arg(x, c("bottomright", "bottom", "bottomleft",
                   "left", "topleft", "top", "topright", "right", "center"))
  else NA
  
  usr <- par("usr")
  if( is.null(width) ) width <- strwidth("mn") # (usr[2L] - usr[1L]) * ifelse(horiz, 0.92, 0.08)
  if( is.null(height) ) height <- (usr[4L] - usr[3L]) * ifelse(horiz, 0.08, 0.92)
  
  if (is.na(auto)) {
    left <- x - xjust * width
    top <- y + (1 - yjust) * height
    
  } else {
    inset <- rep(inset, length.out = 2)
    insetx <- inset[1L] * (usr[2L] - usr[1L])
    left <- switch(auto, bottomright = , topright = ,
                   right = usr[2L] - width - insetx, bottomleft = ,
                   left = , topleft = usr[1L] + insetx, bottom = ,
                   top = , center = (usr[1L] + usr[2L] - width)/2)
    insety <- inset[2L] * (usr[4L] - usr[3L])
    top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] +
                    height + insety, topleft = , top = , topright = usr[4L] -
                    insety, left = , right = , center = (usr[3L] +
                                                           usr[4L] + height)/2)
  }
  
  xpd <- par(xpd=TRUE); on.exit(par(xpd))
  
  ncols <- length(cols)
  nlbls <- length(labels)
  if(horiz) {
    rect( xleft=left, xright=left+width/ncols*seq(ncols,0,-1), ytop=top, ybottom=top-height,
          col=rev(cols), border=border)
    if(!is.null(labels)){
      if(cntrlbl) xlbl <- left + width/(2*ncols)+(width-width/ncols)/(nlbls-1) * seq(0,nlbls-1,1)
      else xlbl <- left + width/(nlbls-1) * seq(0,nlbls-1,1)
      ylbl <- top - (height + max(strheight(labels, cex=cex)) * 1.2) 
      text(y=ylbl
           # Gleiche Korrektur wie im vertikalen Fall
           # , x=x+width/(2*ncols)+(width-width/ncols)/(nlbls-1) * seq(0,nlbls-1,1)
           , x=xlbl, labels=labels, adj=adj, cex=cex, ...)
    }
  } else {
    rect( xleft=left, ybottom=top-height, xright=left+width, ytop=top-height/ncols*seq(0,ncols,1),
          col=rev(cols), border=border)
    if(!is.null(labels)){
      # Korrektur am 13.6:
      # die groesste und kleinste Beschriftung sollen nicht in der Mitte der Randfarbkaestchen liegen,
      # sondern wirklich am Rand des strips
      # alt: , y=y-height/(2*ncols)- (height- height/ncols)/(nlbls-1)  * seq(0,nlbls-1,1)
      #, y=y-height/(2*ncols)- (height- height/ncols)/(nlbls-1)  * seq(0,nlbls-1,1)
      
      # 18.4.2015: reverse labels, as the logic below would misplace...
      labels <- rev(labels)
      
      if(cntrlbl) ylbl <- top - height/(2*ncols) - (height- height/ncols)/(nlbls-1)  * seq(0, nlbls-1,1)
      else ylbl <- top - height/(nlbls-1) * seq(0, nlbls-1, 1)
      xlbl <- left + width + strwidth("0", cex=cex) + max(strwidth(labels, cex=cex)) * adj[1]
      text(x=xlbl
           , y=ylbl, labels=labels, adj=adj, cex=cex, ... )
    }
  }
  if(!is.na(frame)) rect( xleft=left, xright=left+width, ytop=top, ybottom=top-height, border=frame)
  
  if (!is.null(title)) 
    text(left + width * title.adj, top + strheight("M")*1.4, labels = title, 
         adj = c(title.adj, 0), cex=cex)
  
  invisible(list(rect=list(w=width, h=height, left=left, top=top), 
                 text=list(x=if(is.null(labels)) NULL else xlbl, 
                           y=if(is.null(labels)) NULL else ylbl)))
  
}


