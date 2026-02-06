
#' Date/Time/Directory Stamp the Current Plot
#' 
#' Stamp the current plot in the extreme lower right corner. A free text or
#' expression can be defined as text to the stamp.
#' 
#' The text can be freely defined as option. If user and date should be
#' included by default, the following option using an expression will help:
#' \preformatted{setDescToolsXOption(stamp=expression(gettextf('%s/%s',
#' Sys.getenv('USERNAME'), Today() )))}
#' 
#' For \R results may not be satisfactory if \code{par(mfrow=)} is in effect.
#' 
#' @param txt an optional single text string. If it is not given, the function
#' will look for a defined option named \code{stamp}. If not found the current
#' date will be taken as text. If the stamp option is defined as expression the
#' function will evaluate it. This can be used to define dynamic texts.
#' @param las numeric in \code{c(1, 3)}, defining direction of the text. 1
#' means horizontal, 3 vertical. Default is taken from \code{par("las")}.
#' @param cex numeric \bold{c}haracter \bold{ex}pansion factor; multiplied by
#' \code{par("cex")} yields the final character size. Defaults to 0.6.
#' @param col the text color
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{text}}
#' @keywords aplot
#' @examples
#' 
#' plot(1:20)
#' stamp()



#' @export
stamp <- function(txt = NULL, las = NULL, cex = 0.6, col="grey40") {
  
  ## resolve text
  if (is.null(txt)) {
    txt <- .getOption("stamp")
    if (is.null(txt)) {
      txt <- format(Sys.time(), "%Y-%m-%d")
    } else if (is.expression(txt)) {
      txt <- eval(parse(text = txt))
    }
  }
  
  ## handle las locally
  old_las <- par("las")
  if (is.null(las)) {
    las <- old_las
  } else {
    op_las <- par(las = las)
    on.exit(par(op_las), add = TRUE)
  }
  
  ## IMPORTANT: line is NEGATIVE for outer text
  line_pos <- -(0.3 + 1.4 * cex)
  
  if (las == 3) {
    ## vertical stamp (right bottom)
    mtext(
      paste0("  ", txt),
      side  = 4,
      line  = line_pos,   # << FIX
      adj   = 0,
      srt   = 90,
      outer = TRUE,
      cex   = cex,
      col   = col
    )
    
  } else {
    ## horizontal stamp (bottom right)
    mtext(
      paste0(txt, "  "),
      side  = 1,
      line  = line_pos,   # << FIX
      adj   = 1,
      outer = TRUE,
      cex   = cex,
      col   = col
    )
  }
  
  invisible(txt)
}
