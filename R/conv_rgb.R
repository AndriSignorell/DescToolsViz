
#' Find the Nearest Named R-Color to a Given RGB-Color
#' 
#' R color to RGB (red/green/blue) conversion is straight on. The specific 
#' function is merely a wrapper to \code{\link{col2rgb}()}, defined in order to
#' follow this package's naming conventions.
#' 
#' Converting a RGB-color to a named R-Color means looking for a color in the
#' R-palette, which is nearest to the given RGB-color. This function uses the
#' minimum of squared distance (\code{"euclidean"}) or the minimum absolute
#' distance (\code{"manhattan"}) as proximity measure. \cr \code{rgbToLong()}
#' converts a RGB-color to a long integer in numeric format. \code{LongToRGB()}
#' does it the other way round. 
#' 
#' It may not be clear from the start which method, rgb or hsv, yield the more
#' natural results. Trying and comparing is a recommended strategy. Moreover
#' the shortest numerical distance will not always be the best choice, when
#' comparing the colours visually.
#' 
#' @name conv_rgb
#' @aliases hexToCol rgbToCol colToRgb rgbToLong longToRgb 
#'
#' @param hex a color or a vector of colors specified as 
#'        hexadecimal string of the form "#RRGGBB" or "#RRGGBBAA"
#' @param col vector of any of the three kind of R colors, i.e., either a color
#' name (an element of \code{\link{colors}}()), a hexadecimal string of the
#' form "#rrggbb" or "#rrggbbaa", or an integer i meaning palette()\verb{[i]}.
#' Non-string values are coerced to integer.
#' @param method character string specifying the color space to be used. Can be
#' \code{"rgb"} (default) or \code{"hsv"}.
#' @param metric character string specifying the metric to be used for
#' calculating distances between the colors.  Available options are
#' \code{"euclidean"} (default) and \code{"manhattan"}. Euclidean distances are
#' root sum-of-squares of differences, and manhattan distances are the sum of
#' absolute differences.
#' @param alpha logical value indicating whether alpha channel (opacity) values
#' should be returned.
#' 
#' @return the name of the nearest found R color.
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{colToRgb}} and the other conversion functions
#' @keywords color
#' @examples
#' 
#' rgbToCol(matrix(c(162,42,42), nrow=3))
#' 
#' rgbToLong(matrix(c(162,42,42), nrow=3))
#' 
#' colToRgb("peachpuff")
#' colToRgb(c(blu = "royalblue", reddish = "tomato")) # names kept
#' 
#' colToRgb(1:8)
#' 



#' @rdname conv_rgb
#' @export
hexToCol <- function(hex, method="rgb", metric="euclidean")
  rgbToCol(hex, method=method, metric=metric)



#' @rdname conv_rgb
#' @export
rgbToCol <- function(col, method="rgb", metric="euclidean") {
  
  switch( match.arg( arg=method, choices=c("rgb","hsv") )
          , "rgb" = {
            # accepts either a matrix with 3 columns RGB or a hexstr
            
            if(!is.matrix(col)) {
              col <- lapply(col, function(x) c(strtoi(substr(x,2,3), 16L), 
                                               strtoi(substr(x,4,5), 16L), 
                                               strtoi(substr(x,6,7), 16L)))
              col <- do.call("cbind", col)
            }
            coltab <- col2rgb(colors())
            
            switch( match.arg( arg=metric, choices=c("euclidean","manhattan") )
                    , "euclidean" = {
                      colors()[apply(col, 2, function(x) 
                           which.min(apply(apply(coltab, 2, "-", x)^2, 2, sum)))]
                    }
                    , "manhattan" = {
                      colors()[apply(col, 2, function(x) 
                           which.min(apply(abs(apply(coltab, 2, "-", x)), 2, sum)))]
                    }
            )
          }
          , "hsv" ={
            # accepts either a matrix with 3 columns RGB or a hexstr
            col <- colToHsv(col)
            if(!is.matrix(col)) {
              col <- lapply(col, function(x) c(strtoi(substr(x,2,3), 16L), 
                                               strtoi(substr(x,4,5), 16L), 
                                               strtoi(substr(x,6,7), 16L)))
              col <- do.call("cbind", col)
            }
            coltab <- colToHsv(colors())
            
            switch( match.arg( arg=metric, choices=c("euclidean","manhattan") )
                    , "euclidean" = {
                      colors()[apply(col, 2, function(x) 
                          which.min(apply(apply(coltab, 2, "-", x)^2, 2, sum)))]
                    }
                    , "manhattan" = {
                      colors()[apply(col, 2, function(x) 
                          which.min(apply(abs(apply(coltab, 2, "-", x)), 2, sum)))]
                    }
            )
          }
  )
  
  # alternative?
  # Identify closest match to a color:  plotrix::color.id
  
  # old:
  # coltab <- col2rgb(colors())
  # cdist <- apply(coltab, 2, function(z) sum((z - col)^2))
  # colors()[which(cdist == min(cdist))]
}



# ColToDec is col2rgb??
#' @rdname conv_rgb
#' @export
colToRgb <- function(col, alpha = FALSE) 
  col2rgb(col, alpha)



#' @rdname conv_rgb
#' @export
rgbToLong <- function(col) (c(1, 256, 256^2) %*% col)[1,]


# example:  RgbToLong(colToRgb(c("green", "limegreen")))

#' @rdname conv_rgb
#' @export
longToRgb <- function(col)
  sapply(col, function(x) c(red=x %% 256, 
                            green=(x %/% 256) %% 256, 
                            blue=(x %/% 256^2) %% 256))


# if ever needed...
# '~~> LONG To RGB
# R = Col Mod 256
# G = (Col \ 256) Mod 256
# B = (Col \ 256 \ 256) Mod 256




