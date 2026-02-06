
#' Conversion Between RGB and CMYK 
#' 
#' These function convert colors between RGB and CMYK system. 
#' 
#'
#' @name conv_cols 
#' @aliases colToHex hexToRgb rgbToHex rgbToCmy cmykToRgb cmykToCmy cmyToCmyk rgbToCmyk 
#' @param col the matrix of the color to be converted 
#' @param alpha logical value indicating whether alpha channel (opacity) values
#' should be returned.
#' @param hex a color or a vector of colors specified as 
#'        hexadecimal string of the form "#RRGGBB" or "#RRGGBBAA"
#' @param cyan cyan values of the color(s) to be converted 
#' @param magenta magenta values of the color(s) to be converted 
#' @param yellow yellow values of the color(s) to be converted 
#' @param black black values of the color(s) to be converted 
#' @param maxColorValue the value for the color 
#' 
#' @return the converted value 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{rgbToCol}} 
#' @examples
#' 
#' cmykToRgb(0.42, 45.23, 85.14, maxColorValue=100)
#' 


#' @rdname conv_cols
#' @export
colToHex <- function(col, alpha=1) {
  col.rgb <- col2rgb(col)
  col <- apply( col.rgb, 2, function(x) sprintf("#%02X%02X%02X", x[1], x[2], x[3]) )
  if(alpha != 1 ) col <- paste( col, .decToHex( round( alpha * 255, 0)), sep="")
  return(col)
  # old: sprintf("#%02X%02X%02X", col.rgb[1], col.rgb[2], col.rgb[3])
}


#' @rdname conv_cols
#' @export
hexToRgb <- function(hex) {
  # converts a hexstring color to matrix with 3 red/green/blue rows
  # example: HexToRgb(c("#A52A2A","#A52A3B"))

  hex <- gsub("^#", "", hex)
  if(all(is.na(hex)))
    return(matrix(NA, nrow=3, ncol=length(hex)))
  
  # if there are any RRGGBBAA values mixed with RRGGBB then pad FF (for opaque) on RGBs
  if(any(nchar(hex)==8)){
    # old:
    # hex <- strPad(x = hex, width = 8, pad = "FF")
    hex[nchar(hex) == 6] <- paste0(hex[nchar(hex) == 6], "FF")
    i <- 4
  } else {
    i <- 3
  }
  c2 <- sapply(hex, function(x) c(red=   strtoi(substr(x,1,2), 16L),
                                  green= strtoi(substr(x,3,4), 16L),
                                  blue=  strtoi(substr(x,5,6), 16L),
                                  alpha= strtoi(substr(x,7,8), 16L))
  )
  
  res <- cbind(c2[1:i,])
  if(dim(res)[2]==1)
    colnames(res) <- hex
  
  return(res)
  
}


#' @rdname conv_cols
#' @export
rgbToHex <- function(col){
  paste0("#", paste0(.decToHex(round(col)), collapse=""))
}



#' @rdname conv_cols
#' @export
cmykToRgb <- function(cyan, magenta, yellow, black, maxColorValue=1){
  
  if (missing(black)) {
    res <- rgb(red= maxColorValue- cyan,
               green= maxColorValue - magenta,
               blue = maxColorValue - yellow, 
               maxColorValue = maxColorValue)
    
  } else {
    
    res <-  rgb(
      red= ((maxColorValue-cyan) * (maxColorValue-black)) / maxColorValue,
      green= ((maxColorValue-magenta) * (maxColorValue-black)) / maxColorValue,
      blue = ((maxColorValue-yellow) * (maxColorValue-black)) / maxColorValue,
      maxColorValue = maxColorValue)
    
  }
  
  return(res)
  
}


#' @rdname conv_cols
#' @export
rgbToCmy <- function(col, maxColorValue=1) {
  
  if(!is.matrix(col)) {
    col <- lapply(col, function(x) c(strtoi(substr(x,2,3), 16L), 
                                     strtoi(substr(x,4,5), 16L), 
                                     strtoi(substr(x,6,7), 16L)))
    col <- do.call("cbind", col)
  }
  
  cbind(
    C = 1 - ( col[,1] / maxColorValue ),
    M = 1 - ( col[,2] / maxColorValue ),
    Y = 1 - ( col[,3] / maxColorValue )
  )   
  
}


#' @rdname conv_cols
#' @export
cmyToCmyk <- function(col){
  # CMY values <- From 0 to 1
  
  if (is.null(dim(col))) 
    if (length(col) > 2) 
      col <- matrix(col, ncol=3, byrow=TRUE)
  
  var.K <- rep(1, dim(col)[1])
  
  CC <- which(col[,1] < var.K)
  if (length(CC)>0) var.K[CC] <- col[CC,1]
  
  CM <- which(col[,2] < var.K)
  if (length(CM)>0) var.K[CM] <- col[CM,2]
  
  CY <- which(col[,3] < var.K)
  if (length(CY)>0) var.K[CY] <- col[CY,3]
  
  cbind(
    C = ( col[,1] - var.K ) / ( 1 - var.K ),
    M = ( col[,2] - var.K ) / ( 1 - var.K ), 
    Y = ( col[,3] - var.K ) / ( 1 - var.K ), 
    K = var.K )
}


#' @rdname conv_cols
#' @export
cmykToCmy <- function(col){
  
  #CMYK values <- From 0 to 1
  
  if (is.null(dim(col))) 
    if (length(col)>2) 
      col <- matrix(col, ncol=4,byrow=TRUE)
  cbind(
    C = ( col[,1] * ( 1 - col[,4] ) + col[,4] ),
    M = ( col[,2] * ( 1 - col[,4] ) + col[,4] ),
    Y = ( col[,3] * ( 1 - col[,4] ) + col[,4] )
  )
  
}



#' @rdname conv_cols
#' @export
colToHsv <- function(col, alpha = FALSE) 
  rgb2hsv(colToRgb(col, alpha))


