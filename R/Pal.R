
#' Some Custom Palettes
#' 
#' Some more custom palettes. 
#' 
#' hred, horange, hyellow, hecru, hblue and hgreen are constants, pointing to
#' the according color from the palette Pal("Helsana").
#' 
#' %% Use \code{options(palette=Pal("Dark"))} to set the default palette used
#' throughout DescTools to \code{"Dark"}.
#' 
#' @name Pal
#' @aliases Pal plot.palette hblue hred horange hgreen hyellow hecru
#' @param pal name or number of the palette. One of \code{RedToBlack (1)},
#' \code{RedBlackGreen (2)}, \code{SteeblueWhite (3)}, \code{RedWhiteGreen
#' (4)}, \code{RedWhiteBlue0 (5)}, \code{RedWhiteBlue1 (6)},
#' \code{RedWhiteBlue2 (7)}, \code{RedWhiteBlue3 (8)}, \code{Helsana (9)},
#' \code{Tibco (10)}, \code{RedGreen1 (11)}, \code{Spring (12)}, \code{Soap
#' (13)}, \code{Maiden (14)}, \code{Dark (15)}, \code{Accent (16)},
#' \code{Pastel (17)}, \code{Fragile (18)}, \code{Big (19)}, \code{Long (20)},
#' \code{Night (21)}, \code{Dawn (22)}, \code{Noon (23)}, \code{Light (24)}
#' 
#' @param n integer, number of colors for the palette. 
#' @param alpha the alpha value to be added. This can be any value from 0
#' (fully transparent) to 1 (opaque). \code{NA} is interpreted so as to delete
#' a potential alpha channel. Default is 0.5. 
#' @param x a palette to be plotted.
#' @param cex extension for the color squares. Defaults to 3.
#' @param \dots further arguments passed to the function.
#' 
#' @return a vector of colors 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{colorRampPalette}}
#' @keywords color
#' @examples
#' 
#' canvas(c(0,1))
#' colorLegend(x=0, y=1, width=0.1, col=Pal(1, n=50))
#' colorLegend(x=0.15, y=1, width=0.1, col=Pal(2, n=50))
#' colorLegend(x=0.3, y=1, width=0.1, col=Pal(3, n=50))
#' colorLegend(x=0.45, y=1, width=0.1, col=Pal(4, n=50))
#' colorLegend(x=0.6, y=1, width=0.1, col=Pal(5, n=50))
#' colorLegend(x=0.75, y=1, width=0.1, col=Pal(6, n=50))
#' colorLegend(x=0.9, y=1, width=0.1, col=Pal(7))
#' colorLegend(x=1.05, y=1, width=0.1, col=Pal(8))
#' 
#' text(1:8, y=1.05, x=seq(0,1.05,.15)+.05)
#' title(main="DescTools palettes")
#' 
#' par(mfrow=c(4,2), mar=c(1,1,2,1))
#' barplot(1:9, col=Pal("Tibco"), axes=FALSE, main="Palette 'Tibco'" )
#' 
#' barplot(1:7, col=Pal("Helsana"), axes=FALSE, main="Palette 'Helsana'" )
#' barplot(1:7, col=alpha(Pal("Helsana")[c("ecru","hellgruen","hellblau")], 0.6),
#'         axes=FALSE, main="Palette 'Helsana' (Alpha)" )
#' 
#' barplot(1:10, col=Pal("RedToBlack", 10), axes=FALSE, main="Palette 'RedToBlack'" )
#' barplot(1:10, col=Pal("RedBlackGreen", 10), axes=FALSE, main="Palette 'RedGreenGreen'" )
#' barplot(1:10, col=Pal("SteeblueWhite", 10), axes=FALSE, main="Palette 'SteeblueWhite'" )
#' barplot(1:10, col=Pal("RedWhiteGreen", 10), axes=FALSE, main="Palette 'RedWhiteGreen'" )
#' 


#' @rdname pal
#' @export
Pal <- function(pal, n=100, alpha=1) {
  
  if(missing(pal)) {
    res <- .getOption("palette")
    
  } else {
    
    palnames <- c("RedToBlack","RedBlackGreen","SteeblueWhite","RedWhiteGreen",
                  "RedWhiteBlue0","RedWhiteBlue1","RedWhiteBlue2","RedWhiteBlue3","Helsana","Helsana1","Tibco","RedGreen1",
                  "Spring","Soap","Maiden","Dark","Accent","Pastel","Fragile","Big","Long","Night","Dawn","Noon","Light",
                  "GrandBudapest","Moonrise1","Royal1","Moonrise2","Cavalcanti","Royal2","GrandBudapest2","Moonrise3",
                  "Chevalier","Zissou","FantasticFox","Darjeeling","Rushmore","BottleRocket","Darjeeling2","Helsana2",
                  "Tequila")
    
    
    if(is.numeric(pal)){
      pal <- palnames[pal]
    } else {
      # allow partial matching
      pal <- palnames[pmatch(pal, palnames)]
    }
    
    big <- c("#800000", "#C00000", "#FF0000", "#FFC0C0",
             "#008000","#00C000","#00FF00","#C0FFC0",
             "#000080","#0000C0", "#0000FF","#C0C0FF",
             "#808000","#C0C000","#FFFF00","#FFFFC0",
             "#008080","#00C0C0","#00FFFF","#C0FFFF",
             "#800080","#C000C0","#FF00FF","#FFC0FF",
             "#C39004","#FF8000","#FFA858","#FFDCA8")
    
    switch(pal
           , RedToBlack    = res <- colorRampPalette(c("red","yellow","green","blue","black"), space = "rgb")(n)
           , RedBlackGreen = res <- colorRampPalette(c("red", "black", "green"), space = "rgb")(n)
           , SteeblueWhite = res <- colorRampPalette(c("steelblue","white"), space = "rgb")(n)
           , RedWhiteGreen = res <- colorRampPalette(c("red", "white", "green"), space = "rgb")(n)
           , RedWhiteBlue0 = res <- colorRampPalette(c("red", "white", "blue"))(n)
           , RedWhiteBlue1 = res <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                                                       "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(n)
           , RedWhiteBlue2 = res <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(n)
           , RedWhiteBlue3 = res <- colorRampPalette(c("#9A0941", "white", "#8296C4"))(n) 
           , Helsana       = res <- c("rot"="#9A0941", "orange"="#F08100", "gelb"="#FED037"
                                      , "ecru"="#CAB790", "hellrot"="#D35186", "hellblau"="#8296C4", "hellgruen"="#B3BA12"
                                      , "hellgrau"="#CCCCCC", "dunkelgrau"="#666666", "weiss"="#FFFFFF")
           , Helsana1      = res <- c("black"="#000000", "hellblau"="#8296C4", "rot"="#9A0941", "orange"="#F08100", "gelb"="#FED037"
                                      , "ecru"="#CAB790", "hellgruen"="#B3BA12", "hellrot"="#D35186"
                                      , "hellgrau"="#CCCCCC", "dunkelgrau"="#666666")
           , Helsana2      = res <- c("#9a0941","#62aedf","#9181c6", "#e55086","#f2f2f2","#b6ca2f","#fec600","#bea786")
           , Tibco         = res <- apply( mcol <- matrix(c(
             0,91,0, 0,157,69, 253,1,97, 60,120,177,
             156,205,36, 244,198,7, 254,130,1,
             96,138,138, 178,113,60
           ), ncol=3, byrow=TRUE), 1, function(x) rgb(x[1], x[2], x[3], maxColorValue=255))
           , RedGreen1 =  res <- c(rgb(227,0,11, maxColorValue=255), 
                                   rgb(227,0,11, maxColorValue=255),
                                   rgb(230,56,8, maxColorValue=255), 
                                   rgb(234,89,1, maxColorValue=255),
                                   rgb(236,103,0, maxColorValue=255), 
                                   rgb(241,132,0, maxColorValue=255),
                                   rgb(245,158,0, maxColorValue=255), 
                                   rgb(251,184,0, maxColorValue=255),
                                   rgb(253,195,0, maxColorValue=255), 
                                   rgb(255,217,0, maxColorValue=255),
                                   rgb(203,198,57, maxColorValue=255), 
                                   rgb(150,172,98, maxColorValue=255),
                                   rgb(118,147,108, maxColorValue=255))
           
           , Spring =  res <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3","#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
           , Soap =  res <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3","#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
           , Maiden =  res <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072","#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9","#BC80BD","#CCEBC5")
           , Dark =  res <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A","#66A61E", "#E6AB02", "#A6761D", "#666666")
           , Accent =  res <- c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99","#386CB0", "#F0027F", "#BF5B17", "#666666")
           , Pastel =  res <- c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4","#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2")
           , Fragile =  res <- c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4","#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC")
           , Big =  res <- big
           , Long =  res <- big[c(12,16,25,24,
                                  2,11,6,15,18,26,23,
                                  3,10,7,14,19,27,22,
                                  4,8,20,28)]
           , Night =  res <- big[seq(1, 28, by=4)]
           , Dawn =  res <- big[seq(2, 28, by=4)]
           , Noon =  res <- big[seq(3, 28, by=4)]
           , Light = res <- big[seq(4, 28, by=4)]
           
           , GrandBudapest = res < c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236")
           , Moonrise1 = res <- c("#F3DF6C", "#CEAB07", "#D5D5D3", "#24281A")
           , Royal1 = res <- c("#899DA4", "#C93312", "#FAEFD1", "#DC863B")
           , Moonrise2 = res <- c("#798E87","#C27D38", "#CCC591", "#29211F")
           , Cavalcanti = res <- c("#D8B70A", "#02401B","#A2A475", "#81A88D", "#972D15")
           , Royal2 = res <- c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089")
           , GrandBudapest2 = res <- c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4")
           , Moonrise3 = res <- c("#85D4E3", "#F4B5BD", "#9C964A", "#CDC08C", "#FAD77B")
           , Chevalier = res <- c("#446455", "#FDD262", "#D3DDDC", "#C7B19C")
           , Zissou = res <- c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")
           , FantasticFox = res <- c("#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20")
           , Darjeeling = res <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6")
           , Rushmore = res <- c("#E1BD6D", "#EABE94", "#0B775E", "#35274A", "#F2300F")
           , BottleRocket = res <- c("#A42820", "#5F5647", "#9B110E", "#3F5151", "#4E2A1E", "#550307", "#0C1707")
           , Darjeeling2 = res <- c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE",  "#000000")
           , Tequila = res <- c("#642580", "#853b88","#ab4189","#c52966","#d34376","#d55586","#d55586","#ba3723","#cc6101","#c6904a","#eebd00","#f7d501","#060c18","#00323b","#00484f")
           
    )
    
    attr(res, "name") <- pal
    class(res) <- append(class(res), "palette")
    
  }
  
  if(alpha != 1)
    res <- alpha(res, alpha = alpha)
  
  return(res)
  
}



#' @rdname pal
#' @method print palette
#' @export
print.palette <- function(x, ...){
  cat(attr(x, "name"), "\n")
  cat(x, "\n")
}



#' @rdname pal
#' @method plot palette
#' @export
plot.palette <- function(x, cex = 3, ...) {
  plotPal(x, cex, ...)
}


#' @rdname pal
#' @export
plotPal <- function(x, cex = 3, ...) {
  
  # # use new window, but store active device if already existing
  # if( ! is.null(dev.list()) ){
  #   curwin <- dev.cur()
  #   on.exit( {
  #     dev.set(curwin)
  #     par(oldpar)
  #   }
  #   )
  # }
  # windows(width=3, height=2.5, xpos=100, ypos=600)
  
  oldpar <- par(mar=c(0,0,0,0), mex=0.001, xaxt="n", yaxt="n", ann=FALSE, xpd=NA)
  on.exit(par(oldpar))
  
  palname <- if (!is.null(attr(x, "name")) && !is.na(attr(x, "name"))) {
                attr(x, "name")
              } else {
                "no name"
              }
  
  n <- length(x)
  
  x <- rev(x)
  plot( x=rep(1, n), y=1:n, pch=22, cex=cex, col="grey60", 
        bg=x, xlab="", ylab="", axes=FALSE,
        frame.plot=FALSE, ylim=c(0, n + 2), xlim=c(0.8, n))
  
  text( x=4.5, y=n + 1.2, labels="alpha", adj=c(0,0.5), cex=0.8)
  text( x=0.8, y=n + 2.0, 
        labels=gettextf("\"%s\" Palette colors", palname), 
        adj=c(0,0.5), cex=1.2)
  text( x=c(1,2.75,3.25,3.75,4.25), y= n +1.2, adj=c(0.5,0.5), 
        labels=c("1.0", 0.8, 0.6, 0.4, 0.2), cex=0.8 )
  abline(h=n+0.9, col="grey")
  
  palnames <- paste(n:1, names(x))
  
  sapply(1:n, function(i){
    xx <- c(2.75, 3.25, 3.75, 4.25)
    yy <- rep(i, 4)
    points(x=xx, y=yy, pch=22, cex=cex, col="grey60", 
           bg=alpha(x[i], alpha=c(0.8, 0.6, 0.4, 0.2)))
    text(x=1.25, y=i, adj=c(0,0.5), cex=0.8, 
         labels=palnames[i])
    
  })
  
  invisible()
  
  # points( x=rep(2.75,7), y=1:7, pch=15, cex=2, col=hc(7:1, alpha=0.8) )
  # points( x=rep(3.25,7), y=1:7, pch=15, cex=2, col=hc(7:1, alpha=0.6) )
  # points( x=rep(3.75,7), y=1:7, pch=15, cex=2, col=hc(7:1, alpha=0.4) )
  # points( x=rep(4.25,7), y=1:7, pch=15, cex=2, col=hc(7:1, alpha=0.2) )
  
  
}


