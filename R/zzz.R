

.onLoad <- function(libname, pkgname) {
  
  # presetting DescToolsX options not already defined by the user
  
  # here the same as in DescToolsX to ensure they are defined, even
  # id DescToolsViz are loaded as single package
  
  op <- options()
  pkg.op <- list(
    
    DescToolsX.palette   = c("#8296C4", "#9A0941", "#F08100", "#FED037",
                             "#CAB790", "#B3BA12", "#D35186", "#8FAE8C",  
                             "#5F6F9A", "#E6E2D3", "#6E5A3C", "#5B2A45"),
    DescToolsX.plotit    = TRUE,
    DescToolsX.stamp     = expression(gettextf("%s / %s", Sys.getenv("USERNAME"),
                                               format(Sys.Date(), "%Y-%m-%d"))),
    
    DescToolsX.theme     = list(
      
          grid.col = "grey90",
          grid.lty = 3,
          grid.lwd = 1,
          grid.group.col = "grey40",
          grid.group.lty = 2,
          grid.group.lwd = 1,
          
          pch.pch  = 21,
          pch.col = par("fg"),
          pch.bg  = "white",
          pch.cex = 1.2,
          
          line.col = par("fg"),
          line.lwd = 1,
          line.lty = 1,

          group.cex = par("cex.axis") * 1.05
      ),
    
    abs.sty   = structure(list(digits = 0, big.mark = "",
                               label = "Number format for counts"), 
                          class = "Style"),
    per.sty   = structure(list(digits = 1, fmt = "%",
                               name = "per", label = "Percentage number format"),
                          class = "Style"),
    num.sty   = structure(list(digits = 3, big.mark = "",
                               label = "Number format for numeric values"), 
                          class = "Style"),
    pval.sty   = structure(list(fmt="p", eps=1e-3,
                                label = "Number format for p-values"),
                           class = "Style")
  )
  
  toset <- !(names(pkg.op) %in% names(op))
  if (any(toset)) options(pkg.op[toset])
  
}


#' @useDynLib DescToolsViz, .registration = TRUE


#' @importFrom Rcpp sourceCpp
#' 
#' @importFrom graphics plot hist abline par points text
#'             axTicks axis grid layout lines mtext rect title polygon
#'             strheight strwidth clip  image grconvertX grconvertY
#'             segments barplot box matplot layout.show arrows 
#'             plot.new plot.window close.screen screen split.screen
#'             rug curve contour persp
#'             
#' @importFrom grDevices rgb col2rgb rgb2hsv colors colorRampPalette adjustcolor
#'             xy.coords heat.colors dev.size gray.colors
#'             
#' @importFrom utils head tail combn readRegistry modifyList
#' 
#' @importFrom stats qt sd as.dendrogram dist hclust order.dendrogram filter 
#'             relevel setNames is.ts time prop.test predict qnorm formula var
#'             model.frame model.response model.weights terms na.omit
#'             acf plot.ts pacf complete.cases dnorm quantile
NULL
