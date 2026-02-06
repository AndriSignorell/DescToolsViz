

#' QQ-Plot for Any Distribution 
#' 
#' Create a QQ-plot for a variable of any distribution. The assumed underlying
#' distribution can be defined as a function of f(p), including all required
#' parameters. Confidence bands are provided by default. 
#' 
#' The function generates a sequence of points between 0 and 1 and transforms
#' those into quantiles by means of the defined assumed distribution. 
#' 
#' @param x the data sample 
#' @param qdist the quantile function of the assumed distribution. Can either
#' be given as simple function name or defined as own function using the
#' required arguments. Default is \code{qnorm()}. See examples.
#' 
#' @param main the main title for the plot. This will be "Q-Q-Plot" by default
#' @param xlab the xlab for the plot 
#' @param ylab the ylab for the plot 
#' @param datax logical. Should data values be on the x-axis? Default is
#' \code{FALSE}.
#' @param add logical specifying if the points should be added to an already
#' existing plot; defaults to \code{FALSE}.
#' @param args.qqline arguments for the qqline. This will be estimated as a
#' line through the 25\% and 75\% quantiles by default, which is the same
#' procedure as \code{\link{qqline}()} does for normal distribution (instead of
#' set it to \code{abline(a = 0, b = 1))}. The quantiles can however be
#' overwritten by setting the argument \code{probs} to some user defined
#' values. Also the method for calculating the quantiles can be defined
#' (default is 7, see \code{\link{quantile}}). The line defaults are set to
#' \code{col = par("fg")}, \code{lwd = par("lwd")} and \code{lty = par("lty")}.
#' No line will be plotted if \code{args.qqline} is set to \code{NA}.
#' @param conf.level confidence level for the confidence interval. Set this to
#' \code{NA}, if no confidence band should be plotted.  Default is \code{0.95}.
#' The confidence intervals are calculated pointwise method based on a
#' Kolmogorov-Smirnov distribution. 
#' @param args.cband list of arguments for the confidence band, such as color
#' or border (see \code{\link{drawBand}}). 
#' @param \dots the dots are passed to the plot function. 
#' 
#' @note The code is inspired by the tip 10.22 "Creating other
#' Quantile-Quantile plots" from R Cookbook and based on R-Core code from the
#' function \code{qqline}. The calculation of confidence bands are rewritten
#' based on an algorithm published in the package
#' \code{BoutrosLab.plotting.general}.
#' 
#' @author Andri Signorell <andri@@signorell.net>, Ying Wu
#' <Ying.Wu@@stevens.edu> 
#' @seealso \code{\link{qqnorm}}, \code{\link{qqline}}, \code{\link{qqplot}} 
#' @references Teetor, P. (2011) \emph{R Cookbook}. O'Reilly, pp. 254-255.
#' @keywords univar hplot
#' @examples
#' 
#' y <- rexp(100, 1/10)
#' plotQQ(y, function(p) qexp(p, rate=1/10))
#' 
#' w <- rweibull(100, shape=2)
#' plotQQ(w, qdist = function(p) qweibull(p, shape=4))
#' 
#' z <- rchisq(100, df=5)
#' plotQQ(z, function(p) qchisq(p, df=5),
#'        args.qqline=list(col=2, probs=c(0.1, 0.6)),
#'        main=expression("Q-Q plot for" ~~ {chi^2}[nu == 3]))
#' abline(0,1)
#' 
#' # add 5 random sets
#' for(i in 1:5){
#'   z <- rchisq(100, df=5)
#'   plotQQ(z, function(p) qchisq(p, df=5), add=TRUE, args.qqline = NA,
#'          col="grey", lty="dotted")
#' }
#' 



#' @export
plotQQ <- function(x, qdist=stats::qnorm, main=NULL, xlab=NULL, ylab=NULL, 
                   datax=FALSE, add=FALSE,
                   args.qqline=NULL, conf.level=0.95, args.cband = NULL, ...) {
  

  .withGraphicsState({

    # qqplot for an optional distribution
    
    # example:
    # y <- rexp(100, 1/10)
    # plotQQ(y, function(p) qexp(p, rate=1/10))
    
    
    # resolve potential pars given in the dots...
    par(
      main = .resolvePar("main", main, "Q-Q Plot"),
      xlab = .resolvePar("xlab", xlab, "Theoretical Quantiles"),
      ylab = .resolvePar("ylab", ylab, "Sample Quantiles")
    )
    .applyParFromDots(...)
    
    y <- sort(x)
    p <- stats::ppoints(y)
    x <- qdist(p)
    
    if(datax){
      xy <- x
      x <- y
      y <- xy
      rm(xy)
    }
    
    if(is.null(main)) main <- gettextf("Q-Q-Plot (%s)", deparse(substitute(qdist)))
    if(is.null(xlab)) xlab <- "Theoretical Quantiles"
    if(is.null(ylab)) ylab <- "Sample Quantiles"
    
    if(!add)
      plot(x=x, y, main=main, xlab=xlab, ylab=ylab, type="n", ...)
    
    # add confidence band if desired
    if (!(is.na(conf.level) || identical(args.cband, NA)) ) {
      
      # cix <- qdist(ppoints(x))
      # ciy <- replicate(1000, sort(qdist(runif(length(x)))))
      # ci <- apply(ciy, 1, quantile, c(-1, 1) * conf.level/2 + 0.5)
      
      args.cband1 <- list(col = alpha(Pal()[1], 0.25), border = NA)
      if (!is.null(args.cband))
        args.cband1[names(args.cband)] <- args.cband
      
      # (x, distribution = qnorm,
      #  conf = 0.95, conf.method = "both",
      #  reference.line.method = "quartiles") {
  
      ci <- .create.qqplot.fit.confidence.interval(
                      y, distribution = qdist, 
                      conf=conf.level, conf.method = "pointwise");
      
      do.call("drawBand", c(args.cband1,
                            list(x = c(ci$z, rev(ci$z))),
                            list(y = c(ci$upper.pw, rev(ci$lower.pw)) )
      ))
      
    }
    
    points(x=x, y=y, ...)
    
    # John Fox implements an envelope option in car::qqplot, in the sense of:
    #   (unfortunately using ddist...)
    #
    #   # add qqline if desired
    #   if(!identical(args.band, NA)) {
    #     n <- length(x)
    #     zz <- qnorm(1 - (1 - args.band$conf.level) / 2)
    #     SE <- (slope / d.function(z, ...)) * sqrt(p * (1 - p) / n)
    #     fit.value <- int + slope * z
    #
    #     upper <- fit.value + zz * SE
    #     lower <- fit.value - zz * SE
    #
    #     lines(z, upper, lty = 2, lwd = lwd, col = col.lines)
    #     lines(z, lower, lty = 2, lwd = lwd, col = col.lines)
    #   }
    
    # example in qqplot
    #
    # ## "QQ-Chisquare" : --------------------------
    # y <- rchisq(500, df = 3)
    # ## Q-Q plot for Chi^2 data against true theoretical distribution:
    # qqplot(qchisq(ppoints(500), df = 3), y,
    #        main = expression("Q-Q plot for" ~~ {chi^2}[nu == 3]))
    # qqline(y, distribution = function(p) qchisq(p, df = 3),
    #        prob = c(0.1, 0.6), col = 2)
    # mtext("qqline(*, dist = qchisq(., df=3), prob = c(0.1, 0.6))")
    
    
    # add qqline if desired
    if(!identical(args.qqline, NA)) {
      
      # define default arguments for ci.band
      args.qqline1 <- list(probs = c(0.25, 0.75), qtype=7, col=par("fg"), 
                           lwd=par("lwd"), lty=par("lty"))
      # override default arguments with user defined ones
      if (!is.null(args.qqline)) args.qqline1[names(args.qqline)] <- args.qqline
      
      # estimate qqline, instead of set it to abline(a = 0, b = 1)
      # plot qqline through the 25% and 75% quantiles (same as qqline does for normal dist)
      ly <- stats::quantile(y, prob=args.qqline1[["probs"]], 
                     type=args.qqline1[["qtype"]], na.rm = TRUE)
      lx <- qdist(args.qqline1[["probs"]])
      
      slope <- diff(ly) / diff(lx)
      int <- ly[1L] - slope * lx[1L]
      do.call("abline", 
              c(args.qqline1[c("col","lwd","lty")], list(a=int, b=slope)) )
      
    }
    
  })
}



# == internal helper functions ========================================================

# The BoutrosLab.statistics.general package is copyright (c) 2012 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

.ks.test.critical.value <- function(n, conf, alternative = "two.sided") {
  
  if(alternative == "one-sided") conf <- 1- (1-conf)*2
  
  # for the small sample size
  
  if (n < 50) {
    # use the exact distribution from the C code in R
    exact.kolmogorov.pdf <- function(x) {
      # p <- .Call("pKolmogorov2x", p = as.double(x), as.integer(n), 
      #            PACKAGE = "DescToolsGraphics");
      p <- pKolmogorov2x(x, n)
      return(p - conf);
    }
    
    critical.value <- stats::uniroot(exact.kolmogorov.pdf, lower = 0, upper = 1)$root;
  }
  
  # if the sample size is large(>50), under the null hypothesis, the absolute value of the difference
  # of the empirical cdf and the theoretical cdf should follow a kolmogorov distribution
  
  if (n >= 50) {
    # pdf of the kolmogorov distribution minus the confidence level
    kolmogorov.pdf <- function(x) {
      i <- 1:10^4;
      sqrt(2*pi) / x * sum(exp(-(2*i - 1)^2*pi^2/(8*x^2))) - conf;
    }
    
    # the root of the function above
    # is the critical value for a specific confidence level multiplied by sqrt(n);
    critical.value <- stats::uniroot(kolmogorov.pdf, 
                                     lower = 10^(-6), upper = 3)$root / sqrt(n);
  }
  
  return(critical.value);
}



# The BoutrosLab.statistics.general package is copyright (c) 2011 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

.create.qqplot.fit.confidence.interval <- function(x, distribution = stats::qnorm,
                                                  conf = 0.95, conf.method = "both",
                                                  reference.line.method = "quartiles") {
  
  # remove the NA and sort the sample
  # the QQ plot is the plot of the sorted sample against the corresponding quantile from the theoretical distribution
  sorted.sample <- sort(x[!is.na(x)]);
  
  # the corresponding probabilities, should be (i - 0.5)/n, where i = 1,2,3,...,n
  probabilities <- stats::ppoints(length(sorted.sample));
  
  # the corresponding quantile under the theoretical distribution
  theoretical.quantile <- distribution(probabilities);
  
  if(reference.line.method == "quartiles") {
    # get the numbers of the 1/4 and 3/4 quantile in order to draw a reference line
    quantile.x.axis <- stats::quantile(sorted.sample, c(0.25, 0.75));
    quantile.y.axis <- distribution(c(0.25, 0.75));
    
    # the intercept and slope of the reference line
    b <- (quantile.x.axis[2] - quantile.x.axis[1]) / (quantile.y.axis[2] - quantile.y.axis[1]);
    a <- quantile.x.axis[1] - b * quantile.y.axis[1];
  }
  if(reference.line.method == "diagonal") {
    a <- 0;
    b <- 1;
  }
  if(reference.line.method == "robust") {
    coef.linear.model <- stats::coef(stats::lm(sorted.sample ~ theoretical.quantile));
    a <- coef.linear.model[1];
    b <- coef.linear.model[2];
  }
  
  
  # the reference line
  fit.value <- a + b * theoretical.quantile;
  
  # create some vectors to store the returned values
  upper.pw <- NULL;
  lower.pw <- NULL;
  upper.sim <- NULL;
  lower.sim <- NULL;
  u <- NULL;	# a vector of logical value of whether the probabilities are in the interval [0,1] for the upper band
  l <- NULL;	# a vector of logical value of whether the probabilities are in the interval [0,1] for the lower band
  
  ### pointwise method
  if (conf.method == "both" | conf.method == "pointwise") {
    
    # create the numeric derivative of the theoretical quantile distribution
    numeric.derivative <- function(p) {
      # set the change in independent variable
      h <- 10^(-6);
      if (h * length(sorted.sample) > 1) { h <- 1 / (length(sorted.sample) + 1); }
      # the function
      return((distribution(p + h/2) - distribution(p - h/2)) / h);
    }
    
    # the standard errors of pointwise method
    data.standard.error <- b * numeric.derivative(probabilities) * 
         stats::qnorm(1 - (1 - conf)/2) * sqrt(probabilities * 
                           (1 - probabilities) / length(sorted.sample));
    
    # confidence interval of pointwise method
    upper.pw <- fit.value + data.standard.error;
    lower.pw <- fit.value - data.standard.error;
  }
  
  ### simultaneous method
  if (conf.method == "both" | conf.method == "simultaneous") {
    
    # get the threshold value for the statistics---the absolute difference of the empirical cdf and the theoretical cdf
    # Note that this statistics should follow a kolmogorov distribution when the sample size is large
    
    # the critical value from the Kolmogorov-Smirnov Test
    critical.value <- .ks.test.critical.value(length(sorted.sample), conf);
    
    # under the null hypothesis, get the CI for the probabilities
    # the probabilities of the fitted value under the empirical cdf
    expected.prob <- stats::ecdf(sorted.sample)(fit.value);
    
    # the probability should be in the interval [0, 1]
    u <- (expected.prob + critical.value) >= 0 & (expected.prob + critical.value) <= 1;
    l <- (expected.prob - critical.value) >= 0 & (expected.prob - critical.value) <= 1;
    
    # get the corresponding quantiles from the theoretical distribution
    z.upper <- distribution((expected.prob + critical.value)[u]);
    z.lower <- distribution((expected.prob - critical.value)[l]);
    
    # confidence interval of simultaneous method
    upper.sim <- a + b * z.upper;
    lower.sim <- a + b * z.lower;
  }
  
  
  # return the values for constructing the Confidence Bands of one sample QQ plot
  # the list to store the returned values
  returned.values <- list(
    a = a,
    b = b,
    z = theoretical.quantile,
    upper.pw = upper.pw,
    lower.pw = lower.pw,
    u = u,
    l = l,
    upper.sim = upper.sim,
    lower.sim = lower.sim
  );
  return (returned.values);
}




