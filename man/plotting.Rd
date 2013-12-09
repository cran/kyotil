\name{plotting}
\alias{abline.pt.slope}
\alias{abline.pts}
\alias{butterfly.plot}
\alias{empty.plot}
\alias{getMfrow}
\alias{my.forest.plot}
\alias{my.interaction.plot}
\alias{myboxplot}
\alias{myboxplot.formula}
\alias{myboxplot.list}
\alias{mylegend}
\alias{mymatplot}
\alias{mypairs}
\alias{mypdf}
\alias{mypostscript}
\alias{panel.cor}
\alias{panel.hist}
\alias{panel.nothing}
\alias{corplot}
\alias{corplot.default}
\alias{corplot.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting Functions}
\description{
mypostscript and mypdf sets the width and height based on mfrow input. 
}
\usage{

%+%(a, b)

abline.pt.slope(pt1, slope, ...)

abline.pts(pt1, pt2 = NULL)

butterfly.plot(dat, dat2 = NULL, add = FALSE, xaxislabels = rep("", 4), x.ori = 0, 
    xlab = "", ylab = "", cex.axis = 1, ...)

empty.plot()

getMfrow(len)

my.forest.plot(dat, xlim = NULL, xlab = "", main = "", col.1 = "red", col.2 = "blue")

my.interaction.plot(dat, x.ori = 0, xaxislabels = rep("", 2), cex.axis = 1, add = FALSE,
    xlab = "", ylab = "", pcol = NULL, lcol = NULL, ...)

myboxplot(object, ...)

\method{myboxplot}{formula}(formula, data, cex = 0.5, ylab = "", xlab = "", main = "", 
box = TRUE, at = NULL, pch = 1, col = 1, test = c("t", "w"), ...)

\method{myboxplot}{list}(object, cex = 0.5, ylab = "", xlab = "", main = "", box = TRUE,
    at = NULL, pch = 1, col = 1, test = c("t", "w"), ...)

mylegend(legend, x, lty = NULL, bty = "n", ...)

mymatplot(x, make.legend = TRUE, legend = NULL, legend.x = 9, lty = 1:5, pch = NULL, 
    col = 1:6, legend.title = NULL, legend.cex = 1, xlab = NULL, ylab = "", 
    draw.x.axis = TRUE, bg = NA, ...)

mypairs(dat, method = "pearson")

mypdf(...)

mypostscript(file = "temp", mfrow = c(1, 1), mfcol = NULL, width = NULL, height = NULL, 
    ext = "eps", oma = NULL, mar = NULL, main.outer = FALSE, save2file = TRUE, ...)

panel.cor(x, y, digits = 2, prefix = "", cex.cor, method = "pearson", ...)

panel.hist(x, ...)

panel.nothing(x, ...)

corplot(object, ...)

\method{corplot}{default}(object, y, ...)

\method{corplot}{formula}(formula, data, main = "", method = c("pearson", "spearman"), 
    col = 1, cex = 0.5, plot.abline = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{


  \item{pt1}{
%%     ~~Describe \code{pt1} here~~
}
  \item{slope}{
%%     ~~Describe \code{slope} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}

  \item{pt2}{
%%     ~~Describe \code{pt2} here~~
}

  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{dat2}{
%%     ~~Describe \code{dat2} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{xaxislabels}{
%%     ~~Describe \code{xaxislabels} here~~
}
  \item{x.ori}{
%%     ~~Describe \code{x.ori} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{cex.axis}{
%%     ~~Describe \code{cex.axis} here~~
}



  \item{len}{
%%     ~~Describe \code{len} here~~
}

  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{col.1}{
%%     ~~Describe \code{col.1} here~~
}
  \item{col.2}{
%%     ~~Describe \code{col.2} here~~
}

  \item{pcol}{
%%     ~~Describe \code{pcol} here~~
}
  \item{lcol}{
%%     ~~Describe \code{lcol} here~~
}

  \item{object}{
%%     ~~Describe \code{object} here~~
}

  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{box}{
%%     ~~Describe \code{box} here~~
}
  \item{at}{
%%     ~~Describe \code{at} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{test}{
%%     ~~Describe \code{test} here~~
}


  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{bty}{
%%     ~~Describe \code{bty} here~~
}

  \item{make.legend}{
%%     ~~Describe \code{make.legend} here~~
}
  \item{legend.x}{
%%     ~~Describe \code{legend.x} here~~
}
  \item{legend.title}{
%%     ~~Describe \code{legend.title} here~~
}
  \item{legend.cex}{
%%     ~~Describe \code{legend.cex} here~~
}
  \item{draw.x.axis}{
%%     ~~Describe \code{draw.x.axis} here~~
}
  \item{bg}{
%%     ~~Describe \code{bg} here~~
}

  \item{method}{
%%     ~~Describe \code{method} here~~
}


  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{mfrow}{
%%     ~~Describe \code{mfrow} here~~
}
  \item{mfcol}{
%%     ~~Describe \code{mfcol} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{ext}{
%%     ~~Describe \code{ext} here~~
}
  \item{oma}{
%%     ~~Describe \code{oma} here~~
}
  \item{mar}{
%%     ~~Describe \code{mar} here~~
}
  \item{main.outer}{
%%     ~~Describe \code{main.outer} here~~
}
  \item{save2file}{
%%     ~~Describe \code{save2file} here~~
}

  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{prefix}{
%%     ~~Describe \code{prefix} here~~
}
  \item{cex.cor}{
%%     ~~Describe \code{cex.cor} here~~
}





  \item{plot.abline}{
%%     ~~Describe \code{plot.abline} here~~
}
}
\examples{
mypdf(mfrow=c(1,2))
    plot(1:10)
    plot(1:10)
dev.off()
}
