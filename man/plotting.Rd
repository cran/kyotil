\name{plotting}
\alias{abline.pt.slope}
\alias{abline.pts}
\alias{abline.shade}
\alias{butterfly.plot}
\alias{empty.plot}
\alias{getMfrow}
\alias{myforestplot}
\alias{my.interaction.plot}
\alias{myboxplot}
\alias{myboxplot.formula}
\alias{myboxplot.data.frame}
\alias{myboxplot.list}
\alias{mylegend}
\alias{mymatplot}
\alias{mypairs}
\alias{myhist}
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

abline.pt.slope(pt1, slope, x2=NULL, ...)

abline.pts(pt1, pt2 = NULL)

butterfly.plot(dat, dat2 = NULL, add = FALSE, xaxislabels = rep("", 4), x.ori = 0, 
    xlab = "", ylab = "", cex.axis = 1, ...)

empty.plot()

getMfrow(len)

myhist (x, add.norm=TRUE, col.norm="blue", ...)

myforestplot(dat, xlim=NULL, xlab="", main="", col.1="red", col.2="blue",
 plot.labels=TRUE,order=FALSE,decreasing=FALSE, vline=TRUE,cols=NULL,log="")

my.interaction.plot(dat, x.ori = 0, xaxislabels = rep("", 2), cex.axis = 1, add = FALSE,
    xlab = "", ylab = "", pcol = NULL, lcol = NULL, ...)

myboxplot(object, ...)

abline.shade(x, col=c(0,1,0))


\method{myboxplot}{formula}(formula, data, cex=.5, xlab="", ylab="", main="", box=TRUE, 
at=NULL, na.action=NULL, pch=1, col=1, test="", reshape.formula=NULL, jitter=TRUE, 
add.interaction=FALSE,  drop.unused.levels = TRUE, ...)
    

\method{myboxplot}{data.frame}(object, cex = 0.5, ylab = "", xlab = "", main = "", box = TRUE,
    at = NULL, pch = 1, col = 1, test = "", ...)

\method{myboxplot}{list}(object, ...)

mylegend(legend, x, lty = NULL, bty = "n", ...)

mymatplot (x, y, type="b", lty=1:5, pch=NULL, col=1:6, xlab=NULL, ylab="", 
    draw.x.axis=TRUE, bg=NA, lwd=1, at=NULL, make.legend=TRUE, legend=NULL, 
    legend.x=9, legend.title=NULL, legend.cex=1, legend.inset=0, ...) 
                 
mypairs(dat, ...)

mypdf(...)

mypostscript(file = "temp", mfrow = c(1, 1), mfcol = NULL, width = NULL, height = NULL, 
    ext = "eps", oma = NULL, mar = NULL, main.outer = FALSE, save2file = TRUE, ...)

panel.cor(x, y, digits = 2, prefix = "", cex.cor, ...)

panel.hist(x, ...)

panel.nothing(x, ...)

corplot(object, ...)

\method{corplot}{default}(object, y, ...)

\method{corplot}{formula}(formula, data, main = "", method = c("pearson", "spearman"), 
col=1,cex=.5,add.diagonal.line=TRUE,add.lm.fit=FALSE,col.lm=2,add.deming.fit=FALSE,
    col.deming=4,add=FALSE,log="",same.xylim=FALSE,xlim=NULL,ylim=NULL, ...)



}

\arguments{


  \item{add.norm}{Boolean, whether to add normal approximation density line}
  \item{col.norm}{string, color of added normal density line}
  \item{pt1}{
%%     ~~Describe \code{pt1} here~~
}
  \item{slope}{
%%     ~~Describe \code{slope} here~~
}
  \item{jitter}{Boolean}
  \item{add.interaction}{Boolean}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}

  \item{pt2}{
%%     ~~Describe \code{pt2} here~~
}

  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{lwd}{
line width.
}
  \item{legend.inset}{
legend inset
}
  \item{dat2}{
%%     ~~Describe \code{dat2} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{log}{
}
  \item{add.lm.fit}{
}
  \item{add.deming.fit}{
}
  \item{col.lm}{
}
  \item{col.deming}{
}
  \item{reshape.formula}{
a formula object.
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

  \item{same.xylim}{Boolean. Whether xlim and ylim should be the same
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
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
string. For example, "t","w","f","k", "tw"
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

  \item{type}{
%%     ~~Describe \code{type} here~~
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
}
  \item{plot.labels}{Boolean}
  \item{order}{Boolean}
  \item{decreasing}{Boolean}
  \item{add.diagonal.line}{
%%     ~~Describe \code{add.diagonal.line} here~~
}
  \item{x2}{
%%     ~~Describe \code{add.diagonal.line} here~~
}
  \item{vline}{
%%     ~~Describe \code{add.diagonal.line} here~~
}
  \item{cols}{
%%     ~~Describe \code{add.diagonal.line} here~~
}
  \item{na.action}{
%%     ~~Describe \code{add.diagonal.line} here~~
}
  \item{drop.unused.levels}{
%%     ~~Describe \code{add.diagonal.line} here~~
}
}

\details{
myboxplot shows data points along with boxes. The data poins are jittered and the pattern of jittering is made reproducible in repeated calls. The test can only take one type of test currently.

myforestplot is modified from code from Allan deCamp/SCHARP.
dat should have three columns. first column should be point estimate, second and third lci and uci, fourth p value. 
col.1 is the color used for CIs that do not include null, col.2 is used for CIs that do include null.
If order is TRUE, the rows are ordered by the first column of dat. descreasing can be used to change the behavior of order.

corplot.formula uses MethComp::Deming by Bendix Carstensen to fit Deming regression.
}

\examples{
mypdf(mfrow=c(1,2))
    plot(1:10)
    plot(1:10)
dev.off()

set.seed(1)
x=1:50+rnorm(50,0,4)
y=1:50+rnorm(50,0,4)
dat=data.frame(x, y)
corplot(y~x,dat,add.lm.fit=TRUE,add.deming.fit=TRUE,col.lm="red",col.deming="blue")

}
