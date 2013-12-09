\name{math.functions}
\alias{math.functions}
\alias{as.binary}
\alias{binom.coef}
\alias{expit}
\alias{H}
\alias{logDiffExp}
\alias{logit}
\alias{logMeanExp}
\alias{logSumExp}
\alias{logSumExpFor2}
\alias{permn}
\alias{Stirling2}
\title{
Math Functions
}
\description{
H calculates entropy. 
}
\usage{

as.binary(n, base = 2, r = FALSE)

binom.coef(n, m)

expit(x)

H(p)

logDiffExp(logx1, logx2)

logit(x)

logMeanExp(logx, B = NULL)

logSumExp(logx)

logSumExpFor2(logx, logy)

permn(x, fun = NULL, ...)

Stirling2(n, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{base}{
%%     ~~Describe \code{base} here~~
}
  \item{r}{
%%     ~~Describe \code{r} here~~
}

  \item{m}{
%%     ~~Describe \code{m} here~~
}

  \item{x}{
%%     ~~Describe \code{x} here~~
}

  \item{p}{
either a count vector or a probability vector, but can not be a vector of membership indicator
}

  \item{logx1}{
%%     ~~Describe \code{logx1} here~~
}
  \item{logx2}{
%%     ~~Describe \code{logx2} here~~
}


  \item{logx}{
%%     ~~Describe \code{logx} here~~
}
  \item{B}{
%%     ~~Describe \code{B} here~~
}


  \item{logy}{
%%     ~~Describe \code{logy} here~~
}

  \item{fun}{
%%     ~~Describe \code{fun} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}

}

\examples{
H(rep(1/5,5))
H(rep(3,5))
}
