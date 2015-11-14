\name{random.functions}
\alias{dbern}
\alias{dcorbern}
\alias{dmixnorm}
\alias{dnorm.norm.gamma}
\alias{rbern}
\alias{rejective.sampling}
\alias{rnorm.cor}
\alias{rmixnorm}
\alias{rnorm.norm.gamma}
\title{
Random Functions
}
\description{
rbern generates Bernoulli random variables.
}
\usage{

dbern(x, prob, log = FALSE)

dcorbern(x, p, a, log = FALSE)

dmixnorm(x, mix.p, sd1, sd2, log = FALSE)

dnorm.norm.gamma(x, p, same.distr = FALSE, log = FALSE)

rbern(n, prob, generalized = FALSE)

rejective.sampling(N, n, pik)

rnorm.cor(n, mu, sd, alpha)

rnorm.norm.gamma(n, mu.0, lambda, alpha, beta)

rmixnorm (n, mix.p, mu1, mu2, sd1, sd2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{prob}{
%%     ~~Describe \code{prob} here~~
}
  \item{log}{
%%     ~~Describe \code{log} here~~
}

  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{a}{
%%     ~~Describe \code{a} here~~
}

  \item{mix.p}{
%%     ~~Describe \code{mix.p} here~~
}
  \item{sd1}{
%%     ~~Describe \code{sd1} here~~
}
  \item{sd2}{
%%     ~~Describe \code{sd2} here~~
}

  \item{same.distr}{
%%     ~~Describe \code{same.distr} here~~
}

  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{generalized}{
%%     ~~Describe \code{generalized} here~~
}

  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{pik}{
%%     ~~Describe \code{pik} here~~
}

  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{mu1}{
%%     ~~Describe \code{mu} here~~
}
  \item{mu2}{
%%     ~~Describe \code{mu} here~~
}
  \item{sd}{
%%     ~~Describe \code{sd} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}

  \item{mu.0}{
%%     ~~Describe \code{mu.0} here~~
}
  \item{lambda}{
%%     ~~Describe \code{lambda} here~~
}
  \item{beta}{
%%     ~~Describe \code{beta} here~~
}
}
\examples{

set.seed(1)
rbern(n=10, p=1/2)
rbern(n=2, p=c(.999,.001))

}
