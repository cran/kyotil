\name{print.functions}
\alias{format.int}
\alias{make.latex.coef.table}
\alias{mytex}
\alias{mytex.begin}
\alias{mytex.end}
\alias{mywrite}
\alias{mywrite.csv}
\alias{roundup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print Functions
}
\description{
roundup prints a specified number of digits after decimal point even if 0s are needed at the end. 
format.int prints a specified number of digits before decimal point even if 0s are needed at the beginning.
}
\usage{

\method{format}{int}(x, digits, fill = "0", ...)

make.latex.coef.table(models, model.names = NULL, row.major = FALSE, round.digits = NULL)

mytex(dat = NULL, file.name = "temp", digits = NULL, display = NULL, align = "r", 
    append = FALSE, preamble = "", include.rownames = TRUE, include.dup.rownames = FALSE,
    floating = FALSE, lines = TRUE, ...)

mytex.begin(file.name, preamble = "")

mytex.end(file.name)

mywrite(x, ...)

mywrite.csv(x, file = "tmp", row.names = FALSE, digits = NULL, ...)

roundup(value, digits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{value}{
%%     ~~Describe \code{value} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{fill}{
%%     ~~Describe \code{fill} here~~
}

  \item{models}{
%%     ~~Describe \code{models} here~~
}
  \item{model.names}{
%%     ~~Describe \code{model.names} here~~
}
  \item{row.major}{
%%     ~~Describe \code{row.major} here~~
}
  \item{round.digits}{
%%     ~~Describe \code{round.digits} here~~
}

  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
}
  \item{display}{
%%     ~~Describe \code{display} here~~
}
  \item{align}{
%%     ~~Describe \code{align} here~~
}
  \item{append}{
%%     ~~Describe \code{append} here~~
}
  \item{preamble}{
%%     ~~Describe \code{preamble} here~~
}
  \item{include.rownames}{
%%     ~~Describe \code{include.rownames} here~~
}
  \item{include.dup.rownames}{
%%     ~~Describe \code{include.dup.rownames} here~~
}
  \item{floating}{
%%     ~~Describe \code{floating} here~~
}
  \item{lines}{
%%     ~~Describe \code{lines} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}



  \item{x}{
%%     ~~Describe \code{x} here~~
}

  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{row.names}{
%%     ~~Describe \code{row.names} here~~
}

}
\examples{

roundup (3.1, 2) # 3.10

format.int(3, 2) # 03

}
