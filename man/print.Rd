\name{print.functions}
\alias{formatInt}
\alias{formatDouble}
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
formatInt prints a specified number of digits before decimal point even if 0s are needed at the beginning.
}
\usage{

formatInt(x, digits, fill = "0", ...)

make.latex.coef.table(models, model.names = NULL, row.major = FALSE, round.digits = NULL)

mytex (dat=NULL, file.name="temp", 
    digits=NULL, display=NULL, align="r", 
    include.rownames=TRUE, include.dup.rownames=FALSE, include.colnames=TRUE,
    col.headers=NULL,
    comment=FALSE, floating=FALSE, 
    lines=TRUE, hline.after=NULL, 
    add.to.row=NULL, 
    sanitize.text.function = NULL, #function(x) x,
    append=FALSE, preamble="", stand.alone=TRUE,
...) 
       
mytex.begin(file.name, preamble = "")

mytex.end(file.name)

mywrite(x, ...)

mywrite.csv(x, file = "tmp", row.names = FALSE, digits = NULL, ...)

roundup(value, digits)

formatDouble(value, digits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{include.colnames}{Boolean}
  \item{col.headers}{string. Column headers}
  \item{comment}{Boolean, whether to include the version and timestamp comment}
  \item{hline.after}{vector}
  \item{add.to.row}{a list}
  \item{sanitize.text.function}{a function}
  \item{stand.alone}{Boolean. If true, only one latex file that is stand alone file is made; otherwise both a file that is to be inputted and a standalone version are made}

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

formatInt(3, 2) # 03



}
