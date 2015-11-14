\name{base.functions}
\alias{cbind.uneven}
\alias{binary}
\alias{binary2}
\alias{f2c}
\alias{ftoi}
\alias{keepWarnings}
\alias{meanmed}
\alias{methods4}
\alias{myaggregate}
\alias{myreshapewide}
\alias{mysapply}
\alias{myscale}
\alias{mysystem}
\alias{mytapply}
\alias{read.sv}
\alias{read.tsv}
\alias{table.prop}
\alias{unix}
\alias{multi.outer}

\title{
Some Base Functions
}
\description{
cbind.uneven binds together a list of matrixes/dataframes of different lengths, rows are matched by names
binary returns binary representation of an integer. 
binary2 returns binary representatin of an integer with leading 0, the length of string is n.
mysystem can call any exe file that is in the PATH
f2c convert temperature from f to c/



}
\usage{

cbind.uneven(li)
binary(i)

multi.outer (f, ... ) 


binary2(i, n)

f2c(f)

ftoi(f)

keepWarnings(expr)

meanmed(x, na.rm = FALSE)

methods4(classes, super = FALSE, ANY = FALSE)

myaggregate(x, by, FUN, new.col.name = "aggregate.value", ...)

myreshapewide(formula, dat, idvar = NULL)

mysapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE, ret.mat = TRUE)

myscale(x)

mysystem(cmd, ...)

mytapply(X, INDEX, FUN = NULL, ..., simplify = TRUE)

read.sv(file, header = TRUE, ...)

read.tsv(file, header = TRUE, sep = "\t", ...)

table.prop(x, y, digit = 1, style = 2,whole.table.add.to.1=FALSE)

unix()
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{li}{a list}  
  \item{i}{
%%     ~~Describe \code{i} here~~
}

  \item{n}{
%%     ~~Describe \code{n} here~~
}

  \item{f}{
In multi.out, f is a function.}
  \item{formula}{
a formula object.}


  \item{expr}{
%%     ~~Describe \code{expr} here~~
}

  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}

  \item{classes}{
%%     ~~Describe \code{classes} here~~
}
  \item{super}{
%%     ~~Describe \code{super} here~~
}
  \item{ANY}{
%%     ~~Describe \code{ANY} here~~
}

  \item{by}{
%%     ~~Describe \code{by} here~~
}
  \item{whole.table.add.to.1}{
Boolean
}
  \item{new.col.name}{
%%     ~~Describe \code{new.col.name} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}

  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{idvar}{
%%     ~~Describe \code{idvar} here~~
}

  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{simplify}{
%%     ~~Describe \code{simplify} here~~
}
  \item{USE.NAMES}{
%%     ~~Describe \code{USE.NAMES} here~~
}
  \item{ret.mat}{
%%     ~~Describe \code{ret.mat} here~~
}


  \item{cmd}{
%%     ~~Describe \code{cmd} here~~
}

  \item{INDEX}{
%%     ~~Describe \code{INDEX} here~~
}

  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{header}{
%%     ~~Describe \code{header} here~~
}

  \item{sep}{
%%     ~~Describe \code{sep} here~~
}

  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{digit}{
%%     ~~Describe \code{digit} here~~
}
  \item{style}{
%%     ~~Describe \code{style} here~~
}

  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
}
\examples{

binary(5) ###  101
binary2(5, 4)


\dontrun{
# the formula in myreshapewide can only have one variable in the right hand side
    myreshapewide(fi~week, dat, c("ptid","stim"))
}

}
