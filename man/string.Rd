\name{string.functions}
\title{String Functions}
\description{ \%+\% concatenates its arguments and returns a string.}
\usage{

a \%+\% b

contain(s1, s2)

endsWith(s1, s2)

escapeUnderline(name)

fileStem(file.name)

firstIndex(s1, s2)

getExt(file.name)

getFileStem(file.name)

getStem(file.name)

lastIndex(s1, s2)

myprint(object, ...)

\method{myprint}{default}(..., newline = TRUE, digits = 3)

remove.prefix(s, sep = "_")

startsWith(s1, s2)
}

\alias{string.functions}
\alias{\%+\%}
\alias{contain}
\alias{endsWith}
\alias{escapeUnderline}
\alias{fileStem}
\alias{firstIndex}
\alias{getExt}
\alias{getFileStem}
\alias{getStem}
\alias{lastIndex}
\alias{myprint}
\alias{myprint.default}
\alias{remove.prefix}
\alias{startsWith}


\arguments{

  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}

  \item{s1}{
%%     ~~Describe \code{s1} here~~
}
  \item{s2}{
%%     ~~Describe \code{s2} here~~
}


  \item{name}{
%%     ~~Describe \code{name} here~~
}

  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
}






  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}

  \item{newline}{
%%     ~~Describe \code{newline} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}

  \item{s}{
%%     ~~Describe \code{s} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
}

}
\examples{

x=1
x \%+\% "b" \%+\% "c"

}
