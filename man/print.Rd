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

mytex(dat=NULL, file.name="temp", 
    digits=NULL, display=NULL, align="r", 
    include.rownames=TRUE, include.colnames=TRUE,
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

roundup (value, digits, na.to.empty=TRUE) 

formatDouble(value, digits, na.to.empty=TRUE)

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

  \item{na.to.empty}{
%%     ~~Describe \code{value} here~~
}
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


\dontrun{ 

# col.headers has to have the RIGHT number of columns in the next three examples!

# include.rownames=T, no duplicate rownames, align can have 4/5 elements, col.headers has 5 columns
tab=diag(1:4); rownames(tab)<-colnames(tab)<-1:4
mytex (tab, file="tmp", include.rownames = TRUE, 
    align=c("c","c","c|","c","c"), col.headers=
    "\\hline\n & \\multicolumn{2}{c|}{Vaccine} & \\multicolumn{2}{c}{Control} \\\\ \n")

# include.rownames=T, duplicate rownames, align can have 4 or 5 elements, col.headers has 5 columns
tab=diag(1:4); rownames(tab)=rep(1,4); colnames(tab)<-1:4
mytex (tab, file="tmp", include.rownames = TRUE, 
    align=c("c","c|","c","c"), col.headers=
    "\\hline\n & \\multicolumn{2}{c|}{Vaccine} & \\multicolumn{2}{c}{Control} \\\\ \n") 

# include.rownames=F, align can have 4 or 5 elements, col.headers has 4 columns!
tab=diag(1:4); rownames(tab)<-colnames(tab)<-1:4
mytex (tab, file="tmp", include.rownames = FALSE, 
    align=c("c","c","c|","c","c"), col.headers=
    "\\hline\n     \\multicolumn{2}{c|}{Vaccine} & \\multicolumn{2}{c}{Control} \\\\ \n") 

}



}
