\name{get.sim.res}
\alias{get.sim.res}
\title{
Read simulation results
}
\description{
Go through a folder and read all files and combine the results into a multidimensional array.
}
\usage{
get.sim.res(foldername, verbose = TRUE)
}
\arguments{
  \item{foldername}{string
}
  \item{verbose}{
Boolean
}
}
\details{
Depends on package abind to combine arrays from files.
}
\value{
A multidimensional array.
}
