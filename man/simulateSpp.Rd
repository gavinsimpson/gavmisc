\name{simulateSpp}
\alias{simulateSpp}
\title{Simulate species abundance data}
\usage{
  simulateSpp(n, m, method = c("packing", "jamil"), ...)
}
\arguments{
  \item{n}{numeric; the number of samples/sites.}

  \item{m}{numeric, the number of species/variables.}

  \item{method}{character; one of the available model
  choices.}

  \item{...}{arguments passed on to the functions
  corresponding to the method used.}
}
\value{
  a matrix of \code{n} rows and \code{m} columns containing
  the simulated species abundance data.
}
\description{
  Simulate species abundance data according to stated
  approach or following a particular methodology.
}
\author{
  Gavin L. Simpson
}

