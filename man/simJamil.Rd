\name{simJamil}
\alias{simJamil}
\title{Simulate species abundance data following Jamil & ter Braak
(2013)}
\usage{
  simJamil(n, m, x, gl = 4, randx = TRUE, tol = 0.5,
    tau = gl/2, randm = TRUE, ...)
}
\arguments{
  \item{n}{numeric; the number of samples/sites.}

  \item{m}{numeric, the number of species/variables.}

  \item{x}{numeric; values for the environmental gradient.
  Can be missing, in which case suitbale values are
  generated. See Details.}

  \item{gl}{numeric; gradient length in arbitrary units.
  The default is 4 units with gradient values ranging from
  -2 to 2.}

  \item{randx}{logical; should locations along the gradient
  (\code{x}) be located randomly or equally-spaced?}

  \item{tol}{numeric; the species tolerances. Can be a
  vector of length \code{m}, hence allowing for varying
  tolerances along the gradient \code{x}.}

  \item{tau}{numeric; constant that ensures some of the
  optima are located beyond the observed gradient end
  points.}

  \item{randm}{logical; should species optima along the
  gradient be located randomly or equally-spaced?}

  \item{...}{arguments passed on to the functions
  corresponding to the method used.}

  \item{method}{character; one of the available model
  choices.}
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

