##' @title Simulate species abundance data following Jamil & ter Braak
##' (2013)
##'
##' @description Simulate species abundance data according to stated
##' approach or following a particular methodology.
##'
##' @param n numeric; the number of samples/sites.
##' @param m numeric, the number of species/variables.
##' @param x numeric; values for the environmental gradient. Can be
##' missing, in which case suitbale values are generated. See Details.
##' @param gl numeric; gradient length in arbitrary units. The default
##' is 4 units with gradient values ranging from -2 to 2.
##' @param randx logical; should locations along the gradient (\code{x})
##' be located randomly or equally-spaced?
##' @param tol numeric; the species tolerances. Can be a vector of
##' length \code{m}, hence allowing for varying tolerances along the
##' gradient \code{x}.
##' @param tau numeric; constant that ensures some of the optima are
##' located beyond the observed gradient end points.
##' @param randm logical; should species optima along the gradient be
##' located randomly or equally-spaced?
##' @param ... arguments passed on to the functions corresponding to the
##' method used.
##'
##' @param method character; one of the available model choices.
##' @return a matrix of \code{n} rows and \code{m} columns containing the
##' simulated species abundance data.
##'
##' @author Gavin L. Simpson
`simJamil` <- function(n, m, x, gl = 4, randx = TRUE,
                       tol = 0.5, tau = gl/2, randm = TRUE, ...) {
    if(missing(x)) {
        ## generate n values as random sample from
        ## uniform(-gl/2, gl/2)
        gl <- round(gl) / 2
        if(randx)
            x <- runif(n, min = -gl, max = gl)
        else
            x <- seq.int(from = -gl, to = gl, length.out = n)
        x <- sort(x)
    } else {
        if(!isTRUE(all.equal(length(x), n)))
            warning("Length of supplied 'x' != 'n'. Making 'x' match 'n'.")
        x <- rep_len(x, length.out = n)
    }

    ## vector u of m optima from uniform(-tau+tol, tau+tol)
    taut <- tau + tol
    if(randm)
        u <- runif(m, min = -taut, max = taut)
    else
        u <- seq.int(from = -taut, to = taut, length.out = m)

    ## generate vector a of length m from normal distribution
    a <- rnorm(m)

    ## generate binomial probabilities pij from unimodal response curve
    pij <- plogis(t(a - t(outer(x, u, "-")^2 / (2*tol^2))))

    pij
}
