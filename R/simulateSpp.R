##' @title Simulate species abundance data
##'
##' @description Simulate species abundance data according to stated approach
##' or following a particular methodology.
##'
##' @param n numeric; the number of samples/sites.
##' @param m numeric, the number of species/variables.
##' @param method character; one of the available model choices.
##' @param ... arguments passed on to the functions corresponding to the
##' method used.
##'
##' @return a matrix of \code{n} rows and \code{m} columns containing the
##' simulated species abundance data.
##'
##' @author Gavin L. Simpson
`simulateSpp` <- function(n, m, method = c("packing", "jamil"), ...) {
    method <- match.arg(method)

    ## dispatch internal function to do the simulation
    abun <- switch(method,
                   packing = simPacking(n, m, ...),
                   jamil   = simJamil(n, m, ...)
                   )

    ## return
    abun
}
