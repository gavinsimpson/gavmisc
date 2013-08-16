##' @title ggplot-esque colours from scale_colours_hue()
##'
##' @description A colour palette that returns colours akin to those
##' used by ggplot for discrete colour scales.
##'
##' @param n integer; number of colours to generate
##' @param h numeric; length 2 giving range of hues to use, in [0, 360]
##' @param l numeric; luminance (lightness), in [0, 100]
##' @param c numeric; chroma (colour intensity)
##' @param direction numeric; direction to travel around the colour wheel,
##'   1 = clockwise, -1 = counter-clockwise
##' @param h.start numeric; hue to start at
##' @return A vector of character strings which can be used as colour
##'   specifications by R graphics functions.
##' @author Gavin L. Simpson based on code by Hadley Wickham in his ggplot2
##'   package.
##'
ggHueColours <- function(n, h = c(0, 360) + 15, l = 65, c = 100,
                         direction = 1, h.start = 0) {
    turn <- function(x, h.start, direction) {
        (x + h.start) %% 360 * direction
    }

    if ((diff(h) %% 360) < 1) {
        h[2] <- h[2] - 360 / n
    }

    hcl(h = turn(seq(h[1], h[2], length = n), h.start = h.start,
        direction = direction), c = c, l =  l)
}
