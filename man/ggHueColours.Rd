\name{ggHueColours}
\alias{ggHueColours}
\title{ggplot-esque colours from scale_colours_hue()}
\usage{
  ggHueColours(n, h = c(0, 360) + 15, l = 65, c = 100,
    direction = 1, h.start = 0)
}
\arguments{
  \item{n}{integer; number of colours to generate}

  \item{h}{numeric; length 2 giving range of hues to use,
  in [0, 360]}

  \item{l}{numeric; luminance (lightness), in [0, 100]}

  \item{c}{numeric; chroma (colour intensity)}

  \item{direction}{numeric; direction to travel around the
  colour wheel, 1 = clockwise, -1 = counter-clockwise}

  \item{h.start}{numeric; hue to start at}
}
\value{
  A vector of character strings which can be used as colour
  specifications by R graphics functions.
}
\description{
  A colour palette that returns colours akin to those used
  by ggplot for discrete colour scales.
}
\author{
  Gavin L. Simpson based on code by Hadley Wickham in his
  ggplot2 package.
}

