\name{glyphs}
\alias{glyphs}
\title{Create the data needed to generate a glyph plot.}
\usage{
  glyphs(data, x.minor, y.minor, x.major = NULL,
    y.major = NULL, polar = FALSE, height = rel(0.95),
    width = rel(0.95), y_scale = identity,
    x_scale = identity, quiet = FALSE)
}
\arguments{
  \item{data}{A data frame containing variables named in
  \code{x.minor}, \code{y.minor}, \code{x.major} and
  \code{y.major}.}

  \item{x.minor,y.minor,x.major,y.major}{The name of the
  variable (as a string) for the major and minor x and y
  axes.  Together, each unique}

  \item{polar}{A logical of length 1, specifying whether
  the glyphs should be drawn in polar coordinates.
  Defaults to \code{FALSE}.}

  \item{height,width}{The height and width of each glyph.
  Defaults to 95\% of the \code{\link[ggplot2]{resolution}}
  of the data. Specify the width absolutely by supplying a
  numeric vector of length 1, or relative to the}

  \item{y_scale,x_scale}{The scaling function to be applied
  to each set of minor values within a grid cell.  Defaults
  to \code{\link{identity}} so that no scaling is
  performed.}
}
\description{
  Create the data needed to generate a glyph plot.
}

