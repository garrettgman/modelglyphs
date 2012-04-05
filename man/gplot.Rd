\name{gplot}
\alias{gplot}
\title{Plot grouped data
gplot is a wrapper for \code{\link{ggplot}} that preprocesses grouped data before plotting. gplot allows the user to specify both minor and major x, y axiis and uses \code{\link{glyphs}} to resolve the resulting position aesthetics. It also applies all mappings in a groupwise fashion. Unlike ggplot, gplot can only plot one aesthetic at a time. However, gplot returns a ggplot object which can be combined with additional layers and ggplot2 objects in the usual fashion.}
\usage{
  gplot(ens, ..., x.major = NULL, y.major = NULL,
    x.minor = NULL, y.minor = NULL, geom = "point",
    facets = NULL, polar = FALSE, height = rel(0.95),
    width = rel(0.95), y_scale = identity,
    x_scale = identity, quiet = TRUE)
}
\arguments{
  \item{ens}{A data frame of class 'grouped'}

  \item{...}{aesthetics and parameters to be passed on to
  the \code{\link{ggplot}} call}

  \item{x.major}{variable or function of a variable to be
  used for the major x axis}

  \item{y.major}{variable or function of a variable to be
  used for the major y axis}

  \item{x.minor}{variable or function of a variable to be
  used for the minor x axis}

  \item{y.minor}{variable or function of a variable to be
  used for the minor y axis}

  \item{geom}{character vector specifying geom to use.}

  \item{facets}{faceting formula to use. Picks
  \code{\link{facet_wrap}} or \code{\link{facet_grid}}
  depending on whether the formula is one sided or
  two-sided}

  \item{polar,height,width,y_scale,x_scale,quiet}{arguments
  to be passedo n to glyphs when resolving x and y position
  aesthetics.}
}
\description{
  Plot grouped data gplot is a wrapper for
  \code{\link{ggplot}} that preprocesses grouped data
  before plotting. gplot allows the user to specify both
  minor and major x, y axiis and uses \code{\link{glyphs}}
  to resolve the resulting position aesthetics. It also
  applies all mappings in a groupwise fashion. Unlike
  ggplot, gplot can only plot one aesthetic at a time.
  However, gplot returns a ggplot object which can be
  combined with additional layers and ggplot2 objects in
  the usual fashion.
}

