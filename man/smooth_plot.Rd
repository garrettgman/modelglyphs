\name{smooth_plot}
\alias{smooth_plot}
\title{Quickly plot smoothed model summaries of grouped data}
\usage{
  smooth_plot(ens, ..., x.minor, y.minor, x.major, y.major,
    facets = NULL, polar = FALSE, height = rel(0.95),
    width = rel(0.95), y_scale = identity,
    x_scale = identity, quiet = TRUE, method, se)
}
\arguments{
  \item{ens}{A dataframe that inherits from class
  `grouped'}

  \item{x.minor}{The variable to map to the local x axis
  for each sub-plot.}

  \item{y.minor}{The variable to map to the local y axis
  for each sub-plot.}

  \item{x.major}{The variable to map to the global x axis.
  If x.major is not specified, smooth_plot will use the
  x.major attribute of ens, see \code{\link{x_major}}.}

  \item{y.major}{The variable to map to the global y axis.
  If x.major is not specified, smooth_plot will use the
  y.major attribute of ens, see \code{\link{y_major}}.}

  \item{facets}{Facetting formula to use. Picks
  \{code{\link{facet_wrap}} or \code{\link{facet_grid}}
  depending on whether the formula is one or two sided.}

  \item{polar}{A logical of length 1, specifying whether
  x.minor and y.minor aesthetics should be transformed to
  polar coordinates.  Defaults to \code{FALSE}. See
  \code{\link{glyphs}}.}

  \item{height,width}{The height and width of each glyph.
  See \code{\link{glyphs}}. Defaults to 95\% of the
  \code{\link[ggplot2]{resolution}} of the data. Specify
  the width absolutely by supplying a numeric vector of
  length 1, or relative to the resolution of the data by
  using \code{\link{rel}}.}

  \item{x_scale,y_scale}{The scaling function to be applied
  to each set of minor values within a grid cell.  Defaults
  to \code{\link{identity}} so that no scaling is
  performed.  See \code{\link{glyphs}}.}

  \item{quiet}{A logical that determines whether glyphing
  messages are suppressed when transforming x.minor and
  y.minor. See \code{\link{glyphs}}.}
}
\description{
  smooth_plot is a wrapper for \code{\link{ggplot}} and
  \code{\link{geom_point}} form the ggplot2 package.
}

