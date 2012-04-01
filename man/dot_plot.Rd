\name{dot_plot}
\alias{dot_plot}
\title{Quickly plot point plots of grouped data}
\usage{
  dot_plot(ens, ..., x.major, y.major, facets = NULL,
    polar = FALSE, height = rel(0.95), width = rel(0.95),
    y_scale = identity, x_scale = identity, quiet = TRUE)
}
\arguments{
  \item{ens}{A dataframe that inherits from class
  `grouped'}

  \item{x.major}{The variable to map to the x axis. If
  x.major is not specified, dot_plot will use the x.major
  attribute of ens, see \code{\link{x_major}}.}

  \item{y.major}{The variable to map to the x axis. If
  x.major is not specified, dot_plot will use the y.major
  attribute of ens, see \code{\link{y_major}}.}

  \item{facets}{Facetting formula to use. Picks
  \{code{\link{facet_wrap}} or \code{\link{facet_grid}}
  depending on whether the formula is one or two sided.}

  \item{polar}{A logical of length 1, specifying whether
  x.minor and y.minor aesthetics should be transformed to
  polar coordinates.  Defaults to \code{FALSE}. See
  \code{\link{glyphs}}. polar is ignored if x.minor and
  y.minor aesthetics are not specified.}

  \item{height,width}{The height and width of each glyph.
  See \code{\link{glyphs}}. Defaults to 95\% of the
  \code{\link[ggplot2]{resolution}} of the data. Specify
  the width absolutely by supplying a numeric vector of
  length 1, or relative to the resolution of the data by
  using \code{\link{rel}}. width and height are ignored if
  x.minor and y.minor aesthetics are not specified.}

  \item{x_scale,y_scale}{The scaling function to be applied
  to each set of minor values within a grid cell.  Defaults
  to \code{\link{identity}} so that no scaling is
  performed.  See \code{\link{glyphs}}. x_scale and y_scale
  are ignored if x.minor and y.minor aesthetics are not
  specified.}

  \item{quiet}{A logical that determines whether glyphing
  messages are suppressed when transforming x.minor and
  y.minor. See \code{\link{glyphs}}. x_scale and y_scale
  are ignored if x.minor and y.minor aesthetics are not
  specified.}
}
\description{
  dot_plot plots data that has been organized into groups
  by the modelglyphs package. Each group is summarized by a
  single point whose aesthetics are set by the call to
  dot_plot. Aesthetics may be mapped to functions of the
  existing data. dot_plot automatically applies any
  transformations to the data in a groupwise fashion. Since
  each group is represented by a single point, a viable
  transformation must return only one value per group.
}
\details{
  If x.minor and y.minor arguments are supplied to
  dot_plot, dot_plot will use \code{\link{glyphs}} to
  position points relative to their location on the x.major
  and y.major axiis.

  dot_plot is a wrapper for \code{\link{ggplot}} and
  \code{\link{geom_point}} form the ggplot2 package.
}

