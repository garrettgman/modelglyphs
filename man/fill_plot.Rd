\name{fill_plot}
\alias{fill_plot}
\title{Quickly create a tile plot for a function of the data}
\usage{
  fill_plot(ens, ...)
}
\arguments{
  \item{ens}{A mg_ensemble object}

  \item{fill}{A function call to be applied to each subset
  in the data. This function should return only one value.
  This value will be mapped to fill. If more than one value
  is returned, only the first value will be used. Variables
  in ens should be referred to by name only. The call will
  be evaluated with \code{\link{with}} where each subset
  will be used as the enclosing data object. ... further
  arguments to be passed to geom_tile}
}
\description{
  fill_plot applies a fill function to each subset of data
  in an mg_ensemble object and then displays the results as
  fills in a tileplot. If the subsets are arranged evenly
  on the x, y grid, each tile will correspong to one subset
  in the ensemble.
}

