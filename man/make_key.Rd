\name{make_key}
\alias{make_key}
\title{Create a key to match group id's with x and y locations on a plot}
\usage{
  make_key(data, x.major, y.major)
}
\arguments{
  \item{data}{A data frame with a .gid variable}

  \item{x.major}{The name of the x.major variable, as a
  character string}

  \item{y.major}{The name of the y.major variable, as a
  character string}
}
\description{
  make_key uses the x.major and y.major variables of an
  mg_ensemble object to create a dataframe that shows where
  each group should be plotted according to an x.major and
  y.major axis.
}
\keyword{internal}

