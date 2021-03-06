\name{make_key}
\alias{make_key}
\title{Create a key to match group id's with x and y locations on a plot}
\usage{
  make_key(ens)
}
\arguments{
  \item{ens}{an object of class mg_ensemble}
}
\description{
  make_key uses the x_major, y_major, and groups attributes
  of an mg_ensemble object to create a dataframe that shows
  where each group should be plotted according to an
  x_major and y_major axis.
}
\keyword{internal}

