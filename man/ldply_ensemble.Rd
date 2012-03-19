\name{ldply_ensemble}
\alias{ldply_ensemble}
\title{Performs ldply, then labels and reorders output}
\usage{
  ldply_ensemble(ens, FUN, ...)
}
\arguments{
  \item{ens}{An ensemble object}

  \item{FUN}{A function to be applied to the model objectss
  in the ensemble object}

  \item{arguments}{to be passed to FUN}
}
\description{
  ldply_ensemble applies a function to an ensemble object
  and then organizes the output to match the data frame the
  ensemble object was created from. \code{\link{glyphs}}
  can be immediately applied to the output without
  preprocessing it. The output can also be merged to the
  original data frame with cbind.
}
\keyword{internal}

