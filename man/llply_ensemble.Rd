\name{llply_ensemble}
\alias{llply_ensemble}
\title{Performs llply, then reorders output and returns as a vector}
\usage{
  llply_ensemble(model, FUN, ...)
}
\arguments{
  \item{model}{An ensemble object}

  \item{FUN}{A function to be applied to the model objectss
  in the ensemble object}

  \item{arguments}{to be passed to FUN}
}
\description{
  llply_ensemble applies a function to an ensemble object
  and then organizes the output to match the data frame the
  ensemble object was created from. The output can be
  merged to the original data frame with cbind. Or by
  directly saving it as a new column in the dataframe.
}
\keyword{internal}

