\name{model}
\alias{model}
\title{Pass modelling information to ensemble()}
\usage{
  model(formula, FUN = "lm", ...)
}
\arguments{
  \item{formula}{A formula object to be used in the
  intended model}

  \item{FUN}{a character string that identifies the type of
  modelling method to be used. FUN should match the name of
  a modelling function in R}

  \item{...}{other arguments to pass to the modelling
  function indicated by type}
}
\description{
  model collects the arguments needed to perform a method
  of regression. model is intended to be used in
  conjunction with \code{\link{ensemble}} to create
  ensemble objects. model does not collect the name of the
  data set to be used.
}
\keyword{export}
\keyword{internal}

