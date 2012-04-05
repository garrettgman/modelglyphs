\name{gmodel}
\alias{gmodel}
\title{Performs group-wise modelling on a grouped data frame}
\usage{
  gmodel(data, .fun, formula, ...)
}
\arguments{
  \item{data}{A data frame of class 'grouped'}

  \item{.fun}{A character string. The name of the modelling
  function to use}

  \item{formula,}{the modelling formula to use}

  \item{...}{additional arguments to pass to the modelling
  function}
}
\description{
  gmodels fits a model to each subset of data in a grouped
  data frame.
}

