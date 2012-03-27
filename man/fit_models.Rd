\name{fit_models}
\alias{fit_models}
\title{Performs group-wise maodelling on an mg_ensemble object}
\usage{
  fit_models(data, .fun, formula, ...)
}
\arguments{
  \item{data}{An mg_ensemble object}

  \item{.fun}{A modelling function to use}

  \item{formula,}{the modelling formula to use}

  \item{...}{additional arguments to pass to the modelling
  function}
}
\description{
  fit_models fits a model to each subset of data in an
  mg_ensemble object.
}

