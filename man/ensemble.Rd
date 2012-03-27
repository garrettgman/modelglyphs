\name{ensemble}
\alias{ensemble}
\title{Ensemble organizes a data set into an ensemble of sub-data sets.}
\usage{
  ensemble(data, grouping, x.major = NULL, y.major = NULL)
}
\arguments{
  \item{data}{a data frame to organize as an ensemble of
  sub data sets}

  \item{grouping}{a mg_group object to use for splitting
  data into subsets}

  \item{x.major}{a character string that specifies the
  variable to be used on the x axis when plotting the
  ensembles. Defaults to the first grouping variable.}

  \item{y.major}{a character string that specifies the
  variable to be used on the y axis when plotting the
  ensembles. Defaults to the second grouping variable.}
}
\value{
  an S3 ensemble object.
}
\description{
  Ensemble organizes a data set into an ensemble of
  sub-data sets.
}

