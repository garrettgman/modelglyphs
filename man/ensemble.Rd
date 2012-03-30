\name{ensemble}
\alias{ensemble}
\title{Ensemble organizes a data set into an ensemble of sub-data sets.}
\usage{
  ensemble(data, grouping, x.major = NULL, y.major = NULL,
    key = NULL)
}
\arguments{
  \item{data}{a data frame to organize as an ensemble of
  sub data sets}

  \item{grouping}{a mg_group object to use for splitting
  data into subsets}

  \item{x.major}{a character string that specifies the
  default variable to be used on the x axis when plotting
  the ensembles. Defaults to the first grouping variable.}

  \item{y.major}{a character string that specifies the
  default variable to be used on the y axis when plotting
  the ensembles. Defaults to the second grouping variable.}

  \item{a}{data.frame that specifes group specific
  information for each subset in the ensemble. The key must
  contain a .gid variable that lists group membership as
  well as the x.major and y.major variables specified for
  the group. It may also contain additional variables. If
  no key is provided, ensemble() will generate one that
  records the relationship between .gid, x.major, and
  y.major for each group.}
}
\value{
  an S3 ensemble object.
}
\description{
  Ensemble organizes a data set into an ensemble of
  sub-data sets.
}

