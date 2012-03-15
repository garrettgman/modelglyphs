\name{cross}
\alias{cross}
\title{Group by interactions of x and y}
\usage{
  cross(x, y, data)
}
\arguments{
  \item{x}{A variable name in \link{data}}

  \item{y}{A variable name in \link{data}}

  \item{data}{A data frame}
}
\value{
  a mg_grouping class object
}
\description{
  cross creates a mg_grouping object that groups a data set
  by the unique combinations of x and y.
}

