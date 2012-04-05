\name{assignInGroupspace}
\alias{assignInGroupspace}
\title{Assign a variable in the environment associated with a grouped data object}
\usage{
  assignInGroupspace(x, value, ens)
}
\arguments{
  \item{x}{A character string. The name of the variable to
  be assigned}

  \item{value}{The value to be assigned to x}

  \item{ens}{The object of class 'grouped'. x will be
  assigned in the environment specified by the environment
  slot of ens's group.info attribute}
}
\description{
  Assign a variable in the environment associated with a
  grouped data object
}
\keyword{internal}

