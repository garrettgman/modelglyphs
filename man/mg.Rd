\name{mg}
\alias{mg}
\title{Coerce an object to a modelglyphs class}
\usage{
  mg(x, ens, class, collate = FALSE)
}
\arguments{
  \item{x}{An object to be coerced to a modelglyphs class.}

  \item{ens}{An mg_ensemble object to borrow attributes
  from.}

  \item{class}{A chracter string. The name of the class to
  give the new object.}

  \item{collate}{logical. Should the output be arranged in
  the order of the data the mg_ensemble was fitted to and
  assigned the same row.names.}
}
\description{
  mg coerces an object to the modelglyphs class specified
  by the class argument.
}
\keyword{internal}

