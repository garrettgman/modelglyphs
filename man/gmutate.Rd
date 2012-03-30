\name{gmutate}
\alias{gmutate}
\title{Create groupwise mutations for an ensemble object}
\usage{
  gmutate(ens, ..., .progress = "none", .parallel = FALSE)
}
\arguments{
  \item{ens}{An ensemble object}

  \item{...}{arguments to be passed to
  \code{\link{summarise}}}

  \item{.progress}{name of the progress bar to use, see
  \code{\link{create_progress_bar}}}

  \item{.parallel}{if TRUE, apply function in parallel,
  using parallel backend provided by foreach}
}
\description{
  gmutate is a wrapper for a combination of mutate and
  d*ply functions from plyr. It automatically uses
  characteristics of the ensemble object to streamline the
  plyr call.
}

