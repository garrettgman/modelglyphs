\name{gtransform}
\alias{gtransform}
\title{Create groupwise transformations for an ensemble object}
\usage{
  gtransform(ens, ..., .progress = "none",
    .parallel = FALSE)
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
  gtransform is a wrapper for a combination of transform
  and d*ply functions from plyr. It automatically uses
  characteristics of the ensemble object to streamline the
  plyr call.
}

