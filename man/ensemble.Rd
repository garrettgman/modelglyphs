\name{ensemble}
\alias{ensemble}
\title{Performs group-wise modelling on a data frame}
\usage{
  ensemble(data, x_major, y_major, model)

  ensemble(data, x_major, y_major, model)
}
\arguments{
  \item{data}{a data frame to apply group-wise modelling
  to}

  \item{x_major}{a character string that specifies the
  variable to be on the major x axis when using the
  ensemble to make glyph plots. The data frame will be
  split into groups based on the unique combinations of
  x_major and y_major.}

  \item{y_major}{a character string that specifies the
  variable to be on the major y axis when using the
  ensemble to make glyph plots.}

  \item{model}{Information necessary to identify and apply
  a regression method. this information should be supplied
  as the output of a call to \code{\link{model}}.}

  \item{data}{a data frame to apply group-wise modelling
  to}

  \item{x_major}{a character string that specifies the
  variable to be on the major x axis when using the
  ensemble to make glyph plots. The data frame will be
  split into groups based on the unique combinations of
  x_major and y_major.}

  \item{y_major}{a character string that specifies the
  variable to be on the major y axis when using the
  ensemble to make glyph plots.}

  \item{model}{Information necessary to identify and apply
  a regression method. this information should be supplied
  as the output of a call to \code{\link{model}}.}
}
\value{
  an S3 ensemble object.

  an S3 ensemble object.
}
\description{
  ensemble splits the data frame into pieces based on the
  unique combinations of the x_major and y_major variables.
  It then applies the specified model to each piece and
  saves the output as an ensemble object. Note, ensemble
  removes the x_major and Y_major variables from the
  dataframe for purposes of modelling. Each subset of the
  data will only have one unique value of each variable,
  which makes them inappropriate to include in the model.

  ensemble splits the data frame into pieces based on the
  unique combinations of the x_major and y_major variables.
  It then applies the specified model to each piece and
  saves the output as an ensemble object. Note, ensemble
  removes the x_major and Y_major variables from the
  dataframe for purposes of modelling. Each subset of the
  data will only have one unique value of each variable,
  which makes them inappropriate to include in the model.
}

