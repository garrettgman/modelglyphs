\name{scatter_plot}
\alias{scatter_plot}
\title{Quickly plot scatterplots of ensemble model data}
\usage{
  scatter_plot(data, x.minor, y.minor, title = "", ...)
}
\arguments{
  \item{data}{Any type of data object whose class is
  defined in the modelglyphs package. The class of the
  object will begin with "mg_".}

  \item{x.minor}{The name of the variable in data to be
  used as the x axis when generating each individual
  scatterplot. x.minor does not need to be related to the
  x_major attribute of the parent mg_ensemble object.}

  \item{y.minor}{The name of the variable in data to be
  used as the y axis when generating each individual
  scatterplot. y.minor does not need to be related to the
  y_major attribute of the parent mg_ensemble object.}

  \item{title}{Optional. The title of the graph as a
  character string.}
}
\description{
  scatter_plot plots data derived from an mg_ensemble
  object. The x_major and y_major attributes of the
  mg_ensemble are used as the x and y axes of the plot.
  Each model in the ensemble is mapped to a small cloud of
  points. These small scatterplots are generated according
  to the x.minor and y.minor arguments of scatter_plot. The
  resulting plot is placed into the larger plot according
  to the model's location relative to x_major and y_major.
}
\details{
  Scatter plots are meant to be quick and exploratory.
}

