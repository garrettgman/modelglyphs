\name{significance_plot}
\alias{significance_plot}
\title{Quickly plot the p-values of an ensemble of models}
\usage{
  significance_plot(data, p.value, color = NULL,
    title = "", ...)
}
\arguments{
  \item{data}{Any type of data object whose class is
  defined in the modelglyphs package. The class of the
  object will begin with "mg_".}

  \item{p.value}{The name of the variable in data that
  contains p values. The name should be written as a
  character vector.}

  \item{color}{Optional. The name of a second variable,
  written as a character vector.}

  \item{title}{Optional. The title of the graph as a
  character string.}
}
\description{
  significance_plot plots data derived from an mg_ensemble
  object. The x_major and y_major attributes of the
  mg_ensemble are used as the x and y axes of the plot.
  Each model in the ensemble is mapped to a point. The size
  of each point corresponds to the p-value associated with
  that model (or its derivative data). An additional
  variable can also be mapped to the color of the point
  (optional).
}
\details{
  Significance_plots are meant to be quick and exploratory.
}

