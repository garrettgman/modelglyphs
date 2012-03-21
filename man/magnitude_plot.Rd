\name{magnitude_plot}
\alias{magnitude_plot}
\title{Quickly plot the magnitudes of an ensemble of models}
\usage{
  magnitude_plot(data, magnitude, title = "", ...)
}
\arguments{
  \item{data}{Any type of data object whose class is
  defined in the modelglyphs package. The class of the
  object will begin with "mg_".}

  \item{magnitude}{The name of the variable in data whoe
  magnitude will be plotted. The name should be written as
  a character vector.}

  \item{title}{Optional. The title of the graph as a
  character string.}
}
\description{
  mgnitude_plot plots data derived from an mg_ensemble
  object. The x_major and y_major attributes of the
  mg_ensemble are used as the x and y axes of the plot.
  Each model in the ensemble is mapped to a point. The size
  of each point corresponds to the magnitude of a quantity
  associated with that model (or its derivative data). An
  additional variable can also be mapped to the color of
  the point (optional).
}
\details{
  Magnitude plots are meant to be quick and exploratory.
}

