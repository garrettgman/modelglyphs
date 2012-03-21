\name{line_plot}
\alias{line_plot}
\title{Quickly plot line plots of ensemble model data}
\usage{
  line_plot(data, x.minor, y.minor, y.scale, x.scale,
    title = "", ...)
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

  \item{x.scale,y.scale}{The scaling function to be applied
  to each set of minor values within a grid cell.}

  \item{title}{Optional. The title of the graph as a
  character string.}
}
\description{
  line_plot plots data derived from an mg_ensemble object.
  The x_major and y_major attributes of the mg_ensemble are
  used as the x and y axes of the plot. Each model in the
  ensemble is mapped to a line that simulates a line graph
  of x.minor vs. y.minor for the data in the model. These
  small line plots are generated according to the x.minor
  and y.minor arguments of line_plot. The resulting plot is
  placed into the larger plot according to the model's
  location relative to x_major and y_major.
}
\details{
  Line plots are meant to be quick and exploratory.
}

