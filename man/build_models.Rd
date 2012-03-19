\name{build_models}
\alias{build_models}
\title{Performs group-wise maodelling on an mg_ensemble object}
\usage{
  build_models(ensemble)
}
\arguments{
  \item{data}{A data frame with a variable named "gid."}

  \item{model}{A mg_model object}
}
\description{
  build_models fits the model specified by an mg_ensemble's
  model_info attribute to the data sepcified by the
  mg_ensemble's data_set attribute according to the
  groupings suggested by the dat_set's gid variable. These
  groupings align with the grouping information stored in
  the mg_ensemble object's groupa attribute.
}
\keyword{internal}

