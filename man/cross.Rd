\name{cross}
\alias{cross}
\title{Group by the interaction of two variables}
\usage{
  cross(var1, var2, drop = TRUE)
}
\arguments{
  \item{var1}{the name of a variable to group on written as
  a character string}

  \item{var2}{the name of a variable to group on written as
  a character string}

  \item{drop}{logical should group names ignore the
  existence of var1 and var2 combinations that do not
  appear in the data frame}
}
\description{
  cross creates an mg_groups object that groups a data
  frame by the interaction of two variables. Each row will
  be assigned to the group that corresponds with the unique
  combination of var1 and var2 values found in that row.
  var1 and var2 should be variables that appear in the
  dataframe to be grouped.
}

