#' @include update.r
#' @include mg_ensemble-class.r
NULL

#' Get/set the x_major attribute of an mg_ensemble object.
#' @aliases x_major x_major<-
#' @param x An object of class mg_ensemble
#' @export x_major "x_major<-"
x_major <- function(x) {
	stopifnot(is.ensemble(x))
	attr(x, "x_major")
}

"x_major<-" <- function(x, value) {
	stopifnot(is.ensemble(x))
	attr(x, "x_major") <- value
	update(x)
}


#' Get/set the y_major attribute of an mg_ensemble object.
#'
#' @aliases y_major y_major<-
#' @param x An object of class mg_ensemble
#' @export y_major "y_major<-"
y_major <- function(x) {
	stopifnot(is.ensemble(x))
	attr(x, "y_major")
}

"y_major<-" <- function(x, value) {
	stopifnot(is.ensemble(x))
	attr(x, "y_major") <- value
	update(x)
}


#' Get/set the grouping attribute of an mg_ensemble object.
#'
#' @aliases groups groups<-
#' @param x An object of class mg_ensemble
#' @export groups "groups<-"
groups <- function(x) {
	stopifnot(is.ensemble(x))
	attr(x, "groups")
}

"groups<-" <- function(x, value) {
	stopifnot(is.ensemble(x))
	attr(x, "groups") <- value
	update(x)
}

#' Get/set the data_set attribute of an mg_ensemble object.
#'
#' @aliases data_set data_set<-
#' @param x An object of class mg_ensemble
#' @export data_set "data_set<-"
data_set <- function(x) {
	stopifnot(is.ensemble(x))
	attr(x, "data_set")
}

"data_set<-" <- function(x, value) {
	stopifnot(is.ensemble(x))
	attr(x, "data_set") <- value
	update(x)
}


#' Get/set the model info attribute of an mg_ensemble object.
#'
#' model_info should be the output of \code{\link{model}}.
#'
#' @aliases model_info model_info<-
#' @param x An object of class mg_ensemble
#' @export model_info "model_info<-"
model_info <- function(x) {
	stopifnot(is.ensemble(x))
	attr(x, "mod_type")
	update(x)
}

