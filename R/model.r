#' Pass modelling information to ensemble()
#'
#' model collects the arguments needed to perform a method of regression. model is
#' intended to be used in conjunction with \code{\link{ensemble}} to create ensemble 
#' objects. model does not collect the name of the data set to be used.
#'
#' @param formula A formula object to be used in the intended model
#' @param FUN a character string that identifies the type of modelling method to be used. FUN should match the name of a modelling function in R
#' @param ... other arguments to pass to the modelling function indicated by type
#'
#' @keywords internal
#' export 
model <- function(formula, FUN = "lm", ...) {
	mod <- structure(as.list(match.call()[-1]), class = c("mg_model", "list"))
	if (is.null(mod$formula)) stop("missing formula argument")
	if (is.null(mod$FUN)) mod$FUN <- "lm"
	mod
}



is.model <- function(x) inherits(x, "mg_model")

fit_model <- function(model) {
	stopifnot(is.model(model))
	FUN <- model$FUN
	model <- model[setdiff(names(model), "FUN")]
	function(data, ...){
		do.call(FUN, c(data = substitute(data), model, list(...)))
	}
}