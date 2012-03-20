#' @S3method format mg_ensemble
format.mg_ensemble <- function(x, ...) {
	cat(paste("ensemble of", length(x), "models:"), 
		paste("\nmethod =", model_info(x)$FUN, "\nformula =", 
		deparse(model_info(x)$formula)))
}

#' @S3method print mg_ensemble
print.mg_ensemble <- function(x, ...) {
	format(x)
}

#' Is x an ensemble object?
#'
#' is.ensemble tests whether an object inherits from the mg_ensemble class.
#'
#' @param x An object to be tested for membership in the S3 mg_ensemble class
#'
#' @export
is.ensemble <- function(x) {
	inherits(x, "mg_ensemble")
}


