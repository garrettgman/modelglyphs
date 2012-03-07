#' @S3method format ensemble
format.ensemble <- function(x, ...) {
	cat(paste("ensemble of", length(x), "models:"), 
		paste("\nmethod =", attr(x, "method"), "\nformula =", 
		deparse(attr(x, "formula"))))
}

#' @S3method print ensemble
print.ensemble <- function(x, ...) {
	format(x)
}

#' Is x an ensemble object?
#'
#' is.ensemble tests whether an object inherits from the ensemble class.
#'
#' @param x An object to be tested for membership in the S3 ensemble class
#'
#' @export
is.ensemble <- function(x) {
	inherits(x, "ensemble")
}
