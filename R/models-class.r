#' Is an object a models object from the modelglyphs package?
#'
#' @param x An object to be tested
#' @return logical TRUE if x inherits from the 'models' class, FALSE otherwise
#' @export
is.models <- function(x) {
	inherits(x, "models")
}

format.models <- function(x, ...){
	cat(paste("An ensemble of ", length(x), " models:\n\nCall:\n", 
		deparse(x[[1]]$call), "\n", sep = ""))
}

print.models <- function(x, ...){
	format(x)
}

str.models <- function(object, ...){
	cat(paste("A list of", length(object), "models\nEach has the following structure (from first element):\n"))
	str(object[[1]])
}