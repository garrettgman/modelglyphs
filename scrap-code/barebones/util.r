#' Add the mg.info attribute of one object to another
#'
#' @param out An object to add an mg.info attribute to
#' @param inp An object with an mg.info attribute to copy to out
#' @param class A character string to add to the start of the new object's class vector.
#' @keywords internal
#' @export
mg <- function(out, inp, class = NULL) {
	attr(out, "mg.info") <- attr(inp, "mg.info")
	class(out) <- c(class, class(out))
	out
}

#' Add x.major and y.major variables to a modelglyphs object
#'
#' @keywords internal
#' @export
xy <- function(mg) {
	stopifnot(inherits(mg, "data.frame"))
	
	x <- key(mg)[, 2][mg$.gid]
	y <- key(mg)[, 3][mg$.gid]
	
	vars <- setdiff(names(mg), ".gid")
	new <- data.frame(cbind(mg$.gid, x, y, mg[ , vars]))
	attributes(new) <- attributes(mg)
	names(new) <- c(".gid", x_major(mg), y_major(mg), vars)
	new
}

