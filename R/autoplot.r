#' @S3method autoplot glyphplot
autoplot.glyphplot <- function(object, ...) {
	require(ggplot2)
	qplot(gx, gy, data = object, geom = "line", group = gid)
}


# @param variable the name of the variable, as a character vector
#' @S3method autoplot mg_coef
autoplot.mg_coef <- function(object, variable = NULL, ...) {
	require(ggplot2)
	variable <- as.character(substitute(variable))
	if (length(variable) == 0) stop("missing argument: variable")
	object <- object[object$variable == variable,]
	qplot(x, y, data = object, geom = "tile", group = gid, fill = coefficient)
	# qplot(x, y, data = object, geom = "point", size = coefficient)
}


# e2 <- ensemble(nasa, cross("long", "lat"), model(surftemp ~ temperature), "long", "lat")


#' @S3method autoplot mg_ensemble
autoplot.mg_ensemble <- function(object, ...) {
	require(ggplot2)
	get_r2 <- function(mod) summary(mod)$adj.r.squared
	object <- add_labels(ldply(object, get_r2), object)
	qplot(x, y, data = object, geom = "point", size = V1)
}