#' @S3method autoplot glyphplot
autoplot.glyphplot <- function(object, ...) {
	require(ggplot2)
	qplot(gx, gy, data = object, geom = "line", group = gid)
}


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
