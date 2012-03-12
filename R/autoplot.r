#' @S3method autoplot glyphplot
autoplot.glyphplot <- function(object, ...) {
	require(ggplot2)
	qplot(gx, gy, data = object, geom = "line", group = gid)
}