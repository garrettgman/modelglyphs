#' @S3method autoplot glyphplot
autoplot.glyphplot <- function(object, ...) {
	require(ggplot2)
	qplot(gx, gy, data = object, geom = "line", group = gid)
}

# e1 <- ensemble(test.data, cross("long", "lat"), model(Fertility ~ Agriculture), "long", "lat")
# e2 <- ensemble(nasa, cross("long", "lat"), model(surftemp ~ temperature), "long", "lat")
# e3 <- ensemble(nasa, cross("long", "lat"), model(surftemp ~ temperature, FUN = "gam"), "long", "lat")
# e4 <- ensemble(nasa, cross("long", "lat"), model(surftemp ~ temperature, FUN = "loess"), "long", "lat")


#' @S3method autoplot mg_ensemble
autoplot.mg_ensemble <- function(object, ...) {
	require(ggplot2)
	get_r2 <- function(mod) summary(mod)$adj.r.squared
	object <- add_labels(ldply(object, get_r2), object)
	qplot(x, y, data = object, geom = "point", size = V1)
}