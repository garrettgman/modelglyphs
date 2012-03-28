#' Quickly create a tile plot for a function of the data
#'
#' fill_plot applies a fill function to each subset of data in an mg_ensemble object and then displays the results as fills in a tileplot. If the subsets are arranged evenly on the x, y grid, each tile will correspong to one subset in the ensemble.
#'
#' @param ens A mg_ensemble object
#' @param fill A function call to be applied to each subset in the data. This function should return only one value. This value will be mapped to fill. If more than one value is returned, only the first value will be used. Variables in ens should be referred to by name only. The call will be evaluated with \code{\link{with}} where each subset will be used as the enclosing data object.
#' ... further arguments to be passed to geom_tile
#' @export
fill_plot <- function(ens, fill, ...) {
	require(ggplot2)
	fill.fun <- match.call()$fill
	
	get_fills <- function(data) {
		with(data, eval(fill.fun))
	}
	
	fills <- ddply(ens, ".gid", get_fills)
	fills$.x <- key(ens)[, 2][fills$.gid]
	fills$.y <- key(ens)[, 3][fills$.gid]
	
	plot.title <- paste("V1 =", deparse(fill.fun))
	
	ggplot(fills, aes(.x, .y)) +
		geom_tile(aes(fill = V1), ...) +
		opts(title = plot.title) +
		xlab(x_major(ens)) +
		ylab(y_major(ens))
}



#' Quickly plot scatterplots of ensemble model data
#'
#' scatter_plot plots data derived from an mg_ensemble object. The x_major and y_major attributes of the mg_ensemble are used as the x and y axes of the plot. Each model in the ensemble is mapped to a small cloud of points. These small scatterplots are generated according to the x.minor and y.minor arguments of scatter_plot. The resulting plot is placed into the larger plot according to the model's location relative to x_major and y_major. 
#'
#' Scatter plots are meant to be quick and exploratory. 
#'
#' @param data Any type of data object whose class is defined in the modelglyphs package. The class of the object will begin with "mg_".
#' @param x.minor The name of the variable in data to be used as the x axis when generating each individual scatterplot. x.minor does not need to be related to the x_major attribute of the parent mg_ensemble object. 
#' @param y.minor The name of the variable in data to be used as the y axis when generating each individual scatterplot. y.minor does not need to be related to the y_major attribute of the parent mg_ensemble object.
#' @param x.scale,y.scale The scaling function to be applied to each set of
#'  minor values within a grid cell.
#' @param title Optional. The title of the graph as a character string.
#' @export
scatter_plot <- function(data, x.minor, y.minor, x.scale = identity, 
	y.scale = identity, size = 1/2, ...) {
	require(ggplot2)
	
	g.data <- suppressMessages(glyphs(data, x.minor, y.minor, 
		x_scale = x.scale, y_scale = y.scale))
		
	plot.title <- paste("Ensemble of", x.minor, "vs.", y.minor)
		
	ggplot(g.data, aes(.x, .y, group = .gid)) +
		geom_point(size = size, ...) +
		opts(title = plot.title) + 
		xlab(x_major(data)) +
		ylab(y_major(data))
}

# dot_plot(e1, size = max(abs(surftemp - temperature)), color = max(surftemp - temperature) == max(abs(surftemp - temperature)))
#' @export
dot_plot <- function(ens, size, color, ...){
	size.fun <- match.call()$size
	get_sizes <- function(data) {
		with(data, eval(size.fun))
	}
	
	sizes <- ddply(ens, ".gid", get_sizes)
	
	color.fun <- match.call()$color
	get_colors <- function(data) {
		with(data, eval(color.fun))
	}
	colors <- ddply(ens, ".gid", get_colors)
	
	to.plot <- join(sizes, colors, by = ".gid")
	names(to.plot) <- c(".gid", "V1", "V2")
	
	to.plot$.x <- key(ens)[, 2][to.plot$.gid]
	to.plot$.y <- key(ens)[, 3][to.plot$.gid]
	
	plot.title <- paste("V1 =", deparse(size.fun), "\nV2 =", deparse(color.fun))
	
	ggplot(to.plot, aes(.x, .y)) +
		geom_point(aes(size = V1, color = V2), ...) +
		opts(title = plot.title) +
		xlab(x_major(ens)) +
		ylab(y_major(ens))
}
	
	
