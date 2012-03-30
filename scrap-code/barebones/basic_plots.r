#' Quickly create a tile plot for a function of the data
#'
#' fill_plot applies a fill function to each subset of data in an mg_ensemble object and then displays the results as fills in a tileplot. If the subsets are arranged evenly on the x, y grid, each tile will correspong to one subset in the ensemble.
#'
#' @param ens A mg_ensemble object
#' @param fill A function call to be applied to each subset in the data. This function should return only one value. This value will be mapped to fill. If more than one value is returned, only the first value will be used. Variables in ens should be referred to by name only. The call will be evaluated with \code{\link{with}} where each subset will be used as the enclosing data object.
#' ... further arguments to be passed to geom_tile
#' @export
fill_plot <- function(ens, ...) {
	data <- groupwise(ens, ...)
	
	aesthetics <- quick.aes(match.call())
	plot.title <- quick.title(match.call())
	
	data$.x <- key(ens)[ ,2][data$.gid]
	data$.y <- key(ens)[ ,3][data$.gid]
	
	ggplot(data, aes(.x, .y)) +
		geom_tile(mapping = aesthetics) +
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
scatter_plot <- function(ens, x.minor, y.minor, x.scale = identity, 
	y.scale = identity, ...) {
		browser()
	data <- withgroups(ens, ...)
	g.data <- suppressMessages(glyphs(data, x.minor, y.minor, 
		x_scale = x.scale, y_scale = y.scale))
	
	aesthetics <- quick.aes(match.call())
		
	plot.title <- paste("Ensemble of", x.minor, "vs.", y.minor)
		
	ggplot(g.data, aes(.x, .y, group = .gid)) +
		geom_point(mapping = aesthetics) +
		opts(title = plot.title) + 
		xlab(x_major(data)) +
		ylab(y_major(data))
}

# dot_plot(e1, size = max(abs(surftemp - temperature)), color = max(surftemp - temperature) == max(abs(surftemp - temperature)))
#' @export
dot_plot <- function(ens, ...){
	data <- groupwise(ens, ...)
	
	aesthetics <- quick.aes(match.call())
	plot.title <- quick.title(match.call())
	
	data$.x <- key(ens)[ ,2][data$.gid]
	data$.y <- key(ens)[ ,3][data$.gid]
	
	ggplot(data, aes(.x, .y)) +
		geom_point(mapping = aesthetics) +
		opts(title = plot.title) +
		xlab(x_major(ens)) +
		ylab(y_major(ens))
}

quick.title <- function(args) {
	args <- as.list(args)
	aesthetics <- c("color", "colour", "size", "shape", "alpha", "fill")
	args <- args[names(args) %in% aesthetics]
	lines <- paste(names(args), as.character(args), sep = " = ")
	do.call("paste", c(as.list(lines), sep = "\n"))
}


quick.aes <- function(args) {
	arg.names <- names(args)
	aesthetics <- c("color", "colour", "size", "shape", "alpha", "fill")
	aesthetics <- arg.names[arg.names %in% aesthetics]
	nz.aesthetics <- aesthetics 
	nz.aesthetics[aesthetics == "color"] <- "colour"
	mappings <- aes()
	mappings[nz.aesthetics] <- lapply(aesthetics, as.name)
	mappings
}
	