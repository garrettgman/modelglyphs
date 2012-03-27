# fill_plot(e1, fill = max(temperature))

#' Quickly create a tile plot for a function of the data
#'
#' fill_plot applies a fill function to each subset of data in an mg_ensemble object and then displays the results as fills in a tileplot. If the subsets are arranged evenly on the x, y grid, each tile will correspong to one subset in the ensemble.
#'
#' @param ens A mg_ensemble object
#' @param fill A function call to be applied to each subset in the data. This function should return only one value. This value will be mapped to fill. If more than one value is returned, only the first value will be used. Variables in ens should be referred to by name only. The call will be evaluated with \code{\link{with}} where each subset will be used as the enclosing data object.
#' ... further arguments to be passed to geom_tile
#' @export
fill_plot <- function(ens, fill, ...) {
	fill.fun <- match.call()$fill
	
	get_fills <- function(data) {
		with(data, eval(fill.fun))
	}
	
	fills <- ddply(ens, ".gid", get_fills)
	fills$x <- key(ens)[, 2][fills$.gid]
	fills$y <- key(ens)[, 3][fills$.gid]
	
	plot.title <- paste("V1 =", deparse(fill.fun))
	
	ggplot(fills, aes(x, y)) +
		geom_tile(aes(fill = V1), ...) +
		opts(title = plot.title) +
		xlab(x_major(ens)) +
		ylab(y_major(ens))
}


