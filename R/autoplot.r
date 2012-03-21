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


#' Quickly plot the p-values of an ensemble of models
#'
#' significance_plot plots data derived from an mg_ensemble object. The x_major and y_major attributes of the mg_ensemble are used as the x and y axes of the plot. Each model in the ensemble is mapped to a point. The size of each point corresponds to the p-value associated with that model (or its derivative data). An additional variable can also be mapped to the color of the point (optional).
#'
#' Significance_plots are meant to be quick and exploratory. 
#'
#' @param data Any type of data object whose class is defined in the modelglyphs package. The class of the object will begin with "mg_".
#' @param p.value The name of the variable in data that contains p values. The name should be written as a character vector.
#' @param color Optional. The name of a second variable, written as a character vector.
#' @param title Optional. The title of the graph as a character string.
#' @export
significance_plot <- function(data, p.value, color = NULL, title = "", ...) {
	require(ggplot2)
	
	if (!is.mg(data)) {
		stop("data is not a recognized modelglyphs class")
	}
		
	data$.signif <- data[[p.value]]
	
	if (is.null(color)) {
		p <- ggplot(data, aes(x, y)) +
			geom_point(aes(size = .signif), ...)		
	} else {
		data$.color <- data[[color]]
		max.value <- max(abs(range(data$.color))) + 1e-3
		values <- c(-max.value, 0, max.value)
		
		p <- ggplot(data, aes(x, y)) +
			geom_point(aes(color = .color, size = .signif), ...) +
			scale_colour_gradientn(color, 
				colours = RColorBrewer::brewer.pal(11, "RdYlBu")[11:1], 
				values = values, rescaler = function(x, ...) x, 
				oob = identity) + guides(color = guide_colorbar())
	}
			
	p +	scale_area(p.value, range = c(6, 1), 
		breaks = c(0.01,0.05,0.1,0.25, 0.5,1)) +
		opts(title = title)
}
	
	
#' Quickly plot the magnitudes of an ensemble of models
#'
#' magnitude_plot plots data derived from an mg_ensemble object. The x_major and y_major attributes of the mg_ensemble are used as the x and y axes of the plot. Each model in the ensemble is mapped to a point. The size of each point corresponds to the magnitude of a quantity associated with that model (or its derivative data). An additional variable can also be mapped to the color of the point (optional).
#'
#' Magnitude plots are meant to be quick and exploratory. 
#'
#' @param data Any type of data object whose class is defined in the modelglyphs package. The class of the object will begin with "mg_".
#' @param magnitude The name of the variable in data whose magnitude will be plotted. The name should be written as a character vector.
#' @param title Optional. The title of the graph as a character string.
#' @export
magnitude_plot <- function(data, magnitude, title = "", ...) {
	require(ggplot2)
	
	if (!is.mg(data)) {
		stop("data is not a recognized modelglyphs class")
	}
		
	data$.mag <- data[[magnitude]]
	
	data$.dir <- abs(data[[magnitude]])
	max.value <- max(abs(range(data$.mag))) + 1e-3
	values <- c(-max.value, 0, max.value)
		
	ggplot(data, aes(x, y)) +
		geom_point(aes(color = .mag, size = .dir), ...) +
		scale_colour_gradientn(magnitude, 
			colours = RColorBrewer::brewer.pal(11, "RdYlBu")[11:1], 
			values = values, rescaler = function(x, ...) x, 
			oob = identity) + 
		guides(color = guide_colorbar()) +
		scale_area(paste("|", magnitude, "|")) +
		opts(title = title)
}
	
# scatter_plot
# residuals by temperature
#' Quickly plot scatterplots of ensemble model data
#'
#' scatter_plot plots data derived from an mg_ensemble object. The x_major and y_major attributes of the mg_ensemble are used as the x and y axes of the plot. Each model in the ensemble is mapped to a small cloud of points. These small scatterplots are generated according to the x.minor and y.minor arguments of scatter_plot. The resulting plot is placed into the larger plot according to the model's location relative to x_major and y_major. 
#'
#' Scatter plots are meant to be quick and exploratory. 
#'
#' @param data Any type of data object whose class is defined in the modelglyphs package. The class of the object will begin with "mg_".
#' @param x.minor The name of the variable in data to be used as the x axis when generating each individual scatterplot. x.minor does not need to be related to the x_major attribute of the parent mg_ensemble object. 
#' @param y.minor The name of the variable in data to be used as the y axis when generating each individual scatterplot. y.minor does not need to be related to the y_major attribute of the parent mg_ensemble object.
#' @param title Optional. The title of the graph as a character string.
#' @export
scatter_plot <- function(data, x.minor, y.minor, title = "", ...) {
	require(ggplot2)
	
	if (!is.mg(data)) {
		stop("data is not a recognized modelglyphs class")
	}
	
	g.data <- glyphs(data, x.minor, y.minor)
		
	ggplot(g.data, aes(gx, gy, group = gid)) +
		geom_point(...) +
		opts(title = title)
}

# replace is.ensemble() with is.mg() in accessors?

# lines_plot

