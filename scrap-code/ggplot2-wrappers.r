#' Quickly plot point plots of grouped data
#'
#' dot_plot plots data that has been organized into groups by the modelglyphs package. Each group is summarized by a single point whose aesthetics are set by the call to dot_plot. Aesthetics may be mapped to functions of the existing data. dot_plot automatically applies any transformations to the data in a groupwise fashion. Since each group is represented by a single point, a viable transformation must return only one value per group. 
#'
#' If x.minor and y.minor arguments are supplied to dot_plot, dot_plot will use \code{\link{glyphs}} to position points relative to their location on the x.major and y.major axiis.
#'
#'dot_plot is a wrapper for \code{\link{ggplot}} and \code{\link{geom_point}} form the ggplot2 package.
#'
#'
#' @param ens A dataframe that inherits from class `grouped'
#' @param x.major The variable to map to the x axis. If x.major is not specified, dot_plot will use the x.major attribute of ens, see \code{\link{x_major}}.
#' @param y.major The variable to map to the x axis. If x.major is not specified, dot_plot will use the y.major attribute of ens, see \code{\link{y_major}}.
#' @param facets Facetting formula to use. Picks \{code{\link{facet_wrap}} or \code{\link{facet_grid}} depending on whether the formula is one or two sided.
#' @param polar A logical of length 1, specifying whether x.minor and y.minor aesthetics should be transformed to polar coordinates.  Defaults to \code{FALSE}. See \code{\link{glyphs}}. polar is ignored if x.minor and y.minor aesthetics are not specified.
#' @param height,width The height and width of each glyph.  See \code{\link{glyphs}}. Defaults to 95\% of the \code{\link[ggplot2]{resolution}} of the data. Specify the width absolutely by supplying a numeric vector of length 1, or relative to the resolution of the data by using \code{\link{rel}}. width and height are ignored if x.minor and y.minor aesthetics are not specified.
#' @param x_scale,y_scale The scaling function to be applied to each set of
#'  minor values within a grid cell.  Defaults to \code{\link{identity}} so
#'  that no scaling is performed.  See \code{\link{glyphs}}. x_scale and y_scale are ignored if x.minor and y.minor aesthetics are not specified.
#' @param quiet A logical that determines whether glyphing messages are suppressed when transforming x.minor and y.minor. See \code{\link{glyphs}}. x_scale and y_scale are ignored if x.minor and y.minor aesthetics are not specified.
#' @export
dot_plot <- function(ens, ..., x.major, y.major, facets = NULL, polar = FALSE, height = rel(0.95), width = rel(0.95), y_scale = identity, x_scale = identity, quiet = TRUE) {
	
	if (missing(ens)) stop("dot_plot needs missing argument: ens")
	if (!is.grouped(ens)) stop("ens must be of class 'grouped'")
	
	args <- as.list(match.call()[-1])
	# args <- match.call(expand.dots = FALSE)$`...`
	env <- parent.frame()
	
	data <- quick_glyph(ens, args, polar, height, width, y_scale, 
		x_scale, quiet)
	data <- quick_summaries(data, args)
	aesthetics <- quick_aes(args) # aes_all
	parameters <- quick_params(args)
	plot.title <- quick_title(args)
	
	if (missing(x.major)) {
		x.lab <- x_major(ens)
	} else {
		x.lab <- deparse(substitute(x.major))
	}
	
	if (missing(y.major)) {
		y.lab <- y_major(ens)
	} else {
		y.lab <- deparse(substitute(y.major))
	}
	
	p <- ggplot(data, aesthetics, environment = env)
	point.layer <- do.call("geom_point", parameters)
	p <- p + point.layer
	
	if (!is.null(facets)) {
		p <- ifelse(length(facets) == 3, 
			p + facet_grid(facets), 
			p + facet_wrap(facets))
	}
	
	p + opts(title = plot.title) +
		xlab(x.lab) +
		ylab(y.lab)
}


#' Quickly plot scatterplots of grouped data
#'
#' scatter_plot plots data that has been organized into groups by the modelglyphs package. Each group is summarized by a cloud of points whose aesthetics are set by the call to scatter_plot. Aesthetics may be mapped to functions of the existing data. scatter_plot automatically applies any transformations to the data in a groupwise fashion, but unlike with \code{\link{dot_plot}} the transformations used in scatter plot can return more than one value per group. 
#'
#' Scatter_plot represents each group of data as a scatterplot of points. Each scatterplot is like a graph within a graph. At the local level, the points of the scatterplot are organized according to the x.minor and y.minor arguments (which form a local x and y grid). The resulting scatterplot graphs are then arranged in a larger graph according to the x.major and y.major arguments (which form a globla x and y grid). See \code{\link{glyphs}} for details.
#'
#'scatter_plot is a wrapper for \code{\link{ggplot}} and \code{\link{geom_point}} form the ggplot2 package.
#'
#'
#' @param ens A dataframe that inherits from class `grouped'
#' @param x.minor The variable to map to the local x axis for each sub-scatterplot. 
#' @param y.minor The variable to map to the local y axis for each sub-scatterplot. 
#' @param x.major The variable to map to the global x axis. If x.major is not specified, scatter_plot will use the x.major attribute of ens, see \code{\link{x_major}}.
#' @param y.major The variable to map to the global y axis. If x.major is not specified, scatter_plot will use the y.major attribute of ens, see \code{\link{y_major}}.
#' @param facets Facetting formula to use. Picks \{code{\link{facet_wrap}} or \code{\link{facet_grid}} depending on whether the formula is one or two sided.
#' @param polar A logical of length 1, specifying whether x.minor and y.minor aesthetics should be transformed to polar coordinates.  Defaults to \code{FALSE}. See \code{\link{glyphs}}. 
#' @param height,width The height and width of each glyph.  See \code{\link{glyphs}}. Defaults to 95\% of the \code{\link[ggplot2]{resolution}} of the data. Specify the width absolutely by supplying a numeric vector of length 1, or relative to the resolution of the data by using \code{\link{rel}}. 
#' @param x_scale,y_scale The scaling function to be applied to each set of
#'  minor values within a grid cell.  Defaults to \code{\link{identity}} so
#'  that no scaling is performed.  See \code{\link{glyphs}}.
#' @param quiet A logical that determines whether glyphing messages are suppressed when transforming x.minor and y.minor. See \code{\link{glyphs}}. 
#' @export
scatter_plot <- function(ens, ..., x.minor, y.minor, x.major, y.major, facets = NULL, polar = FALSE, height = rel(0.95), width = rel(0.95), y_scale = identity, x_scale = identity, quiet = TRUE) {
	
	if (missing(ens)) stop("scatter_plot needs missing argument: ens")
	if (!is.grouped(ens)) stop("ens must be of class 'grouped'")
	if (missing(x.minor)) stop("scatter_plot needs missing argument: x.minor")
	if (missing(y.minor)) stop("scatter_plot needs missing argument: y.minor")
	
	args <- as.list(match.call()[-1])
	env <- parent.frame()
	
	data <- quick_glyph(ens, args, polar, height, width, y_scale, 
		x_scale, quiet)
	data <- quick_transforms(data, args)
	aesthetics <- quick_aes(args)
	parameters <- quick_params(args)
	plot.title <- quick_title(args)
	
	if (missing(x.major)) {
		x.lab <- x_major(ens)
	} else {
		x.lab <- deparse(substitute(x.major))
	}
	
	if (missing(y.major)) {
		y.lab <- y_major(ens)
	} else {
		y.lab <- deparse(substitute(y.major))
	}
	
	p <- ggplot(data, aesthetics, environment = env)
	point.layer <- do.call("geom_point", parameters)
	p <- p + point.layer
	
	if (!is.null(facets)) {
		p <- ifelse(length(facets) == 3, 
			p + facet_grid(facets), 
			p + facet_wrap(facets))
	}
	
	p + opts(title = plot.title) +
		xlab(x.lab) +
		ylab(y.lab)
}

#' Quickly create a tile plot for a function of the data
#'
#' fill_plot applies a fill function to each subset of data in an mg_ensemble object and then displays the results as fills in a tileplot. If the subsets are arranged evenly on the x, y grid, each tile will correspong to one subset in the ensemble.
#'
#' @param ens A mg_ensemble object
#' @param fill A function call to be applied to each subset in the data. This function should return only one value. This value will be mapped to fill. If more than one value is returned, only the first value will be used. Variables in ens should be referred to by name only. The call will be evaluated with \code{\link{with}} where each subset will be used as the enclosing data object.
#' ... further arguments to be passed to geom_tile
#' @export

#' Quickly create a tile plot for grouped data
#'
#' fill_plot plots data that has been organized into groups by the modelglyphs package. fill_plot applies a fill function to each subset of data and then displays the results as fills in a tileplot. If the subsets are arranged evenly on the x, y grid, each tile will correspond to one group of data. Since each group is represented by a single fill, a viable fill transformation must return only one value per group.
#'
#'fill_plot is a wrapper for \code{\link{ggplot}} and \code{\link{geom_point}} form the ggplot2 package.
#'
#'
#' @param ens A dataframe that inherits from class `grouped'
#' @param x.major The variable to map to the global x axis. If x.major is not specified, fill_plot will use the x.major attribute of ens, see \code{\link{x_major}}.
#' @param y.major The variable to map to the global y axis. If x.major is not specified, fill_plot will use the y.major attribute of ens, see \code{\link{y_major}}.
#' @param facets Facetting formula to use. Picks \{code{\link{facet_wrap}} or \code{\link{facet_grid}} depending on whether the formula is one or two sided.
#' @param polar A logical of length 1, specifying whether x.minor and y.minor aesthetics should be transformed to polar coordinates.  Defaults to \code{FALSE}. See \code{\link{glyphs}}. 
#' @param height,width The height and width of each glyph.  See \code{\link{glyphs}}. Defaults to 95\% of the \code{\link[ggplot2]{resolution}} of the data. Specify the width absolutely by supplying a numeric vector of length 1, or relative to the resolution of the data by using \code{\link{rel}}. 
#' @param x_scale,y_scale The scaling function to be applied to each set of
#'  minor values within a grid cell.  Defaults to \code{\link{identity}} so
#'  that no scaling is performed.  See \code{\link{glyphs}}.
#' @param quiet A logical that determines whether glyphing messages are suppressed when transforming x.minor and y.minor. See \code{\link{glyphs}}. 
fill_plot <- function(ens, ..., x.major, y.major, facets = NULL, polar = FALSE, height = rel(0.95), width = rel(0.95), y_scale = identity, x_scale = identity, quiet = TRUE) {
	
	if (missing(ens)) stop("fill_plot needs missing argument: ens")
	if (!is.grouped(ens)) stop("ens must be of class 'grouped'")
	
	args <- as.list(match.call()[-1])
	env <- parent.frame()
	
	data <- quick_glyph(ens, args, polar, height, width, y_scale, 
		x_scale, quiet)
	data <- quick_summaries(data, args)
	aesthetics <- quick_aes(args)
	parameters <- quick_params(args)
	plot.title <- quick_title(args)
	
	if (missing(x.major)) {
		x.lab <- x_major(ens)
	} else {
		x.lab <- deparse(substitute(x.major))
	}
	
	if (missing(y.major)) {
		y.lab <- y_major(ens)
	} else {
		y.lab <- deparse(substitute(y.major))
	}
	
	p <- ggplot(data, aesthetics, environment = env)
	tile.layer <- do.call("geom_tile", parameters)
	p <- p + tile.layer
	
	if (!is.null(facets)) {
		p <- ifelse(length(facets) == 3, 
			p + facet_grid(facets), 
			p + facet_wrap(facets))
	}
	
	p + opts(title = plot.title) +
		xlab(x.lab) +
		ylab(y.lab)
}


#' Quickly plot line plots of grouped data
#'
#'line_plot is a wrapper for \code{\link{ggplot}} and \code{\link{geom_point}} form the ggplot2 package.
#'
#'
#' @param ens A dataframe that inherits from class `grouped'
#' @param x.minor The variable to map to the local x axis for each sub-scatterplot. 
#' @param y.minor The variable to map to the local y axis for each sub-scatterplot. 
#' @param x.major The variable to map to the global x axis. If x.major is not specified, line_plot will use the x.major attribute of ens, see \code{\link{x_major}}.
#' @param y.major The variable to map to the global y axis. If x.major is not specified, line_plot will use the y.major attribute of ens, see \code{\link{y_major}}.
#' @param facets Facetting formula to use. Picks \{code{\link{facet_wrap}} or \code{\link{facet_grid}} depending on whether the formula is one or two sided.
#' @param polar A logical of length 1, specifying whether x.minor and y.minor aesthetics should be transformed to polar coordinates.  Defaults to \code{FALSE}. See \code{\link{glyphs}}. 
#' @param height,width The height and width of each glyph.  See \code{\link{glyphs}}. Defaults to 95\% of the \code{\link[ggplot2]{resolution}} of the data. Specify the width absolutely by supplying a numeric vector of length 1, or relative to the resolution of the data by using \code{\link{rel}}. 
#' @param x_scale,y_scale The scaling function to be applied to each set of
#'  minor values within a grid cell.  Defaults to \code{\link{identity}} so
#'  that no scaling is performed.  See \code{\link{glyphs}}.
#' @param quiet A logical that determines whether glyphing messages are suppressed when transforming x.minor and y.minor. See \code{\link{glyphs}}. 
#' @export
line_plot <- function(ens, ..., x.minor, y.minor, x.major, y.major, facets = NULL, polar = FALSE, height = rel(0.95), width = rel(0.95), y_scale = identity, x_scale = identity, quiet = TRUE) {
	
	if (missing(ens)) stop("line_plot needs missing argument: ens")
	if (!is.grouped(ens)) stop("ens must be of class 'grouped'")
	if (missing(x.minor)) stop("line_plot needs missing argument: x.minor")
	if (missing(y.minor)) stop("line_plot needs missing argument: y.minor")
	
	args <- as.list(match.call()[-1])
	env <- parent.frame()
	
	data <- quick_glyph(ens, args, polar, height, width, y_scale, 
		x_scale, quiet)
	data <- quick_transforms(data, args)
	aesthetics <- quick_aes(args)
	aesthetics$group <- quote(.gid)
	parameters <- quick_params(args)
	plot.title <- quick_title(args)
	
	if (missing(x.major)) {
		x.lab <- x_major(ens)
	} else {
		x.lab <- deparse(substitute(x.major))
	}
	
	if (missing(y.major)) {
		y.lab <- y_major(ens)
	} else {
		y.lab <- deparse(substitute(y.major))
	}
	
	p <- ggplot(data, aesthetics, environment = env)
	line.layer <- do.call("geom_line", parameters)
	p <- p + line.layer
	
	if (!is.null(facets)) {
		p <- ifelse(length(facets) == 3, 
			p + facet_grid(facets), 
			p + facet_wrap(facets))
	}
	
	p + opts(title = plot.title) +
		xlab(x.lab) +
		ylab(y.lab)
}


#' Quickly plot smoothed model summaries of grouped data
#'
#'smooth_plot is a wrapper for \code{\link{ggplot}} and \code{\link{geom_point}} form the ggplot2 package.
#'
#'
#' @param ens A dataframe that inherits from class `grouped'
#' @param x.minor The variable to map to the local x axis for each sub-plot. 
#' @param y.minor The variable to map to the local y axis for each sub-plot. 
#' @param x.major The variable to map to the global x axis. If x.major is not specified, smooth_plot will use the x.major attribute of ens, see \code{\link{x_major}}.
#' @param y.major The variable to map to the global y axis. If x.major is not specified, smooth_plot will use the y.major attribute of ens, see \code{\link{y_major}}.
#' @param facets Facetting formula to use. Picks \{code{\link{facet_wrap}} or \code{\link{facet_grid}} depending on whether the formula is one or two sided.
#' @param polar A logical of length 1, specifying whether x.minor and y.minor aesthetics should be transformed to polar coordinates.  Defaults to \code{FALSE}. See \code{\link{glyphs}}. 
#' @param height,width The height and width of each glyph.  See \code{\link{glyphs}}. Defaults to 95\% of the \code{\link[ggplot2]{resolution}} of the data. Specify the width absolutely by supplying a numeric vector of length 1, or relative to the resolution of the data by using \code{\link{rel}}. 
#' @param x_scale,y_scale The scaling function to be applied to each set of
#'  minor values within a grid cell.  Defaults to \code{\link{identity}} so
#'  that no scaling is performed.  See \code{\link{glyphs}}.
#' @param quiet A logical that determines whether glyphing messages are suppressed when transforming x.minor and y.minor. See \code{\link{glyphs}}. 
#' @export
smooth_plot <- function(ens, ..., x.minor, y.minor, x.major, y.major, facets = NULL, polar = FALSE, height = rel(0.95), width = rel(0.95), y_scale = identity, x_scale = identity, quiet = TRUE, method, se) {
	
	if (missing(ens)) stop("smooth_plot needs missing argument: ens")
	if (!is.grouped(ens)) stop("ens must be of class 'grouped'")
	if (missing(x.minor)) stop("smooth_plot needs missing argument: x.minor")
	if (missing(y.minor)) stop("smooth_plot needs missing argument: y.minor")
	
	args <- as.list(match.call()[-1])
	env <- parent.frame()
	
	data <- quick_glyph(ens, args, polar, height, width, y_scale, 
		x_scale, quiet)
	data <- quick_transforms(data, args)
	aesthetics <- quick_aes(args)
	aesthetics$group <- quote(.gid)
	if (!missing(method)) aesthetics$method <- substitute(method)
	if (!missing(se)) aesthetics$se <- substitute(se)
	parameters <- quick_params(args)
	plot.title <- quick_title(args)
	
	if (missing(x.major)) {
		x.lab <- x_major(ens)
	} else {
		x.lab <- deparse(substitute(x.major))
	}
	
	if (missing(y.major)) {
		y.lab <- y_major(ens)
	} else {
		y.lab <- deparse(substitute(y.major))
	}
	
	p <- ggplot(data, aesthetics, environment = env)
	smooth.layer <- do.call("geom_smooth", parameters)
	p <- p + smooth.layer
	
	if (!is.null(facets)) {
		p <- ifelse(length(facets) == 3, 
			p + facet_grid(facets), 
			p + facet_wrap(facets))
	}
	
	p + opts(title = plot.title) +
		xlab(x.lab) +
		ylab(y.lab)
}


	
	
quick_glyph <- function(ens, args, polar, height, width, y_scale, 
	x_scale, quiet) {
	
	xy <- c("x.minor", "y.minor") %in% names(args)
	n.xy <- sum(xy)
	
	if (n.xy == 1) {
		stop(paste("missing argument:", c("x.minor", "y.minor")[xy]))
	}
	
	if (is.null(args$x.major)) {
		xmajor <- x_major(ens)
	} else {
		xmajor <- as.character(args$x.major)
	}
		
	if (is.null(args$y.major)) {
		ymajor <- y_major(ens)
	} else {
		ymajor <- as.character(args$y.major)
	}
	
	if (n.xy == 0) {
		names(ens)[names(ens) == xmajor] <- ".x"
		names(ens)[names(ens) == ymajor] <- ".y"
		ens
	} else {
		xminor <- as.character(args$x.minor)
    	yminor <- as.character(args$y.minor)
    	
    	glyphs(ens, xminor, yminor, xmajor, ymajor, polar, 
    		height, width, y_scale, x_scale, quiet)
	} 
}
	
	
	

quick_summaries <- 	function(data, args) {
	all_aes <- c("x.major", "y.major", "shape", "colour", "color", 
    	"size", "alpha", "facets", "fill")
    summaries <- compact(args[all_aes])
    summaries <- summaries[!is.constant(summaries)]
    if (is.null(summaries$x.major)) {
    	summaries$x.major <- quote(.x[1])
    }
    if (is.null(summaries$y.major)) { 
    	summaries$y.major <- quote(.y[1])
    }	
    summaries$ens <- quote(data)
    
    data <- do.call("gsummarise", summaries)
    names(data)[names(data) == "x.major"] <- ".x"
    names(data)[names(data) == "y.major"] <- ".y"
    
    data
}


quick_transforms <- function(data, args) {
	all_aes <- c("x.major", "y.major", "shape", "colour", "color", 
    	"size", "alpha", "facets")
    transforms <- compact(args[all_aes])
    transforms <- transforms[!is.constant(transforms)]
    if (length(transforms) != 0) {
    	if (is.null(transforms$x.major)) {
    		transforms$x.major <- quote(.x[1])
    	}
    	if (is.null(transforms$y.major)) { 
    		transforms$y.major <- quote(.y[1])
    	}	
    	transforms$ens <- quote(data)
    
    	data <- do.call("gmutate", transforms)
    	
    	names(data)[names(data) == "x.major"] <- ".x"
    	names(data)[names(data) == "y.major"] <- ".y"
    }
    
    data
}
    
    
quick_aes <- function(args) {
	arg.names <- names(args)
	aesthetics <- c("color", "colour", "size", "shape", "alpha", "fill")
	aesthetics <- arg.names[arg.names %in% aesthetics]
	nz.aesthetics <- aesthetics 
	nz.aesthetics[aesthetics == "color"] <- "colour"
	mappings <- aes()
	mappings[nz.aesthetics] <- lapply(aesthetics, as.name)
	mappings$x <- quote(.x)
	mappings$y <- quote(.y)
	mappings
}

quick_params <- function(args) {
	all_aes <- c("x.major", "y.major", "shape", "colour", "color", 
    	"size", "alpha", "fill")
    params <- compact(args[all_aes])
    
    if (length(is.constant(params)) > 0) {
    	params[is.constant(params)]
    } else {
	    list()
	}
}

quick_title <- function(args) {
	args <- as.list(args)
	aesthetics <- c("x.minor", "y.minor", "color", "colour", "size", "shape", "alpha", "fill")
	args <- args[names(args) %in% aesthetics]
	args <- args[!is.constant(args)]
	lines <- paste(names(args), as.character(args), sep = " = ")
	do.call("paste", c(as.list(lines), sep = "\n"))
}

is.constant <- function(x) {
  sapply(x, function(x) "I" %in% all.names(asOneSidedFormula(x)))
}
