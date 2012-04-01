#' @export
dot_plot <- function(ens, ..., x.major, y.major, facets = NULL, polar = FALSE, height = rel(0.95), width = rel(0.95), y_scale = identity, x_scale = identity, quiet = TRUE) {
	
	if (missing(ens)) stop("dot_plot needs missing argument: ens")
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
		x.lab <- deparse(x.major)
	}
	
	if (missing(y.major)) {
		y.lab <- y_major(ens)
	} else {
		y.lab <- deparse(y.major)
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
	
#' @export
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
    	"size", "alpha")
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
    	"size", "alpha")
    params <- compact(args[all_aes])
    params[is.constant(params)]
}

quick_title <- function(args) {
	args <- as.list(args)
	aesthetics <- c("color", "colour", "size", "shape", "alpha", "fill")
	args <- args[names(args) %in% aesthetics]
	lines <- paste(names(args), as.character(args), sep = " = ")
	do.call("paste", c(as.list(lines), sep = "\n"))
}
