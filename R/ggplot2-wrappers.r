#' Plot grouped data
#' gplot is a wrapper for \code{\link{ggplot}} that preprocesses grouped data before plotting. gplot allows the user to specify both minor and major x, y axiis and uses \code{\link{glyphs}} to resolve the resulting position aesthetics. It also applies all mappings in a groupwise fashion. Unlike ggplot, gplot can only plot one aesthetic at a time. However, gplot returns a ggplot object which can be combined with additional layers and ggplot2 objects in the usual fashion.
#'
#' @params ens A data frame of class 'grouped'
#' @params ... aesthetics and parameters to be passed on to the \code{\link{ggplot}} call
#' @params x.major variable or function of a variable to be used for the major x axis
#' @params y.major variable or function of a variable to be used for the major y axis
#' @params x.minor variable or function of a variable to be used for the minor x axis
#' @params y.minor variable or function of a variable to be used for the minor y axis
#' @params geom character vector specifying geom to use.
#' @params facets faceting formula to use. Picks \code{\link{facet_wrap}} or \code{\link{facet_grid}} depending on whether the formula is one sided or two-sided
#' @params polar,height,width,y_scale,x_scale,quiet arguments to be passedo n to glyphs when resolving x and y position aesthetics.
#'
#'
#' @export
gplot <- function(ens, ..., x.major = NULL, y.major = NULL, x.minor = NULL, y.minor = NULL, geom = "point", facets = NULL, polar = FALSE, height = rel(0.95), width = rel(0.95), y_scale = identity, x_scale = identity, quiet = TRUE) {
	
	if (missing(ens)) stop("gqplot needs missing argument: ens")
	if (!is.grouped(ens)) stop("ens must be of class 'grouped'")
	
	args <- as.list(match.call()[-1])
	vars <- match.call(expand.dots = FALSE)$... 
	env <- parent.frame()
	
	cols <- unique(unlist(lapply(args, "all.vars")))	
	data <- ens[, names(ens) %in% c(cols, ".gid", ".x", ".y")]
	
	data <- resolve_axes(ens, x.minor, y.minor, x.major, y.major, 
		polar, height, width, y_scale, x_scale, quiet)		
		
	data <- resolve_variables(data, vars, env)

	aesthetics <- quick_aes(names(vars))
	parameters <- quick_params(vars)
	labels <- quick_labels(args, x_major(ens), y_major(ens))
		
	geom.fun <- paste("geom", geom, sep = "_")
	geom.call <- as.call(c(as.name(geom.fun), parameters))
	geom.layer <- eval(geom.call)
		
	p <- ggplot(data, aesthetics, environment = env) + geom.layer
	
	if (!is.null(facets)) {
		p <- ifelse(length(facets) == 3, 
			p + facet_grid(facets), 
			p + facet_wrap(facets))
	}

	p + opts(title = labels$main) +
		xlab(labels$x) +
		ylab(labels$y)
}


resolve_axes <- function(data, x.minor, y.minor, x.major, y.major, polar, height, width, y_scale, x_scale, quiet) {

	x.minor <- substitute(x.minor, env = parent.frame()) 
	y.minor <- substitute(y.minor, env = parent.frame())
	x.major <- substitute(x.major, env = parent.frame()) 
	y.major <- substitute(y.major, env = parent.frame())
	
	n.xy <- sum(c(!is.null(x.minor), !is.null(y.minor)))
	if (n.xy == 1) stop(paste("missing argument:", c("x.minor", "y.minor")[xy]))

	
	# construct major variables
	if(is.null(x.major)) {
		data$x.major <- data[[x_major(data)]]
	}
	
	if(is.null(y.major)) {
		data$y.major <- data[[y_major(data)]]
	}
	
	if (!(is.null(x.major) & is.null(y.major))) {
		mutates <- list(ens = data, x.major = x.major, y.major = y.major)
		nulls <- vapply(mutates, "is.null", c(TRUE))
		mutates <- mutates[!nulls]
		data <- do.call("gmutate", mutates)
	}
	
	
	# construct (x,y) locations from minor and major variables
	if (n.xy == 0) {
		names(data)[names(data) == "x.major"] <- ".x"
		names(data)[names(data) == "y.major"] <- ".y"
	} else {
		necessary <- c(TRUE, TRUE, TRUE)
		if (deparse(x.minor) %in% names(data)) {
			data$x.minor <- data[[deparse(x.minor)]]
			necessary[2] <- FALSE
		}
		if (deparse(y.minor) %in% names(data)) {
			data$y.minor <- data[[deparse(y.minor)]]
			necessary[3] <- FALSE
		}
		if (sum(necessary) - 1) {
			mutates <- list(ens = data, x.minor = x.minor, y.minor = y.minor)
			mutates <- mutates[necessary]
			data <- do.call("gmutate", mutates)
    	}
    	
    	data <- glyphs(data, "x.minor", "y.minor", "x.major", "y.major", polar, 
    		height, width, y_scale, x_scale, quiet)
    }

    data[setdiff(names(data), c("x.minor", "y.minor", "x.major", "y.major"))]
}


resolve_variables <- function(data, vars, env) {
	summaries <- compact(vars[.all_aes])
	summaries <- summaries[!is.constant(summaries)]
	if (length(summaries) == 0) {
		return(data)
	}
	
	single <- subset(data, .gid == .gid[[1]])
	single_result <- lapply(summaries, eval, envir = single, 
		enclos = env)
	
	lengths <- vapply(single_result, length, integer(1))
	if (all(lengths == 1)) {
		fun <- "gsummarise"
		vars <- .(.x = .x[[1]], .y = .y[[1]])
	} else {
		fun <- "gtransform"
		vars <- NULL
	}
		
	summary_call <- as.call(c(list(as.name(fun), ens = quote(data)), 
		summaries, vars))
	eval(summary_call)
}


quick_aes <- function(names.vars) {
	maps <- aes_all(names.vars)
	maps$x <- quote(.x)
	maps$y <- quote(.y)
	maps
}


quick_params <- function(args) {
    params <- compact(args[.all_aes])
    
    if (length(is.constant(params)) > 0) {
    	params[is.constant(params)]
    } else {
	    list()
	}
}

quick_labels <- function(args, x.major, y.major) {

	# plot title
	args <- as.list(args)
	aesthetics <- c("x.minor", "y.minor", .all_aes[.all_aes != "facets"])
	aess <- args[names(args) %in% aesthetics]
	aess <- aess[!is.constant(aess)]
	lines <- paste(names(aess), as.character(aess), sep = " = ")
	main <- do.call("paste", c(as.list(lines), sep = "\n"))
	
	# x label
	if (is.null(args$x.major)) {
		x.lab <- x.major
	} else {
		x.lab <- deparse(substitute(args)$x.major)
	}
	
	# y label
	if (is.null(args$y.major)) {
		y.lab <- y.major
	} else {
		y.lab <- deparse(substitute(args)$y.major)
	}
	
	list(main = main, x = x.lab, y = y.lab)
}

    		
.all_aes <- c("adj", "alpha", "angle", "bg", "cex", "col", "color", "colour", 
	"fg", "facets", "fill", "group", "hjust", "label", "linetype", "lower", "lty", 
	"lwd", "max", "middle", "min", "order", "pch", "radius", "sample", "shape", 
	"size", "srt", "upper", "vjust", "weight", "width", "x", "xend", 
	"xmax", "xmin", "xintercept", "y", "yend", "ymax", "ymin", 
	"yintercept", "z")   
	
is.constant <- function(x) {
  sapply(x, function(x) "I" %in% all.names(asOneSidedFormula(x)))
}		