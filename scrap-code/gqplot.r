is.constant <- function(x) {
  sapply(x, function(x) "I" %in% all.names(asOneSidedFormula(x)))
}

.g_all_aesthetics <- c("adj", "alpha", "angle", "bg", "cex", "col", "color", "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower", "lty", "lwd", "max", "middle", "min", "order", "pch", "radius", "sample", "shape", "size", "srt", "upper", "vjust", "weight", "width", "x", "x.minor", "xend", "xmax", "xmin", "xintercept", "y", "y.minor", "yend", "ymax", "ymin", "yintercept", "z")

.base_to_gqplot <- c(
  "col"   = "colour",
  "color" = "colour", 
  "pch"   = "shape",
  "cex"   = "size", 
  "lty"   = "linetype", 
  "lwd"   = "size",
  "srt"   = "angle",
  "adj"   = "hjust",
  "bg"    = "fill",
  "fg"    = "colour",
  "min"   = "ymin", 
  "max"   = "ymax",
  "x.minor" = "x",
  "y.minor" = "y"
)

# Rename American or old-style aesthetics name
rename_gaes <- function(x) {
  # Convert prefixes to full names
  full <- charmatch(names(x), .g_all_aesthetics)
  names(x)[!is.na(full)] <- .g_all_aesthetics[full[!is.na(full)]]
  
  rename(x, .base_to_gqplot)
}


#' @export
gqplot <- function (x, y, ..., data, x.major, y.major, facets = NULL, margins = FALSE, 
    geom = "auto", position = list(NULL), stat = list(NULL),
    xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, 
    xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), 
    asp = NA, polar = FALSE, height = rel(0.95), width = rel(0.95), 
    y_scale = identity, x_scale = identity, quiet = TRUE) {
    
    if (missing(data)) stop("gqplot needs missing argument: data")
    if (missing(x.major)) x.major <- x_major(data)
    if (missing(y.major)) y.major <- y_major(data)
    	
    argnames <- names(as.list(match.call(expand.dots = FALSE)[-1]))
    arguments <- as.list(match.call()[-1])
    aesthetics <- compact(arguments[.g_all_aesthetics])
    aesthetics <- aesthetics[!is.constant(aesthetics)] # catches gmutates + glyphs
    aes_names <- names(aesthetics)
    aesthetics <- rename_gaes(aesthetics)
    class(aesthetics) <- "uneval"

	geom[geom == "auto"] <- "point"

    env <- parent.frame()
    
    
    if ("x" %in% aes_names | "y" %in% aes_names) {
    	if (!(all(c("x", "y") %in% aes_names))) {
    		stop(paste("missing aesthetic:", 
    			c("x", "y")[!(c("x", "y") %in% aes_names)]))
    	}
    	xminor <- as.character(substitute(x))
    	yminor <- as.character(substitute(y))
    	xmajor <- as.character(substitute(x.major))
    	ymajor <- as.character(substitute(y.major))
    	data <- glyphs(data, xminor, yminor, xmajor, ymajor, polar, 
    		height, width, y_scale, x_scale, quiet)
    	aesthetics$x = as.name(".x")
		aesthetics$y = as.name(".y")
    } else {
		aesthetics$x = as.name(x.major)
		aesthetics$y = as.name(y.major)
    }
    
    p <- ggplot(data, aesthetics, environment = env)
    if (is.null(facets)) {
        p <- p + facet_null()
    }
    else if (is.formula(facets) && length(facets) == 2) {
        p <- p + facet_wrap(facets)
    }
    else {
        p <- p + facet_grid(facets = deparse(facets), margins = margins)
    }
    if (!is.null(main)) 
        p <- p + opts(title = main)
    if (proto::is.proto(position)) 
        position <- list(position)
    mapply(function(g, s, ps) {
        if (is.character(g)) 
            g <- Geom$find(g)
        if (is.character(s)) 
            s <- Stat$find(s)
        if (is.character(ps)) 
            ps <- Position$find(ps)
        params <- arguments[setdiff(names(arguments), c(aes_names, 
            argnames))]
        params <- lapply(params, eval, parent.frame(n = 1))
        p <<- p + layer(geom = g, stat = s, geom_params = params, 
            stat_params = params, position = ps)
    }, geom, stat, position)
    logv <- function(var) var %in% strsplit(log, "")[[1]]
    if (logv("x")) 
        p <- p + scale_x_log10()
    if (logv("y")) 
        p <- p + scale_y_log10()
    if (!is.na(asp)) 
        p <- p + opts(aspect.ratio = asp)
    if (!missing(xlab)) {
        p <- p + xlab(xlab)
    } else {
    	p <- p + xlab(as.character(substitute(x.major)))
    }
    if (!missing(ylab)) {
        p <- p + ylab(ylab)
    } else {
    	p <- p + ylab(as.character(substitute(y.major)))
    }
    if (!missing(xlim)) 
        p <- p + xlim(xlim)
    if (!missing(ylim)) 
        p <- p + ylim(ylim)
    p
}
environment(gqplot) <- environment(qplot)

	# 1.a. catch every variable that needs fed to gmutate
	# 1.b. mutate it
	
	# 2.a. catch every variable that needs fed to glyphs
	# 2.b. glyph it
	
	# 3. reconstruct qplot call