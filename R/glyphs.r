#' Create the data needed to generate a glyph plot.
#'
#' @param data A data frame containing variables named in \code{x.minor},
#'   \code{y.minor}, \code{x.major} and \code{y.major}.
#' @param x.minor,y.minor,x.major,y.major The name of the variable (as a
#'   string) for the major and minor x and y axes.  Together, each unique
#    combination of \code{x.major} and \code{y.major} specifies a grid cell.
#' @param polar A logical of length 1, specifying whether the glyphs should
#'   be drawn in polar coordinates.  Defaults to \code{FALSE}.
#' @param height,width The height and width of each glyph. Defaults to 95\% of
#'  the \code{\link[ggplot2]{resolution}} of the data. Specify the width
#'  absolutely by supplying a numeric vector of length 1, or relative to the
#   resolution of the data by using \code{\link{rel}}.
#' @param y_scale,x_scale The scaling function to be applied to each set of
#'  minor values within a grid cell.  Defaults to \code{\link{identity}} so
#'  that no scaling is performed.
#' 
#' @export
glyphs <- function(data, x.minor, y.minor, x.major = NULL, y.major = NULL, polar = FALSE, height = rel(0.95), width = rel(0.95), y_scale = identity, x_scale = identity, quiet = FALSE) {
	
  if (is.null(data$.gid)) {
    data$.gid <- interaction(data[[x.major]], data[[y.major]], drop = TRUE)
  }
  
  if (is.grouped(data) & is.null(x.major) & is.null(y.major)) {
  	x.major <- x_major(data)
  	y.major <- y_major(data)
  } else if (is.null(x.major) | is.null(y.major)) {
  	stop(paste("glyphs requires the following missing arguments:",
  	  "x.major"[is.null(x.major)], "y.major"[is.null(y.major)]))
  }
  
  if (is.rel(width)) {
    width <- resolution(data[[x.major]], zero = FALSE) * unclass(width)
    if (!quiet) message("Using width ", format(width, digits = 3))
  }
    
  if (is.rel(height)) {
    height <- resolution(data[[y.major]], zero = FALSE) * unclass(height)
    if (!quiet) message("Using height ", format(height, digits = 3))
  }
  
  if (!identical(x_scale, identity) || !identical(y_scale, identity)) {
    data <- ddply(data, ".gid", function(df) {
      df[[x.minor]] <- x_scale(df[[x.minor]])
      df[[y.minor]] <- y_scale(df[[y.minor]])
      df
    })
  }
  
  if (polar) {
    theta <- 2 * pi * rescale01(data[[x.minor]])
    r <- rescale01(data[[y.minor]])
    
    data$.x <- data[[x.major]] + width  / 2 * r * sin(theta) 
    data$.y <- data[[y.major]] + height / 2 * r * cos(theta)
    data <- data[order(data[[x.major]], data[[x.minor]]), ] 
  } else {
    data$.x <- data[[x.major]] + rescale11(data[[x.minor]]) * width / 2
    data$.y <- data[[y.major]] + rescale11(data[[y.minor]]) * height / 2 
  }
  
  structure(data, 
    width = width, height = height, polar = polar, 
    x.major = x.major, y.major = y.major,
    class = c("glyphplot", "data.frame"))
}

#' Create reference lines for a glyph plot
ref_lines <- function(data) {
  stopifnot(is.glyphplot(data))
  
  glyph <- attributes(data)
  
  cells <- unique(data[c(glyph$x.major, glyph$y.major, ".gid")])

  if (glyph$polar) {
    ref_line <- function(df) {
      theta <- seq(0, 2 * pi, length = 30)
      data.frame(
        .gid = df$.gid,
        .x = df[[glyph$x.major]] + glyph$width / 4 * sin(theta),
        .y = df[[glyph$y.major]] + glyph$height / 4 * cos(theta)
      )
      
    }
  } else {
    ref_line <- function(df) {
      data.frame(
        .gid = df$gid,
        .x = df[[glyph$x.major]] + c(-1, 1) * glyph$width / 2,
        .y = df[[glyph$y.major]]
      )
    }
  }
  ddply(cells, ".gid", ref_line)
}

#' Create reference boxes for a glyph plot
ref_boxes <- function(data, fill = NULL) {
  stopifnot(is.glyphplot(data))
  glyph <- attributes(data)
  cells <- data.frame(unique(data[c(glyph$x.major, glyph$y.major, ".gid", fill)]))
  
  df <- data.frame(xmin = cells[[glyph$x.major]] - glyph$width/2, 
      xmax = cells[[glyph$x.major]] + glyph$width/2, 
      ymin = cells[[glyph$y.major]] - glyph$height/2,
      ymax = cells[[glyph$y.major]] + glyph$height/2) 
  if (!is.null(fill)){
    df$fill <- cells[[fill]]
  }
  df
}


# Glyph plot class -----------------------------------------------------------

glyphplot <- function(data, width, height, polar, x_major, y_major) {
  structure(data, 
    width = width, height = height, polar = polar, 
    x.major = x.major, y.major = y.major,
    class = c("glyphplot", "data.frame"))  
}
is.glyphplot <- function(x) inherits(x, "glyphplot")

"[.glyphplot" <- function(x, ...) {
  glyphplot(NextMethod(), 
    width = attr(x, "width"), height = attr(x, "height"),
    x_major = attr(x, "x.major"), y_major = attr(x, "y.major"),
    polar = attr(x, "polar"))
}

print.glyphplot <- function(x, ...) {
  NextMethod()
  if (attr(x, "polar")) {
    cat("Polar ")
  } else {
    cat("Cartesian ")
  }
  width <- format(attr(x, "width"), digits = 3)
  height <- format(attr(x, "height"), digits = 3)

  cat("glyphplot: \n")
  cat("  Size: [", width, ", ", height,  "]\n", sep = "")
  cat("  Major axes: ", attr(x, "x.major"), ", ", attr(x, "y.major"), "\n", 
    sep = "")
  # cat("\n")
}


# Relative dimensions --------------------------------------------------------

rel <- function(x) {
  structure(x, class = "rel")
}
print.rel <- function(x, ...) print(noquote(paste(x, " *", sep = "")))
is.rel <- function(x) inherits(x, "rel")

# Rescaling functions --------------------------------------------------------

range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

max1 <- function(x) {
  x / max(x, na.rm = TRUE)
}
mean0 <- function(x) {
  x - mean(x, na.rm = TRUE)
}
min0 <- function(x) {
  x - min(x, na.rm = TRUE)
}


rescale01 <- function(x, xlim=NULL) {
  if (is.character(x)) {
  	x <- as.numeric(factor(x))
  }	
  if (is.factor(x)) {
  	x <- as.numeric(x)
  }
  if (is.null(xlim)) {
	  rng <- range(x, na.rm = TRUE)
   } else {
   	  rng <- xlim
   }
   (x - rng[1]) / (rng[2] - rng[1])
}
rescale11 <- function(x, xlim=NULL) 2 * rescale01(x, xlim) - 1

	
	


