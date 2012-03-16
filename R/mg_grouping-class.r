#' Is x an mg_grouping object?
#'
#' @param x An object to test
#' @return A logical. TRUE if x is a mg_grouping object. FALSE otherwise.
#' @keywords internal
#' @export
is.mg_grouping <- function(x) {
	inherits(x, "mg_grouping")
}


#' Group by interactions of x and y
#'
#' cross creates a mg_grouping object that groups a data set by the unique 
#' combinations of x and y.
#' 
#' @param x A variable name in \link{data}
#' @param y A variable name in \link{data}
#' @param data A data frame
#' @return a mg_grouping class object
#'
#' @export
cross <- function(x, y, data){
	x_major <- as.character(substitute(x))
	y_major <- as.character(substitute(y))
	group <- interaction(data[[x_major]], data[[y_major]])
	
	structure(group, x_major = x_major, y_major = y_major, 
		class = c("mg_grouping", "factor"))
}

#' @S3method $ mg_grouping
"$.mg_grouping" <- function(x, name) {
	attr(x, name)
}
	

