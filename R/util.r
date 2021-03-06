#' Returns a data.frame with new names
#'
#' @keywords internal
#' @export
renamed <- function(df, index, name) {
	names(df)[index] <- name
	df
}

#' add a class attribute
#'
#' add_class adds the specified class to the beginning of an object's class attribute vector. The object retains all previous class memberships.
#'
#' @param x an object to be given a new class 
#' @param new.class a character string
#' @keywords internal
#' @export
add_class <- function(x, new.class) {
	vec <- class(x)
	class(x) <- c(new.class, vec)
	x
}

#' switch the first class attribute
#'
#' switch_class replaces the first element of an object's class vector with the 
#' specified class. The object retains all other class memberships.
#'
#' @param x an object to be given a new class 
#' @param new.class a character string
#' @keywords internal
#' @export
switch_class <- function(x, new.class) {
	vec <- class(x)[-1]
	class(x) <- c(new.class, vec)
	x
}



#' Add the x_major and y_major variables to a dataframe with gid
#'
#' @keywords internal
#' @export
add_labels <- function(df, ens) {
	x <- key(ens)$x[df$gid]
	y <- key(ens)$y[df$gid]
	
	vars <- setdiff(names(df), "gid")
	new <- data.frame(cbind(df$gid, x, y, df[ , vars]))
	names(new) <- c("gid", x_major(ens), y_major(ens), vars)
	new
}

#' Coerce an object to a modelglyphs class
#'
#' mg coerces an object to the modelglyphs class specified by the class argument. 
#'
#' @keywords internal
#' @param x An object to be coerced to a modelglyphs class.
#' @param ens An mg_ensemble object to borrow attributes from.
#' @param class A chracter string. The name of the class to give the new object.
#' @param collate logical. Should the output be arranged in the order of the data the mg_ensemble was fitted to and assigned the same row.names.
#' @export
mg <- function(x, ens, class, collate = FALSE) {

	if (substr(class, 1, 3) != "mg_") {
		stop("Invalid mg class name. Must begin with 'mg_'")
	}
	
	x <- add_labels(x, ens)
	
	if (collate) {
		if (nrow(x) == length(collate(ens))) {
			x <- x[collate(ens), ]
			row.names(x) <- attr(data_set(ens), "row.names")
		} else {
			message("collate did not occur:\nx and data have different lengths.")
		}
	}
		
	x <- add_class(x, class)
	attr(x, "x_major") <- x_major(ens)
	attr(x, "y_major") <- y_major(ens)
	attr(x, "model_info") <- model_info(ens)
	
	x
}


#' Performs ldply, then labels and reorders output
#'
#' ldply_ensemble applies a function to an ensemble object and then organizes the output to 
#' match the data frame the ensemble object was created from. \code{\link{glyphs}} can 
#' be immediately applied to the output without preprocessing it. The output can also 
#' be merged to the original data frame with cbind.
#'
#' @param ens An ensemble object
#' @param FUN A function to be applied to the model objectss in the ensemble object
#' @param arguments to be passed to FUN
#'
#' @keywords internal
#' @export
ldply_ensemble <- function(ens, FUN, ...) {
	df <- ldply(ens, FUN, ...)
	df <- add_labels(df, ens)[collate(ens), ]
	row.names(df) <- attr(data_set(ens), "row.names")
	df
}

#' Performs llply, then reorders output and returns as a vector
#'
#' llply_ensemble applies a function to an ensemble object and then organizes the output to 
#' match the data frame the ensemble object was created from. The output can
#' be merged to the original data frame with cbind. Or by directly saving it as a new 
#' column in the dataframe.
#'
#' @param ens A mg_ensemble object
#' @param FUN A function to be applied to the model objectss in the ensemble object
#' @param arguments to be passed to FUN
#'
#' @keywords internal
#' @export
llply_ensemble <- function(ens, FUN, ...) {
	vec <- unlist(llply(ens, FUN, ...), recursive = TRUE, use.names = FALSE)
	vec[collate(ens)]
}
