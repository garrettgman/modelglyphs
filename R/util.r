#' Returns a data.frame with new names
#'
#' @keywords internal
#' @export
renamed <- function(df, index, name) {
	names(df)[index] <- name
	df
}

#' Add the x_major and y_major variables to a dataframe with gid
#'
#' @keywords internal
#' @export
add_labels <- function(df, ens) {
	x <- attr(ens, "x")[df$gid]
	y <- attr(ens, "y")[df$gid]
	
	vars <- setdiff(names(df), "gid")
	new <- data.frame(cbind(df$gid, x, y, df[ , vars]))
	names(new) <- c("gid", attr(ens, "x_name"), attr(ens, "y_name"), vars)
	new
}


#' Performs ldply, then labels and reorders output
#'
#' ldply_ensemble applies a function to an ensemble object and then organizes the output to 
#' match the data frame the ensemble object was created from. \code{\link{glyphs}} can 
#' be immediately applied to the output without preprocessing it. The output can also 
#' be merged to the original data frame with cbind.
#'
#' @param model An ensemble object
#' @param FUN A function to be applied to the model objectss in the ensemble object
#' @param arguments to be passed to FUN
#'
#' @keywords internal
#' @export
ldply_ensemble <- function(model, FUN, ...) {
	df <- ldply(model, FUN, ...)
	df <- add_labels(df, model)[attr(model, "reorder"), ]
	row.names(df) <- attr(model, "row.names")
	df
}

#' Performs llply, then reorders output and returns as a vector
#'
#' llply_ensemble applies a function to an ensemble object and then organizes the output to 
#' match the data frame the ensemble object was created from. The output can
#' be merged to the original data frame with cbind. Or by directly saving it as a new 
#' column in the dataframe.
#'
#' @param model An ensemble object
#' @param FUN A function to be applied to the model objectss in the ensemble object
#' @param arguments to be passed to FUN
#'
#' @keywords internal
#' @export
llply_ensemble <- function(model, FUN, ...) {
	df <- unlist(llply(model, FUN, ...), recursive = TRUE, use.names = FALSE)
	df[attr(model, "reorder")]
}