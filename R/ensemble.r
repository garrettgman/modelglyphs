#' Performs group-wise modelling on a data frame
#'
#' ensemble splits the data frame into pieces based on the unique combinations 
#' of the x_major and y_major variables. It then
#' applies the specified model to each piece and saves the output as an ensemble
#' object. Note, ensemble removes the x_major and Y_major variables from the dataframe for
#' purposes of modelling. Each subset of the data will only have one unique 
#' value of each variable, which makes them inappropriate to include in the 
#' model.
#' 
#' @param data a data frame to apply group-wise modelling to
#' @param groups a variable that provides a group identifier for doing group_wise modelling.
#' @param model Information necessary to identify and apply a regression method. this information should be supplied as the output of a call to \code{\link{model}}.
#' @param x_major a character string that specifies the variable to be on the major x axis when using the ensemble to make glyph plots. The data frame will be split into groups based on the unique combinations of x_major and y_major.
#' @param y_major a character string that specifies the variable to be on the major y axis when using the ensemble to make glyph plots.
#' 
#' @return an S3 ensemble object. 
#'
#' @export
ensemble <- function(data, groups, model, x_major = NULL, y_major = NULL) {
	
	

	half.empty <- structure(list(), 
		data_set = data, 
		groups = groups, 
		x_major = x_major, 
		y_major = y_major, 
		model_info = model,
		key = NULL,
		collate = NULL,
		class = c("mg_ensemble", "list"))
	update(half.empty)
}


#' @S3method format mg_ensemble
format.mg_ensemble <- function(x, ...) {
	cat(paste("ensemble of", length(x), "models:"), 
		paste("\nmethod =", model_info(x)$FUN, "\nformula =", 
		deparse(model_info(x)$formula)))
}

#' @S3method print mg_ensemble
print.mg_ensemble <- function(x, ...) {
	format(x)
}

#' Is x an ensemble object?
#'
#' is.ensemble tests whether an object inherits from the mg_ensemble class.
#'
#' @param x An object to be tested for membership in the S3 mg_ensemble class
#'
#' @export
is.ensemble <- function(x) {
	inherits(x, "mg_ensemble")
}

#' Is x a modelglyphs object?
#'
#' is.mg tests whether an object inherits from a modelglyphs class. Such objects should all contain a data set with a gid variable and have the following attributes: model_info, key, x_major, y_major.
#'
#' @param x An object to be tested for membership in a modelglyphs class
#'
#' @export
is.mg <- function(x) {
	substr(class(x)[1], 1, 3) == "mg_"
}