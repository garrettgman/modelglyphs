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
