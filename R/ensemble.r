#' Performs group-wise modelling on a data frame
#'
#' ensemble splits the data frame into pieces based on the by variable. It then
#' applies the specified model to each piece and saves the output as an ensemble
#' object. Note, ensemble removes the grouping variable(s) from the dataframe for
#' purposes of modelling. Each subset of the data will only have one unique 
#' value of the grouping variable, which makes it inappropriate to include in the 
#' model.
#' 
#' @param data a data frame to apply group-wise modelling to
#' @param by A character string that lists the variable name(s) to performing grouping with. The data frame will be split into groups based on the unique values of the by variable(s).
#' @param model A character string to be matched to a modelling function. The intended modelling function must be loaded in the current R session.
#' @param formula a formula object to use as the model
#' @param ... further named arguments to be passed to the modelling function
#' 
#' @return an S3 ensemble object. 
#'
#' @export
ensemble <- function(data, by, formula, ... , MODEL = "lm") {
	model <- match.fun(MODEL)
	fit <- function(data) model(data = data[ , -which(names(data) %in% by)], 
		formula, ...)
	
	models <- dlply(data, by, fit)
	
	ord.call <- as.call(c(quote(order), unname(mapply(as.name, by))))
	ord <- eval(ord.call, data, parent.frame())
	
	structure(models, 
		method = as.character(MODEL),
		formula = formula, 
		reorder = order(ord),
		class = c("ensemble", "list"))
}

