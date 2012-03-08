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
#' @param x_major a character string that specifies the variable to be on the major x axis when using the ensemble to make glyph plots. The data frame will be split into groups based on the unique combinations of x_major and y_major.
#' @param y_major a character string that specifies the variable to be on the major y axis when using the ensemble to make glyph plots.
#' @param model Information necessary to identify and apply a regression method. this information should be supplied as the output of a call to \code{\link{model}}.
#' 
#' @return an S3 ensemble object. 
#'
#' @export
ensemble <- function(data, x_major, y_major, model) {
	data$gid <- id(list(data[[x_major]], data[[y_major]]), drop = TRUE)
	
	fit <- function(data, model) {
		data <- data[ , setdiff(names(data), c(x_major, y_major, "gid"))]
		model.args <- c(data = as.name("data"), model[setdiff(names(model), "type")])
		
		do.call(model$type, model.args)
	}
	
	models <- dlply(data, "gid", fit, model)
	
	# can be subset by gid to return x and y
	translations <- ddply(data, "gid", 
		function(df) c(df[[x_major]][1], df[[y_major]][1])) 
	
	structure(models, 
		method = model$type,
		formula = model$formula, 
		reorder = order(order(data$gid)),
		x = translations$V1,
		y = translations$V2,
		x_name = x_major,
		y_name = y_major,
		class = c("ensemble", "list"))
}



#' Pass modelling information to ensemble()
#'
#' model collects the arguments needed to perform a method of regression. model is
#' intended to be used in conjunction with \code{\link{ensemble}} to create ensemble 
#' objects. model does not collect the name of the data set to be used.
#'
#' @param formula A formula object to be used in the intended model
#' @param type a character string that identifies the type of modelling method to be used. type should match the name of a modelling function in R
#' @param ... other arguments to pass to the modelling function indicated by type
#'
#' @keywords internal
#' export 
model <- function(formula, type = "lm", ...) {
	mod <- structure(as.list(match.call()[-1], class = "uneval"))
	expand_mod(mod)
}

expand_mod <- function(mod) {
	if (is.null(mod$type)) mod$type <- "lm"
	if (!is.null(mod$data)) mod$data <- NULL
	mod
}


