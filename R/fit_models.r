#' Performs group-wise maodelling on an mg_ensemble object
#'
#' fit_models fits a model to each subset of data in an mg_ensemble object. 
#'
#' @param data An mg_ensemble object
#' @param .fun A modelling function to use
#' @param formula, the modelling formula to use
#' @param ... additional arguments to pass to the modelling function
#'
#' @export 
fit_models <- function(data, .fun, formula, ...) {
    	
    	models <- dlply(data, ".gid", .fun, formula = formula, ...)
    	
		mg.info <- attr(data, "mg.info")
		mg.info$mod.type <- as.character(substitute(.fun))
		mg.info$formula <- formula
		
		mg_class <- paste("mg_", mg.info$mod.type, sep = "")
		
		structure(models, mg.info = mg.info, 
			class = c(mg_class, "mg_models", "list"))
}