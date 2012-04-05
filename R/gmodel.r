#' Performs group-wise modelling on a grouped data frame
#'
#' gmodels fits a model to each subset of data in a grouped data frame. 
#'
#' @param data A data frame of class 'grouped'
#' @param .fun A character string. The name of the modelling function to use
#' @param formula, the modelling formula to use
#' @param ... additional arguments to pass to the modelling function
#'
#' @export 
gmodel <- function(data, .fun, formula, ...) {
		lm <- function(...) stats::lm(model = FALSE, ...)
    	.Fun <- get(.fun)
    	models <- dlply(data, ".gid", .Fun, formula = formula, ...)
    	
		g.info <- group_info(data)
		m.type <- .fun
		m.formula <- formula
		m.time <- Sys.time()
		m.call <- as.call(list(as.name(.fun), formula = formula, 
			data = substitute(data)))
			
		models <- lapply(models, function(x) {x$call <- m.call; x})	
		mg_class <- c("grouped", "models", class(models[[1]]))
		
		mg <- structure(models, group.info = g.info, 
			model.info = list(type = m.type, formula = m.formula, time = m.time), 
			class = mg_class)
		
		n <- n_mod(data)
		n_mod(data) <- n + 1
		mod.name <- paste("mod", n + 1, sep = ".")
		
		assignInGroupspace(mod.name, mg, mg)
		
		mg
}