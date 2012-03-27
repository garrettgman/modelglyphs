#' S3method coef mg_ensemble
coef.mg_ensemble <- function(object, ...){
	coefs <- ldply(object, coef)
	coefs <- reshape2::melt(coefs, id = "gid", value.name = "coefficient")
	mg(coefs, object, "mg_coef")
}

#' S3method coef mg_summary
coef.mg_summary <- function(object, ...){
	coef_df <- function(summ, ...) {
		cfs <- coef(summ)
		cbind(variable = row.names(cfs), as.data.frame(cfs))
	}
	output <- ldply(object, coef_df, ...)
	mg(output, object, "mg_coef_summary")
}


#' @S3method autoplot mg_coef
autoplot.mg_coef <- function(object, variable = NULL, ...) {
	require(ggplot2)
	if (is.null(variable)) stop("missing argument: variable")
	stopifnot(variable %in% object$variable)
	object <- object[object$variable == variable,]

	plot.title <- paste("Magnitudes of", variable, "\n", mg_call(object))

	magnitude_plot(object, "coefficient", title = plot.title, ...)	
}


#' @S3method autoplot mg_coef
autoplot.mg_coef_summary <- function(object, variable = NULL, p.value = "Pr(>|t|)", color = TRUE, ...) {
	
	if (is.null(variable)) stop("missing argument: variable")
	stopifnot(variable %in% object$variable)
	object <- object[object$variable == variable,]
	
	color.var <- ifelse(color, "Estimate", NULL)
	
	info <- model_info(object)
	plot.title <- paste("Significance of", variable, "\n", mg_call(object))
	
	significance_plot(object, p.value, color.var, 
		title = plot.title, ...)
}