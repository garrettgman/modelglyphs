#' S3method coef mg_ensemble
coef.mg_ensemble <- function(object, ...){
	coefs <- ldply(object, coef)
	coefs <- reshape2::melt(coefs, id = "gid", value.name = "coefficient")
	output <- add_labels(coefs, object)
	attr(output, "model_info") <- model_info(object)
	add_class(output, "mg_coef")
}

#' S3method coef mg_summary
coef.mg_summary <- function(object, ...){
	coef_df <- function(summ, ...) {
		cfs <- coef(summ)
		cbind(variable = row.names(cfs), as.data.frame(cfs))
	}
	output <- ldply(object, coef_df, ...)
	output <- add_labels(output, object)
	attr(output, "model_info") <- model_info(object)
	add_class(output, "mg_coef_summary")
}


#' @S3method autoplot mg_coef
autoplot.mg_coef <- function(object, variable = NULL, ...) {
	require(ggplot2)
	if (is.null(variable)) stop("missing argument: variable")
	stopifnot(variable %in% object$variable)
	object <- object[object$variable == variable,]
	
	max.value <- max(abs(range(object$coefficient))) + 1e-3
	values <- c(-max.value, 0, max.value)
	
	ggplot(object, aes(x,y)) + 
		geom_point(aes(color = coefficient, size = abs(coefficient)), ...) + 
		scale_colour_gradientn(colours = RColorBrewer::brewer.pal(11, 
			"RdYlBu")[11:1], values = values, rescaler = function(x, ...) x, 
			oob = identity) +
		scale_area()
}


#' @S3method autoplot mg_coef
autoplot.mg_coef_summary <- function(object, variable = NULL, p.value = "Pr(>|t|)", color = TRUE, ...) {
	
	if (is.null(variable)) stop("missing argument: variable")
	stopifnot(variable %in% object$variable)
	object <- object[object$variable == variable,]
	
	color.var <- ifelse(color, "Estimate", NULL)
	
	info <- model_info(object)
	plot.title <- paste(variable, " coefficients for\n", info$FUN, 
		"(", deparse(info$formula), ")", sep = "")
	
	significance_plot(object, p.value, color.var, 
		title = plot.title, ...)
}