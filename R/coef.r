#' S3method coef mg_ensemble
coef.mg_ensemble <- function(object, ...){
	coefs <- ldply(object, coef)
	coefs <- reshape2::melt(coefs, id = "gid", value.name = "coefficient")
	output <- add_labels(coefs, object)
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
autoplot.mg_coef_summary <- function(object, variable = NULL, color = "Pr(>|t|)", ...) {
	require(ggplot2)
	if (is.null(variable)) stop("missing argument: variable")
	stopifnot(variable %in% object$variable)
	object <- object[object$variable == variable,]
	
	#if (color == "Pr(>|t|)") {
	#	values <- c(-1.0001, 0, 1.0001)
	#} else {
		max.value <- max(abs(range(object$Estimate))) + 1e-3
		values <- c(-max.value, 0, max.value)
	#}
	
	object$signed.color <- (1 - object[[color]]) * sign(object$Estimate)
	object$size.var <- object[[color]]
	ggplot(object, aes(x,y)) + 
		geom_point(aes(color = Estimate, size = size.var), ...) + 
		scale_colour_gradientn(colours = RColorBrewer::brewer.pal(11, 
			"RdYlBu")[11:1], values = values, rescaler = function(x, ...) x, 
			oob = identity) +
		scale_area(color, range = c(6, 1), breaks = c(0.05,0.2,0.4,0.6,0.8,1)) +
		guides(color = guide_colorbar()) + 
		opts(title = "coef(summary(e2)): (Intercept)")
}