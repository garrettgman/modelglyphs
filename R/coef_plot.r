#' @export
coef_plot <- function(mods, variable = NULL, ...) {
	# 0. get coefs
	coefs <- coef(mods)
	
	# 1. add x and y
	coefs$x <- key(mods)[ ,2][coefs$.gid]
	coefs$y <- key(mods)[ ,3][coefs$.gid]
	names(coefs)[names(coefs) == "Pr(>|t|)"] <- "p.value"
	plot.title <- paste(variable, "coefficient by group\n", mg_call(mods))
	
	# 2. calculate size and color 
#	max.value <- max(abs(range(coefs$Estimate))) + 1e-3
#		values <- c(-max.value, 0, max.value)
		
	
	
	# 3. make plot
	ggplot(coefs, aes(x, y)) +
		geom_point(aes(size = p.value, ...)) +
#		geom_point(aes(color = Estimate, size = p.value), ...) +
		facet_wrap( ~ variable) +
#		scale_colour_gradientn("Estimate", 
#			colours = RColorBrewer::brewer.pal(11, "RdYlBu")[11:1], 
#			values = values, rescaler = function(x, ...) x, 
#			oob = identity) + guides(color = guide_colorbar()) +	
		scale_area("Pr(>|t|)", range = c(4, 0.5), 
			breaks = c(0.01,0.05,0.1,0.25, 0.5,1)) +
		opts(title = plot.title) +
		xlab(x_major(mods)) +
		ylab(y_major(mods))

}
		
		
		
		
		
		
		
		
		
	
#	if (!inherits(mods, "mg_models"))
#		stop("mods must be of class mg_models")
	
#	coefs <- coef(mods)
#	coefs <- coefs[coefs$variable == variable, ]
#	coefs <- xy(coefs)
	
#	.x <- as.name(x_major(mods))
#	.y <- as.name(y_major(mods))
#	plot.title <- paste(variable, "coefficient by group\n", mg_call(mods))
	
#	if (color) {
#		max.value <- max(abs(range(coefs$Estimate))) + 1e-3
#		values <- c(-max.value, 0, max.value)
		
#		p <-  ggplot(data = coefs, aes(.x, .y)) +
#			geom_point(aes(color = Estimate, size = coefs[["Pr(>|t|)"]]), ...) +
#			scale_colour_gradientn(color, 
#				colours = RColorBrewer::brewer.pal(11, "RdYlBu")[11:1], 
#				values = values, rescaler = function(x, ...) x, 
#				oob = identity) + guides(color = guide_colorbar())
#	} else {
#		p <- ggplot(coefs, aes(.x, .y)) + 
#			geom_point(aes(size = coefs[["Pr(>|t|)"]]))
#	}
	
#	p +	scale_area("Pr(>|t|)", range = c(6, 1), 
#		breaks = c(0.01,0.05,0.1,0.25, 0.5,1)) +
#		opts(title = plot.title) +
#		xlab(x_major(mods)) +
#		ylab(y_major(mods))
#}