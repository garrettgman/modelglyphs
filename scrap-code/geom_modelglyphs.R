geom_modelglyphs <- function(mapping = NULL, data = NULL, models = NULL, polar = FALSE, stat = "identity", position = "identity", ...) {
	
	# 2. check for x_minor and y_minor with stop message
	err <- NULL
	if(is.null(mappings$x_minor)) 
		err <- paste(err, "x_minor")
	if(is.null(mappings$y_minor)) 
		err <- paste(err, "y_minor")
	if(length(err) > 0)
		stop(paste("geom_modelglyphs requires the following missing aesthetics:", err))
		
	# 3. check for group


	glyph.df <- data.frame(group = attr(models, "group"))
	
	#	b. x, y - run x.minor and y.minor through glyphs
	
	#	c. remaining aesthetics
	# 4. call GeomLines
}




aes(x.minor = .variables, y.minor = .coefs, fill = .AIC) -> mapping
	
	which(mapping == as.symbol("."))