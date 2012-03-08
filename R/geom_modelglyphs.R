geom_modelglyphs <- function(mapping = NULL, data = NULL, models = NULL, polar = FALSE, stat = "identity", position = "identity", ...) {
	
	# 1. interpret mapping correctly (directly feed correct functions for now)
	# 2. check for x.minor and y.minor with stop message
	err <- NULL
	if(is.null(mappings$x.minor)) 
		err <- paste(err, "x.minor")
	if(is.null(mappings$y.minor)) 
		err <- paste(err, "y.minor")
	if(length(err) > 0)
		stop(paste("geom_modelglyphs requires the following missing aesthetics:", err))
	
	# 3. create data set to feed to GeomLine
	#	a. group - retrieve info from models
	glyph.df <- data.frame(group = attr(models, "group"))
	
	#	b. x, y - run x.minor and y.minor through glyphs
	
	#	c. remaining aesthetics
	# 4. call GeomLines
}




aes(x.minor = .variables, y.minor = .coefs, fill = .AIC) -> mapping
	
	which(mapping == as.symbol("."))