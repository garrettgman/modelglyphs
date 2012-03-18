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
#' @param groups a variable that provides a group identifier for doing group_wise modelling.
#' @param model Information necessary to identify and apply a regression method. this information should be supplied as the output of a call to \code{\link{model}}.
#' @param x_major a character string that specifies the variable to be on the major x axis when using the ensemble to make glyph plots. The data frame will be split into groups based on the unique combinations of x_major and y_major.
#' @param y_major a character string that specifies the variable to be on the major y axis when using the ensemble to make glyph plots.
#' 
#' @return an S3 ensemble object. 
#'
#' @export
ensemble <- function(data, groups, model, x_major = NULL, y_major = NULL) {
	
	

	half.empty <- structure(list(), 
		data_set = data, 
		groups = groups, 
		x_major = x_major, 
		y_major = y_major, 
		model_info = model,
		key = NULL,
		collate = NULL,
		class = c("mg_ensemble", "list"))
	update(half.empty)
}


#ensemble <- function(data, x_major, y_major, model) {
#	data$gid <- id(list(data[[x_major]], data[[y_major]]), drop = TRUE)

#	fit <- function(data, model) {
#		data <- data[ , setdiff(names(data), c(x_major, y_major, "gid"))]
#		model.args <- c(data = as.name("data"), model[setdiff(names(model), "type")])

#		do.call(model$type, model.args)
#	}

#	models <- dlply(data, "gid", fit, model)

	# can be subset by gid to return x and y
#	translations <- ddply(data, "gid", 
#		function(df) c(df[[x_major]][1], df[[y_major]][1])) 

#	structure(models, 
#		method = model$type,
#		formula = model$formula, 
#		reorder = order(order(data$gid)),
#		x = translations$V1,
#		y = translations$V2,
#		x_name = x_major,
#		y_name = y_major,
#		row.names = row.names(data),
#		class = c("ensemble", "list"))
#}






#make_groups <- function(ens){
	
#	ens$data$gid <- data[[ens$group$gid]]





#fill_ensemble <- function(ens) {
	
	# determine grouping
	
	# fit models
#	fit <- function(data, model) {
#		data <- data[ , setdiff(names(data), c(ens$x_major, ens$y_major, "gid"))]
#		model.args <- c(data = as.name("data"), model[setdiff(names(model), 
#			"type")])	
#		do.call(model$type, model.args)
#	}
#	models <- dlply(ens$data, "gid", fit, ens$model)
	
	# create key for translating gid into x_major and y_major
	# can be subset by gid to return x and y

#	xy.key <- ddply(ens$data, "gid", 
#		function(df, x_major, y_major) c(df[[x_major]][1], df[[y_major]][1]), 
#		ens$x_major, ens$y_major) 
		
#	structure(models,
#		data = ens$data,
#		group_vars = ens$group,
#		x_major = ens$x_major,
#		y_major = ens$y_major,
#		model = ens$model,
#		key = ens$xy.key,
#		reorder = ens$reorder,
#		class = c("mg_ensemble", "list"))
#}

	
	
	
	
	
	
	
	# determine grouping
#	if (is.mg_grouping(group)) {
#		x_major <- group$x_major
#		y_major <- group$y_major
#		data$gid <- group
#	} else if (as.character(substitute(group)) %in% names(data)) {
#		data$gid <- data[[as.character(substitute(group))]]
#	} else {
#		data$gid <- group
#	}
	
	# fit models
#	fit <- function(data, model) {
#		data <- data[ , setdiff(names(data), c(x_major, y_major, "gid"))]
#		model.args <- c(data = as.name("data"), model[setdiff(names(model), 
#			"type")])	
#		do.call(model$type, model.args)
#	}
#	models <- dlply(data, "gid", fit, model)
	
	# key for translating gid into x_major and y_major
	# can be subset by gid to return x and y
#	xy.key <- ddply(data, "gid", 
#		function(df) c(df[[x_major]][1], df[[y_major]][1])) 
	
#	structure(models, 
#		method = model$type,
#		formula = model$formula, 
#		reorder = order(order(data$gid)),
#		x = xy.key$V1,
#		y = xy.key$V2,
#		x_name = x_major,
#		y_name = y_major,
#		row.names = row.names(data),
#		class = c("ensemble", "list"))
#}


