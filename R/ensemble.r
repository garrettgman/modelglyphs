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
ensemble <- function(data, group, model, x_major = NULL, y_major = NULL) {
		# determine grouping
	if (is.mg_grouping(group)) {
		x_major <- group$x_major
		y_major <- group$y_major
		data$gid <- group
	} else if (as.character(substitute(group)) %in% names(data)) {
		data$gid <- data[[as.character(substitute(group))]]
	} else {
		data$gid <- group
	}
	
	
	empty <- structure(list(), 
		data = data, 
		group_vars = group, 
		x_major = x_major, 
		y_major = y_major, 
		model = model,
		key = xy.key,
		reorder = order(order(ens$data[["gid"]])),
		class = c("mg_ensemble", "list"))
	
	make_groups(empty)
	fill_ensemble(empty)
}

make_groups <- function(ens){
	
	ens$data$gid <- data[[ens$group$gid]]





fill_ensemble <- function(ens) {
	
	# determine grouping
	
	# fit models
	fit <- function(data, model) {
		data <- data[ , setdiff(names(data), c(ens$x_major, ens$y_major, "gid"))]
		model.args <- c(data = as.name("data"), model[setdiff(names(model), 
			"type")])	
		do.call(model$type, model.args)
	}
	models <- dlply(ens$data, "gid", fit, ens$model)
	
	# create key for translating gid into x_major and y_major
	# can be subset by gid to return x and y
	xy.key <- ddply(ens$data, "gid", 
		function(df, x_major, y_major) c(df[[x_major]][1], df[[y_major]][1]), 
		ens$x_major, ens$y_major) 
		
	structure(models,
		data = ens$data,
		group_vars = ens$group,
		x_major = ens$x_major,
		y_major = ens$y_major,
		model = ens$model,
		key = ens$xy.key,
		reorder = ens$reorder,
		class = c("mg_ensemble", "list"))
}

	
	
	
	
	
	
	
	# determine grouping
	if (is.mg_grouping(group)) {
		x_major <- group$x_major
		y_major <- group$y_major
		data$gid <- group
	} else if (as.character(substitute(group)) %in% names(data)) {
		data$gid <- data[[as.character(substitute(group))]]
	} else {
		data$gid <- group
	}
	
	# fit models
	fit <- function(data, model) {
		data <- data[ , setdiff(names(data), c(x_major, y_major, "gid"))]
		model.args <- c(data = as.name("data"), model[setdiff(names(model), 
			"type")])	
		do.call(model$type, model.args)
	}
	models <- dlply(data, "gid", fit, model)
	
	# key for translating gid into x_major and y_major
	# can be subset by gid to return x and y
	xy.key <- ddply(data, "gid", 
		function(df) c(df[[x_major]][1], df[[y_major]][1])) 
	
	structure(models, 
		method = model$type,
		formula = model$formula, 
		reorder = order(order(data$gid)),
		x = xy.key$V1,
		y = xy.key$V2,
		x_name = x_major,
		y_name = y_major,
		row.names = row.names(data),
		class = c("ensemble", "list"))
}

x_major <- function(x) attr(x, "x_major")
y_major <- function(x) attr(x, "y_major")



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


