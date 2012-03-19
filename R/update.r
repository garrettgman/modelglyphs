#' @S3method update mg_ensemble
update.mg_ensemble <- function(object, data_set = NULL, groups = NULL, x_major = NULL, y_major = NULL, model_info = NULL,...) {
	# is there a better way to collect the arguments?
	
	if (!is.null(data_set)) data_set(object) <- data_set
	if (!is.null(groups))	groups(object) <- groups
	if (!is.null(x_major))	x_major(object) <- x_major
	if (!is.null(y_major))	y_major(object) <- y_major
	if (!is.null(model_info))	model_info(object) <- model_info
	
	attr(object, "data_set")$gid <- gid(object)
	collate <- order(order(attr(object, "data_set")$gid))
	key <- key(object)
	
	models <- build_models(object)
	attribute(models) <- attributes(object)
	models
	
	models[] <- build_models(object)
	
	# 4. record model info with a time stamp

	# 5. Return complete ensemble
	# faster to just save over object?	
	# structure(models,
	#	data_set = data_set(object),
	#	groups = groups(object), 
	#	x_major = x_major(object), 
	#	y_major = y_major(object), 
	#	model_info = model(object),
	#	key = key,
	#	collate = collate,
	#	class = c("mg_ensemble", "list"))
}