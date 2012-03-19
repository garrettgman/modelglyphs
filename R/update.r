#' @S3method update mg_ensemble
update.mg_ensemble <- function(object, data_set. = data_set(object), groups. = groups(object), x_major. = x_major(object), y_major. = y_major(object), model_info. = model_info(object),...) {
	# is there a better way to collect the arguments?
	
	if (length(match.call()) > 2) {
		data_set(object) <- data_set
		groups(object) <- groups
		x_major(object) <- x_major
		y_major(object) <- y_major
		model_info(object) <- model_info
	}
	
	attr(object, "data_set")$gid <- gid(object)
	collate <- order(order(attr(object, "data_set")$gid))
	key <- key(object)
	
	models <- build_models(object)
	
	
	# 4. record model info with a time stamp

	# 5. Return complete ensemble
	# faster to just save over object?	
	structure(models,
		data_set = data_set(object),
		groups = groups(object), 
		x_major = x_major(object), 
		y_major = y_major(object), 
		model_info = model(object),
		key = key,
		collate = collate,
		class = c("mg_ensemble", "list"))
}