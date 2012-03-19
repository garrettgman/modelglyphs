#' @S3method update mg_ensemble
update.mg_ensemble <- function(object, data_set = NULL, groups = NULL, x_major = NULL, y_major = NULL, model_info = NULL,...) {
	
	if (!is.null(data_set)) data_set(object) <- data_set
	if (!is.null(groups))	groups(object) <- groups
	if (!is.null(x_major))	x_major(object) <- x_major
	if (!is.null(y_major))	y_major(object) <- y_major
	if (!is.null(model_info))	model_info(object) <- model_info
	
	attr(object, "data_set")$gid <- gid(object)
	attr(object, "collate") <- order(order(attr(object, "data_set")$gid))
	attr(object, "key") <- make_key(object)
	
	models <- build_models(object)
	attributes(models) <- c(attributes(models), attributes(object))
	models
}