#' @include model.r
NULL

#' Performs group-wise maodelling on an mg_ensemble object
#'
#' build_models fits the model specified by an mg_ensemble's model_info
#' attribute to the data sepcified by the mg_ensemble's data_set attribute 
#' according to the groupings suggested by the dat_set's gid variable. These 
#' groupings align with the grouping information stored in the mg_ensemble 
#' object's groupa attribute.
#'
#' @param ensemble An mg_ensemble object
#' @keywords internal
#' @export 
build_models <- function(ensemble) {
	data <- exclude_group_vars(ensemble)
	FUN <- fit_model(model_info(ensemble))
	dlply(data, "gid", FUN) 
}


fit_model <- function(model) {
	stopifnot(is.model(model))

	model_args <- c(list(
	    get(model$FUN, envir = .GlobalEnv), 
	    formula = model$formula, 
		data = as.name("data")), model$args)
	model_call <- as.call(model_args)

	function(data) {
		eval(model_call)
	}
}

exclude_group_vars <- function(ens) {
	# note: still would be better to just test which variables co-vary with gid
	data_set(ens)[, setdiff(names(data_set(ens)), groups(ens)[["variables"]])]
}
	