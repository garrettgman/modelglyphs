mg_env <- new.env(parent = emptyenv())
mg_env$models <- list()

#' Record a model to the list of explored models
#'
#' @param model An mg_model object. Usually this will be taken from an mg_ensemble object.
save_model <- function(model) {
	mg_env$models <- c(mg_env$models, model)
	invisible(TRUE)
}

#' Retrieve a model from the list of explored models
#'
#' get_model retrieves the ith model from the list of ensemble models tried in the current R session. This list is maintained by the modelglyphs package.
#'
#' @param i An integer. The index of the model to be returned.
get_model <- function(i) {
	mg_evn$models[[i]]
}

#' Retrieve the list of explored models
#'
#' get_models returns the entire list of ensemble models tried in the current R session. This list is maintained by the modelglyphs package.
#'
#' @param i An integer. The index of the model to be returned.
get_models <- function(i) {
	mg_evn$models
}

#' Remove a model from the list of explored models
#'
#' remove_model removes the ith model from the list of ensemble models tried in the current R session. This list is maintained by the modelglyphs package.
#'
#' @param i An integer. The index of the model to be removed.
remove_model <- function(i) {
	mg_evn$models[[i]]
}




# Implicitly create new environment

# save_model <- NULL
# get_models <- NULL

# local({
#	models <- list()
	
#	save_model <<- function(model) {
#		models <<- c(models, model)
#		invisible(TRUE)
#	}
	
#	get_model <<- function(i) {
#		models[[i]]
#	}
	
#})


# Use a reference class
# Advantage: easier to create multiple lists of models
# Disavantage: have to know about R5 calling conventions, or create
#   save_model/get_model accessor functions

#ModelList <- setRefClass(fields = c("models" = list), methods = list(
#	save_model = function(model) {
#		models <<- c(models, model)
#		invisible(TRUE)
#	},
#	get_model = function(i) {
#		models[[i]]
#	}
#))
#mg_list <- ModelList$new()
#mg_list$save_model()
#mg_list$get_model()




