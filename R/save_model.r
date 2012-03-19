# Implicitly create new environment

save_model <- NULL
get_models <- NULL

local({
	models <- list()
	
	save_model <<- function(model) {
		models <<- c(models, model)
		invisible(TRUE)
	}
	
	get_model <<- function(i) {
		models[[i]]
	}
	
})


# Explicitly create new environment

mg_env <- new.env(parent = emptyenv())
mg_env$models <- list()

save_model <- function(model) {
	mg_env$models <- c(mg_env$models, model)
	invisible(TRUE)
}

get_model <- function(i) {
	mg_evn$models[[i]]
}


# Use a reference class
# Advantage: easier to create multiple lists of models
# Disavantage: have to know about R5 calling conventions, or create
#   save_model/get_model accessor functions

ModelList <- setRefClass(fields = c("models" = list), methods = list(
	save_model = function(model) {
		models <<- c(models, model)
		invisible(TRUE)
	},
	get_model = function(i) {
		models[[i]]
	}
))
mg_list <- ModelList$new()
mg_list$save_model()
mg_list$get_model()




