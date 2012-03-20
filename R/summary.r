#' S3method summary mg_ensemble
summary.mg_ensemble <- function(object, ...){
	output <- lapply(object, summary, ...)
	attributes(output) <- attributes(object)
	add_class(output, "mg_summary")
}

#' S3method format mg_summary
format.mg_summary <- function(x, ...) {	
	cat(paste("summaries of", length(x), "models:"), 
		paste("\nmethod =", model_info(x)$FUN, "\nformula =", 
		deparse(model_info(x)$formula)))
}

#' @S3method print mg_summary
print.mg_ensemble <- function(x, ...) {
	format(x)
}