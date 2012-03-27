#' @import util.r
#' @import autoplot.r
NULL

#' S3method model.frame mg_ensemble
model.frame.mg_ensemble <- function(formula, ...) {
	output <- ldply(formula, model.frame, ...)
	mg(output, formula, "mg_model.frame", collate = TRUE)
}

#' @S3method autoplot mg_resid
autoplot.mg_model.frame <- function(object, x.minor = NULL, y.minor = NULL, x.scale = identity, y.scale = identity, ...) {
	
	if (is.null(x.minor)) stop("missing argument: x.minor")
	stopifnot(x.minor %in% names(object))
	
	if (is.null(y.minor)) y.minor <- as.character(model_info(object)$formula)[2]
	
	
	plot.title <- paste(y.minor, "vs.", x.minor)
	
	line_plot(object, x.minor, y.minor, title = plot.title, x.scale, y.scale, ...)	
}

