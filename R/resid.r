#' @import util.r
#' @import autoplot.r

#' S3method residuals mg_ensemble
#' S3method resid mg_ensemble
residuals.mg_ensemble <- resid.mg_ensemble <- resid.mg_summary <- function(object, mg = TRUE, ...){
	if (!mg) {
		llply_ensemble(object, resid, ...)
	} else {
		resid_df <- function(mod, ...) {
			data <- mod$model
			data$.resid <- resid(mod, ...)
			data
		}
		data <- ldply(object, resid_df, ...)
		mg(data, object, "mg_resid", collate = TRUE)
	}
}


#' @S3method autoplot mg_resid
autoplot.mg_resid <- function(object, x.minor = NULL, y.minor = ".resid", x.scale = identity, y.scale = identity, ...) {
	
	if (is.null(x.minor)) stop("missing argument: x.minor")
	stopifnot(x.minor %in% names(object))
	
	plot.title <- paste("Scatterplots of", y.minor, "vs.", x.minor, 
		"\n", mg_call(object))
	
	scatter_plot(object, x.minor, y.minor, x.scale, y.scale, title = plot.title, ...)	
}