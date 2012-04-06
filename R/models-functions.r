#' @export
gcoef <- function(mods, xy, ...) {
	UseMethod("gcoef")
}

#' @S3method gcoef lm
gcoef.lm <- function(mods, xy = TRUE, ...) {
	summs <- lapply(mods, summary)
	
	coef_df <- function(summ, ...) {
		cfs <- coef(summ, ...)
		cbind(variable = row.names(cfs), as.data.frame(cfs))
	}
	gcoefs <- ldply(summs, coef_df, ...)
	
	names(gcoefs) <- c(".gid", "variable", "estimate", "std.error", "t.value", 
		"p.value")
	gcoefs$.gid <- as.numeric(gcoefs$.gid)
	
	if (xy) gcoefs <- add_labels(gcoefs, mods)
	gcoefs
}


#' @export
gr_squared <- function(mods, adj, xy, ...){
	UseMethod("gr_squared")
}

#' @S3method gr_squared lm
gr_squared.lm <- function(mods, xy = TRUE, ...){
	summs <- lapply(mods, summary)
	
	.fun <- function(x) {
		c(r.squared = x$r.squared, adj.r.squared = x$adj.r.squared)
	}
	grs <- ldply(summs, .fun, ...)	
	
	names(grs)[1] <- c(".gid")
	grs$.gid <- as.numeric(grs$.gid)
	
	if (xy) grs <- add_labels(grs, mods)
	grs
}
	

#' @export
gaic <-function(mods, xy, ...) {
	UseMethod("gaic")
}


#' @S3method gaic lm
gaic.lm <- function(mods, xy = TRUE, ...) {
	aics <- ldply(mods, AIC, ...)
	names(aics)[2] <- "aic"
	if (xy) aics <- add_labels(aics, mods)
	aics
}


#' @export
gbic <-function(mods, xy, ...) {
	UseMethod("gbic")
}


#' @S3method gaic lm
gbic.lm <- function(mods, xy = TRUE, ...) {
	bics <- ldply(mods, BIC, ...)
	names(bics)[2] <- "bic"
	if (xy) bics <- add_labels(bics, mods)
	bics
}


#' @export
gresid <- function(mods, data, ...){
	UseMethod("gresid")
}


#' @S3method gresid lm
gresid.lm <- function(mods, data = FALSE, ...) {

	vec <- unlist(llply(mods, resid, ...), recursive = TRUE, use.names = FALSE)
	resids <- vec[collater(mods)]

	if (data) {
		cbind(orig_data(mods), resid = resids)
	} else {
		resids
	}
	
}

#' @export
gfitted <- function(mods, data, ...){
	UseMethod("gfitted")
}


#' @S3method gfitted lm
gfitted.lm <- function(mods, data = FALSE, ...) {

	vec <- unlist(llply(mods, fitted, ...), recursive = TRUE, use.names = FALSE)
	fits <- vec[collater(mods)]

	if (data) {
		cbind(orig_data(mods), fitted = fits)
	} else {
		fits
	}
	
}


#' @export
gpredict <- function(mods, newdata, xy, ...) {
	if (missing(newdata)) {
		gfitted(mods, data = TRUE, ...)
	} else {
		UseMethod("gpredict")
	}
}
	
#' @S3method gpredict lm
gpredict.lm <- function(mods, newdata, xy = TRUE, ...) {
	predict_df <- function(object, newdata, ...){
		newdata$predict <- predict(object, newdata, ...)
		newdata
	}
	df <- ldply(mods, predict_df, newdata, ...)
	
	names(df)[1] <- ".gid"
	df$.gid <- as.numeric(df$.gid)
	
	if (xy) df <- add_labels(df, mods)
	df
}