#' S3method coef mg_ensemble
coef.mg_models <- function(object, ...){
	coefs <- ldply(object, coef)
	coefs <- reshape2::melt(coefs, id = "gid", value.name = "coefficient")
	mg(coefs, object, "mg_coef")
}

#' S3method coef mg_lm
coef.mg_lm <- function(object, ...){
	coef_df <- function(mods, ...) {
		cfs <- coef(summary(mods))
		cbind(variable = row.names(cfs), as.data.frame(cfs))
	}
	output <- ldply(object, coef_df, ...)
	mg(output, object, "mg_coef")
}