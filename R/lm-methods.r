add_labels <- function(df, ens) {
	x <- attr(ens, "x")[df$gid]
	y <- attr(ens, "y")[df$gid]
	
	vars <- setdiff(names(df), "gid")
	new <- data.frame(cbind(df$gid, x, y, df[ , vars]))
	names(new) <- c("gid", attr(ens, "x_name"), attr(ens, "y_name"), vars)
	new
}


#' S3method AIC ensemble
AIC.ensemble <- function(object, ..., k = 2) {
	aics <- ldply(object, AIC, ..., k)
	renamed(aics, 2, "aic")
}


#' S3method anova ensemble
anova.ensemble <- function(object, ...){
	anova_df <- function(mod, ...) {
		output <- anova(mod, ...)
		data.frame(variable = attr(output, "row.names"),
			Df = output$Df,
			'Sum Sq' = output$'Sum Sq',
			'Mean Sq' = output$'Mean Sq',
			'F value' = output$'F value',
			'P value' = output$'Pr(>F)'
		)
	}
	
	df <- ldply(object, anova_df, ...)
	add_labels(df, object)
}

#' S3method case.names ensemble
case.names.ensemble <- function(object, ...){
	cases <- unlist(llply(object, case.names, ...))
	cases[attr(object, "reorder")]
}

#' S3method coef ensemble
coef.ensemble <- function(object, ...){
	coefs <- ldply(object, coef)
	coefs <- reshape2::melt(coefs, id = "gid", value.name = "coefficient")
	add_labels(coefs, object)
}

#' S3method confint ensemble
confint.ensemble <- function(object, ...){
	get_conf <- function(mod, ...) {
		int <- as.data.frame(confint(mod, ...))
		int$variable <- row.names(int)
		int[, c(3,1,2)]
	}
	df <- ldply(object, get_conf, ...)
	add_labels(df, object)
}

#' S3method cooks.distance ensemble
cooks.distance.ensemble <- function(model, ...){
	cooks <- unlist(llply(model, cooks.distance, ...))
	cooks[attr(model, "reorder")]
}

#' S3method deviance ensemble
deviance.ensemble <- function(object, ...) {
	devs <- ldply(object, deviance, ...)
	df <- renamed(devs, 2, "deviance")
	add_labels(df, object)
}

#' S3method dfbeta ensemble
dfbeta.ensemble <- function(model, ...) {
	dfbeta_df <- function(model, ...) {
		as.data.frame(dfbeta(model))
	}
	dfbs <- llply(model, dfbeta_df, ...)
	do.call("rbind", dfbs)[attr(model, "reorder"), ]
}

#' S3method dfbetas ensemble
dfbetas.ensemble <- function(model, ...) {
	dfbetas_df <- function(model, ...) {
		as.data.frame(dfbetas(model))
	}
	dfbs <- llply(model, dfbetas_df, ...)
	do.call("rbind", dfbs)[attr(model, "reorder"), ]
}

#' S3method drop1 ensemble
drop1.ensemble <- function(object, ...) {
	drop1_df <- function(mod, ...) {
		output <- drop1(mod, ...)
		data.frame(variable = attr(output, "row.names"),
			Df = output$Df,
			'Sum of Sq' = output$'Sum of Sq',
			RSS = output$RSS,
			AIC = output$AIC)
	}
	
	df <- ldply(object, drop1_df, ...)
	add_labels(df, object)
}

#' S3method dummy.coef ensemble
dummy.coef.ensemble <- function(object, ...) {
	dummy_df <- function(mod, ...) {
		dlist <- dummy.coef(mod, ...)
		data.frame(variable = names(dlist), coef = unlist(dlist))
	}
	ldply(object, dummy_df, ...)
}

#' S3method effects ensemble
effects.ensemble <- function(object, ...) {
	effects_df <- function(mod, ...) {
		output <- effects(mod, ...)
		data.frame(dimension = names(output), effect = as.numeric(output))
	}
	ldply(object, effects_df, ...)
}

#' S3method extractAIC ensemble
extractAIC.ensemble <- function(fit, scale = 0, k = 2, ...) {
	extract_df <- function(fit, scale, k, ...) {
		avec <- extractAIC(fit, scale, k, ...)
		data.frame(edf = avec[1], aic = avec[2])
	}
	ldply(fit, extract_df, scale, k = 2, ...)
}


#' S3method family ensemble
family.ensemble <- function(object, ...) {
	family(object[[1]], ...)
}

#' S3method fitted ensemble
fitted.ensemble <- function(object, ...){
	fits <- unlist(llply(object, fitted, ...), recursive = TRUE,
		use.names = FALSE)
	fits[attr(object, "reorder")]
}

#' S3method formula ensemble
formula.ensemble <- function(x, ...) {
	formula(x[[1]], ...)
}

#' S3method fortify ensemble
fortify.ensemble <- function(model, data = NULL, ...) {
	forts <- llply(model, fortify, ...)
	do.call("rbind", forts)[attr(model, "reorder"), ]
}

#' S3method hatvalues ensemble
hatvalues.ensemble <- function(model, ...) {
	hats <- unlist(llply(model, hatvalues, ...))
	hats[attr(model, "reorder")]
}

#' S3method influence ensemble
influence.ensemble <- function(model, ...) {
	influence_df <- function(model, ...) {
		ins <- influence(model, ...)
		data.frame(hat = ins$hat,
			sigma = ins$sigma,
			wt.res = ins$wt.res,
			as.data.frame(ins$coefficients)
		)
	}
	infs <- llply(model, influence_df, ...)
	do.call("rbind", infs)[attr(model, "reorder"), ]
}

#' S3method kappa ensemble
kappa.ensemble <- function(z, ...) {
	kaps <- ldply(z, kappa, ...)
	renamed(kaps, 2, "kappa")
}

#' S3method logLik ensemble
logLik.ensemble <- function(object, ...) {
	lls <- ldply(object, logLik, ...)
	renamed(lls, 2, "logLik")
}

#' S3method model.frame ensemble
model.frame.ensemble <- function(formula, ...) {
	frames <- llply(formula, model.frame, ...)
	do.call("rbind", frames)[attr(formula, "reorder"), ]
}

#' S3method model.matrix ensemble
model.matrix.ensemble <- function(object, ...) {
	mats <- llply(object, model.matrix, ...)
	do.call("rbind", mats)[attr(object, "reorder"), ]
}

#' S3method nobs ensemble
nobs.ensemble <- function(object, ...) {
	obns <- ldply(object, nobs, ...)
	renamed(obns, 2, "nobs")
}

#' S3method predict ensemble
predict.ensemble <- function(object, ...){
	predicts <- unlist(llply(object, predict, ...), recursive = TRUE,
		use.names = FALSE)
	predicts[attr(object, "reorder")]
}

#' S3method proj ensemble
proj.ensemble <- function(object, ...){
	projs <- llply(object, proj, ...)
	do.call("rbind", projs)[attr(object, "reorder"), ]
}

#' S3method residuals ensemble
residuals.ensemble <- function(object, ...){
	resids <- unlist(llply(object, resid, ...), recursive = TRUE,
		use.names = TRUE)
	resids[attr(object, "reorder")]
}


#' S3method rstandard ensemble
rstandard.ensemble <- function(model, ...){
	rs <- unlist(llply(model, rstandard, ...))
	rs[attr(model, "reorder")]
}

#' S3method rstudent ensemble
rstudent.ensemble <- function(model, ...){
	rs <- unlist(llply(model, rstudent, ...))
	rs[attr(model, "reorder")]
}

#' S3method simulate ensemble
simulate.ensemble <- function(object, nsim = 1, seed = NULL, ...){
	sims <- unlist(llply(object, simulate, ...))
	sims[attr(object, "reorder")]
}

#' S3method summary ensemble
summary.ensemble <- function(object, ...){
	lapply(object, summary, ...)
}

#' S3method variable.names ensemble
variable.names.ensemble <- function(object, ...){
	variable.names(object[[1]], ...)
}

#' S3method vcov ensemble
vcov.ensemble <- function(object, ...){
	lapply(object, vcov, ...)
}

#' S3method weights ensemble
weights.ensemble <- function(object, ...){
	ws <- unlist(llply(object, weights, ...), recursive = TRUE,
		use.names = FALSE)
	ws[attr(object, "reorder")]
}

# note: weighted.residuals works as is so long as weights.ensemble and 
# residuals.ensemble are loaded