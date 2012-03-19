#' S3method AIC mg_ensemble
AIC.mg_ensemble <- function(object, ..., k = 2) {
	aics <- ldply(object, AIC, ..., k)
	renamed(aics, 2, "aic")
}


#' S3method anova mg_ensemble
anova.mg_ensemble <- function(object, ...){
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

#' S3method case.names mg_ensemble
case.names.mg_ensemble <- function(object, ...){
	llply_ensemble(object, case.names, ...)
}

#' S3method coef mg_ensemble
coef.mg_ensemble <- function(object, ...){
	coefs <- ldply(object, coef)
	coefs <- reshape2::melt(coefs, id = "gid", value.name = "coefficient")
	add_labels(coefs, object)
}

#' S3method confint mg_ensemble
confint.mg_ensemble <- function(object, ...){
	get_conf <- function(mod, ...) {
		int <- as.data.frame(confint(mod, ...))
		int$variable <- row.names(int)
		int[, c(3,1,2)]
	}
	df <- ldply(object, get_conf, ...)
	add_labels(df, object)
}

#' S3method cooks.distance mg_ensemble
cooks.distance.mg_ensemble <- function(model, ...){
	llply_ensemble(model, cooks.distance, ...)
}

#' S3method deviance mg_ensemble
deviance.mg_ensemble <- function(object, ...) {
	devs <- ldply(object, deviance, ...)
	df <- renamed(devs, 2, "deviance")
	add_labels(df, object)
}

#' S3method dfbeta mg_ensemble
dfbeta.mg_ensemble <- function(model, ...) {
	dfbeta_df <- function(model, ...) {
		as.data.frame(dfbeta(model))
	}
	ldply_ensemble(model, dfbeta_df, ...)
}

#' S3method dfbetas mg_ensemble
dfbetas.mg_ensemble <- function(model, ...) {
	dfbetas_df <- function(model, ...) {
		as.data.frame(dfbetas(model))
	}
	ldply_ensemble(model, dfbetas_df, ...)
}

#' S3method drop1 mg_ensemble
drop1.mg_ensemble <- function(object, ...) {
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

#' S3method dummy.coef mg_ensemble
dummy.coef.mg_ensemble <- function(object, ...) {
	dummy_df <- function(mod, ...) {
		dlist <- dummy.coef(mod, ...)
		data.frame(variable = names(dlist), coef = unlist(dlist))
	}
	df <- ldply(object, dummy_df, ...)
	add_labels(df, object)
}

#' S3method effects mg_ensemble
effects.mg_ensemble <- function(object, ...) {
	effects_df <- function(mod, ...) {
		output <- effects(mod, ...)
		data.frame(dimension = names(output), effect = as.numeric(output))
	}
	df <- ldply(object, effects_df, ...)
	add_labels(df, object)
}

#' S3method extractAIC mg_ensemble
extractAIC.mg_ensemble <- function(fit, scale = 0, k = 2, ...) {
	extract_df <- function(fit, scale, k, ...) {
		avec <- extractAIC(fit, scale, k, ...)
		data.frame(edf = avec[1], aic = avec[2])
	}
	df <- ldply(fit, extract_df, scale, k = 2, ...)
	add_labels(df, fit)
}


#' S3method family mg_ensemble
family.mg_ensemble <- function(object, ...) {
	family(object[[1]], ...)
}

#' S3method fitted mg_ensemble
fitted.mg_ensemble <- function(object, ...){
	llply_ensemble(object, fitted, ...)}

#' S3method formula mg_ensemble
formula.mg_ensemble <- function(x, ...) {
	formula(x[[1]], ...)
}

#' S3method fortify mg_ensemble
fortify.mg_ensemble <- function(model, data = NULL, ...) {
	ldply_ensemble(model, fortify, ...)
}

#' S3method hatvalues mg_ensemble
hatvalues.mg_ensemble <- function(model, ...) {
	llply_ensemble(model, hatvalues, ...)
}

#' S3method influence mg_ensemble
influence.mg_ensemble <- function(model, ...) {
	influence_df <- function(model, ...) {
		ins <- influence(model, ...)
		data.frame(hat = ins$hat,
			sigma = ins$sigma,
			wt.res = ins$wt.res,
			as.data.frame(ins$coefficients)
		)
	}
	ldply_ensemble(model, influence_df, ...)
}

#' S3method kappa mg_ensemble
kappa.mg_ensemble <- function(z, ...) {
	kaps <- ldply(z, kappa, ...)
	renamed(kaps, 2, "kappa")
}

#' S3method logLik mg_ensemble
logLik.mg_ensemble <- function(object, ...) {
	lls <- ldply(object, logLik, ...)
	renamed(lls, 2, "logLik")
}

#' S3method model.frame mg_ensemble
model.frame.mg_ensemble <- function(formula, ...) {
	ldply_ensemble(formula, model.frame, ...)
}

#' S3method model.matrix mg_ensemble
model.matrix.mg_ensemble <- function(object, ...) {
	model.matrix_df <- function(object, ...) {
		df <- data.frame(model.matrix(object, ...))
		names(df)[names(df) == "X.Intercept."] <- "(Intercept)"
		df
	}
	ldply_ensemble(object, model.matrix_df, ...)
}

#' S3method nobs mg_ensemble
nobs.mg_ensemble <- function(object, ...) {
	obns <- ldply(object, nobs, ...)
	renamed(obns, 2, "nobs")
}

#' S3method predict mg_ensemble
predict.mg_ensemble <- function(object, newdata = NULL, ...){
	if (is.null(match.call()$newdata)) {
		llply_ensemble(object, predict, ...)
	} else {
		predict_df <- function(object, newdata, ...){
			newdata$.predict <- predict(object, newdata, ...)
			newdata
		}
		df <- ldply(object, predict_df, newdata, ...)
		add_labels(df, object)
	}	
}

#' S3method proj mg_ensemble
proj.mg_ensemble <- function(object, ...){
	proj_df <- function(object, ...) {
		df <- data.frame(proj(object, ...))
		names(df)[names(df) == "X.Intercept."] <- "(Intercept)"
		df
	}
	ldply_ensemble(object, proj_df, ...)
}

#' S3method residuals mg_ensemble
residuals.mg_ensemble <- function(object, ...){
	llply_ensemble(object, resid, ...)
}


#' S3method rstandard mg_ensemble
rstandard.mg_ensemble <- function(model, ...){
	llply_ensemble(model, rstandard, ...)
}

#' S3method rstudent mg_ensemble
rstudent.mg_ensemble <- function(model, ...){
	llply_ensemble(model, rstudent, ...)
}

#' S3method simulate mg_ensemble
simulate.mg_ensemble <- function(object, ...){
	ldply_ensemble(object, simulate, ...)
}

#' S3method summary mg_ensemble
summary.mg_ensemble <- function(object, ...){
	lapply(object, summary, ...)
}

#' S3method variable.names mg_ensemble
variable.names.mg_ensemble <- function(object, ...){
	variable.names(object[[1]], ...)
}

#' S3method vcov mg_ensemble
vcov.mg_ensemble <- function(object, ...){
	lapply(object, vcov, ...)
}

#' S3method weights mg_ensemble
weights.mg_ensemble <- function(object, ...){
	llply_ensemble(object, weights, ...)
}

# note: weighted.residuals works as is so long as weights.mg_ensemble and 
# residuals.mg_ensemble are loaded