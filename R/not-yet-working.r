# add1 doesn't work because ensemble does not faithfully recreate the $call slot of the models. For example, lm1 <- lm(Fertility ~ ., data = swiss2) will have the $call lm(formula = Fertility ~ ., data = swiss2), but the analagous model in the ensemble object will have the call model(formula = formula, data = data[, -which(names(data) %in% by)])

#' S3method add1 ensemble
add1.ensemble <- function(object, scope,...){
	add1_df <- function(mod, scope, ...) {
		output <- add1(mod, scope, ...)
		data.frame(variable = attr(output, "row.names"),
			Df = output$Df,
			'Sum of Sq' = output$'Sum of Sq',
			RSS = output$RSS,
			AIC = output$AIC)
	}
	
	ldply(object, add1_df, scope, ...)
}

# I haven't found an example where alias.lm actually does something
#' S3method alias ensemble
alias.ensemble <- function(object, ...) {
}


# There's so much to do here, I'm not sure if its worth it. What would plots look like for a ton of ensembles? A facet grid?
#' S3method plot ensemble
plot.ensemble <- function(object, ...) {

}

# qr returns a list object specific to the input group. It doesn't seem like it would be useful to take part of the qr output and turn it into tidy data
#' S3method qr ensemble
qr.ensemble <- function(object, ...){
}