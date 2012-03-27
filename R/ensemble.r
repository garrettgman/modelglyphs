# e1 <- ensemble(nasa, cross("long", "lat"))
# m1 <- fit_models(e1, lm, formula = surftemp ~ month*year)
# c1 <- coef(m1)
# coef_plot(c1)
# coef_plot(m1)


#' Ensemble organizes a data set into an ensemble of sub-data sets.
#'
#' @param data a data frame to organize as an ensemble of sub data sets
#' @param grouping a mg_group object to use for splitting data into subsets
#' @param x.major a character string that specifies the variable to be used on the x axis when plotting the ensembles. Defaults to the first grouping variable.
#' @param y.major a character string that specifies the variable to be used on the y axis when plotting the ensembles. Defaults to the second grouping variable.
#' 
#' @return an S3 ensemble object. 

#' @export
ensemble <- function(data, grouping, x.major = NULL, y.major = NULL){
	
	if (!inherits(grouping, "mg_group")) 
		stop("grouping must be an mg_group object")
		
	data$.gid <- grouping$FUN(data)
	
	if (is.null(x.major)) x.major <- grouping$variables[1]
	if (is.null(y.major)) y.major <- grouping$variables[2]
	
	key <- make_key(data, x.major, y.major)
	
	mg.info <- list(x.major = x.major,
					y.major = y.major,
					key = key)
					
	structure(data, mg.info = mg.info, class = c("mg_ensemble", "data.frame"))
}


#' Create a key to match group id's with x and y locations on a plot
#'
#' make_key uses the x.major and y.major variables of an mg_ensemble 
#' object to create a dataframe that shows where each group should be plotted 
#' according to an x.major and y.major axis.
#' 
#' @param data A data frame with a .gid variable
#' @param x.major The name of the x.major variable, as a character string
#' @param y.major The name of the y.major variable, as a character string
#' @keywords internal
#' @export
make_key <- function(data, x.major, y.major) {
	
	get_majors <- function(df) {
		c(x = df[[x.major]][1], y = df[[y.major]][1]) 
	}
	
	key <- ddply(data, ".gid", get_majors) 
	names(key)[2:3] <- c(x.major, y.major)
	key
}