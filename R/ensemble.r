# e1 <- ensemble(nasa, cross("long", "lat"))
# s1 <- gsummarise(e1, max.temp = max(temperature))
# t1 <- gtransform(e1, max.temp = max(temperature), op = ozone / pressure)
# m1 <- gmutate(e1, cloud.rng = cloudhigh - cloudlow, norm.rng = cloud.rng / cloudmid)
# s2 <- gsubset(e1, temperature == max(temperature))
# fill_plot(e1, fill = max(temperature))
# gplot(e1, fill = max(temperature), geom = "tile")
# scatter_plot(e1, x.minor = temperature, y.minor = cloudmid, size = I(1/2), x_scale = rescale11, y_scale = rescale11)
# gplot(e1, x.minor = temperature, y.minor = cloudmid, size = I(1/2), x_scale = rescale11, y_scale = rescale11)
# dot_plot(e1, size = max(abs(surftemp - temperature)), color = max(surftemp - temperature) == max(abs(surftemp - temperature)))
# gplot(e1, size = max(abs(surftemp - temperature)), color = max(surftemp - temperature) == max(abs(surftemp - temperature)))
# dot_plot(e1, size = max(abs(surftemp - temperature)), color = max(surftemp - temperature) == max(abs(surftemp - temperature)), x.major = mean(temperature), y.major = mean(surftemp))
# gplot(e1, size = max(abs(surftemp - temperature)), color = max(surftemp - temperature) == max(abs(surftemp - temperature)), x.major = mean(temperature), y.major = mean(surftemp))


# gplot(e1, size = max(abs(surftemp - temperature)), color = mean(surftemp) - mean(temperature), geom = "point")


# dot_plot(e1, x.major = mean(temperature), y.major = max(abs(surftemp - temperature)))
# line_plot(e1, x.minor = temperature, y.minor = cloudmid, size = I(1/2), x_scale = rescale11, y_scale = rescale11)
# to do :line_plot(e1, x.minor = fivenum(temperature), y.minor = fivenum(surftemp), size = I(1/2), x_scale = rescale11, y_scale = rescale11)
# smooth_plot(e1, x.minor = temperature, y.minor = cloudmid, size = I(1/2), x_scale = rescale11, y_scale = rescale11, se = FALSE)

# m1 <- fit_models(e1, lm, formula = surftemp ~ month*year)
# c1 <- coef(m1)
# coef_plot(c1)
# coef_plot(m1)
# fill_plot(e1, fill = max(temperature))
# scatter_plot(e1, "temperature", "cloudmid")


#' Ensemble organizes a data set into an ensemble of sub-data sets.
#'
#' @param data a data frame to organize as an ensemble of sub data sets
#' @param grouping a mg_group object to use for splitting data into subsets
#' @param x.major a character string that specifies the default variable to be used on the x axis when plotting the ensembles. Defaults to the first grouping variable.
#' @param y.major a character string that specifies the default variable to be used on the y axis when plotting the ensembles. Defaults to the second grouping variable.
#' @param a data.frame that specifes group specific information for each subset in the ensemble. The key must contain a .gid variable that lists group membership as well as the x.major and y.major variables specified for the group. It may also contain additional variables. If no key is provided, ensemble() will generate one that records the relationship between .gid, x.major, and y.major for each group.
#' 
#' @return an S3 ensemble object. 

#' @export
ensemble <- function(data, grouping, x.major = NULL, y.major = NULL, key = NULL){
	
	if (!inherits(grouping, "mg_group")) 
		stop("grouping must be an mg_group object")
		
	data$.gid <- grouping$FUN(data)
	
	if (is.null(x.major)) x.major <- grouping$variables[1]
	if (is.null(y.major)) y.major <- grouping$variables[2]
	if (is.null(key)) {
		key <- make_key(data, x.major, y.major)
	} else if (!(".gid" %in% names(key))) {
		stop("key must contain a .gid column")	
	} else if (!all(c(x.major, y.major) %in% names(key))) {
		key1 <- make_key(data, x.major, y.major)
		key <- join(key1, key[, setdiff(names(key), names(key1))], 
			by = ".gid", type = "full")
	}
					
	structure(data, group.info = list(defaults = c(x.major, y.major), key = key), 
		class = c("grouped", "data.frame"))
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


#' Is x a modelglyphs object?
#'
#' is.mg tests whether an object inherits from a modelglyphs class. Such objects should all contain a data set with a gid variable and have the following attributes: model_info, key, x_major, y_major.
#'
#' @param x An object to be tested for membership in a modelglyphs class
#'
#' @export
is.grouped <- function(x) {
	inherits(x, "grouped")
}
