#' Create grouping variable
#'
#' gid uses the information in the groups attribute of an mg_enemble object to 
#' create a grouping variable. This variable should be saved as a column in 
#' the dataset on which group-wise modelling will be performed.
#'
#' @param ens an object of class mg_ensemble
#' @keywords internal
#' @export
gid <- function(ens) {
	groups(ens)$FUN(data_set(ens))	
}

#' Create a key to match group id's with x and y locations on a plot
#'
#' key uses the x_major, y_major, and groups attributes of an mg_ensemble 
#' object to create a dataframe that shows where each group should be plotted 
#' according to an x_major and y_major axis.
#' 
#' @param ens an object of class mg_ensemble
#' @keywords internal
#' @export
key <- function(ens) {
	
	if (!("gid" %in% names(data_set(ens)))) {
		data_set(ens)$gid <- gid(ens)
	}
	
	get_majors <- function(df) {
		c(x = df[[x_major(ens)]][1], y = df[[y_major(ens)]][1]) 
	}
	
	ddply(data_set(ens), "gid", get_majors) 
}


#' Group by the interaction of two variables
#'
#' cross creates an mg_groups object that groups a data frame by the 
#' interaction of two variables. Each row will be assigned to the group that 
#' corresponds with the unique combination of var1 and var2 values found in 
#' that row. var1 and var2 should be variables that appear in the dataframe 
#' to be grouped.
#'
#' @param var1 the name of a variable to group on written as a character string
#' @param var2 the name of a variable to group on written as a character string
#' @param drop logical should group names ignore the existence of var1 and var2 combinations that do not appear in the data frame
#' @export
cross <- function(var1, var2, drop = TRUE) {
	stopifnot(is.character(var1) & is.character(var2))
	
	FUN <- function(data, ...) {
		id(list(data[[var1]], data[[var2]]), drop)
	}

	structure(list(variables = c(var1, var2), FUN = FUN), 
		class = c("mg_group", "list"))
}