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
