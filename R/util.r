#' Add the x_major and y_major variables to a dataframe with gid
#'
#' @keywords internal
#' @export
add_labels <- function(df, ens) {
	x <- key(ens)[[x_major(ens)]][df$.gid]
	y <- key(ens)[[y_major(ens)]][df$.gid]
	
	vars <- setdiff(names(df), ".gid")
	new <- data.frame(cbind(df$.gid, x, y, df[ , vars]))
	names(new) <- c(".gid", x_major(ens), y_major(ens), vars)
	new
}