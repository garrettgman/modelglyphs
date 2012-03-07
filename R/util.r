#' Returns a data.frame with new names
#'
#' @keywords internal
#' @export
renamed <- function(df, index, name) {
	names(df)[index] <- name
	df
}