#' Get/set the x_major attribute of a modelglyphs object.
#' @aliases x_major x_major<-
#' @param x An object with a mg.info attribute
#' @export x_major "x_major<-"
x_major <- function(x) {
	attr(x, "mg.info")$x.major
}

"x_major<-" <- function(x, value) {
	attr(x, "mg.info")$x.major <- value
	x
}


#' Get/set the y_major attribute of a modelglyphs object.
#'
#' @aliases y_major y_major<-
#' @param x An object with a mg.info attribute
#' @export y_major "y_major<-"
y_major <- function(x) {
	attr(x, "mg.info")$y.major
}

"y_major<-" <- function(x, value) {
	attr(x, "mg.info")$y.major <- value
	x
}

#' Get/set the key attribute of a modelglyphs object.
#'
#' @aliases keye key<-
#' @param x An object with a mg.info attribute
#' @export key "key<-"
key <- function(x) {
	attr(x, "mg.info")$key
}

"key<-" <- function(x, value) {
	attr(x, "mg.info")$key <- value
	x
}

#' return the model call of a modelglyphs object
#'
#' mg_call returns the model call of a modelglyphs object as a character string.
#'
#' @param mg A modelglyphs object. The class of a modelglyphs object will begin with "mg_" and the object will have a mg.info attribute
#' @export
mg_call <- function(mg){
	info <- attr(mg, "mg.info")
	paste(info$mod.type, "(", deparse(info$formula), ")", sep = "")
}