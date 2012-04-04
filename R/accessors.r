#' Get/set the group.info attribute of a modelglyphs object.
#' @aliases group_info group_info<-
#' @param x An object with a group.info attribute
#' @export group_info "group_info<-"
group_info <- function(x) {
	attr(x, "group.info")
}

"group_info<-" <- function(x, value) {
	attr(x, "group.info") <- value
	x
}


#' Get/set the x_major attribute of a modelglyphs object.
#' @aliases x_major x_major<-
#' @param x An object with a group.info attribute
#' @export x_major "x_major<-"
x_major <- function(x) {
	group_info(x)$defaults[1]
}

"x_major<-" <- function(x, value) {
	group_info(x)$defaults[1] <- value
	x
}


#' Get/set the y_major attribute of a modelglyphs object.
#'
#' @aliases y_major y_major<-
#' @param x An object with a group.info attribute
#' @export y_major "y_major<-"
y_major <- function(x) {
	group_info(x)$defaults[2]
}

"y_major<-" <- function(x, value) {
	group_info(x)$defaults[2] <- value
	x
}

#' Get/set the key attribute of a modelglyphs object.
#'
#' @aliases key key<-
#' @param x An object with a group.info attribute
#' @export key "key<-"
key <- function(x) {
	group_info(x)$key
}

"key<-" <- function(x, value) {
	group_info(x)$key <- value
	x
}


#' Get/set the environment attribute of a modelglyphs object.
#'
#' @aliases enviro enviro<-
#' @param x An object with a group.info attribute
#' @export enviro "enviro<-"
enviro <- function(x) {
	group_info(x)$environment
}

"enviro<-" <- function(x, value) {
	group_info(x)$environment <- value
	x
}