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
	as.name(group_info(x)$environment)
}

"enviro<-" <- function(x, value) {
	group_info(x)$environment <- value
	x
}

#' Get/set the n.mod variable of a modelglyphs object.
#'
#' @aliases n_mod n_mod<-
#' @param x An object with a group.info attribute
#' @export n_mod "n_mod<-"
n_mod <- function(x) {
	env <- enviro(x)
	eval(bquote(.(env)$n.mod), envir = globalenv())
}

"n_mod<-" <- function(x, value) {
	assignInGroupspace("n.mod", value, x)
	x
}


#' Get/set the collater variable of a modelglyphs object.
#'
#' @aliases collater collater<-
#' @param x An object with a group.info attribute
#' @export collater "collater<-"
collater <- function(x) {
	env <- enviro(x)
	eval(bquote(.(env)$collater), envir = globalenv())
}

"collater<-" <- function(x, value) {
	assignInGroupspace("collater", value, x)
	x
}

#' Get the data set that a modelglyphs object was originally constructed from.
#'
#' @param x An object with a group.info attribute
#' @export
orig_data <- function(x) {
	env <- enviro(x)
	eval(bquote(.(env)$data), envir = globalenv())
}


#' Assign a variable in the environment associated with a grouped data object
#'
#' @keywords internal
#' @param x A character string. The name of the variable to be assigned
#' @param value The value to be assigned to x
#' @param ens The object of class 'grouped'. x will be assigned in the environment specified by the environment slot of ens's group.info attribute
#'
#' @export
assignInGroupspace <- function(x, value, ens) {
	if (!is.grouped(ens))
		stop("assignInGroupspace requires ens to be of class 'grouped'")
	env <- enviro(ens)
	expr <- bquote(.(env)[[.(x)]] <- .(value))
	eval(expr, envir = globalenv())
}

#' Get/set the model.info attribute of a modelglyphs object.
#' @aliases model_info model_info<-
#' @param x An object with a model.info attribute
#' @export model_info "model_info<-"
model_info <- function(x) {
	attr(x, "model.info")
}

"model_info<-" <- function(x, value) {
	attr(x, "model.info") <- value
	x
}

#' Get/set the model type attribute of a modelglyphs object.
#' @aliases m_type m_type<-
#' @param x An object with a model.info attribute
#' @export m_type "m_type<-"
m_type <- function(x) {
	model_info(x)$type
}

"m_type<-" <- function(x, value) {
	model_info(x)$type <- value
	x
}

#' Get/set the model formula attribute of a modelglyphs object.
#' @aliases m_formula m_formula<-
#' @param x An object with a model.info attribute
#' @export m_formula "m_formula<-"
m_formula <- function(x) {
	model_info(x)$formula
}

"m_formula<-" <- function(x, value) {
	model_info(x)$formula <- value
	x
}

#' Get/set the model time attribute of a modelglyphs object.
#' @aliases m_time m_time<-
#' @param x An object with a model.info attribute
#' @export m_time "m_time<-"
m_time <- function(x) {
	model_info(x)$time
}

"m_time<-" <- function(x, value) {
	model_info(x)$time <- value
	x
}

