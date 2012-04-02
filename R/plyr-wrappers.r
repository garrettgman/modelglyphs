#' Create groupwise summaries for an ensemble object
#'
#' gsummarise is a wrapper for a combination of summarise and d*ply functions from plyr. It automatically uses characteristics of the ensemble object to streamline the plyr call.
#'
#' @aliases gsummarise gsummarize
#' @param ens An ensemble object
#' @param ... arguments to be passed to \code{\link{summarise}}
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel, using parallel backend provided by foreach
#' @export gsummarise gsummarize
gsummarise <- gsummarize <- function(ens, ..., .progress = "none", 
	.parallel = FALSE) {	

	if (!is.grouped(ens))
		stop("ens must be an ensemble object")
	
	if (missing(...)) {
		return(ens)
	} else if (is.data.frame(ens)) {
		data <- ddply(ens, ".gid", summarise, ..., .progress = .progress, .parallel = .parallel)
	} else {
		data <- dlply(ens, ".gid", summarise, ..., .progress = .progress, .parallel = .parallel)
	}	
	
	attr(data, "group.info") <- group_info(ens)
	class(data) <- c("grouped", "data.frame")
	data
}


#' Create groupwise transformations for an ensemble object
#'
#' gtransform is a wrapper for a combination of transform and d*ply functions from plyr. It automatically uses characteristics of the ensemble object to streamline the plyr call.
#'
#' @param ens An ensemble object
#' @param ... arguments to be passed to \code{\link{summarise}}
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel, using parallel backend provided by foreach
#' @export
gtransform <- function(ens, ..., .progress = "none", .parallel = FALSE) {	
	if (!is.grouped(ens))
		stop("ens must be an ensemble object")
	
	if (missing(...)) {
		return(ens)
	} else if (is.data.frame(ens)) {
		data <- ddply(ens, ".gid", transform, ..., .progress = .progress, .parallel = .parallel)
	} else {
		data <- dlply(ens, ".gid", transform, ..., .progress = .progress, .parallel = .parallel)
	}	
		
	attr(data, "group.info") <- group_info(ens)
	class(data) <- c("grouped", "data.frame")
	data
}

#' Create groupwise mutations for an ensemble object
#'
#' gmutate is a wrapper for a combination of mutate and d*ply functions from plyr. It automatically uses characteristics of the ensemble object to streamline the plyr call.
#'
#' @param ens An ensemble object
#' @param ... arguments to be passed to \code{\link{summarise}}
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel, using parallel backend provided by foreach
#' @export
gmutate <- function(ens, ..., .progress = "none", .parallel = FALSE) {	
	if (!is.grouped(ens))
		stop("ens must be an ensemble object")
	
	if (missing(...)) {
		return(ens)
	} else if (is.data.frame(ens)) {
		data <- ddply(ens, ".gid", mutate, ..., .progress = .progress, .parallel = .parallel)
	} else {
		data <- dlply(ens, ".gid", mutate, ..., .progress = .progress, .parallel = .parallel)
	}	
		
	attr(data, "group.info") <- group_info(ens)
	class(data) <- c("grouped", "data.frame")
	data
}


#' Perform groupwise subsetting for an ensemble object
#'
#' gsubset is a wrapper for a combination of subset and  d*ply functions from plyr. It automatically uses characteristics of the ensemble object to streamline the plyr call.
#'
#' @param ens An ensemble object
#' @param ... arguments to be passed to \code{\link{summarise}}
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel, using parallel backend provided by foreach
#' @export
gsubset <- function(ens, ..., .progress = "none", .parallel = FALSE) {	
	if (!is.grouped(ens))
		stop("ens must be an ensemble object")
	
	if (missing(...)) {
		return(ens)
	} else if (is.data.frame(ens)) {
		data <- ddply(ens, ".gid", subset, ..., .progress = .progress, .parallel = .parallel)
	} else {
		data <- dlply(ens, ".gid", subset, ..., .progress = .progress, .parallel = .parallel)
	}	
		
	attr(data, "group.info") <- group_info(ens)
	class(data) <- c("grouped", "data.frame")
	data
}

#' Perform groupwise arranging for an ensemble object
#'
#' garrange is a wrapper for a combination of arrange and d*ply functions from plyr. It automatically uses characteristics of the ensemble object to streamline the plyr call.
#'
#' @param ens An ensemble object
#' @param ... arguments to be passed to \code{\link{summarise}}
#' @param .progress name of the progress bar to use, see \code{\link{create_progress_bar}}
#' @param .parallel if TRUE, apply function in parallel, using parallel backend provided by foreach
#' @export
garrange <- function(ens, ..., .progress = "none", .parallel = FALSE) {	
	if (!is.grouped(ens))
		stop("ens must be an ensemble object")
	
	if (missing(...)) {
		return(ens)
	} else if (is.data.frame(ens)) {
		data <- ddply(ens, ".gid", arrange, ..., .progress = .progress, .parallel = .parallel)
	} else {
		data <- dlply(ens, ".gid", arrange, ..., .progress = .progress, .parallel = .parallel)
	}	
	
	attr(data, "group.info") <- group_info(ens)
	class(data) <- c("grouped", "data.frame")
	data
}