#' Create groupwise summaries for an ensemble object
#'
#' groupwise is a wrapper for d*ply functions from plyr. It automatically uses characteristics of the ensemble object to streamline the plyr call.
#'
#' @param ens An ensemble object
#' @export
groupwise <- function(ens, ...) {	
	if (!inherits(ens, "groups"))
		stop("ens must be an ensemble object")
	
	if (missing(...)) {
		return(ens)
	} else if (inherits(ens, "data.frame")) {
		ddply(ens, ".gid", summarise, ...)
	} else {
		dlply(ens, ".gid", summarise, ...)
	}	
}


#' Create groupwise transformations for an ensemble object
#'
#' groupwise is a wrapper for d*ply functions from plyr. It automatically uses characteristics of the ensemble object to streamline the plyr call.
#'
#' @param ens An ensemble object
#' @export
withgroups <- function(ens, ...) {	
	if (!inherits(ens, "groups"))
		stop("ens must be an ensemble object")
	
	if (missing(...)) {
		return(ens)
	} else if (inherits(ens, "data.frame")) {
		ddply(ens, ".gid", transform, ...)
	} else {
		dlply(ens, ".gid", transform, ...)
	}	
}