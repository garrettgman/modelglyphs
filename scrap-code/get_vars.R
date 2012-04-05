


# works
n <- 2
expr <- bquote(all.vars(substitute(.(as.name(vars)))))
eval(expr, env = parent.frame(n))



get_vars <- function(ens, ...) {

	cols <- NULL
	n <- 3
	fin <- penum <- FALSE
	
	vars <- all.vars(substitute(list(...)))
	cols <- unique(c(cols, vars[vars %in% names(ens)]))
	vars <- vars[!(vars %in% names(ens))]

	one_var <- function(var, n) {
		expr <- bquote(all.vars(substitute(.(as.name(var)))))
		eval(expr, env = parent.frame(n))
	}

	while (length(vars) > 0 && !fin) {
		vars <- unlist(lapply(vars, one_var, n = n))
		cols <- unique(c(cols, vars[vars %in% names(ens)]))
		vars <- vars[!(vars %in% names(ens))]
		fin <- penum
		penum <- environmentName(parent.frame(n - 2)) != "R_GlobalEnv"
		n <- n + 1
	}
	
	cols
	
}


vtest <- function(ens, filla, colora, sizea, ...) {
	vars_test(ens = ens, fill = filla, color = colora, size = sizea, ...)
}

vars_test <- function(ens, fill, color, size, ...) {
	get_vars(ens = ens, fill, color, size, ...)
}


vtest(e1, filla = max(temperature), colora = ozone, sizea = cloudmid - cloudlow, x = tundra)