entest <- function(data, env) {
	print("entest = ")
	print(env)
	1
}

ventest <- function(x) {
	env <- parent.frame()
	print("env =")
	print(env)
	print("ventest =")
	print(environment())
	UseMethod("ventest")
}

ventest.a <- function(x) {
	print("ventest.a =")
	print(environment())
	NextMethod(x, env = env)
}

ventest.b <- function(x, env) {
	print("ventest.b =")
	print(ls())
	print(environment())

	ldply(x, entest, env)
}

groupwise <- function(ens, .fun, by = .gid, ...) {
	args.env <- parent.frame()
	NextMethod("groupwise", enclos = args.env)
}

groupwise.data.frame <- function(ens, .fun, by, enclos, ...) {
	to.apply <- function(data) {
		eval(substitute(.fun), data, enclos)
	}
	results <- ddply(ens, by, to.apply)
	mg(results, ens)
}

groupwise.list <- function(ens, .fun, by, enclos) {
	to.apply <- function(data) {
		eval(substitute(.fun), data, enclos)
	}
	results <- dlply(ens, to.apply)
	mg(results, ens)
}