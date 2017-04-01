# CLT Exercies
# April 1, 2017
# Glenn Kerbein

# simulations
## Set simulation vars lambda, exponentials, and seed.

ECHO=TRUE
set.seed(9001)
lambda <- 0.2
exponentials <- 40

simMeans = NULL
for (i in 1 : 1000) {
	simMeans <- c(
		simMeans,
		mean(
			rexp(
				exponentials,
				lambda
			)
		)
	)
}


